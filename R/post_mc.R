library("conflicted")
library("here")
library("dplyr")
library("tidyr")
library("purrr")
library("stringr")
library("forcats")
library("tidybayes")
library("ggplot2")

conflict_prefer("filter", "dplyr")

###################
#    MCMC data    #
###################

mcmc = readr::read_rds(here("data/models/mcmc_lean.rds"))
# or mcmc_init.rds

# ----- crosswalk to group and time labels ----------

cw = readr::read_rds(here("data", "models", "mcmc_factor_crosswalk.rds"))


#######################################
#    long stack of posterior draws    #
#######################################

# one row per parameter, per draw

# use crosswalk to add semantic labels for cycle and "outcome" code
# we are hard-coding the group factorization... might want to relax that

# same as item = `[[`
item = function(x, i) {
    x[[i]]
}

draws =
    gather_draws(mcmc, theta[t, group]) |>
    ungroup() |>
    mutate(
        group_label = cw$group[group],
        cycle_label = cw$t[t] |> as.character() |> as.integer()
    ) |>
    mutate(
        spl = str_split(group_label, ":"),
        gender = map_chr(spl, item, 1),
        party = map_chr(spl, item, 2),
        vote_outcome = map_chr(spl, item, 3),
    ) |>
    select(-t, -group, -.chain, -.variable, -.iteration, -spl)


##########################################
#    summary stats of draws per param    #
##########################################

posterior_stats = draws |>
    summarize(
        mean = mean(.value),
        q05 = quantile(.value, .05),
        q95 = quantile(.value, .95),
        .by = c(group_label, cycle_label)
    ) |>
    mutate(
        split = stringr::str_split(group_label, ":"),
        gender = map_chr(split, item, 1),
        party = map_chr(split, item, 2),
        vote_choice = map_chr(split, item, 3),
    ) |>
    select(-split)

ggplot(posterior_stats) +
    aes(x = cycle_label, y = mean, color = gender, fill = gender) +
    facet_grid(vote_choice ~ party) +
    geom_ribbon(aes(ymin = q05, ymax = q95), alpha = .2, color = NA) +
    geom_line() +
    geom_point() +
    labs(
        title = "Posterior means and 90% quantile intervals",
        subtitle = "Column = PID, row = vote outcome"
    )


#######################
#    compute terms    #
#######################

# long in gender, wide in party x term
terms = draws |>
    group_by(cycle_label, gender, .draw) |>
    summarize(
        dem_partisanship = sum(.value[party == "Dem"]),
        rep_partisanship = sum(.value[party == "Rep"]),
        dem_loyalty = sum(.value[party == "Dem" & vote_outcome == "Dem Cand"]),
        rep_loyalty = sum(.value[party == "Rep" & vote_outcome == "Rep Cand"]),
        dem_nonmobilization = dem_partisanship - dem_loyalty,
        rep_nonmobilization = rep_partisanship - rep_loyalty,
        dem_negmobilization = dem_loyalty - dem_partisanship,
        rep_negmobilization = rep_loyalty - rep_partisanship,
        dem_persuasion =
            sum(.value[party == "Rep" & vote_outcome == "Dem Cand"]),
        rep_persuasion =
            sum(.value[party == "Dem" & vote_outcome == "Rep Cand"]),
        dem_unaffiliated =
            sum(.value[party == "Ind" & vote_outcome == "Dem Cand"]),
        rep_unaffiliated =
            sum(.value[party == "Ind" & vote_outcome == "Rep Cand"]),
        dem_total =
            dem_partisanship + dem_negmobilization +
            dem_persuasion + dem_unaffiliated,
        rep_total =
            rep_partisanship + rep_negmobilization +
            rep_persuasion + rep_unaffiliated,
        .groups = "drop"
    )

core_terms = c("partisanship",
               "negmobilization",
               "persuasion",
               "unaffiliated",
               "total")

long_terms = terms |>
    pivot_longer(
        c(-cycle_label, -gender, -.draw),
        names_to = "party_term",
        values_to = "value"
    ) |>
    mutate(
        spl = str_split(party_term, "_"),
        party = map_chr(spl, item, 1),
        term = map_chr(spl, item, 2)
    ) |>
    select(-spl, -party_term)

dem_adv_draws = long_terms |>
    pivot_wider(
        names_from = party,
        values_from = value
    ) |>
    mutate(dem_advantage = dem - rep)

dem_adv_sums = dem_adv_draws |>
    summarize(
        mean = mean(dem_advantage),
        q05 = quantile(dem_advantage, .05),
        q95 = quantile(dem_advantage, .95),
        .by = c(cycle_label, gender, term)
    )

# ----- compute gender diffs in each term ----------

partial_gap_adv_draws = dem_adv_draws |>
    select(.draw, cycle_label, gender, term, dem_advantage) |>
    pivot_wider(
        names_from = gender,
        values_from = dem_advantage
    ) |>
    mutate(partial_gap = W - M)

partial_gap_sums = partial_gap_adv_draws |>
    summarize(
        mean = mean(partial_gap),
        q05 = quantile(partial_gap, .05),
        q95 = quantile(partial_gap, .95),
        .by = c(cycle_label, term)
    )

# ----- plot partial gaps ----------

# for gap effects, Dem avg. for men is negative
gap_term_sums = dem_adv_sums |>
    mutate(
        across(c(mean, starts_with("q")),
               ~ ifelse(gender == "M", .x * -1, .x))
    )

ggplot(filter(gap_term_sums, term %in% core_terms, term != "total")) +
    aes(x = cycle_label, y = mean, color = gender) +
    facet_grid(. ~ fct_relevel(term,
                               "partisanship",
                               "negmobilization",
                               "persuasion",
                               "unaffiliated")) +
    geom_ribbon(data = filter(partial_gap_sums, term %in% core_terms),
                aes(ymin = q05, ymax = q95),
                fill = "gray",
                color = NA,
                alpha = 0.4) +
    geom_hline(yintercept = 0) +
    geom_line(data = filter(partial_gap_sums, term %in% core_terms),
              aes(y = mean), color = "black") +
    geom_line() +
    geom_point()


data_paper = here("data", "to_paper")
if (file.exists(data_paper) == FALSE) {
    dir.create(data_paper)
}

readr::write_rds(gap_term_sums, here(data_paper, "gender_signed_dem_advantage_components.rds"))
readr::write_rds(partial_gap_sums, here(data_paper, "partial_gaps.rds"))

# ----- plot partial votes ----------

ggplot(filter(dem_adv_sums, term %in% core_terms)) +
    aes(x = cycle_label, y = mean, color = gender, fill = gender) +
    facet_grid(. ~ fct_relevel(term,
                               "partisanship",
                               "negmobilization",
                               "persuasion",
                               "unaffiliated",
                               "total")) +
    geom_ribbon(aes(ymin = q05, ymax = q95),
                color = NA,
                alpha = 0.4) +
    geom_hline(yintercept = 0) +
    geom_line() +
    geom_point()

readr::write_rds(dem_adv_sums, here(data_paper, "dem_advantage_components.rds"))

##################################
#    summary of raw estimates    #
##################################

long_term_sums = long_terms |>
    summarize(
        mean = mean(value),
        q05 = quantile(value, .05),
        q95 = quantile(value, .95),
        .by = c(cycle_label, gender, party, term)
    )

readr::write_rds(long_term_sums, here(data_paper, "vote_components.rds"))

ggplot(long_term_sums |> filter(term != "nonmobilization")) +
    aes(x = cycle_label, y = mean, color = party, shape = gender) +
    facet_grid(. ~ fct_relevel(term,
                               "partisanship",
                               "loyalty",
                               "persuasion",
                               "unaffiliated",
                               "total")) +
    geom_line() +
    geom_point()

