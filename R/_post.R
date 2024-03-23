library("here")
library("dplyr")
library("tidyr")
library("purrr")
library("ggplot2")
library("tidybayes")

box::use(./R/lib/factors[...])
box::use(./R/lib/prefix[...])
# box::use(stan = cmdstanr)

anes <- readr::read_rds(here("data", "clean", "anes_mcmc_codes.RDS"))
mcmc_init <- readRDS(here("data", "mcmc", "group", "init.RDS"))
mcmc_lean <- readRDS(here("data", "mcmc", "group", "lean.RDS"))


# --------------------------------------------------
#  work out a demo
# --------------------------------------------------

crosswalk = anes |>
    select(y_code = f_outcome_lean) |>
    distinct() |>
    na.omit() |>
    mutate(y = as.numeric(y_code)) |>
    mutate(
        fct = map_df(y_code, fct_factorize, names = c("gender", "party", "vote"))
    ) |>
    tidyr::unnest(fct)


# --------------------------------------------------
#  groupy
# --------------------------------------------------

wide = gather_draws(mcmc_lean, theta[t, y]) |>
    group_by(t, y) |>
    # summarize(p = median(.value), .groups = "drop") |>
    rename(p = .value) |>
    left_join(crosswalk, by = "y") |>
    print()


by_cycle_gender = wide |>
    group_by(t, gender, .draw) |>
    summarize(
        # republican vote contributions
        votes_rep = sum(p[vote == "Rep Cand"]),
        partisans_rep = sum(p[party == "Rep"]),
        disloyalty_rep = -1 * sum(index(p, party == "Rep" & vote != "Rep Cand")),
        mobilized_rep = partisans_rep + disloyalty_rep,
        persuasion_rep = sum(index(p, party == "Dem" & vote == "Rep Cand")),
        unaffiliated_rep = sum(index(p, party == "Ind" & vote == "Rep Cand")),
        total_rep = mobilized_rep + persuasion_rep + unaffiliated_rep,
        # democratic vote contributions
        votes_dem = sum(p[vote == "Dem Cand"]),
        partisans_dem = sum(p[party == "Dem"]),
        disloyalty_dem = -1 * sum(index(p, party == "Dem" & vote != "Dem Cand")),
        mobilized_dem = partisans_dem + disloyalty_dem,
        persuasion_dem = sum(index(p, party == "Rep" & vote == "Dem Cand")),
        unaffiliated_dem = sum(index(p, party == "Ind" & vote == "Dem Cand")),
        total_dem = mobilized_dem + persuasion_dem + unaffiliated_dem,
        .groups = "drop"
    )


# summarize across draws

ggplot() +
    aes(x = t, linetype = gender) +
    geom_line(aes(y = persuasion_rep), color = "red") +
    geom_line(aes(y = persuasion_dem), color = "blue")


long_metric_party = by_cycle_gender |>
    pivot_longer(cols = -c(t, gender, .draw)) |>
    mutate(
        metric = factor(
            stringr::str_split(name, "_", simplify = TRUE)[, 1],
            levels = c("partisans", "mobilized", "disloyalty",
                       "persuasion", "unaffiliated", "total", "votes")
        ),
        beneficiary = stringr::str_split(name, "_", simplify = TRUE)[, 2]
    ) |>
    print()

# summarize raw estimates
long_metric_party |>
    group_by(t, gender, metric, beneficiary) |>
    summarize(p = median(value),
              q05 = quantile(value, .05),
              q95 = quantile(value, .95),
              .groups = "drop"
    ) |>
    filter(not(isin(metric, c('total', 'disloyal')))) %>%
    ggplot() +
        aes(x = t, y = p,
            color = beneficiary, fill = beneficiary,
            linetype = gender, shape = gender) +
        facet_wrap(~ metric, nrow = 1) +
        geom_ribbon(aes(ymin = q05, ymax = q95), alpha = 0.1, color = NA) +
        geom_point() +
        geom_line() +
        scale_color_manual(values = c("dem" = "blue", "rep" = "red")) +
        scale_fill_manual(values = c("dem" = "blue", "rep" = "red"))

# summarize democratic advantage
long_dem_advantages = long_metric_party |>
    select(-name) |>
    pivot_wider(values_from = value, names_from = beneficiary) |>
    mutate(net_dem = dem - rep) |>
    print()

stat_dem_advantages = long_dem_advantages |>
    group_by(t, gender, metric) |>
    summarize(p = median(net_dem),
              q05 = quantile(net_dem, .05),
              q95 = quantile(net_dem, .95),
              .groups = "drop"
    ) |>
    filter(isin(metric, c("partisans", "disloyalty", "persuasion", "unaffiliated", "total"))) |>
    print()

ggplot(stat_dem_advantages) +
    aes(x = t, y = p, color = gender, fill = gender) +
    geom_ribbon(aes(ymin = q05, ymax = q95), color = NA, alpha = 0.3) +
    geom_line() +
    facet_wrap(~ metric, nrow = 1)


# summarize gender gap impact
long_gap = long_dem_advantages |>
    select(-rep, -dem) |>
    pivot_wider(names_from = "gender", values_from = "net_dem") |>
    mutate(
        net_gap = W - M,
        M = -1 * M
    ) |>
    pivot_longer(cols = c(M, W, net_gap), names_to = "gender") |>
    group_by(t, metric, gender) |>
    summarize(
        p = median(value),
        q05 = quantile(value, .05),
        q95 = quantile(value, .95)
    ) |>
    filter(isin(metric, c("partisans", "disloyalty", "persuasion", "unaffiliated", "total"))) |>
    print()

ggplot(filter(long_gap, not(and(isin(gender, c("W", "M")), metric == "total")))) +
    aes(x = t, y = p, color = gender, fill = gender, shape = gender, linetype = gender) +
    facet_wrap(~ metric, nrow = 1) +
    geom_hline(yintercept = 0) +
    geom_ribbon(data = filter(long_gap, gender == "net_gap"),
                aes(ymin = q05, ymax = q95), alpha = 0.4, color = NA) +
    geom_line() +
    geom_point(data = filter(long_gap, metric != "total", isin(gender, c("M", "W")))) +
    scale_color_manual(values = c("M" = "darkcyan", "W" = "goldenrod", "net_gap" = "black")) +
    scale_fill_manual(values = c("M" = "darkcyan", "W" = "goldenrod", "net_gap" = "gray")) +
    theme_minimal()



# --------------------------------------------------
#  wide version
# --------------------------------------------------

gather_draws(mcmc_lean, theta[t, y]) |>
    group_by(t, y) |>
    summarize(p = median(.value), .groups = "drop") |>
    left_join(select(crosswalk, y, y_code), by = "y") |>
    select(-y) |>
    pivot_wider(names_from = y_code, values_from = p)



gather_draws(mcmc_lean, theta[t, y]) |>
    group_by(t, y) |>
    summarize(p = median(.value)) |>
    (\(d)
        ggplot(d) +
            aes(x = t, y = p) +
            geom_line() +
            facet_wrap(~ y)
    )()




