# aggregate ANES data into cycle-level data

library("conflicted")
library("here")
library("tidyverse")

# local modules
box::use(ci = helpers/confints)

# conflicts:
conflict_prefer("filter", "dplyr")


# --------------------------------------------------
#  read data sources
# --------------------------------------------------

votes <- read_csv(here("data", "leip-national-pop-vote-data.csv"))

anes <- read_rds(here("data", "clean", "cleaned_anes_cdf.RDS"))


# --------------------------------------------------
#  Prepare election results frame
# --------------------------------------------------

# this will be our merge skeleton
votes <- rename_with(votes, .cols = contains("raw_votes"),
                            .fn = str_remove, "raw_")

# dem share of the two-party vote
votes <- mutate(votes,
                two_party_vote = dem_votes + rep_votes,
                dem_share      = dem_votes / two_party_vote)


compose_left <- function(f, g) {
    function(...) f(g(...))
}

compose_right <- function(f, g) compose_left(g,f)

compose_right(ci$prop_ci, as_tibble)(5, 10)

# --------------------------------------------------
#  prepare aggregated gender gap data
# --------------------------------------------------

# raw votes + two-party vote share
# 1-row per cycle, separate columns men + women
gap_raw <- anes |>
    filter(vote_choice %in% c("Rep Cand", "Dem Cand")) |>
    group_by(cycle, gender) |>
    summarize(dem_votes = sum(wt[vote_choice == "Dem Cand"]),
              two_party_vote = sum(wt),
              .groups = "drop") |>
    pivot_wider(names_from = "gender",
                values_from = c("dem_votes", "two_party_vote")) |>
    print()

# + list-cols: vote share + gender gap estimates w/ CIs
gap_ci <- gap_raw |>
    select(-starts_with("dem_share_")) |>
    group_by(cycle) |>
    mutate(
        dem_share_M = map2(dem_votes_M, two_party_vote_M, ci$prop_ci),
        dem_share_W = map2(dem_votes_W, two_party_vote_W, ci$prop_ci),
        gender_gap = pmap(
            .l = list(dem_votes_W, two_party_vote_W,
                      dem_votes_M, two_party_vote_M),
            .f = ci$diff_prop_ci
        )
    ) |>
    ungroup() |>
    print()

# TODO: export?
# df -> df w/ altered names
# we could do this with repeated rename_all for each DF but that's verbose.
# and creating some partial function to parse names(.x) might need weird tidy eval
# resolution: this is easier than being idiomatic for its own sake
prepend_df_names <- function(df, name, sep = "_") {
    new_names <- paste(name, colnames(df), sep=sep)
    return(setNames(df, new_names))
}

# unpack CI results into one data frame
# convert to tibble, provide unique names to each, and unnest
gender_gap <- gap_ci |>
    mutate(across(.cols = c(starts_with("dem_share"), gender_gap),
                  .fn = map, as_tibble)) |>
    mutate(dem_share_M = map_df(dem_share_M, prepend_df_names, "dem_share_M"),
           dem_share_W = map_df(dem_share_W, prepend_df_names, "dem_share_W"),
           gender_gap = map_df(gender_gap, prepend_df_names, "gender_gap")) |>
    select(cycle, starts_with("dem_share"), gender_gap, everything()) |>
    unnest(cols = c(starts_with("dem_share"), gender_gap)) |>
    print()


if (interactive()) {
    ggplot(gender_gap) +
        aes(x = cycle, y = gender_gap_estimate) +
        geom_ribbon(
            aes(ymin = gender_gap_lower, ymax = gender_gap_upper),
            alpha = 0.3
        ) +
        geom_line()
}


# --------------------------------------------------
#  combine true vote totals + gender gap estimates
# --------------------------------------------------

gender_gap_to_merge <- gender_gap |>
    rename_with(
        .cols = c(starts_with("dem_votes"), starts_with("two_party_vote")),
        .fn = ~ str_glue("svy_{.x}")
    )
                            
gender_gap_votes <- left_join(votes, gender_gap_to_merge, by = "cycle")

if (interactive()) {
    ggplot(gender_gap_votes) +
        aes(x = gender_gap_estimate, y = dem_share) +
        geom_text(aes(label = cycle))+
        geom_smooth(method = "lm")
}

# --------------------------------------------------
#  save clean data
# --------------------------------------------------

# TODO parameterize "clean" path
write_csv(gender_gap_votes, here("data", "clean", "gender_gap_votes.csv"))
write_rds(gender_gap_votes, here("data", "clean", "gender_gap_votes.rds"))


    
# 3. merge true vote share from leip
# 1. aggregate ANES vote choice + confints
# 2. calculate gender gap + confints
# 4. save to clean
