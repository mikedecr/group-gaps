# this file aggregates ANES into years
# meant to replace some of the "lower" components of descriptives.R where we complect...
# - data shaping
# - descriptive cross-sectional graphics
# - regression

##############
#    libs    #
##############

library(conflicted)
library(here)
library(dplyr)
library(tidyr)
library(purrr)

box::use(intervals = ./R/lib/confints)
box::use(prefix = ./R/lib/prefix)


##############################
#    aggregate anes data    #
##############################

anes <- arrow::read_parquet(here("data", "clean", "anes_cdf.pq"))

# - filter to major party voters
# - weighted democratic vote counts and share, by cycle x gender
# - reshape wide to calculate conf. interval for gender gap
agg_anes = anes |>
    dplyr::filter(vote_choice %in% c("Dem Cand", "Rep Cand")) |>
    summarize(
        .by = c(cycle, gender),
        dem_share = weighted.mean(vote_choice == "Dem Cand", w = wt),
        n_dem = sum(wt[vote_choice == "Dem Cand"]),
        n_cycle = sum(wt),
    ) |>
    pivot_wider(names_from = "gender", values_from = c(dem_share, n_dem, n_cycle)) |>
    mutate(.by = cycle,
           ci_diff = pmap(list(n_dem_W, n_cycle_W, n_dem_M, n_cycle_M),
                             intervals$diff_prop_ci)) |>
    mutate(
        dem_share_anes = (n_dem_M + n_dem_W) / (n_cycle_W + n_cycle_M),
        gender_gap_estimate = map_dbl(ci_diff, prefix$item, "estimate"),
        gender_gap_std_err = map_dbl(ci_diff, prefix$item, "std_err"),
        gender_gap_lower = map_dbl(ci_diff, prefix$item, "lower"),
        gender_gap_upper = map_dbl(ci_diff, prefix$item, "upper")
    ) |>
    select(-ci_diff) |>
    print()


###################################
#    national popular vote data    #
####################################

# data from https://uselectionatlas.org/RESULTS/
leip <- readr::read_csv(here("data/leip-national-pop-vote-data.csv")) |>
  mutate(dem_share_official = dem_raw_votes / (dem_raw_votes + rep_raw_votes)) |>
  print()


#################
#    combine    #
#################

agg = full_join(leip, agg_anes, by = "cycle")


###########################
#    save cleaned data    #
###########################

arrow::write_parquet(agg, here("data", "clean", "gap_vote_x_cycle.pq"))

# preview
agg |> head() |> print()


