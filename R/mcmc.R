library("conflicted")
library("here")
library("dplyr")


################################
#    read subset of columns    #
################################

anes <- here("data", "clean", "anes_cdf.pq") |>
    arrow::read_parquet() |>
    as_tibble() |>
    select(-starts_with("VCF"), -"Version") |>
    print()


############################
#    create group codes    #
############################

# group = party x gender x vote
# this step may contain NAs, but they are unleveled

anes <- anes |>
    mutate(
        grp_init = forcats::fct_cross(gender, pid_init, vote_choice, sep = ":"),
        grp_lean = forcats::fct_cross(gender, pid_lean, vote_choice, sep = ":")
    )


################################################
#    map from group data to stan model data    #
################################################

# ----- individual -> grouped data frame ----------

# default is unweighted groups
aggregate_group_weights <- function(time, group, weight = rep(1, length(group))) {
    nas = lapply(list(time, group, weight), \(x) sum(is.na(x)))
    stopifnot(all(nas == 0))
    tibble(t = time, group = group, wt = weight) |>
    count(t, group, wt = wt, name = "wt") |>
    arrange(t)
}


# ----- grouped data frame -> stan data ----------

# if you provide an alpha, it overrides the default
grouped_to_stan = function(aggd, ...) {
    baseline = tidybayes::compose_data(aggd, ...)
    if ("alpha" %in% names(list(...))) {
        return(baseline)
    } else {
        default_alphas = rep(1, baseline[["n_group"]])
        return(c(list(alpha = default_alphas),
                 baseline))
    }
}

# ----- composition ----------

prepare_model_data <- function(time, group, weight = rep(1, length(group)), ...) {
    aggregate_group_weights(time, group, weight) |>
    grouped_to_stan(...)
}

# come back to this...?
# might be easy for robustness / multi groups to have a layer where we go
# (data, group) -> stan_data, fixing the time and wt components

#####################################
#    models for two leaner codes    #
#####################################

stan_data_init = anes |>
    select(cycle, grp_init, wt) |>
    tidyr::drop_na() |>
    (function(d) prepare_model_data(as.factor(d$cycle), d$grp_init, d$wt))()

stan_data_lean = anes |>
    select(cycle, grp_lean, wt) |>
    tidyr::drop_na() |>
    (function(d) prepare_model_data(as.factor(d$cycle), d$grp_lean, d$wt))()

####################
#    stan model    #
####################

model_group <- cmdstanr::cmdstan_model(here("stan", "gaps_group.stan"))

mcmc_init = model_group$sample(data = stan_data_init,
                               parallel_chains = 4,
                               refresh = 10)
mcmc_lean = model_group$sample(data = stan_data_lean,
                               parallel_chains = 4,
                               refresh = 10)

readr::write_rds(mcmc_init, here("data", "models", "mcmc_init.rds"))
readr::write_rds(mcmc_lean, here("data", "models", "mcmc_lean.rds"))

