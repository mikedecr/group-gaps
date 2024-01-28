######################################
#    fit mcmc models on ANES data    #
######################################

# 1. construct cross-product group codes from cleaned anes data
# 2. define functions to aggregate such data into stan-ready format
# 3. fit MCMC models and dump data to file.

library("conflicted")
library("here")
library("dplyr")




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
#    standata for two leaner codes    #
#####################################

# ----- read subset of columns ----------

anes <- here("data", "clean", "anes_cdf.pq") |>
    arrow::read_parquet() |>
    as_tibble() |>
    select(-starts_with("VCF"), -"Version") |>
    print()

# ----- create group codes ----------

# group = party x gender x vote
# this step may contain NAs, but they are unleveled

anes <- anes |>
    mutate(
        grp_init = forcats::fct_cross(gender, pid_init, vote_choice, sep = ":"),
        grp_lean = forcats::fct_cross(gender, pid_lean, vote_choice, sep = ":")
    )

# ----- aggregate into stan data ----------

# this could be a fn of (data, grp)
grp_data_init = anes |>
    select(cycle, grp_init, wt) |>
    tidyr::drop_na() |>
    (function(d) aggregate_group_weights(as.factor(d$cycle), d$grp_init, d$wt))()

grp_data_lean = anes |>
    select(cycle, grp_lean, wt) |>
    tidyr::drop_na() |>
    (function(d) aggregate_group_weights(as.factor(d$cycle), d$grp_lean, d$wt))()

# list of stan-compatible data
stan_data = lapply(
    list(init = grp_data_init, lean = grp_data_lean),
    grouped_to_stan
)

####################
#    stan model    #
####################

# we will want to export other hyperparameters about the stan model
# such as warmup iterations, total samples, thin interval, n.chains,
# NUTS params (delta, tree depth, ...)

model_group <- cmdstanr::cmdstan_model(here("stan", "gaps_group.stan"))

PARALLEL_CHAINS = 4
fit_stan_model = function(d) {
    model_group$sample(data = d,
                       parallel_chains = PARALLEL_CHAINS)
}

# should take <10s per fit
stanfits = lapply(stan_data, fit_stan_model)

######################################
#    save MCMCs and crosswalk key    #
######################################

# crosswalk
crosswalk = grp_data_init |>
    select(fct = group) |>
    mutate(int = as.numeric(fct),
           str = as.character(fct))

arrow::write_parquet(crosswalk, here("data", "models", "mcmc_group_code_crosswalk.pq"))

# MCMCs
# every file is mcmc_{name}.rds
lapply(
    names(stanfits),
    function(name) {
        mcmc = stanfits[[name]]
        filename = as.character(stringr::str_glue("mcmc_{name}.rds"))
        readr::write_rds(mcmc, filename)
    }
)



