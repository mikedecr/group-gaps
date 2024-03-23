######################################
#    fit mcmc models on ANES data    #
######################################

# 1. construct cross-product group codes from cleaned anes data
# 2. define functions to aggregate such data into stan-ready format
# 3. fit MCMC models and dump data to file.

library("conflicted")
library("here")
library("dplyr")
library("ggplot2")


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

# relevant vote outcome
anes <- anes |>
    mutate(
        vote_outcome = case_when(
            voted_maj_party ~ vote_choice,
            (!voted) | (vote_choice == "Other Cand") ~ "Other"
        )
    )

anes |> count(voted, vote_choice, vote_outcome)

# ----- create group codes ----------

# group = party x gender x vote
# this step may contain NAs, but they are unleveled


anes <- anes |>
    mutate(
        grp_init = forcats::fct_cross(gender, pid_init, vote_outcome, sep = ":"),
        grp_lean = forcats::fct_cross(gender, pid_lean, vote_outcome, sep = ":")
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

cfg_parallel_chains <- 8
fit_stan_model = function(d) {
    model_group$sample(data = d,
                       iter_warmup = 10000,
                       iter_sampling = 10000,
                       thin = 5,
                       parallel_chains = cfg_parallel_chains)
}

# should take <10s per fit
stanfits = lapply(stan_data, fit_stan_model)


#####################################
#    chain stats before you save    #
#####################################

# table of ESS and Rhat measurements per parameter, per model
chain_stats = stanfits |>
    lapply(posterior::summarise_draws) |>
    bind_rows(.id = "model") |>
    select(model, variable, rhat, contains("ess")) |>
    dplyr::filter(stringr::str_detect(variable, "theta")) |>
    mutate(param_index = row_number(), .by = model)

# benchmark against some expected values
good_rhats = chain_stats$rhat < 1.01
stopifnot(all(good_rhats))

good_tails = chain_stats$ess_tail > 2000
stopifnot(all(good_tails))

good_bulks = chain_stats$ess_bulk > 4000
stopifnot(all(good_bulks))

# ----- plot rhat and ESS, look for outliers ----------

# want Rhat near 1, ESS should be large and uniform
long_chain_stats = tidyr::pivot_longer(chain_stats,
                                       c(rhat, ess_bulk, ess_tail))

ggplot(long_chain_stats) +
    aes(x = param_index, y = value) +
    geom_point() +
    facet_wrap(~ model + name, scale = "free_y")


##########################
#    autocorrelations    #
##########################

# long, by model
draws = stanfits |>
    lapply(tidybayes::tidy_draws) |>
    bind_rows(.id = "model") |>
    select(model, starts_with("theta"))

# autocorrelation as a "normal function"
auto_cor = function(x) {
    ac_object = acf(x, plot = FALSE)
    as.vector(ac_object$acf)
}

autocors = draws |>
    group_by(model) |>
    reframe(across(starts_with("theta"),
                   auto_cor)) |>
    mutate(lag = row_number(), .by = model)

extreme_autocors = autocors |>
    tidyr::pivot_longer(
        starts_with("theta"),
        names_to = "param",
        values_to = "acf"
    ) |>
    summarize(
        across(acf, list(min = min, max = max, mean = mean, median = median)),
        .by = c(model, lag)
    )

ggplot(extreme_autocors) +
    aes(x = lag, y = acf_mean) +
    facet_wrap(~ model) +
    geom_pointrange(aes(ymin = acf_min, ymax = acf_max)) +
    geom_line(linetype = "dotted") +
    scale_x_continuous(breaks = seq(0, 50, 5))


######################################
#    save MCMCs and crosswalk key    #
######################################

crosswalk_list = grp_data_init |>
    select(t, group) |>
    as.list() |>
    lapply(unique) |>
    lapply(function(x) setNames(x, as.integer(x)))

data_models = here("data", "models")

if (file.exists(data_models) == FALSE) {
    dir.create(data_models)
}

readr::write_rds(crosswalk_list, here(data_models, "mcmc_factor_crosswalk.rds"))

# MCMCs
# every file is mcmc_{name}.rds
lapply(
    names(stanfits),
    function(name) {
        mcmc = stanfits[[name]]
        stub = as.character(stringr::str_glue("mcmc_{name}.rds"))
        path = here(data_models, stub)
        mcmc$save_object(path)
    }
)
