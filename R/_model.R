# --------------------------------------------------
#  shape data for model
#  fit model
#  save estimates
# --------------------------------------------------

library("here")
library("dplyr")
library("tidyr")
library("ggplot2")
library("forcats")
library("purrr")
library("stringr")

# TODO: this doesn't support rscript?
# might need to do other module tagging/naming/fn exporting
box::use(tb = tidybayes)
box::use(stan = cmdstanr)
box::use(pfx = ./R/lib/prefix)

# --------------------------------------------------
#  IO
# --------------------------------------------------

"
This is actually pretty straightforward.
- read data
- create numeric coding
- feed to model
"

anes <- here("data", "clean", "cleaned-anes-cdf.RDS") |>
    readRDS() |>
    as_tibble() |>
    print()


# --------------------------------------------------
#  outcome code
# --------------------------------------------------

# redo this more programmatically.
# vote in party in group
# every term is a factor
# the eventual code is given by a formula
count(anes, cycle, vote_choice, wt = wt) |>
    pivot_wider(names_from = "vote_choice", values_from = "n")

# desire: x -> list(f(x), g(x), ...)

# this evaluates, but we could create a fn
split_apply = function(x, fns) {
    lapply(fns, function(f) f(x))
}

mapify = function(f) {
    function(x) lapply(x, f)
}

# lift = function(f) {
#     function(...) {
#         args = list(...)
#         do.call(f, args)
#     }
# }

anes$vote_choice |>
    factor(levels = c("Dem Cand", "Rep Cand", "Other Cand")) |>
    (\(x) table(x, as.numeric(x)))()

# TODO:
# should handle voted == 1 but vote_choice == ? during anes normalization
anes <- anes |>
    mutate(
        f_gender = as_factor(gender),
        f_pid_init = as_factor(pid_init),
        f_pid_lean = as_factor(pid_lean),
        f_vote = as_factor(vote_choice),
        f_cycle = as_factor(cycle),
        f_outcome_init = fct_cross(f_gender, f_pid_init, f_vote, sep = ":"),
        f_outcome_lean = fct_cross(f_gender, f_pid_lean, f_vote, sep = ":")
    )

# TODO: this should be somewhere else
# totally modular to the model per se...
crosswalk_outcome_init <- anes |>
    count(f_outcome_init, num_outcome = as.numeric(f_outcome_init)) |>
    select(-n) |>
    group_by_all() |>
    mutate(
        splits = map(f_outcome_init, str_split, ":", simplify = TRUE),
        gender = map_chr(splits, pfx$index, 1),
        party = map_chr(splits, pfx$index, 2),
        vote = map_chr(splits, pfx$index, 3),
    ) |>
    ungroup() |>
    select(-splits) |>
    print()

readr::write_rds(anes, here("data", "clean", "anes_mcmc_codes.RDS"))

# --------------------------------------------------
#  setup MCMC data
# --------------------------------------------------

# function: cycle, party, group, outcome
# must be factor or numeric
# it's YOUR job to ensure the data have the appropriate levels

# we have parallel paths depending on party ID coding
# this function is a bit bespoke but takes a tuple
meets_array_assumptions <- function(x) {
    # type assertion
    good_type <- inherits(x, "numeric") || inherits(x, "factor")
    stopifnot(good_type)
    # no missing data
    nna <- sum(is.na(x))
    stopifnot(nna == 0)
}

model_data <- function(group, time, weight) {
    for (x in c(group, time, weight)) {
        meets_array_assumptions(x)
    }
    n_grps <- length(levels(group))
    list(
        N = length(group),
        y = group,
        J = n_grps,
        t = time,
        T = length(levels(time)),
        wt = weight,
        alpha = rep(1, n_grps)
    )
}


anes_init <- anes |>
    select(f_outcome_init, f_cycle, wt) |>
    na.omit()
anes_lean <- anes |>
    select(f_outcome_lean, f_cycle, wt) |>
    na.omit()

mcmc_data_init <- with(anes_init, model_data(f_outcome_init, f_cycle, wt))
mcmc_data_lean <- with(anes_lean, model_data(f_outcome_lean, f_cycle, wt))

lapply(mcmc_data_init, head)
lapply(mcmc_data_init, tail)
lengths(mcmc_data_init)

lapply(mcmc_data_lean, head)
lapply(mcmc_data_lean, tail)
lengths(mcmc_data_lean)

# --------------------------------------------------
#  estimate model
# --------------------------------------------------

model_group <- cmdstanr::cmdstan_model(here("stan", "gaps_group.stan"))

mcmc_init <- model_group$sample(data = mcmc_data_init,
                               parallel_chains = 4,
                               refresh = 10)
mcmc_lean <- model_group$sample(data = mcmc_data_lean,
                               parallel_chains = 4,
                               refresh = 10)

# need to save like this bc some metadata reasons
mcmc_init$save_object(here("data", "mcmc", "group", "init.RDS"))
mcmc_lean$save_object(here("data", "mcmc", "group", "lean.RDS"))

model_long <- cmdstanr::cmdstan_model(here("stan", "gaps_slim.stan"))
mcmc_long_init <- model_long$sample(data = mcmc_data_init,
                                    parallel_chains = 4,
                                    refresh = 10)
mcmc_long_init$save_object(here("data", "mcmc", "long", "init.RDS"))

# --------------------------------------------------
#  post-process estimates
# --------------------------------------------------

stop("End of file, delete or relocate everything below me.")

if (FALSE) {

# use the factor levels to unmake the codes into names?

    captured_gather = function(d) {
        gather_draws(d, theta[t, y])
    }

    mem_gather = memoize(captured_gather)

    mem_gather(mcmc_init)

    mem_gather2 = memoize(gather_draws)

    long_init = mem_gather(mcmc_init) |>
        mutate(t = as.factor(t), y = as.factor(y)) |>
        left_join(anes_init, by = c('t' = 'f_cycle', 'y' = 'f_outcome_init')) |>
        mutate(y = as.numeric(y)) |>
        left_join(crosswalk_outcome_init, by = c("y" = "num_outcome"))

# NOTE: this takes a while
# ggplot(long_init) +
#     aes(x = .value, y = t, fill = party) +
#     facet_wrap(~ f_outcome) +
#     ggridges::geom_density_ridges(stat = "binline", bins = 100)

    long_init |>
        group_by(party, t, gender) |>
        mutate(partisanship = mean(.value), .groups = "drop") |>
        select(t, partisanship, gender, party) |>
        distinct() |>
        filter(party != "Ind") %>%
        ggplot() +
        aes(x = as.numeric(t), y = partisanship, color = party, shape = gender) +
        geom_point() +
        geom_line()

    mem_tidy_draws = memoize(tidy_draws)

    tidy_draws = memoize(tidy_draws)

    tidy_draws(mcmc_init)

    gather_draws(mcmc_init, theta[t, y])

    mem_tidy_draws(mcmc_lean)


    memo_mean = memoise::memoise(mean)

    memo_mean(mtcars |> pull(mpg))

    mcmc_samples = mcmc_init

    gather_draws = tidybayes::gather_draws
    gather_draws(mcmc_init, theta[t, y])

    memoize(gather_draws, omit_args = ...)(mcmc_init, theta[t, y])

    gather_draws(mcmc_init, `theta_.*`, regex = TRUE)

    library("tidybayes")
    library("memoise")


    dplyr::filter(anes, cycle == 2012)
    mem_filter = memoize(dplyr::filter)
    mem_filter(anes, cycle == 2012)



# works fine
    gather_draws(mcmc_samples, theta[t, y])

    mem_gather_draws <- memoise(gather_draws)
    mem_gather_draws(mcmc_init, theta[t, y])
# Error in FUN(X[[i]], ...) : object 'theta' not found



    sumd <- tidybayes::gather_draws(mcmc_init, theta[t, y]) |>
        group_by(t, y) |>
        summarize(estimate = median(.value), .groups = 'drop')

    ggplot(sumd) +
        aes(x = t, y = estimate) +
        facet_wrap(~ y) +
        geom_line()


    ggplot(sumd) +
        aes(x = t, y = estimate, fill = as.factor(y)) +
        geom_col()

}

