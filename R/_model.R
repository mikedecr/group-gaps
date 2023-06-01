library("here")
library("dplyr")
library("tidyr")
library("ggplot2")
library("forcats")
library("purrr")
library("stringr")
box::use(tb = tidybayes)
box::use(stan = cmdstanr)

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


# redo this more programmatically.
# vote in party in group
# every term is a factor
# the eventual code is given by a formula
count(anes, vote_choice)

factor(anes$vote_choice, levels = c("Dem Cand", "Rep Cand", "Other Cand")) |>
    (function(x) table(x, as.numeric(x)))()



# should handle voted == 1 but vote_choice == ? during anes normalization
anes <- anes |>
    mutate(
        f_gender = as_factor(gender),
        f_pid_init = as_factor(pid_init),
        f_pid_lean = as_factor(pid_lean),
        f_vote = as_factor(vote_choice),
        f_cycle = as_factor(cycle),
        f_outcome = fct_cross(f_gender, f_pid_lean, f_vote)
    )



item = `[[`
index = function(x, ...) x[...]

# undo fct_cross...
# fct_factorize = function(f, split=":", ...) {
# }

crosswalk_outcome_init = anes |>
    count(f_outcome, num_outcome = as.numeric(f_outcome)) |>
    select(-n) |>
    group_by_all() |>
    mutate(
        split = map(f_outcome, str_split, ":", simplify = TRUE)
    ) |>
    mutate(
        gender = map_chr(split, index, 1),
        party = map_chr(split, index, 2),
        vote = map_chr(split, index, 3),
    ) |>
    select(-split) |>
    print()




anes <- anes |>
  mutate(vote_code = case_when(vote_choice == "Dem Cand" ~ 1,
                               vote_choice == "Rep Cand" ~ 2,
                               vote_choice == "Other Cand" ~ 3,
                               voted == 0 ~ 3),
         pid_init_code = case_when(pid_init == "Dem" ~ 1,
                                   pid_init == "Rep" ~ 2,
                                   pid_init == "Ind" ~ 3),
         pid_lean_code = case_when(pid_lean == "Dem" ~ 1,
                                   pid_lean == "Rep" ~ 2,
                                   pid_lean == "Ind" ~ 3),
         gender_code = case_when(gender == "M" ~ 1,
                                 gender == "W" ~ 2),
         offset_pid_init = case_when(pid_init_code == 1 ~ 0, 
                                     pid_init_code == 2 ~ 3, 
                                     pid_init_code == 3 ~ 6),
         offset_pid_lean = case_when(pid_lean_code == 1 ~ 0, 
                                     pid_lean_code == 2 ~ 3, 
                                     pid_lean_code == 3 ~ 6),
         offset_gender = case_when(gender_code == 1 ~ 0,
                                   gender_code == 2 ~ 9),
         outcome_code_init = vote_code + offset_pid_init + offset_gender,
         outcome_code_lean = vote_code + offset_pid_lean + offset_gender,
         cycle_code = 1 + ((cycle - 1952) / 4)) |>
  print()

anes |>
    count(outcome_code_init, gender_code, pid_init_code, vote_code) |>
    (\(x) print(x, n = nrow(x)))()


# --------------------------------------------------
#  setup MCMC data
# --------------------------------------------------


mcmc_data_init <- anes |>
    select(f_outcome, f_cycle, wt) |>
    na.omit() |>
    group_by(f_cycle) |>
    mutate(nt = sum(wt)) |>
    (function(d) {
         with(d, list(
              N = nrow(d),
              T = max(as.numeric(f_cycle)),
              J = max(as.numeric(f_outcome)),
              y = f_outcome,
              wt = wt,
              t = f_cycle,
              alpha = rep(1, max(as.numeric(f_outcome)))
         ))
     }
    )()

Map(head, mcmc_data_init)


agg_wide = anes |>
    select(f_outcome, f_cycle, wt) |>
    na.omit() |>
    group_by(f_cycle, f_outcome) |>
    summarize(wt = sum(wt), .groups = "drop") |>
    pivot_wider(names_from = f_cycle, values_from = wt) |>
    mutate(
        across(-f_outcome, replace_na, 0)
    ) |>
    print()

wt_matrix = agg_wide |>
    select(-f_outcome) |>
    as.matrix() |>
    t()

mcmc_data_grp = list(
    T = nrow(wt_matrix),
    J = ncol(wt_matrix),
    W = wt_matrix,
    alpha = rep(1, ncol(wt_matrix))
)

samples_grp = model_grp$sample(mcmc_data_grp, parallel_chains = 4)

sumd = tb$gather_draws(samples_grp, theta[cycle, outcome]) |>
    group_by(cycle, outcome) |>
    summarize(estimate = mean(.value), .groups = "drop")


ggplot(sumd) +
    aes(x = cycle, y= estimate) +
    facet_wrap(~ outcome) +
    geom_line()

# --------------------------------------------------
#  stan model
# --------------------------------------------------

model = cmdstanr::cmdstan_model(here("stan", "gaps_slim.stan"))
model_grp = cmdstanr::cmdstan_model(here("stan", "gaps_agg.stan"))
model_fold = cmdstanr::cmdstan_model(here("stan", "gaps_fold.stan"))

mcmc_init = model$sample(data = mcmc_data_init, parallel_chains = 4, refresh = 10)

mcmc_fold = model_fold$sample(data = mcmc_data_init, parallel_chains = 4, refresh = 10)



tidybayes::gather_draws(mcmc_init, theta[t, y]) |>
    group_by(t, y) |>
    summarize(estimate = sum(.value)) %>%
    ggplot() +
    aes(x = t, y = estimate) +
    facet_wrap(~ y) +
    geom_line()


long_init = tidybayes::gather_draws(mcmc_init, theta[num_cycle, num_outcome]) |>
    group_by(num_cycle, num_outcome, .variable) |>
    summarize(
        estimate = median(.value),
        std_dev = sd(.value)
    ) |>
    left_join(crosswalk_outcome_init)

ggplot(long_init) +
    aes(x = num_cycle, y = estimate, color = party) +
    geom_line() +
    facet_wrap(gender ~ vote)

long_init |>
    group_by(num_cycle, gender, party) |>
    mutate(partisanship = sum(estimate)) |>
    group_by(num_cycle, gender, vote) |>
    mutate(votes = sum(estimate)) |>
    ungroup() |>
    mutate(
        nonmobilization = partisanship - estimate,
        mobilization = estimate,
    ) %>%
    filter(vote != "Other Cand", party != "Ind") |>
    ggplot() +
    aes(x = num_cycle, y = mobilization, color = party, linetype = gender, shape = gender) +
    geom_line() +
    geom_point()
