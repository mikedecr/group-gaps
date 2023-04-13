# ----------------------------------------------------
#   too many differences, uncertainty is weird
#   but sampling might be helpful
#   NUKE: should consider a rounded multinomial version
# ----------------------------------------------------


names(anes)

anes %$% table(vote_choice, exclude = NULL)
anes %$% table(voted, exclude = NULL)
anes %$% table(VCF0706, exclude = NULL)

anes %$% table(pid_init, exclude = NULL)
anes %$% table(pid_lean, exclude = NULL)

anes %$% table(gender, exclude = NULL)


anes <- anes %>%
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
         cycle_code = 1 + ((cycle - 1952) / 4)) %>%
  print()




anes %>%
  count(outcome_code_init, gender_code, pid_init_code, vote_code) %>%
  na.omit() %>% 
  print(n = nrow(.))

# save crosswalk file
crosswalk <- anes %>%
  count(
    outcome_code = outcome_code_lean, 
    gender, pid = pid_lean, vote_code
  ) %>%
  na.omit() %>%
  mutate(
    vote_choice = case_when(
      vote_code == 1 ~ "Dem",
      vote_code == 2 ~ "Rep",
      vote_code == 3 ~ "Other"
    )
  ) %>%
  select(-vote_code, -n) %>%
  mutate_if(is.factor, as.character) %>%
  print(n = nrow(.)) %T>% 
  saveRDS(here("data", "mcmc-params", "y-data.RDS"))




names(anes)

anes %$% table(1 + ((cycle - 1952)/4), exclude = NULL)

anes %$% table(cycle, 1 + (cycle %% 1952) / 4)


mcmc_data_init <- anes %>%
  select(outcome_code_init, cycle, cycle_code, wt) %>%
  na.omit() %>%
  as_data_frame() %>%
  group_by(cycle) %>%  
  mutate(nt = sum(wt)) %$% 
  list(N = nrow(.),
     T = max(cycle_code),
     # nt = unique(nt),
     J = max(outcome_code_init),
     y = outcome_code_init,
     wt = wt,
     t = cycle_code,
     alpha = rep(1, max(outcome_code_init))) 


mcmc_data_lean <- anes %>%
  select(outcome_code_lean, cycle, cycle_code, wt) %>%
  na.omit() %>%
  as_data_frame() %>%
  group_by(cycle) %>%  
  mutate(nt = sum(wt)) %$% 
  list(N = nrow(.),
     T = max(cycle_code),
     # nt = unique(nt),
     J = max(outcome_code_lean),
     y = outcome_code_lean,
     wt = wt,
     t = cycle_code,
     alpha = rep(1, max(outcome_code_lean))) 



# ----------------------------------------------------
#   setup MCMC
# ----------------------------------------------------

# here("code", "stan", "gaps-dynamics.stan") %>%
# here("code", "stan", "ugh.stan") %>%
  
compiled_mod <- 
  here("code", "stan", "gaps.stan") %>%
  stanc(file = .) %>%
  stan_model(stanc_ret = ., verbose = TRUE)


beepr::beep(2)



# tuning parameters
(n_iterations <- 2000)
(warmup_length <- 1000)
(thin_interval <- 1)
(n_chains <- 4)

write(scales::comma(n_iterations), here("data", "mcmc-params", "mcmc-iterations.tex"))
write(scales::comma(warmup_length), here("data", "mcmc-params", "mcmc-warmup.tex"))
write(scales::comma(thin_interval), here("data", "mcmc-params", "mcmc-thin.tex"))
write(scales::comma(n_chains), here("data", "mcmc-params", "mcmc-chains.tex"))




# # --- test -----------------------

# test_mod <- sampling(object = compiled_mod, 
#                      data = mcmc_data_init, 
#                      iter = 500,  
#                      chains = 1)

# beepr::beep(2)


# stan_ac(test_mod)



# --- run -----------------------

seednum <- 999
set.seed(seednum)

bayes_init <- 
  sampling(
    object = compiled_mod, 
    data = mcmc_data_init, 
    iter = n_iterations, 
    warmup = warmup_length, 
    thin = thin_interval, 
    chains = n_chains,
    seed = seednum
  )


bayes_lean <- 
  sampling(
    object = compiled_mod, 
    data = mcmc_data_lean, 
    iter = n_iterations, 
    warmup = warmup_length, 
    thin = thin_interval, 
    chains = n_chains,
    seed = seednum
  )


beepr::beep(2)


# --- clean and save -----------------------

dir.create(here("data", "mcmc"))

# stan version
saveRDS(bayes_init, here("data", "mcmc", "samples-init.RDS"))
saveRDS(bayes_lean, here("data", "mcmc", "samples-lean.RDS"))


# tidy version
tidy_init <- tidy(bayes_init, estimate.method = "median", conf.int = TRUE) %>%
  mutate(leaners = "Leaners as Unaffiliated") %>%
  as_data_frame() %>%
  print()

tidy_lean <- tidy(bayes_lean, estimate.method = "median", conf.int = TRUE) %>%
  mutate(leaners = "Leaners as Partisans") %>%
  as_data_frame() %>%  
  print()

saveRDS(tidy_init, here("data", "mcmc", "tidy-init.RDS"))
saveRDS(tidy_lean, here("data", "mcmc", "tidy-lean.RDS"))

# ggs version
# ggs_init <- ggs(bayes_init, description = "init")
# ggs_lean <- ggs(bayes_lean, description = "lean") 

# saveRDS(ggs_init, here("data", "mcmc", "ggs-init.RDS"))
# saveRDS(ggs_lean, here("data", "mcmc", "ggs-lean.RDS"))


stop("stop before informed dynamic model")

# ----------------------------------------------------
#   Doing it with random walk priors on a link scale
# ----------------------------------------------------

# compile the model
compiled_mod_priors <- 
  here("code", "stan", "gaps-priors.stan") %>%
  stanc(file = .) %>%
  stan_model(stanc_ret = ., verbose = TRUE)

# estimate init and lean
bayes_init_priors <- 
  sampling(
    object = compiled_mod_priors, 
    data = mcmc_data_init, 
    iter = n_iterations, 
    warmup = warmup_length, 
    thin = thin_interval, 
    chains = n_chains,
    seed = seednum
  )

bayes_lean_priors <- 
  sampling(
    object = compiled_mod_priors, 
    data = mcmc_data_lean, 
    iter = n_iterations, 
    warmup = warmup_length, 
    thin = thin_interval, 
    chains = n_chains,
    seed = seednum
  )


# save stanfit
saveRDS(bayes_init_priors, here("data", "mcmc", "samples-init-priors.RDS"))
saveRDS(bayes_lean_priors, here("data", "mcmc", "samples-lean-priors.RDS"))


# save tidy
tidy_init_priors <- 
  tidy(bayes_init_priors, estimate.method = "median", conf.int = TRUE) %>%
  mutate(leaners = "Leaners as Unaffiliated") %>%
  as_data_frame() %>%
  print()

tidy_lean_priors <- 
  tidy(bayes_lean_priors, estimate.method = "median", conf.int = TRUE) %>%
  mutate(leaners = "Leaners as Partisans") %>%
  as_data_frame() %>%  
  print()


saveRDS(tidy_init_priors, here("data", "mcmc", "tidy-init-priors.RDS"))
saveRDS(tidy_lean_priors, here("data", "mcmc", "tidy-lean-priors.RDS"))
