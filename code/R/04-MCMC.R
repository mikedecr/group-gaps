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
  as.data.frame()



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


c_mod <- stanc(file = "R/gaps.stan")
# c_mod <- stanc(file = "R/gaps-dynamics.stan")
# c_mod <- stanc(file = "R/ugh.stan")

compiled_mod <- 
  stanc(file = "code", "stan", "gaps.stan") %>%
  stan_model(stanc_ret = ., verbose = TRUE)

beepr::beep(2)



# tuning parameters
(n_iterations <- 2000)
(warmup_length <- 1000)
(thin_interval <- 1)
(n_chains <- parallel::detectCores())

write(scales::comma(n_iterations), here("tex/refs/mcmc-iterations.tex"))
write(scales::comma(warmup_length), here("tex/refs/mcmc-warmup.tex"))
write(scales::comma(thin_interval), here("tex/refs/mcmc-thin.tex"))
write(scales::comma(n_chains), here("tex/refs/mcmc-chains.tex"))




# # --- test -----------------------

# test_mod <- sampling(object = compiled_mod, 
#                      data = mcmc_data_init, 
#                      iter = 500,  
#                      chains = 1)

# beepr::beep(2)


# stan_ac(test_mod)



# --- run -----------------------


set.seed(999)
bayes_init <- 
  sampling(object = compiled_mod, 
           data = mcmc_data_init, 
           iter = n_iterations,
           warmup = warmup_length, 
           thin = thin_interval, 
           chains = n_chains)



bayes_lean <- 
  sampling(object = compiled_mod, 
           data = mcmc_data_lean, 
           iter = n_iterations,
           warmup = warmup_length, 
           thin = thin_interval, 
           chains = n_chains)

beepr::beep(2)


# # linstat
# bayes_init <- 
#   sampling(object = compiled_mod, 
#            data = mcmc_data_init, 
#            iter = 500,
#            thin = 3, 
#            chains = parallel::detectCores())

# beepr::beep(2)

# bayes_lean <- 
#   sampling(object = compiled_mod, 
#            data = mcmc_data_lean, 
#            iter = 500,
#            thin = 3, 
#            chains = parallel::detectCores())




# --- clean and save -----------------------

dir.create(here("data/posts"))

# stan version
saveRDS(bayes_init, here("data/posts/samples-init.RDS"))
saveRDS(bayes_lean, here("data/posts/samples-lean.RDS"))


# tidy version
tidy_init <- tidy(bayes_init, estimate.method = "median", conf.int = TRUE) %>%
  mutate(leaners = "Leaners as Unaffiliated") %>%
  as_data_frame() %>%
  print()

tidy_lean <- tidy(bayes_lean, estimate.method = "median", conf.int = TRUE) %>%
  mutate(leaners = "Leaners as Partisans") %>%
  as_data_frame() %>%  
  print()

saveRDS(tidy_init, here("data/posts/tidy-init.RDS"))
saveRDS(tidy_lean, here("data/posts/tidy-lean.RDS"))


# ggs version
ggs_init <- ggs(bayes_init, description = "init")
ggs_lean <- ggs(bayes_lean, description = "lean") 

saveRDS(ggs_init, here("data/posts/ggs-init.RDS"))
saveRDS(ggs_lean, here("data/posts/ggs-lean.RDS"))
