# ----------------------------------------------------
#   too many differences, uncertainty is weird
#   but sampling might be helpful
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


names(anes)

anes %$% table(1 + ((cycle - 1952)/4), exclude = NULL)

anes %$% table(cycle, 1 + (cycle %% 1952) / 4)


bayes_data_init <- anes %>%
  select(outcome_code_init, cycle, cycle_code, wt) %>%
  model.frame() %>%
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


bayes_data_lean <- anes %>%
  select(outcome_code_lean, cycle, cycle_code, wt) %>%
  model.frame() %>%
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


c_mod <- stanc(file = "R/gaps-dynamics.stan")

compiled_mod <- stan_model(stanc_ret = c_mod, verbose = TRUE)

beepr::beep(2)




# # --- test -----------------------

# test_mod <- sampling(object = compiled_mod, 
#                      data = bayes_data_init, 
#                      iter = 500,  
#                      chains = 1)

# beepr::beep(2)


# stan_ac(test_mod)



# --- run -----------------------

set.seed(999)
bayes_init <- 
  sampling(object = compiled_mod, 
           data = bayes_data_init, 
           iter = 1000,
           warmup = 200, 
           thin = 3, 
           chains = 4)

beepr::beep(2)


bayes_lean <- 
  sampling(object = compiled_mod, 
           data = bayes_data_lean, 
           iter = 1000,
           warmup = 200, 
           thin = 3, 
           chains = 4)

beepr::beep(2)




# --- clean and save -----------------------

dir.create(here("data/posts"))

# stan version
saveRDS(bayes_init, here("data/posts/samples-init-diff.RDS"))
saveRDS(bayes_lean, here("data/posts/samples-lean-diff.RDS"))



dynamics <- 
  bind_rows(mutate(ggs(bayes_init), leaners = "Leaners as Unaffiliated"), 
            mutate(ggs(bayes_lean), leaners = "Leaners as Partisans")) %>%
  group_by(Iteration, Chain) %>%
  nest() %>%
  mutate(iter = 1:n()) %>%
  unnest() %>%
  select(-Iteration, -Chain) %>%  
  filter(str_detect(Parameter, "diff")) %>%
  filter(!str_detect(Parameter, "_women") & !str_detect(Parameter, "_men")) %>%
  mutate(
    cycle = case_when(str_detect(Parameter, ",") ~ 
                        str_extract(Parameter, "(?<=\\[)(.*)(?=,)"),
                      TRUE ~ as.character(parse_number(Parameter))),
    cycle = (4 * as.numeric(cycle)) + 1948,
    mech = case_when(str_detect(Parameter, "partisanship") ~ "Partisanship",
                     str_detect(Parameter, "mobilization") ~ "Mobilization",
                     str_detect(Parameter, "persuasion") ~ "Persuasion",
                     str_detect(Parameter, "unaffiliated") ~ "Unaffiliated",
                     str_detect(Parameter, "gap_total") ~ "Net Gender Gap",
                     str_detect(Parameter, "adv_total") ~ "Net Democratic Vote"),
    dv = case_when(str_detect(Parameter, "_gap_") ~ "Gap",
                   str_detect(Parameter, "_adv_") ~ "Vote")) %>% 
  print()


vote_dynamics <- dynamics %>%
  filter(str_detect(Parameter, "_adv_")) %>% 
  mutate(mech = fct_relevel(mech, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Vote")) %>%
  print() 

gap_dynamics <- dynamics %>%
  filter(str_detect(Parameter, "_gap_")) %>% 
  mutate(mech = fct_relevel(mech, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Gender Gap")) %>%
  print() 


count(gap_dynamics, Parameter) %>%
  as.data.frame()

count(vote_dynamics, Parameter) %>%
  as.data.frame()


ggplot(vote_dynamics, aes(x = cycle, y = value)) +
  geom_hline(yintercept = 0, color = mgray) +
  geom_line(aes(group = as.factor(iter)), 
            size = 0.05, alpha = 0.2) +
  facet_grid(leaners ~ mech) +
  scale_x_continuous(breaks = seq(1952, 2012, 12),
                     minor = seq(1952, 2012, 4)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8)) +
  labs(x = "Election Cycle", y = "Change in Democratic Vote")




ggplot(gap_dynamics, aes(x = cycle, y = value)) +
  geom_hline(yintercept = 0, color = mgray) +
  geom_line(aes(group = as.factor(iter)), 
            size = 0.1, alpha = 0.05) +
  facet_grid(leaners ~ mech) +
  scale_x_continuous(breaks = seq(1952, 2012, 12),
                     minor = seq(1952, 2012, 4)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8)) +
  labs(x = "Election Cycle", y = "Change in Gender Gap")




# ----------------------------------------------------
#   tidy gap dynamics
# ----------------------------------------------------

bind_rows(mutate(tidy(bayes_init, conf.int = TRUE, conf.level = .9), 
                 leaners = "Leaners as Unaffiliated"), 
          mutate(tidy(bayes_lean, conf.int = TRUE, conf.level = .9), 
                 leaners = "Leaners as Partisans")) %>%
  filter(str_detect(term, "diff_gap")) %>%
  mutate(
    cycle = case_when(str_detect(term, ",") ~ 
                        str_extract(term, "(?<=\\[)(.*)(?=,)"),
                      TRUE ~ as.character(parse_number(term))),
    cycle = (4 * as.numeric(cycle)) + 1948,
    mech = case_when(str_detect(term, "partisanship") ~ "Partisanship",
                     str_detect(term, "mobilization") ~ "Mobilization",
                     str_detect(term, "persuasion") ~ "Persuasion",
                     str_detect(term, "unaffiliated") ~ "Unaffiliated",
                     str_detect(term, "gap_total") ~ "Net Gender Gap",
                     str_detect(term, "adv_total") ~ "Net Democratic Vote"),
    mech = fct_relevel(mech, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Gender Gap")
   ) %>% 
  ggplot(aes(x = cycle, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_hline(yintercept = 0) +
    geom_ribbon(color = NA, alpha = 0.4) +
    geom_line() +
    facet_grid(leaners ~ mech) +
    scale_x_continuous(breaks = seq(1952, 2012, 12), 
                       minor = seq(1952, 2012, 4)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.8)) + 
    labs(x = "Election Cycle", y = "Change in Gender Gap")


# # tidy version
# tidy_init <- tidy(bayes_init, conf.int = TRUE) %>%
#   mutate(leaners = "Leaners as Unaffiliated") %>%
#   as_data_frame() %>%
#   print()

# tidy_lean <- tidy(bayes_lean, conf.int = TRUE) %>%
#   mutate(leaners = "Leaners as Partisans") %>%
#   as_data_frame() %>%  
#   print()

# saveRDS(tidy_init, here("data/posts/tidy-init.RDS"))
# saveRDS(tidy_lean, here("data/posts/tidy-lean.RDS"))


# # ggs version
# ggs_init <- ggs(bayes_init, description = "init")
# ggs_lean <- ggs(bayes_lean, description = "lean") 

# saveRDS(ggs_init, here("data/posts/ggs-init.RDS"))
# saveRDS(ggs_lean, here("data/posts/ggs-lean.RDS"))






