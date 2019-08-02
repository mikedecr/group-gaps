# ----------------------------------------------------
#   Burden & DeCrescenzo, Gender gap
#   ---
#   File 04: 
#   run model, plot RHS, plot partial terms
# ----------------------------------------------------


# check for NUKE



# ----------------------------------------------------
#   data (should be unchanged since cleaning)
# ----------------------------------------------------

anes <- readRDS("data/cleaned-anes-cdf.RDS") %>% 
  print()





# --- slight data preparations -----------------------

# create vote outcome variable:
# you don't vote, vote D, vote R, or vote other

table(as.factor(anes$VCF0706), exclude = NULL)

anes <- anes %>% 
mutate(vote = fct_recode(as.factor(VCF0706), 
                                 "dvote" = "1",
                                 "rvote" = "2", 
                                 "nonvote" = "7", 
                                 "other" = "3", 
                                 "other" = "4", 
                                 "missing" = "0"))


# data_table %$% table(cycle, vote, exclude = NULL)

table(anes$vote, exclude = NULL)



# ----------------------------------------------------
#   Estimate
# ----------------------------------------------------


mod_init <- run_model(anes, anes$pid_init) %>% print
mod_lean <- run_model(anes, anes$pid_lean) %>% print




# ----------------------------------------------------
#   plot RHS
# ----------------------------------------------------




# drop "mobilization" and instead plot loyal votes directly
# (already in the RHS sublists)

rhs <- 
  bind_rows(mutate(mod_init$rhs, leaners = "Leaners as Unaffiliated"), 
            mutate(mod_lean$rhs, leaners = "Leaners as Partisans")) %>%
  select(-MD_mob, -WD_mob, -MR_mob, -WR_mob) %>%
  gather(key = Source, value = N, MD_pid:WR_loyal) %>%
  select(cycle, Source, N, leaners) %>%
  separate(Source, into = c("PG", "Source")) %>%
  left_join(., mod_init$denoms, by = "cycle") %>%
  mutate(prop = N / super_denom, 
         Source = fct_recode(Source, "Partisanship" = "pid", 
                             "Mobilization" = "loyal", 
                             "Persuasion" = "per", 
                             "Unaffiliated" = "other"), 
         Source = fct_relevel(Source, 
                              "Partisanship", "Mobilization", "Persuasion", "Unaffiliated"), 
         # ifelse(Source == "pid", "Partisanship", ifelse(Source == "mob", "Mobilization", ifelse(Source == "per", "Persuasion", "Other"))), Source = factor(Source, levels = c("Partisanship", "Mobilization", "Persuasion", "Other")), 
         PG = fct_recode(PG, 
                         "Dem. Votes (Men)" = "MD", 
                         "Dem. Votes (Women)" = "WD", 
                         "Rep. Votes (Men)" = "MR", 
                         "Rep. Votes (Women)" = "WR"), 
         PG = fct_relevel(PG, 
                          "Dem. Votes (Men)", "Dem. Votes (Women)", "Rep. Votes (Men)", "Rep. Votes (Women)"), 
         party = ifelse(grepl("Dem.", PG), "Dem.", "Rep."), 
         gender = ifelse(grepl("Men", PG), "Men", "Women"), 
         leaners = fct_relevel(leaners, "Leaners as Unaffiliated") 
         # , swap parties for Persuasion...since it's currently coded as the "benefitting" party) 
         # party = ifelse(party == "Democratic" & Source == "Persuasion", "Rswitch", party), party = ifelse(party == "Republican" & Source == "Persuasion", "Dswitch", party), 
         # party = ifelse(party == "Rswitch", "Republican", party),
         #  party = ifelse(party == "Dswitch", "Democratic", party)
      ) %>%
  select(cycle, party, leaners, gender, Source, prop, PG) %>% 
  print




ggplot(data = rhs, aes(x = cycle, y = prop, color = PG, shape = PG)) +
  facet_grid(leaners ~ Source) +
  geom_hline(yintercept = 0) +
  geom_line(size = 0.4, show.legend = FALSE) +
  geom_point(fill = "white") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c(dblue, dblue, rred, rred)) +
  scale_shape_manual(values = c(16, 21, 16, 21)) +
  labs(x = "Election Cycle", 
       y = "Percent of Eligible Electorate", 
       shape = NULL, 
       color = NULL) +
  scale_x_continuous(breaks = seq(1956, 2012, 8)) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.75),
        legend.position = "bottom")




ggsave("tex/graphics/rhs.pdf", height = 5, width = 8) 



# ----------------------------------------------------
#   NUKE dropped:
#   re-doing regressions using "net gender gap"
# ----------------------------------------------------





# ----------------------------------------------------
#   Plot partial impacts with CIs
# ----------------------------------------------------

# add denominators to RHS

init_merge <- mutate(mod_init$rhs, leaners = "Leaners as Unaffiliated") %>%
              left_join(., mod_init$denoms, by = "cycle") %>%
              print

lean_merge <- mutate(mod_lean$rhs, leaners = "Leaners as Partisans") %>%
              left_join(., mod_lean$denoms, by = "cycle") %>%
              print



# Differencing across parties requires separate Dem and Rep tables

dems <- bind_rows(init_merge, lean_merge) %>%
  select(cycle, contains("D_"), contains("dem_"), super_denom, leaners) %>%
  select(-contains("loyal")) %>% 
  gather(key = combo, value = n_dems, 
         -cycle, -super_denom, -leaners) %>% 
  mutate(gender = case_when(str_detect(combo, "MD") | 
                              str_detect(combo, "M.dem") ~ "M",
                            str_detect(combo, "WD") | 
                              str_detect(combo, "W.dem") ~ "W"),
         Source = case_when(str_detect(combo, "dem.votes") ~ "Net Democratic Votes",
                            str_detect(combo, "pid") ~ "Partisanship",
                            str_detect(combo, "mob") ~ "Mobilization",
                            str_detect(combo, "per") ~ "Persuasion",
                            str_detect(combo, "other") ~ "Unaffiliated")) %>% 
  print()


table(dems$combo, dems$Source, exclude = NULL)
table(dems$gender, dems$Source, exclude = NULL)


reps <- bind_rows(init_merge, lean_merge) %>% 
  select(cycle, contains("R_"), contains("rep_"), super_denom, leaners) %>%
  select(-contains("loyal")) %>% 
  gather(key = combo, value = n_reps, 
         -cycle, -super_denom, -leaners) %>% 
  mutate(gender = case_when(str_detect(combo, "MR") | 
                              str_detect(combo, "M.rep") ~ "M", 
                            str_detect(combo, "WR") | 
                              str_detect(combo, "W.rep") ~ "W"),
         Source = case_when(str_detect(combo, "rep.votes") ~ "Net Democratic Votes",
                            str_detect(combo, "pid") ~ "Partisanship",
                            str_detect(combo, "mob") ~ "Mobilization",
                            str_detect(combo, "per") ~ "Persuasion",
                            str_detect(combo, "other") ~ "Unaffiliated")) %>%
  print()

table(reps$combo, reps$Source, exclude = NULL)
table(reps$gender, reps$Source, exclude = NULL)



# compute CIs for differences in proportions
# This will fail for negative successes, so make mobilization positive
# Undo after finding CI

wide <- 
  full_join(select(dems, -combo), select(reps, -combo), 
            by = c("cycle", "super_denom", "gender", "Source", "leaners")) %>%
  mutate(n_dems = ifelse(Source == "Mobilization", -1 * n_dems, n_dems),
         n_reps = ifelse(Source == "Mobilization", -1 * n_reps, n_reps),
         leaners = fct_relevel(leaners, "Leaners as Unaffiliated", "Leaners as Partisans")) %>% 
  group_by(gender, Source, leaners) %>%
  nest() %>% 
  mutate(intervals = map(data, ~ diff_prop_ci(.$n_dems, .$super_denom, 
                                              .$n_reps, .$super_denom))) %>% 
  unnest() %>% 
  mutate(estimate = ifelse(Source == "Mobilization", -1 * estimate, estimate),
         lower = ifelse(Source == "Mobilization", -1 * lower, lower),
         upper = ifelse(Source == "Mobilization", -1 * upper, upper),
         Source = fct_relevel(Source, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Votes")) %>% 
  select(cycle, gender, Source, estimate:upper, leaners) %>% 
  print



# gender labels for plotting 5-panel partial effects plot

gender.labels <- 
  data_frame(x = rep(1985, 2), 
             y = c(.18, -.05), 
             lab = c("Women", "Men"), 
             # leaners = "Leaners as Unaffiliated", 
             Source = as.factor("Partisanship")) %>% 
  mutate(Source = fct_relevel(Source, 
                              "Partisanship", 
                              "Mobilization", 
                              "Persuasion", 
                              "Unaffiliated", 
                              "Net Democratic Votes")) %>%
  print()



ggplot(wide, aes(x = cycle, y = estimate)) +
  facet_grid(leaners ~ Source) +
  geom_ribbon(aes(ymin = lower, ymax = upper,
                  fill = gender),
              color = NA,
              alpha = 0.3,
              show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_line(aes(color = gender),
            show.legend = FALSE) +
  geom_point(aes(shape = gender, color = gender),
             fill = "white",
             show.legend = FALSE) +
  scale_shape_manual(values = c(16, 21)) +
  scale_linetype_manual(values = c(1, 2)) +
  scale_color_manual(values = c(mcolor, wcolor)) +
  scale_fill_manual(values = c(mcolor, wcolor)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1956, 2012, 8)) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.75)) +
  labs(x = "Election Cycle",
       y = "Effect on Net Democratic Votes\n(Percent of Voting-Eligible Electorate)") +
  geom_text(data = gender.labels, aes(x = x, y = y, label = lab),
            size = 2.75)


ggsave("tex/graphics/vote-partials.pdf", height = 4.5, width = 9) 

