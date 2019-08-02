# ----------------------------------------------------
#   making confidence intervals for partials figure
# ----------------------------------------------------

init.merge <- mutate(mod.init$rhs, leaners = "Leaners as Unaffiliated") %>%
              left_join(., mod.init$denoms, by = "cycle") %>%
              print

lean.merge <- mutate(mod.lean$rhs, leaners = "Leaners as Partisans") %>%
              left_join(., mod.lean$denoms, by = "cycle") %>%
              print



dems <- bind_rows(init.merge, lean.merge) %>% 
        select(cycle, contains("D."), contains("dem."), super.denom, leaners) %>%
        select(-contains("loyal")) %>% 
        gather(key = combo, value = n.dems, 
               -cycle, -super.denom, -leaners) %>% 
        mutate(gender = case_when(str_detect(combo, "MD") | 
                                    str_detect(combo, "M.dem") ~ "M", 
                                  str_detect(combo, "WD") | 
                                    str_detect(combo, "W.dem") ~ "W"),
               Source = case_when(str_detect(combo, "dem.votes") ~ "Net Democratic Votes",
                                  str_detect(combo, "pid") ~ "Partisanship",
                                  str_detect(combo, "mob") ~ "Mobilization",
                                  str_detect(combo, "per") ~ "Persuasion",
                                  str_detect(combo, "other") ~ "Unaffiliated")) %>% 
        print


table(dems$combo, dems$Source, exclude = NULL)
table(dems$gender, dems$Source, exclude = NULL)


reps <- bind_rows(init.merge, lean.merge) %>% 
        select(cycle, contains("R."), contains("rep."), super.denom, leaners) %>%
        select(-contains("loyal")) %>% 
        gather(key = combo, value = n.reps, 
               -cycle, -super.denom, -leaners) %>% 
        mutate(gender = case_when(str_detect(combo, "MR") | 
                                    str_detect(combo, "M.rep") ~ "M", 
                                  str_detect(combo, "WR") | 
                                    str_detect(combo, "W.rep") ~ "W"),
               Source = case_when(str_detect(combo, "rep.votes") ~ "Net Democratic Votes",
                                  str_detect(combo, "pid") ~ "Partisanship",
                                  str_detect(combo, "mob") ~ "Mobilization",
                                  str_detect(combo, "per") ~ "Persuasion",
                                  str_detect(combo, "other") ~ "Unaffiliated")) %>%
        print

table(reps$combo, reps$Source, exclude = NULL)
table(reps$gender, reps$Source, exclude = NULL)

# ----------------------------------------------------
#   merge by mechanism and gender?
# ----------------------------------------------------

wide <- 
  full_join(select(dems, -combo), select(reps, -combo), 
            by = c("cycle", "super.denom", "gender", "Source", "leaners")) %>%
  # proportion CI will fail for negative numbers. Make mobilization positive for computation. Needs to be undone again.
  mutate(n.dems = ifelse(Source == "Mobilization", -1 * n.dems, n.dems),
         n.reps = ifelse(Source == "Mobilization", -1 * n.reps, n.reps)) %>% 
  group_by(gender, Source, leaners) %>%
  nest() %>% 
  mutate(intervals = map(data, ~ diff.prop.ci(.$n.dems, .$super.denom, 
                                              .$n.reps, .$super.denom))) %>% 
  unnest() %>% 
  mutate(estimate = ifelse(Source == "Mobilization", -1 * estimate, estimate),
         lower = ifelse(Source == "Mobilization", -1 * lower, lower),
         upper = ifelse(Source == "Mobilization", -1 * upper, upper),
         Source = fct_relevel(Source, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Votes")) %>% 
  select(cycle, gender, Source, estimate:upper, leaners) %>% 
  print


ggplot(wide, aes(x = cycle, y = estimate)) +
  facet_grid(leaners ~ Source) +
  geom_ribbon(aes(ymin = lower, ymax = upper,
                  fill = gender),
              color = NA,
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



