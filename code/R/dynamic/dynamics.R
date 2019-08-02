# ----------------------------------------------------
#   Burden & DeCrescenzo, Gender gap
#   ---
#   File 05 (testing): 
#   dynamic analysis (first differences) of model 
# ----------------------------------------------------


# check for NUKE


ls()

diff_partials <- 
  bind_rows(mutate(mod_lean$partials, leaners = "Leaners as Partisans"), 
            mutate(mod_init$partials, leaners = "Leaners as Unaffiliated")) %>% 
  arrange(leaners, Gender, Source, cycle) %>% 
  group_by(Gender, Source, leaners) %>% 
  mutate(diff_gap = gap - lag(gap), 
         diff_vote = Impact - lag(Impact),
         gap_running_sum = cumsum(ifelse(is.na(diff_gap), 0, diff_gap)),
         vote_running_sum = cumsum(ifelse(is.na(diff_vote), 0, diff_vote))) %>%
  print()



# potentially divergent findings for men and women overall. When women contribute to the gap, it helps Democrats. When men do, mixed. 
# when women grow the gap via partisanship, bigger partial impact on vote than men

ggplot(data = diff_partials, aes(x = diff_gap, y = diff_vote)) +
  geom_point(aes(color = Gender)) +
  geom_smooth(aes(color = Gender,
                  fill = Gender),
              method = "lm") +
  facet_grid(leaners ~ Source)


ggplot(diff_partials, aes(x = cycle, y = diff_vote)) +
  geom_line(aes(color = Gender)) +
  geom_point(aes(color = Gender)) +
  facet_grid(leaners ~ Source)



ggplot(diff_partials, aes(x = cycle, y = diff_gap)) +
  geom_line(aes(color = Gender)) +
  geom_point(aes(color = Gender)) +
  facet_grid(leaners ~ Source)


ggplot(diff_partials, aes(x = cycle, y = vote_running_sum)) +
  geom_line(aes(color = Gender)) +
  geom_point(aes(color = Gender)) +
  facet_grid(leaners ~ Source)



ggplot(diff_partials, aes(x = cycle, y = gap_running_sum)) +
  geom_line(aes(color = Gender)) +
  geom_point(aes(color = Gender)) +
  facet_grid(leaners ~ Source)



beepr::beep(2)