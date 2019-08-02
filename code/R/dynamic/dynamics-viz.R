# ----------------------------------------------------
#   Dynamics using posterior samples
# ----------------------------------------------------



(bayes_init <- readRDS("data/posts/samples-init.RDS"))
(bayes_lean <- readRDS("data/posts/samples-lean.RDS"))

ggs_init <- ggs(bayes_init, description = "init")
ggs_lean <- ggs(bayes_lean, description = "lean")


samps <- 
  bind_rows(mutate(ggs_init, leaners = "unaffiliated"), 
            mutate(ggs_lean, leaners = "partisans")) %>%
  filter(str_detect(Parameter, "women") | str_detect(Parameter, "men")) %>% 
  filter(str_detect(Parameter, "adv_")) %>% 
  mutate(
    party = case_when(str_detect(Parameter, "dem") ~ "Democratic",
                        str_detect(Parameter, "rep") ~ "Republican"),
    gender = case_when(str_detect(Parameter, "women") ~ "Women", 
                       str_detect(Parameter, "men") ~ "Men"),
    cycle = case_when(str_detect(Parameter, ",") ~ 
                        str_extract(Parameter, "(?<=\\[)(.*)(?=,)"),
                      TRUE ~ as.character(parse_number(Parameter))),
    cycle = (4 * as.numeric(cycle)) + 1948,
    mech = case_when(str_detect(Parameter, "partisanship") ~ "Partisanship",
                     str_detect(Parameter, "mobilization") ~ "Mobilization",
                     str_detect(Parameter, "persuasion") ~ "Persuasion",
                     str_detect(Parameter, "unaffiliated") ~ "Unaffiliated",
                     str_detect(Parameter, "vote") ~ "Net Democratic Vote"),
    mech = fct_relevel(mech, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Vote")
   ) %>% 
  print()

beepr::beep(2)

samps %>%
count(party)



diffs <- samps %>%
  select(-party, -Parameter) %>% 
  group_by(Iteration, Chain) %>%
  nest() %>%
  mutate(iter = 1:n()) %>%
  unnest() %>% 
  select(-Chain, -Iteration) %>%
  group_by(iter, leaners, gender, mech) %>%
  mutate(lag1 = lag(value),
         diff1 = value - lag1) %>%
  ungroup() %>%
  print()

ggplot(diffs, aes(x = lag1, y = lag(value))) + geom_point()

beepr::beep(2)


diffs %>% 
  ggplot(aes(x = cycle, y = diff1, color = gender)) +
    geom_line(aes(group = iter), size = 0.2, alpha = 0.2) +
    facet_grid(leaners ~ mech) 



