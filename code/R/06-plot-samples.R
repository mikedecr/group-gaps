
list.files(here("data/posts"))

(ggs_init <- readRDS(here("data/posts/ggs-init.RDS")))
(ggs_lean <- readRDS(here("data/posts/ggs-lean.RDS")))



vote_samples <- ggs_init %>%
  filter(str_detect(Parameter, "women") | str_detect(Parameter, "men")) %>% 
  filter(str_detect(Parameter, "adv_")) %>% 
  filter(!str_detect(Parameter, "gap_")) %>% 
  filter(!str_detect(Parameter, "loyal")) %>% 
  mutate(party = case_when(str_detect(Parameter, "dem") ~ "Democratic",
                             str_detect(Parameter, "rep") ~ "Republican"),
         gender = case_when(str_detect(Parameter, "women") ~ "Women", 
                            str_detect(Parameter, "men") ~ "Men"),
         cycle = case_when(str_detect(Parameter, ",") ~ 
                             str_extract(Parameter, "(?<=\\[)(.*)(?=,)"),
                           TRUE ~ as.character(parse_number(Parameter))),
         cycle = (4 * as.numeric(cycle)) + 1948,
         PG = paste(party, gender),
         mech = case_when(str_detect(Parameter, "partisanship") ~ "Partisanship",
                          str_detect(Parameter, "mobilization") ~ "Mobilization",
                          str_detect(Parameter, "persuasion") ~ "Persuasion",
                          str_detect(Parameter, "unaffiliated") ~ "Unaffiliated",
                          str_detect(Parameter, "vote") ~ "Net Democratic Vote"),
         mech = fct_relevel(mech, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Vote"),
         ) %>%
  print()


n_samples <- 50

vote_samples %>%
  group_by(Iteration, Chain) %>%
  nest() %>%
  mutate(iter = 1:n()) %>%
  sample_n(n_samples) %>% 
  unnest() %>%
  ggplot(aes(x = cycle, y = value)) +
    facet_grid(. ~ mech) +
    geom_hline(yintercept = 0) +
    geom_line(aes(linetype = as.factor(iter), color = gender),
              size = 0.5, alpha = 0.3,
              show.legend = FALSE) +
    scale_linetype_manual(values = rep(1, n_samples)) +
    scale_color_manual(values = c(mcolor, wcolor)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(1952, 2012, 12)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.75),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Election Cycle",
         y = "Effect on Net Democratic Votes\n(Pct. of Voting-Eligible Electorate)") +
    geom_text(data = gender_labels_vote, aes(x = x, y = y, label = lab),
              size = 3.5) 





vote_samples %>%
  arrange(cycle) %>% 
  filter(Iteration %in% sample(unique(Iteration), 10, replace = FALSE)) %>% 
  filter(gender == "Men") %>% 
  ggplot(aes(x = cycle, y = value)) +
    facet_grid(. ~ mech) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_line(aes(group = as.factor(Iteration)),
              # alpha = 0.1,
              lineend = "butt",
              linejoin = "mitre",
              show.legend = FALSE) +
    scale_size_manual(values = rep(0.5, 10)) +
    scale_color_manual(values = c(mcolor, wcolor)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(1952, 2012, 12)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.75),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Election Cycle",
         y = "Effect on Net Democratic Votes\n(Pct. of Voting-Eligible Electorate)") +
    geom_text(data = gender_labels_vote, aes(x = x, y = y, label = lab),
              size = 2.75) +
    theme_bw()



