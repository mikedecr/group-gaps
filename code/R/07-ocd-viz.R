# ----------------------------------------------------
#   analyze posterior samples
# ----------------------------------------------------

bayes_init <- readRDS("data/posts/samples-init.RDS")
bayes_lean <- readRDS("data/posts/samples-lean.RDS")


# --- quick diagnosis -----------------------

check_hmc_diagnostics(bayes_init)
check_hmc_diagnostics(bayes_lean)



# ----------------------------------------------------
#   checking
# ----------------------------------------------------

ggs_init <- ggs(bayes_init, description = "init")
ggs_lean <- ggs(bayes_lean, description = "lean")


ggs_init










tidy_bayes <- 
  bind_rows(
    mutate(broom::tidyMCMC(bayes_init, conf.int = TRUE), pid = "Leaners as Unaffiliated"), 
    mutate(broom::tidyMCMC(bayes_lean, conf.int = TRUE), pid = "Leaners as Partisans")
   ) %>% 
  as_data_frame() %>% 
  mutate(pid = fct_rev(pid),
         party = case_when(str_detect(term, "dem") ~ "Democratic",
                             str_detect(term, "rep") ~ "Republican"),
         gender = case_when(str_detect(term, "women") ~ "Women", 
                            str_detect(term, "men") ~ "Men")) %>% 
  print()



# ----------------------------------------------------
#   right-hand side
# ----------------------------------------------------

tidy_bayes %>%
  filter(!str_detect(term, "theta") & 
         !str_detect(term, "adv_") &
         !str_detect(term, "mobilization")) %>% 
  mutate(PG = paste(party, gender),
         year = sapply(str_split(term, pattern = "\\[") , function(x) x[2]),
         year = sapply(str_split(year, pattern = "\\]") , function(x) x[1]),
         year = ((as.numeric(year) - 1) * 4) + 1952,
         term = case_when(str_detect(term, "partisanship") ~ "Partisanship",
                          str_detect(term, "loyal") ~ "Mobilization",
                          str_detect(term, "persuasion") ~ "Persuasion",
                          str_detect(term, "unaffiliated") ~ "Unaffiliated"),
         term = fct_relevel(term, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated")) %>%
  ggplot(aes(x = year, y = estimate, color = PG)) +
    facet_grid(pid ~ term) +
    geom_hline(yintercept = 0) +
    geom_line(size = 0.4, show.legend = FALSE) +
    geom_point(aes(shape = PG), fill = "white") +
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

ggsave(here::here("tex/graphics/mc-rhs.pdf"), height = 5, width = 8)





# ----------------------------------------------------
#   partial effects
# ----------------------------------------------------


gender.labels <- 
  data_frame(x = rep(1985, 2), 
             y = c(.18, -.05), 
             lab = c("Women", "Men"), 
             # leaners = "Leaners as Unaffiliated", 
             term = as.factor("Partisanship")) %>% 
  mutate(term = fct_relevel(term, 
                              "Partisanship", 
                              "Mobilization", 
                              "Persuasion", 
                              "Unaffiliated", 
                              "Net Democratic Votes")) %>%
  print()


tidy_bayes %>% 
  filter(str_detect(term, "adv_")) %>% 
  filter(!str_detect(term, "loyal")) %>% 
  mutate(PG = paste(party, gender),
        year = sapply(str_split(term, pattern = "\\[") , function(x) x[2]),
        year = sapply(str_split(year, pattern = "\\]") , function(x) x[1]),
        year = ((as.numeric(year) - 1) * 4) + 1952,
        term = case_when(str_detect(term, "partisanship") ~ "Partisanship",
                         str_detect(term, "mobilization") ~ "Mobilization",
                         str_detect(term, "persuasion") ~ "Persuasion",
                         str_detect(term, "unaffiliated") ~ "Unaffiliated",
                         str_detect(term, "vote") ~ "Net Democratic Vote"),
        term = fct_relevel(term, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Vote")) %>%
  ggplot(aes(x = year, y = estimate)) +
    facet_grid(pid ~ term) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
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

ggsave("tex/graphics/mc-partials.pdf", height = 4.5, width = 9) 


# ----------------------------------------------------
#   densities!
# ----------------------------------------------------

