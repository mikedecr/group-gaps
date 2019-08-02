# ----------------------------------------------------
#   analyze posterior samples
# ----------------------------------------------------
# (bayes_init <- readRDS("data/posts/samples-init.RDS"))
# (bayes_lean <- readRDS("data/posts/samples-lean.RDS"))

(tidy_init <- readRDS(here("data/posts/tidy-init.RDS")))
(tidy_lean <- readRDS(here("data/posts/tidy-lean.RDS")))

# ggs_init <- ggs(bayes_init, description = "init")
# ggs_lean <- ggs(bayes_lean, description = "lean")


# --- quick diagnosis -----------------------

######### INSERET DIAGNOSTIC FILE #######

# check_hmc_diagnostics(bayes_init)
# check_hmc_diagnostics(bayes_lean)




# ----------------------------------------------------
#   checking
# ----------------------------------------------------

# ???
# ----------------------------------------------------
#   densities!
# ----------------------------------------------------




# ----------------------------------------------------
#   mashing
# ----------------------------------------------------





# cycle: if term has a comma, grab between "[" and ",", else parse
# then convert to numeric and scale back up
tidy_bayes <- bind_rows(tidy_init, tidy_lean) %>%
  mutate(leaners = fct_rev(leaners),
         party = case_when(str_detect(term, "dem") ~ "Democratic",
                             str_detect(term, "rep") ~ "Republican"),
         gender = case_when(str_detect(term, "women") ~ "Women", 
                            str_detect(term, "men") ~ "Men"),
         cycle = case_when(str_detect(term, ",") ~ 
                             str_extract(term, "(?<=\\[)(.*)(?=,)"),
                           TRUE ~ as.character(parse_number(term))),
         cycle = (4 * as.numeric(cycle)) + 1948) %>%  
  as_data_frame() %>% 
  print()








# ----------------------------------------------------
#   right-hand side
# ---------------------------------------------------- 
rhs <- tidy_bayes %>%
  filter(!str_detect(term, "theta") & 
         !str_detect(term, "adv_") &
         !str_detect(term, "gap_") &
         !str_detect(term, "mobilization")) %>% 
  mutate(PG = paste(party, gender),
         term = case_when(str_detect(term, "partisanship") ~ "Partisanship",
                          str_detect(term, "loyal") ~ "Partisanship +\nMobilization",
                          str_detect(term, "persuasion") ~ "Persuasion",
                          str_detect(term, "unaffiliated") ~ "Unaffiliated"),
         term = fct_relevel(term, "Partisanship", "Partisanship +\nMobilization", "Persuasion", "Unaffiliated")) %>%
  print()


ggplot(rhs, aes(x = cycle, y = estimate, color = PG)) +
  facet_grid(leaners ~ term) +
  geom_hline(yintercept = 0, color = mgray, size = 0.25) +
  geom_line(size = 0.4, show.legend = FALSE) +
  geom_point(aes(shape = PG), fill = "white") +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c(dblue, dblue, rred, rred)) +
  scale_shape_manual(values = c(16, 21, 16, 21)) +
  labs(x = "Election Cycle", 
       y = "Percent of Eligible Electorate", 
       shape = NULL, 
       color = NULL) +
  scale_x_continuous(breaks = seq(1952, 2016, 16)) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.75),
        legend.position = "bottom",
        panel.grid.minor.x = element_blank())

ggsave(here::here("tex/appendix/mc-rhs-appendix.pdf"), 
       height = 5, width = 8, device = cairo_pdf)



rhs %>%
  filter(leaners == "Leaners as Partisans") %>% 
  ggplot(aes(x = cycle, y = estimate, color = PG)) +
    facet_grid(. ~ term) +
    geom_hline(yintercept = 0, color = mgray, size = 0.25) +
    geom_line(size = 0.4, show.legend = FALSE) +
    geom_point(aes(shape = PG), fill = "white") +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = c(dblue, dblue, rred, rred)) +
    scale_shape_manual(values = c(22, 16, 22, 16)) +
    # scale_shape_manual(values = c(15, 16, 21, 22)) +
    labs(x = "Election Cycle", y = "Percent of Eligible Electorate",
         shape = NULL, color = NULL) +
    scale_x_continuous(breaks = seq(1952, 2016, 16)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.75),
          legend.position = "bottom",
          panel.grid.minor.x = element_blank())

ggsave(here::here("tex/graphics/mc-rhs.pdf"), 
       height = 3.5, width = 7, device = cairo_pdf)



# ----------------------------------------------------
#   partial effects on gap
# ----------------------------------------------------


gender_labels_gap <- 
  data_frame(x = c(1979, 1990, 1996, 1982) ,
             y = c(.14, -.05, -0.075, 0.075), 
             lab = c("Women", "Men", "Women", "Men"), 
             mech = as.factor(rep(c("Partisanship", "Mobilization"), 
                                  each = 2))) %>% 
  mutate(mech = fct_relevel(mech, "Partisanship", "Mobilization", 
                            "Persuasion", "Unaffiliated", 
                            "Net Democratic Votes")) %>%
  print()



tidy_bayes %>%
  count(cycle) %>%
  as.data.frame



# gaps frame
mc_gap <- tidy_bayes %>%
  filter(str_detect(term, "gap")) %>%
  mutate(mech = case_when(str_detect(term, "partisanship") ~ "Partisanship",
                          str_detect(term, "mobilization") ~ "Mobilization",
                          str_detect(term, "persuasion") ~ "Persuasion",
                          str_detect(term, "unaffiliated") ~ "Unaffiliated",
                          str_detect(term, "total") ~ "Net Gender Gap"),
         mech = fct_relevel(mech, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Gender Gap")) %>%
  select(-party, -gender, -std.error) %>% 
  select(cycle, mech, estimate, contains("conf"), leaners, matches(".")) %>%   
  print()

gap_partials <- tidy_bayes %>%
  filter(str_detect(term, "women") | str_detect(term, "men")) %>% 
  filter(str_detect(term, "adv_")) %>% 
  filter(!str_detect(term, "loyal")) %>% 
  filter(!str_detect(term, "vote")) %>% 
  mutate(PG = paste(party, gender),
         mech = case_when(str_detect(term, "partisanship") ~ "Partisanship",
                          str_detect(term, "mobilization") ~ "Mobilization",
                          str_detect(term, "persuasion") ~ "Persuasion",
                          str_detect(term, "unaffiliated") ~ "Unaffiliated"),
         mech = fct_relevel(mech, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Gender Gap"),
         estimate = ifelse(gender == "Men", -1 * estimate, estimate)) %>%
  print()



ggplot(gap_partials, aes(x = cycle, y = estimate)) +
    facet_grid(leaners ~ mech) +
    geom_hline(yintercept = 0, color = mgray, size = 0.25) +
    geom_ribbon(data = mc_gap, 
                aes(x = cycle, y = estimate, 
                    ymin = conf.low, ymax = conf.high),
                color = NA, fill = mgray,
                alpha = 0.5) +
    geom_line(aes(color = gender), show.legend = FALSE, size = 0.5) +
    geom_point(aes(shape = gender, color = gender),
               fill = "white",
               show.legend = FALSE) +
    geom_line(data = mc_gap, aes(x = cycle, y = estimate)) +
    scale_shape_manual(values = c(22, 16)) +
    scale_linetype_manual(values = c(1, 2)) +
    scale_color_manual(values = c(mcolor, wcolor)) +
    scale_fill_manual(values = c(mcolor, wcolor)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(1952, 2016, 16)) +
    theme(axis.text.x = element_text(angle = 45, vjust=0.75),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Election Cycle",
         y = "Effect of Democartic Advantage \n on Net Gender Gap\n(Pct. of Total Electorate)") +
    geom_text(data = gender_labels_gap, aes(x = x, y = y, label = lab),
              size = 2.75)

ggsave(here("tex/appendix/mc-gap-partials-appendix.pdf"), 
       height = 4.5, width = 9, device = cairo_pdf)





gap_partials %>%
  filter(leaners == "Leaners as Partisans") %>% 
  ggplot(aes(x = cycle, y = estimate)) +
    facet_grid(. ~ mech) +
    geom_hline(yintercept = 0, color = mgray, size = 0.25) +
    geom_ribbon(data = filter(mc_gap, leaners == "Leaners as Unaffiliated"), 
                aes(ymin = conf.low, ymax = conf.high),
                color = NA, fill = mgray,
                alpha = 0.5) +
    geom_line(aes(color = gender), show.legend = FALSE, size = 0.5) +
    geom_point(aes(shape = gender, color = gender),
               fill = "white",
               show.legend = FALSE) +
    geom_line(data = filter(mc_gap, leaners == "Leaners as Unaffiliated"), 
              aes(x = cycle, y = estimate)) +
    scale_shape_manual(values = c(22, 16)) +
    scale_linetype_manual(values = c(1, 2)) +
    scale_color_manual(values = c(mcolor, wcolor)) +
    scale_fill_manual(values = c(mcolor, wcolor)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(1952, 2016, 16)) +
    theme(axis.text.x = element_text(angle = 45, vjust=0.75),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Election Cycle",
         y = "Effect of Democratic Advantage \n on Net Gender Gap\n(Pct. of Total Electorate)") +
    geom_text(data = gender_labels_gap, aes(x = x, y = y, label = lab),
              size = 2.75)

ggsave(here("tex/graphics/mc-gap-partials.pdf"), 
       height = 3, width = 9, device = cairo_pdf)





# ----------------------------------------------------
#   partial effects on vote
# ----------------------------------------------------


gender_labels_vote <- 
  data_frame(x = c(1985, 1990), 
             y = c(.16, -.06), 
             lab = c("Women", "Men"), 
             # leaners = "Leaners as Unaffiliated", 
             mech = as.factor("Partisanship")) %>% 
  mutate(mech = fct_relevel(mech, 
                            "Partisanship", 
                            "Mobilization", 
                            "Persuasion", 
                            "Unaffiliated", 
                            "Net Democratic Vote")) %>%
  print()


vote_partials <- tidy_bayes %>% 
  filter(str_detect(term, "women") | str_detect(term, "men")) %>% 
  filter(str_detect(term, "adv_")) %>% 
  filter(!str_detect(term, "gap_")) %>% 
  filter(!str_detect(term, "loyal")) %>% 
  mutate(PG = paste(party, gender),
         mech = case_when(str_detect(term, "partisanship") ~ "Partisanship",
                          str_detect(term, "mobilization") ~ "Mobilization",
                          str_detect(term, "persuasion") ~ "Persuasion",
                          str_detect(term, "unaffiliated") ~ "Unaffiliated",
                          str_detect(term, "vote") ~ "Net Democratic Vote"),
         mech = fct_relevel(mech, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Vote")) %>%
  print()





ggplot(vote_partials, aes(x = cycle, y = estimate)) +
  facet_grid(leaners ~ mech) +
  geom_hline(yintercept = 0, color = mgray, size = 0.25) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
                  fill = gender),
              color = NA,
              alpha = 0.3,
              show.legend = FALSE) +
  geom_line(aes(color = gender),
            show.legend = FALSE) +
  geom_point(aes(shape = gender, color = gender),
             fill = "white",
             show.legend = FALSE) +
  scale_shape_manual(values = c(22, 16)) +
  scale_linetype_manual(values = c(1, 2)) +
  scale_color_manual(values = c(mcolor, wcolor)) +
  scale_fill_manual(values = c(mcolor, wcolor)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1952, 2016, 16)) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.75),
        panel.grid.minor.x = element_blank()) +
  labs(x = "Election Cycle",
       y = "Effect of Democratic Advantage \n on Net Democratic Votes\n(Pct. of Total Electorate)") +
  geom_text(data = gender_labels_vote, aes(x = x, y = y, label = lab),
            size = 2.75)

ggsave(here("tex/appendix/mc-vote-partials-appendix.pdf"), 
       height = 4.5, width = 9, device = cairo_pdf)



vote_partials %>%
  filter(leaners == "Leaners as Partisans") %>% 
  ggplot(aes(x = cycle, y = estimate)) +
    facet_grid(. ~ mech) +
    geom_hline(yintercept = 0, color = mgray, size = 0.25) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high,
                    fill = gender),
                color = NA,
                alpha = 0.5,
                show.legend = FALSE) +
    geom_line(aes(color = gender), show.legend = FALSE, size = 0.5) +
    geom_point(aes(shape = gender, color = gender),
               fill = "white",
               show.legend = FALSE) +
    scale_shape_manual(values = c(22, 16)) +
    scale_linetype_manual(values = c(1, 2)) +
    scale_color_manual(values = c(mcolor, wcolor)) +
    scale_fill_manual(values = c(mcolor, wcolor)) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = seq(1952, 2016, 16)) +
    theme(axis.text.x = element_text(angle = 45, vjust=0.75),
          panel.grid.minor.x = element_blank()) +
    labs(x = "Election Cycle",
         y = "Effect of Democratic Advantage \n on Net Democratic Votes\n(Pct. of Total Electorate)") +
    geom_text(data = gender_labels_vote, aes(x = x, y = y, label = lab),
              size = 2.75) +
    theme(panel.grid.minor.x = element_blank())


ggsave(here("tex/graphics/mc-vote-partials.pdf"), 
       height = 3, width = 9, device = cairo_pdf)




vote_partials %>%
  filter(leaners == "Leaners as Partisans") %>% 
  filter(gender == "Women") %>%
  filter(mech == "Unaffiliated") %>% 
  filter(estimate == max(estimate))



# # ----------------------------------------------------
# #   compare measures
# # ----------------------------------------------------
# # vote tables
# compare_netdem <- tidy_bayes %>%
#   filter(str_detect(term, "adv_vote_men") | 
#          str_detect(term, "adv_vote_women")) %>%
#   mutate(trend = "Net Democratic Vote") %>% 
#   rename(dem_vote = estimate, lower = conf.low, upper = conf.high) %>%
#   select(cycle, gender, dem_vote, lower, upper, trend) %>%
#   arrange(cycle, gender) %>%
#   print()

# compare_demshare <- plot_vote_gender %>%
#   select(cycle, gender, contains("dem.share")) %>%
#   rename(dem_vote = dem.share, lower = dem.share.lower, upper = dem.share.upper) %>% 
#   mutate(trend = "Two-Party Vote") %>% 
#   arrange(cycle, gender) %>%
#   print()

# # gap tables
# compare_netgap <- tidy_bayes %>%
#   filter(str_detect(term, "gap_total")) %>%
#   mutate(trend = "Net Gender Gap") %>%
#   rename(gap = estimate, lower = conf.low, upper = conf.high) %>%
#   select(cycle, gap, lower, upper, trend) %>%
#   print()

# compare_gap <- gap_frame %>%
#   select(cycle, contains("gap")) %>% 
#   rename(gap = gender_gap, lower = gap_lower, upper = gap_upper) %>% 
#   mutate(trend = "Gender Gap") %>% 
#   print()



# # --- patwork attempt -----------------------

# # vote plots 
# ggplot(compare_demshare, aes(x = cycle, y = dem_vote, color = gender, fill = gender)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper, color = NULL, ),
#               alpha = 0.3, show.legend = FALSE) +
#   facet_grid(. ~ trend) +
#   geom_hline(yintercept = 0, color = mgray, size = 0.25) +
#   labs(y = "Percentage Vote Margin") +
# ggplot(compare_netdem, aes(x = cycle, y = dem_vote, color = gender, fill = gender)) +
#   geom_ribbon(aes(ymin = lower, ymax = upper, color = NULL), 
#               alpha = 0.3, show.legend = FALSE) +
#   facet_grid(. ~ trend) +
#   geom_hline(yintercept = 0, color = mgray, size = 0.25) +
#   labs(y = "Percentage of Eligible Electorate") +
# # gap plots 
# ggplot(compare_gap, aes(x = cycle, y = gap)) +
#   facet_grid(. ~ trend) +
#   geom_hline(yintercept = 0, color = mgray, size = 0.25) +
#   coord_cartesian(ylim = c(-0.1, 0.15)) +
#   labs(y = "Gender Difference in Vote Share") +
# ggplot(compare_netgap, aes(x = cycle, y = gap)) +
#     facet_grid(. ~ trend) +
#     geom_hline(yintercept = 0, color = mgray, size = 0.25) +
#   coord_cartesian(ylim = c(-0.1, 0.15)) +
#     labs(y = "Gender Difference in Net Votes") +
# # layout
# plot_layout(ncol = 2, nrow = 2) &
#   geom_line(show.legend = FALSE) &
#   geom_point(show.legend = FALSE) &
#   labs(x = "Election Cycle") &
#   scale_x_continuous(breaks = seq(1952, 2016, 16)) &
#   scale_y_continuous(labels = scales::percent) &
#   theme(axis.ticks = element_blank(),
#         panel.grid.minor = element_blank())



# bind_rows(mutate(compare_netdem, y = dem_vote), 
#           mutate(compare_demshare, y = dem_vote), 
#           mutate(compare_netgap, y = gap), 
#           mutate(compare_gap, y = gap)) %>%
#   mutate(trend = fct_relevel(trend, "Two-Party Vote", "Net Democratic Vote", "Gender Gap", "Net Gender Gap"),
#          gender = ifelse(is.na(gender), "Z", gender)) %>% 
#   ggplot(aes(x = cycle, y = y)) +
#     facet_wrap(~ trend, nrow = 2, scales = "free") +
#     geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender), 
#                 show.legend = FALSE,
#                 alpha = 0.4) +
#     geom_line(aes(color = gender), 
#               show.legend = FALSE) +
#     scale_color_manual(values = c(mcolor, wcolor, "gray20")) +
#     scale_fill_manual(values = c(mcolor, wcolor, "gray20"))



