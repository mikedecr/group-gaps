# ----------------------------------------------------
#   Burden & DeCrescenzo, Gender gap
#   ---
#   File 02: Descriptive graphics and initial analysis
# ----------------------------------------------------


# check for NUKE



# ----------------------------------------------------
#   data
# ----------------------------------------------------

anes <- readRDS(here("data", "clean", "cleaned-anes-cdf.RDS")) %>% 
  print()

anes %$% table(pid_init, exclude = NULL)




# ----------------------------------------------------
#   Trends in Party ID
# ----------------------------------------------------

# calculate proportion in each party ID and plot

plot_pid_tab <- anes %>% 
  group_by(pid_lean, gender, cycle) %>% 
  summarize(n = sum(wt)) %>% 
  group_by(cycle, gender) %>% 
  mutate(n.cycle = sum(n), 
         prop = (n / n.cycle), 
         lower = prop_ci(n, n.cycle)$lower, 
         upper = prop_ci(n, n.cycle)$upper) %>% 
  ungroup() %>% 
  mutate(gender = ifelse(gender == "M", "Men", "Women"), 
         gender = fct_relevel(gender, "Women", "Men")) %>%
  print()


plot_pid_tab %>% 
  filter(pid_lean %in% c("Dem", "Rep")) %>% 
  ggplot(aes(x = cycle,  y = prop)) +
    facet_wrap(~ gender) +
    geom_hline(yintercept = 0.5) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = pid_lean), 
                alpha = 0.3, 
                show.legend = FALSE) +
    geom_line(aes(color = pid_lean), 
              show.legend = FALSE) +
    geom_point(aes(shape = pid_lean, 
                   color = pid_lean),
                fill = "white",
               show.legend = FALSE) +
    annotate("text", x = 1980, y = 0.63, size = 3.5, label = "Democrats") +
    annotate("text", x = 1980, y = 0.25, size = 3.5, label = "Republicans") +
    coord_cartesian(ylim = c(.20, .75)) +
    scale_color_manual(values = c(dblue, rred)) +
    scale_fill_manual(values = c(dblue, rred)) +
    scale_shape_manual(values = c(16, 22)) +
    labs(y = "Percent Identifiers", 
         x = "Election Cycle") +
    scale_x_continuous(breaks = seq(1952, 2016, 8)) +
    scale_y_continuous(breaks = seq(0, 1, .10), labels = scales::percent) +
    theme(axis.text.x = element_text(angle = 45, vjust=0.75),
          panel.grid.minor = element_blank())

 # needs grayscale-friendly

 # Question: do we really need CIs here? Do they add anything to the interpretation?


ggsave(here("tex/graphics/party-ID-time.pdf"), height = 3, width = 6, device = cairo_pdf)






# ----------------------------------------------------
#   NUKE dropped
# ----------------------------------------------------

# defection by party and gender



# ----------------------------------------------------
#   turnout by gender
# ----------------------------------------------------

# filter: must be asked about voting

plot_turnout_gender <- anes %>% 
  filter(!is.na(voted)) %>% 
  group_by(gender, cycle, voted) %>%
  summarize(n = sum(wt)) %>%
  group_by(gender, cycle) %>%
  mutate(n.cycle = sum(n), 
         prop = (n / n.cycle), 
         lower = prop_ci(n, n.cycle)$lower, 
         upper = prop_ci(n, n.cycle)$upper) %>%
  ungroup() %>%
  print()


# plot only turned out

plot_turnout_gender %>% 
  filter(voted == 1) %>% 
  mutate(gender = case_when(gender == "M" ~ "Men", 
                            gender == "W" ~ "Women")) %>%
  ggplot(aes(x = cycle, y = prop)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = gender),
                alpha = 0.3,
                show.legend = FALSE) +
    geom_line(aes(color = gender),
              show.legend = FALSE) +
    geom_point(aes(color = gender, shape = gender),
               fill = "white",
               size = 2,
               show.legend = FALSE) +
    annotate("text", x = 1967, y = 0.66, size = 3, label = "Women") +
    annotate("text", x = 1978, y = 0.82, size = 3, label = "Men") +
    scale_color_manual(values = c(mcolor, wcolor)) +
    scale_fill_manual(values = c(mcolor, wcolor)) +
    scale_shape_manual(values = c(22, 16)) +
    labs(x = "Election Cycle", 
         y = "Percent Turnout (Self-Reported)") +
    coord_cartesian(ylim = c(0.55, 0.90)) +
    scale_x_continuous(breaks = seq(1952, 2016, 8)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    theme(axis.text.x = element_text(angle = 45, vjust=0.75))


ggsave("tex/graphics/turnout-gender-time.pdf", 
       height = 3.5, width = 5, device = cairo_pdf)





# ----------------------------------------------------
#   NUKE dropped:;
# ----------------------------------------------------

# gender as share of electorate
# party x gender as share of electorate





# ----------------------------------------------------
#   vote choice over time
#   THE GENDER GAP
# ----------------------------------------------------


vote.label.frame <- 
  tibble(lab = c("Men", "Women"), 
             cycle = c(1991, 1984), 
             dem.share = c(0.37, 0.63)) %>% 
  print()




plot_vote_gender <- anes %>% 
  filter(vote_choice %in% c("Dem Cand", "Rep Cand")) %>%
  group_by(cycle, gender, vote_choice) %>%
  summarize(n = sum(wt)) %>%
  group_by(cycle, gender) %>%
  mutate(n.cycle = sum(n), 
         dem.share = (n / n.cycle), 
         dem.share.lower = prop_ci(n, n.cycle)$lower, 
         dem.share.upper = prop_ci(n, n.cycle)$upper) %>%
  ungroup() %>%
  mutate(gender = ifelse(gender == "M", "Men", "Women")) %>%
  filter(vote_choice == "Dem Cand") %>%
  print()


fig_vote_gender <- 
  ggplot(plot_vote_gender, 
         aes(x = cycle, y = dem.share, 
             ymin = dem.share.lower, ymax = dem.share.upper, 
             color = gender, fill = gender))  +
  geom_hline(yintercept = 0.5, color = "gray") +
  geom_ribbon(aes(color = NULL), 
              alpha = 0.3,
              show.legend = FALSE) +
  geom_line(show.legend = FALSE) +
  geom_point(aes(shape = gender),
             size = 2,
             fill = "white",
             show.legend = FALSE) +
  annotate("text", size = 3.5, x = 1986, y = 0.68, label = "Women") +
  annotate("text", size = 3.5, x = 1991, y = 0.37, label = "Men") +
  labs(x = "Election Cycle", 
       y = "Democratic Share of\nTwo-Party Vote", 
       color = NULL, fill = NULL) +
  scale_color_manual(values = c(mcolor, wcolor)) +
  scale_fill_manual(values = c(mcolor, wcolor)) +
  scale_shape_manual(values = c(22, 16)) +
  scale_x_continuous(breaks = seq(1952, 2016, 8),
                     minor = seq(1952, 2016, 4)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust=0.75),
        panel.grid.minor = element_blank())

fig_vote_gender


ggsave("tex/graphics/gender-gap.pdf", 
       height = 3.5, width = 5, device = cairo_pdf)



# --- gap as difference -----------------------

gap_frame <- anes %>%
  filter(vote_choice %in% c("Dem Cand", "Rep Cand")) %>%
  group_by(cycle, gender, vote_choice) %>%
  summarize(n = sum(wt)) %>% 
  group_by(cycle, gender) %>%
  mutate(n_cycle = sum(n), 
         dem_share = n / n_cycle) %>%
  filter(vote_choice == "Dem Cand") %>%
  select(-vote_choice) %>% 
  group_by(gender) %>%
  nest() %>%
  mutate(data = ifelse(gender == "M", 
                       map(data, ~ setNames(., c("cycle", "m_n", "m_n_cycle", "m_dem_share"))), 
                       map(data, ~ setNames(., c("cycle", "w_n", "w_n_cycle", "w_dem_share"))))) %>%
  split(.$gender) %>% 
  lapply(unnest, data) %>% 
  lapply(select, -gender) %>% 
  reduce(., left_join, by = "cycle") %>% 
  mutate(gender_gap = w_dem_share - m_dem_share,
         gap_lower = diff_prop_ci(w_n, w_n_cycle, m_n, m_n_cycle)$lower,
         gap_upper = diff_prop_ci(w_n, w_n_cycle, m_n, m_n_cycle)$upper) %>% 
  print()



fig_vote_diff <- 
  ggplot(gap_frame, aes(x = cycle, y = gender_gap)) + 
    geom_hline(yintercept = 0, color = "gray") +
    geom_ribbon(aes(ymin = gap_lower, ymax = gap_upper),
                alpha = 0.3) + 
    geom_line() +
    geom_line() +
    geom_point(shape = 16) +
    labs(x = "Election Cycle", y = "Gender Gap\n(Women minus Men)") +
    scale_y_continuous(breaks = seq(-0.1, 0.2, .05), 
                       labels = scales::percent) +
    scale_x_continuous(breaks = seq(1952, 2016, 8),
                       minor = seq(1952, 2016, 4)) +
    theme(axis.text.x = element_text(angle = 45, vjust=0.75),
          panel.grid.minor = element_blank()) 

fig_vote_diff

ggsave("tex/graphics/gap-diff.pdf", 
       height = 3.5, width = 5, device = cairo_pdf) 



# --- sew plots together -----------------------

(gap_grid <- gridExtra::grid.arrange(fig_vote_gender, fig_vote_diff, 
                                    nrow = 1, ncol = 2))

ggsave(plot = gap_grid, here("tex/graphics/gap-grid.pdf"), 
       height = 3.5, width = 7.5, device = cairo_pdf)



# ----------------------------------------------------
#   NUKE drop:
# ----------------------------------------------------

# gender gap as difference in vote






# ----------------------------------------------------
#   Gap-vote relationship
# ----------------------------------------------------

# data from https://uselectionatlas.org/RESULTS/
leip <- read_csv(here("data/leip-national-pop-vote-data.csv")) %>%
  mutate(dem_vote_share = dem_raw_votes / (dem_raw_votes + rep_raw_votes)) %>%
  left_join(gap_frame) %>%
  print()

 # %>% 
 #  transmute(cycle = cycle,
 #            dem_vote_share = dem.vote.share / 100) %>% 
 #  left_join(., gap_frame, by = "cycle") %>%
 #  select(cycle, gender_gap, dem_vote_share) %>%
 #  print



ggplot(data = leip, aes(x = gender_gap, dem_vote_share)) +
  geom_hline(yintercept = 0.5) +
  geom_vline(xintercept = 0) +
  geom_smooth(method = "lm", 
              color = "gray20", alpha = 0.3, 
              size = 0.5) +
  geom_point(shape = 16) +
  geom_text(aes(y = dem_vote_share + 0.009, label = cycle)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Gender Gap (%)", y = "Democratic Share of Two-Party Vote (%)")


ggsave("tex/graphics/gap-vote-scatter.pdf", 
       height = 4, width = 5, device = cairo_pdf)




# --- statistical summary -----------------------

# longitudinal regression
long_reg <- lm(dem_vote_share ~ gender_gap, data = leip) %>% 
            tidy %>% 
            print()


# store regression coefficient, std.err,
#  and p-val from longitudinal regression
long_beta <- long_reg %>%
  filter(term == "gender_gap") %$%
  round(estimate, 2) %>% 
  print()

long_se <- long_reg %>%
  filter(term == "gender_gap") %$%
  round(std.error, 2) %>% 
  print()

long_p <- long_reg %>%
  filter(term == "gender_gap") %$% 
  round(p.value, 2) %>%
  print()


write(long_beta, file = "tex/refs/long-coef.tex")
write(long_p, file = "tex/refs/long-pval.tex")


# store correlation
long_corr <- leip %$%
  cor(dem_vote_share, gender_gap) %>%
  round(., 2) %>%
  print ()

write(long_corr, "tex/refs/long-corr.tex")


# compare to time trend and differenced
long_reg

trend_red <- lm(dem_vote_share ~ gender_gap + I(cycle / 4), data = leip) %>% 
             tidy() %>%
             print()

diff_reg <- lm(diff(dem_vote_share) ~ diff(gender_gap), data = leip) %>% 
            tidy() %>%
            print()

# save diff reg stats

# ----------------------------------------------------
#   cross-sectional relationships
# ----------------------------------------------------
states <- tibble(state.name, state.abb) %>% 
  rename(state = state.name) %>% 
  print()


exits <- read_csv("data/exits-04-08.csv") %>%
                  mutate(wvote = dvote.women / (dvote.women + rvote.women),
                         mvote = dvote.men / (dvote.men + rvote.men),
                         gender_gap = (wvote - mvote),
                         dem_share = dshare / (dshare + rshare)) %>%
                  select(-wvote, -mvote) %>% 
                  left_join(., states, by = "state") %>%
                  rename(abb = state.abb) %>%
                  mutate(abb = ifelse(is.na(abb), "DC", abb)) %>%
                  # filter(abb != "DC") %>%
                  print()



ggplot(data = exits, aes(x = gender_gap, y = dem_share)) +
  facet_grid(. ~ cycle) +
  geom_hline(yintercept = 0.5) +
  geom_vline(xintercept = 0) +
  geom_smooth(method = "lm",
              color = "gray20", 
              size = 0.75, 
              alpha = 0.35) +
  geom_text(aes(label = abb), size = 2.25) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(breaks = seq(0.10, 1, 0.2), labels = scales::percent) +
  coord_cartesian(ylim = c(0.2, 0.95)) +
  labs(x = "Gender Gap (%)", 
       y = "Democratic Share of\nTwo-Party Vote (%)")


ggsave("tex/graphics/exit-poll-scatter.pdf", 
       height = 3, width = 6, device = cairo_pdf)




# --- exit poll stats -----------------------

# -------------------------------------
### regression coefficients and p-values from
### state exit poll regressions, 04 and 08


# 04 regression
reg04 <- lm(dem_share ~ gender_gap,
            data = filter(exits, cycle == 2004)) %>% 
         tidy %>% 
         print()

# coef, std.err, pvalue from 04
exit_beta_04 <- reg04 %>%
  filter(term == "gender_gap") %$% 
  round(estimate, 2) %>%
  print()

exit_se_04 <- reg04 %>%
  filter(term == "gender_gap") %$% 
  round(std.error, 2) %>%
  print()


exit_p_04 <- reg04 %>%
  filter(term == "gender_gap") %$% 
  round(p.value, 2) %>%
  print()

write(exit_beta_04, file = "tex/refs/exit-04-coef.tex")
write(exit_p_04, file = "tex/refs/exit-04-pval.tex")



# correlation from 04
exit_corr_04 <- filter(exits, cycle == 2004) %$%
  cor(gender_gap, dem_share) %>%
  round(., 2) %>%
  print 

write(exit_corr_04, file = "tex/refs/exit-04-corr.tex")




# 08 regression
reg08 <- lm(dem_share ~ gender_gap,
            data = filter(exits, cycle == 2008)) %>%
         tidy %>%
         print

# coef, std.err, pvalue from 08
exit_beta_08 <- reg08 %>%
  filter(term == "gender_gap") %$% 
  round(estimate, 2) %>%
  print()

exit_se_08 <- reg08 %>%
  filter(term == "gender_gap") %$% 
  round(std.error, 2) %>%
  print()


exit_p_08 <- reg08 %>%
  filter(term == "gender_gap") %$% 
  round(p.value, 2) %>%
  print()

write(exit_beta_08, file = "tex/refs/exit-08-coef.tex")
write(exit_p_08, file = "tex/refs/exit-08-pval.tex")



# correlation from 08
exit_corr_08 <- filter(exits, cycle == 2008) %$%
  cor(gender_gap, dem_share) %>%
  round(., 2) %>%
  print 

write(exit_corr_08, file = "tex/refs/exit-08-corr.tex")





# ----------------------------------------------------
#   top-line as one graphic
# ----------------------------------------------------


toplines <- exits %>%
  select(cycle, gender_gap, dem_share, abb) %>%
  rename(dem_vote_share = dem_share,
         textlab = abb) %>%
  mutate(df = "exits") %>% 
  bind_rows(mutate(leip, df = "leip")) %>%
  mutate(panel = case_when(df == "leip" ~ "ANES, 1952-2016",
                           df == "exits" & cycle == "2004" ~ "State Exit Polls, 2004",
                           df == "exits" & cycle == 2008 ~ "State Exit Polls, 2008"),
         textlab = case_when(df == "leip" ~ 
                               paste0("'", str_sub(as.character(cycle), -2L, -1L)),
                             TRUE ~ textlab),) %>% 
  as.data.frame() %>%
  print()


exit_stat_labels <- 
  tibble(panel = factor(c("ANES, 1952-2016", "State Exit Polls, 2004", "State Exit Polls, 2008")), 
             r = c(long_corr, exit_corr_04, exit_corr_08),
             beta = c(long_beta, exit_beta_04, exit_beta_08),
             se = c(long_se, exit_se_04, exit_se_08),
             p = c(long_p, exit_p_04, exit_p_08)) %>%
  print()

# exit_stat_labels %$% 
# sprintf(TeX("$\\beta = %s\np = %s$", output = "character"), beta, p)


ggplot(toplines, aes(x = gender_gap, y = dem_vote_share)) +
  facet_wrap( ~ panel) +
  coord_cartesian(ylim = c(0.2, 0.95),
                  xlim = c(-.07, .18)) +
  geom_hline(yintercept = 0.5, color = "gray") +
  geom_vline(xintercept = 0, color = "gray") +
  # geom_label(data = exit_stat_labels, 
  # aes(x = 0.11, y = 0.225,
                # label = sprintf(TeX("$\\hat{\\beta} = %s$ $(se = %s)$", output = "character"), beta, se)), 
  # size = 2.5,
  #           parse = FALSE,
  #           label.size = 0.25) +
  geom_text(data = exit_stat_labels, 
            aes(x = 0.13, y = 0.825,
                label = as.character(str_glue("r = {r}\nb = {beta}\nse = {se}\np = {p}"))),
            size = 3) +
  geom_smooth(method = "lm", 
              color = "gray20", size = 0.75, alpha = 0.35) +
  geom_point(data = filter(toplines, df == "exits"),
            aes(label = textlab),
             size = 1) + 
  geom_point(data = filter(toplines, df == "leip"), 
            aes(label = textlab),
            size = 1,  
            show.legend = FALSE) +
  scale_x_continuous(breaks = seq(-0.20, .20, .05), 
                     labels = scales::percent) +
  scale_y_continuous(breaks = seq(.1, .9, .2), labels = scales::percent) +
  labs(x = "Gender Gap", y = "Democratic Share of\nTwo-Party Vote") +
  # theme(panel.grid.minor = element_blank()) +
  theme(panel.grid = element_blank()) +
  NULL

ggsave(here("tex/graphics/topline-3-panel.pdf"), 
       height = 3, width = 7, device = cairo_pdf)




