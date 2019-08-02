# ----------------------------------------------------
#   revision of 'anes smarter code.r'
#   reflects changes in mathematical approach as suggested by Ryan Moore
#   
#   Current paper version: 
#   no bootstrap
#   no dynamics
#   
#   TO DO:
#   ifelse() to case_when()
#   fix tabs
# ----------------------------------------------------


rm(list = ls())


# ----------------------------------------------------
#   directories
# ----------------------------------------------------

setwd("/Users/michaeldecrescenzo/Box Sync/research/gender-gap/Burden-DeCrescenzo")

dir.create("tex")
dir.create("tex/graphics")
dir.create("tex/refs")
dir.create("tex/tables")

# ----------------------------------------------------
#   packages
# ----------------------------------------------------

library("foreign")
library("magrittr")
library("forcats")
library("tidyverse")
library("ggplot2")
library("stringr")
library("broom")
# library("ggthemes")
# library("reshape2")
library("zeallot")
library("beepr")

#  custom ggtheme, import from Github
source("https://raw.githubusercontent.com/mikedecr/theme-mgd/master/theme_mgd.R")
theme_set(theme_mgd()) # set default theme
theme_set(theme_bw()) # set default theme
library("extrafont")   # load font faces into R

dblue <- "#259FDD"
rred <- "#FC5E47"
mcolor <- "mediumpurple"
wcolor <- "lightseagreen"


# confidence interval functions
# alarm()
# are these defined in the rest of the file though (and better?)
# source("/Users/michaeldecrescenzo/Box Sync/R learning/Custom/prop ci.R")



# ----------------------------------------------------
#   Data
# ----------------------------------------------------

# this is actually faster than haven::read_dta() ?
anes <- read.dta("data/anes_cdf_2012_cleaned.dta", 
                 convert.underscore = TRUE) %>%
				as_data_frame(.) %>%
				print






# ----------------------------------------------------
#   Functions.
#   Put into some sub-file
				beepr::beep(2)
# ----------------------------------------------------

# confidence intervals for a vector of proportions
prop.ci <- function(successes, n, level=0.05) {

		# get parameters
		p1 <- successes/n
		q1 <- 1 - p1

		# compute standard error
		SE <- sqrt((p1*q1)/n)

		# find upper and lower bound
		lower <- p1 - qnorm(1-(level/2))*SE
		upper <- p1 + qnorm(0.975)*SE

		# return data frame
		return(data.frame(lower, upper))

}



# confidence intervals for difference in two proportions
diff.prop.ci <- function(success1, n1, success2, n2, level=0.05) {

		# get parameters
		p1 <- success1 / n1
		q1 <- 1 - p1
		p2 <- success2 / n2
		q2 <- 1 - p2

		# compute standard error
		var1 <- (p1*q1) / n1
		var2 <- (p2*q2) / n2
		SE.diff <- sqrt(var1 + var2)

		# compute CI
		estimate <- p1 - p2
		lower <- estimate - qnorm(1-(level/2))*SE.diff
		upper <- estimate + qnorm(1-(level/2))*SE.diff

		# return data frame
		return(data.frame(estimate, lower, upper))

}




# ----------------------------------------------------
#   party ID figure
# ----------------------------------------------------

pid.init.tab <- anes %>% 
                group_by(pid.init, gender, cycle) %>% 
                summarize(n = sum(wt)) %>% 
                group_by(cycle, gender) %>% 
                mutate(n.cycle = sum(n), 
                       perc = 100 * (n / n.cycle), 
                       lower = 100 * prop.ci(n, n.cycle)$lower, 
                       upper = 100 * prop.ci(n, n.cycle)$upper) %>% 
                ungroup() %>% 
                mutate(gender = ifelse(gender == "M", "Men", "Women"), 
                       gender = factor(gender, levels = c("Women", "Men"))) %>% 
                print


pid.lean.tab <- anes %>%
																group_by(pid.sorted, gender, cycle) %>%
																summarize(n = sum(wt)) %>%
																group_by(cycle, gender) %>%
																mutate(n.cycle = sum(n),
																			 perc = 100 * (n / n.cycle),
																			 lower = 100 * prop.ci(n, n.cycle)$lower,
																			 upper = 100 * prop.ci(n, n.cycle)$upper) %>%
																ungroup() %>%
																mutate(
																	gender = ifelse(gender == "M", "Men", "Women"),
																	gender = factor(gender, levels = c("Women", "Men"))) %>%
																print


party.labels <- data_frame(cycle = rep(1980, 2), 
                           perc = c(25, 65), 
                           party = c("Republicans", "Democrats")) %>%
	               print



filter(pid.lean.tab, pid.sorted %in% c("Dem", "Rep")) %>% 
ggplot(aes(x = cycle,  y = perc)) +
		facet_grid(. ~ gender) +
		geom_hline(yintercept = 50) +
		geom_ribbon(aes(ymin = lower, ymax = upper, fill = pid.sorted), 
              alpha = 0.3, 
              show.legend = FALSE) +
		geom_line(aes(color = pid.sorted), 
            show.legend = FALSE) +
		geom_point(aes(color = pid.sorted),
		           shape = 16,
		           show.legend = FALSE) +
		geom_text(data = party.labels,
		          aes(x = cycle, y = perc, label = party),
		          size = 3,
		          show.legend = FALSE) +
		coord_cartesian(ylim = c(20, 75)) +
		scale_color_manual(values = c(dblue, rred)) +
		scale_fill_manual(values = c(dblue, rred)) +
		labs(y = "Percent Identifiers", 
       x = "Election Cycle") +
		scale_x_continuous(breaks = seq(1956, 2012, 8)) +
		scale_y_continuous(breaks=seq(0, 100, 10)) +
		theme(axis.text.x = element_text(angle = 45, vjust=0.75))

	# needs grayscale-friendly

	# Question: do we really need CIs here? Do they add anything to the interpretation?


ggsave("tex/graphics/MM-party-ID-over-time.pdf", height = 3.25, width = 6.5)
embed_fonts("tex/graphics/MM-party-ID-over-time.pdf")



# ----------------------------------------------------
#   defection by party x gender
#   Not in paper; maybe just delete
# ----------------------------------------------------

# vote choice by party and gender, initial party ID
tab.loyalty.init <- anes %>%
																				filter(voted.maj.party == 1) %>%
																				mutate(pid.init = ifelse(pid.init %in% c("Ind", "Other"), "Unaffiliated", ifelse(pid.init == "Dem", "Democrats", "Republicans"))) %>%
																				rename(party = pid.init) %>%
																				group_by(cycle, gender, party, voted.dem) %>%
																				summarize(voters = sum(wt, na.rm = TRUE)) %>%
																				ungroup %>%
																				group_by(cycle, gender, party) %>%
																				mutate(n = sum(voters)) %>%
																				ungroup %>%
																				mutate(dem.share = 100 * voters / n,
																				       lower = 100 * prop.ci(voters, n)$lower,
																				       upper = 100 * prop.ci(voters, n)$upper,
																				       gender = ifelse(gender == "M", "Men", "Women")) %>%
																				filter(voted.dem == 1) %>%
																				print

# vote choice by party and gender, leaners as partisans
tab.loyalty.lean <- anes %>%
																				filter(voted.maj.party == 1) %>%
																				mutate(pid.sorted = ifelse(pid.sorted %in% c("Ind", "Other"), "Unaffiliated", ifelse(pid.sorted == "Dem", "Democrats", "Republicans"))) %>%
																				rename(party = pid.sorted) %>%
																				group_by(cycle, gender, party, voted.dem) %>%
																				summarize(voters = sum(wt, na.rm = TRUE)) %>%
																				ungroup %>%
																				group_by(cycle, gender, party) %>%
																				mutate(n = sum(voters)) %>%
																				ungroup %>%
																				mutate(dem.share = 100 * voters / n,
																				       lower = 100 * prop.ci(voters, n)$lower,
																				       upper = 100 * prop.ci(voters, n)$upper,
																				       gender = ifelse(gender == "M", "Men", "Women")) %>%
																				filter(voted.dem == 1) %>%
																				print



{
bind_rows(mutate(tab.loyalty.init,
                 leaners = "Leaners as Unaffiliated"),
          mutate(tab.loyalty.lean,
                 leaners = "Leaners as Partisans")
          ) %>%
mutate(leaners = fct_relevel(leaners, "Leaners as Unaffiliated", "Leaners as Partisans")) %>%
ggplot(aes(x = cycle, y = dem.share,
           color = party, fill = party)) +
	facet_grid(leaners ~ gender) +
	geom_ribbon(aes(ymin = lower, ymax = upper, fill = party),
	            color = NA) +
	geom_line() +
	geom_point() +
	scale_color_manual(values = c(dblue, rred, "gray20")) +
	scale_fill_manual(values = c(dblue, rred, "gray50")) +
	labs(x = "Election Cycle",
	     y = "Democratic Share of Two-Party Vote (%)",
	     color = NULL, fill = NULL) +
	scale_x_continuous(breaks = seq(1956, 2012, 8)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75))
}


{
bind_rows(mutate(tab.loyalty.init,
                 leaners = "Leaners as Unaffiliated"),
          mutate(tab.loyalty.lean,
                 leaners = "Leaners as Partisans")
          ) %>%
mutate(leaners = fct_relevel(leaners, "Leaners as Unaffiliated", "Leaners as Partisans"),
       major = ifelse(party == "Unaffiliated", "Party Unaffiliated", "Major Party Identifiers"),
       party.gender = paste(gender, party, sep = ".")) %>%
ggplot(aes(x = cycle, y = dem.share,
           color = party.gender, fill = party.gender)) +
	facet_grid(leaners ~ major) +
	geom_ribbon(aes(ymin = lower, ymax = upper,
	                fill = party.gender),
	            color = NA,
	            show.legend = FALSE) +
	geom_line(show.legend = FALSE) +
	geom_point(aes(shape = gender),
	           fill = "white") +
	scale_color_manual(values = c(rep(mcolor, 3), rep(wcolor, 3))) +
	scale_fill_manual(values = c(rep(mcolor, 3), rep(wcolor, 3))) +
	scale_shape_manual(values = c(16, 21)) +
	labs(x = "Election Cycle",
	     y = "Democratic Share of Two-Party Vote (%)",
	     title = "Party Voting",
	     color = NULL, fill = NULL,
	     shape = NULL) +
	scale_x_continuous(breaks = seq(1956, 2012, 8)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75)) +
	guides(shape = guide_legend(override.aes = list(color = c(mcolor, wcolor))))
}







# ----------------------------------------------------
#   Turnout by gender
# ----------------------------------------------------

tab.turnout.gender <- anes %>% 
																					 # must have been asked about voting
																					 filter(got.vote.q == 1) %>% 
																						group_by(gender, cycle, voted) %>%
																						summarize(n = sum(wt)) %>%
																						group_by(gender, cycle) %>%
																						mutate(n.cycle = sum(n),
																									 perc = 100 * (n / n.cycle),
																									 lower = 100 * prop.ci(n, n.cycle)$lower,
																									 upper = 100 * prop.ci(n, n.cycle)$upper) %>%
																						ungroup() %>%
																						mutate(gender = case_when(gender == "M" ~ "Men", 
																						                          gender == "W" ~ "Women")) %>%
																						print



turnout.labels <- data_frame(cycle = rep(1976, 2), 
                             perc = c(63, 83), 
                             lab = c("Women", "Men")) %>%
                  print


ggplot(data = filter(tab.turnout.gender, voted == 1), 
       aes(x = cycle, y = perc)) +
		geom_ribbon(aes(ymin = lower, ymax = upper,
		                fill = gender),
		            show.legend = FALSE) +
		geom_line(aes(color = gender),
		          show.legend = FALSE) +
		geom_point(aes(color = gender, 
		               shape = gender),
		           fill = "white",
		           size = 2,
		           show.legend = FALSE) +
		geom_text(data = turnout.labels,
		          aes(x = cycle, y = perc,
		              label = lab),
		          size = 3.5,
		          show.legend = FALSE) +
		scale_color_manual(values = c(mcolor, wcolor)) +
		scale_fill_manual(values = c(mcolor, wcolor)) +
		scale_shape_manual(values = c(16, 21)) +
		labs(x = "Election Cycle", 
		     y = "Percent Turnout (Self-Reported)") +
		coord_cartesian(ylim = c(55, 90)) +
		scale_x_continuous(breaks = seq(1956, 2012, 8)) +
		scale_y_continuous(breaks=seq(0, 100, 10)) +
		theme(axis.text.x = element_text(angle = 45, vjust=0.75))

	# do grayscale version




ggsave("tex/graphics/MM-turnout-gender.pdf", height = 3.5, width = 5)
embed_fonts("tex/graphics/MM-turnout-gender.pdf")





# although men become more Republican, is their share of the total electorate appears to be decreasing? Might counter-act PID's negative impact on Democratic voting over this time period


# ----------------------------------------------------
#   directly measure women's share of self-reported voters
#   This appears to be slightly different...
#   come back to this
  beepr::beep(2)
#   Not exactly sure how this should be interpreted. 
#   Increase UNTIL 1980, but the gap continues to widen since then?
# ----------------------------------------------------

anes %>%
filter(got.vote.q == 1) %>%
group_by(cycle, gender, voted) %>%
summarize(n = sum(wt)) %>%
filter(voted == 1) %>%
group_by(cycle) %>%
mutate(n.cycle = sum(n),
			 w.share = 100 * (n / n.cycle),
			 w.lower = 100 * prop.ci(n, n.cycle)$lower,
			 w.upper = 100 * prop.ci(n, n.cycle)$upper) %>%
filter(gender == "W") %>%
ggplot(aes(x = cycle, y = w.share,
					 ymin = w.lower, ymax = w.upper)) +
	geom_hline(yintercept = 50) +
	geom_ribbon() +
	geom_line() +
	labs(x = "Election Cycle", 
      y = "Women's Share of Self-Reported Voters") +
	scale_x_continuous(
		breaks = seq(1956, 2012, 8)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75))






# ----------------------------------------------------
#   party x gender share of the electorate
#   This is actually pretty interesting...
# ----------------------------------------------------


anes %>%
filter(got.vote.q == 1) %>%
group_by(cycle, gender, voted, pid.sorted) %>%
summarize(n = sum(wt)) %>%
filter(voted == 1) %>%
group_by(cycle) %>%
mutate(n.cycle = sum(n), 
       share = 100 * (n / n.cycle), 
       lower = 100 * prop.ci(n, n.cycle)$lower, 
       upper = 100 * prop.ci(n, n.cycle)$upper) %>%
filter(pid.sorted %in% c("Dem", "Rep")) %>%
mutate(party = ifelse(pid.sorted == "Dem", "Dem. Identifiers", "Rep. Identifiers"), 
       gender = ifelse(gender == "M", "Men", "Women")) %>%
ggplot(aes(x = cycle, y = share, 
           ymin = lower, ymax = upper, 
           color = party, fill = party)) +
	facet_grid(. ~ gender) +
	geom_ribbon(aes(color = NULL), 
	            alpha = 0.2, 
	            show.legend = FALSE) +
	geom_line() +
	labs(title = "Party Identification Among Voters", 
	     x = "Election Cycle", y = "Share of All Voters", 
	     color = NULL, fill = NULL) +
	scale_color_manual(values = c("dodgerblue", "orangered")) +
	scale_fill_manual(values = c("dodgerblue", "orangered")) +
	scale_x_continuous(breaks = seq(1956, 2012, 8)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75))


ggsave("tex/graphics/MM-party-gender-share-electorate.pdf", height = 3.5, width = 6.5)
embed_fonts("tex/graphics/MM-party-gender-share-electorate.pdf")



# ----------------------------------------------------
#   vote choice by gender
# ----------------------------------------------------

vote.label.frame <- data_frame(lab = c("Men", "Women"), 
                               cycle = c(1991, 1984), 
                               dem.share = c(37, 63)) %>% 
                    print




anes %>%
filter(voted.maj.party == 1) %>%
group_by(cycle, gender, voted.dem) %>%
summarize(n = sum(wt)) %>%
group_by(cycle, gender) %>%
mutate(n.cycle = sum(n), 
       dem.share = 100 * (n / n.cycle), 
       dem.share.lower = 100 * prop.ci(n, n.cycle)$lower, 
       dem.share.upper = 100 * prop.ci(n, n.cycle)$upper) %>%
ungroup() %>%
mutate(gender = ifelse(gender == "M", "Men", "Women")) %>%
filter(voted.dem == 1) %>%
ggplot(aes(x = cycle, y = dem.share, 
           ymin = dem.share.lower, ymax = dem.share.upper, 
           color = gender, fill = gender))  +
		geom_ribbon(aes(color = NULL), show.legend = FALSE) +
		geom_hline(yintercept = 50) +
		geom_line(show.legend = FALSE) +
		geom_point(aes(shape = gender),
		           size = 2,
		           fill = "white",
		           show.legend = FALSE) +
		geom_text(data = vote.label.frame,
		          aes(x = cycle, y = dem.share, 
		              label = lab, 
		              ymin = NULL, ymax = NULL,
		              fill = NULL, color = NULL),
		          size = 3.5,
		          show.legend = FALSE) +
		labs(x = "Election Cycle", 
		     y = "Democratic Two-Party Vote Share (%)", 
		     color = NULL, fill = NULL) +
		scale_color_manual(values = c(mcolor, wcolor)) +
		scale_fill_manual(values = c(mcolor, wcolor)) +
		scale_shape_manual(values = c(16, 21)) +
		scale_x_continuous(
			breaks = seq(1956, 2012, 8)) +
		theme(axis.text.x = element_text(angle = 45, vjust=0.75))


ggsave("tex/graphics/MM-gender-gap.pdf", height = 3.5, width = 5)
embed_fonts("tex/graphics/MM-gender-gap.pdf")





# ----------------------------------------------------
#   Gender gap over time
# ----------------------------------------------------

# make a list split by gender
gender.split.dshare <- anes %>%
																							filter(voted.maj.party == 1) %>%
																							group_by(cycle, gender, voted.dem) %>%
																							summarize(n = sum(wt)) %>% # numerator
																							group_by(cycle, gender) %>%
																							mutate(n.cycle = sum(n), # denominator
																										 dem.share = 100 * (n / n.cycle)) %>%
																							filter(voted.dem == 1) %>%
																							ungroup() %>%
																							split(.$gender) %>%
																							print

# merge gender frames into columns to take difference
m.dshare <- gender.split.dshare$M
w.dshare <- gender.split.dshare$W
# { m.dshare : w.dshare } %<-% gender.split.dshare

gap.frame <- w.dshare %>%
		rename(n.w = n, w.cycle = n.cycle, w.dem.share = dem.share) %>%
		left_join(m.dshare, ., by = "cycle") %>%
		mutate(gender.gap = 100 * ((n.w / w.cycle) - (n / n.cycle)), 
		       gap.lower = 100 * diff.prop.ci(n.w, w.cycle, n, n.cycle)$lower, 
		       gap.upper = 100 * diff.prop.ci(n.w, w.cycle, n, n.cycle)$upper) %>%
		select(cycle, gender.gap:gap.upper) %>%
		print


ggplot(data = gap.frame, aes(x = cycle,  y = gender.gap, ymin = gap.lower, ymax = gap.upper)) +
		geom_ribbon() +
		geom_hline(yintercept = 0) +
		geom_line() +
		geom_point(shape = 16) +
		# geom_pointrange(fatten = 1) +
		labs(x = "Election Cycle", y = "Gender Gap") +
		scale_y_continuous(breaks = seq(-10, 20, 5)) +
		scale_x_continuous(
			breaks = seq(1956, 2012, 8)) +
		theme(axis.text.x = element_text(angle = 45, vjust=0.75)) 






# ----------------------------------------------------
#   Longitudinal correlation, gap and vote
# ----------------------------------------------------


### Writes CSV from Leip data...

# pop.vote <-
# 	data.frame(
# 		cycle = seq(1952, 2012, 4),
# 		dem.raw.vote = c(
# 			27375090, # 52
# 			26028028, # 56
# 			34220984, # 60
# 			43129040, # 64
# 			31271839, # 68
# 			29173222, # 72
# 			40831881, # 76
# 			35480115, # 80
# 			37577352, # 84
# 			41809476, # 88
# 			44909806, # 92
# 			47400125, # 96
# 			51009810, # 00
# 			59027115, # 04
# 			69499428, # 08
# 			65918507
# 			),
# 		rep.raw.vote = c(
# 			34075529, # 52
# 			35579180, # 56
# 			34108157, # 60
# 			27175754, # 64
# 			31783783, # 68
# 			47168710, # 72
# 			39148634, # 76
# 			43903230, # 80
# 			54455472, # 84
# 			48886597, # 88
# 			39104550, # 92
# 			39198755, # 96
# 			50462412, # 00
# 			62039572, # 04
# 			59950323, # 08
# 			60934407
# 			)
# 		) %>%
# 	mutate(dem.vote.share = 100 * dem.raw.vote / (dem.raw.vote + rep.raw.vote)) %>%
# 	select(cycle, dem.vote.share) %>%
# 	print

# dir.create("Pop Vote Data")
# write_csv(pop.vote, "Pop Vote Data/leip-national-pop-vote-data.csv")



####
# make graphic

# read in leip data
leip <- read_csv("data/leip-national-pop-vote-data.csv") %>%
								select(cycle, dem.vote.share) %>%
								left_join(., gap.frame, by = "cycle") %>%
								select(cycle, dem.vote.share, gender.gap) %>%
								print

ggplot(data = leip, aes(x = gender.gap, dem.vote.share)) +
		geom_hline(yintercept = 50) +
		geom_vline(xintercept = 0) +
		geom_smooth(method = "lm",
			          color = "gray20", alpha = 0.3,
			          size = 0.5) +
		geom_point(shape = 16) +
		geom_text(aes(y = dem.vote.share + 0.9, label = cycle)) +
		labs(x = "Gender Gap (%)", y = "Democratic Share of Two-Party Vote (%)")

ggsave("tex/graphics/MM-gap-vote-scatter.pdf", height = 4, width = 5)
embed_fonts("tex/graphics/MM-gap-vote-scatter.pdf")



# longitudinal regression
long.reg <- lm(dem.vote.share ~ gender.gap, data = leip) %>% 
            tidy %>% 
            print

# store regression coefficient and p-val from longitudinal regression
filter(long.reg, term == "gender.gap") %$%
{
	write(round(estimate, 2), file = "tex/refs/long-coef.tex")
	write(round(p.value, 2), file = "tex/refs/long-pval.tex")
}

# store correlation
leip %$%
cor(dem.vote.share, gender.gap) %>%
round(., 2) %>%
print %>%
write(., "tex/refs/long-corr.tex")


#----------------------------------------
# Regression with time trend
#----------------------------------------
summary(lm(dem.vote.share ~ gender.gap, data = leip))
summary(lm(dem.vote.share ~ gender.gap + I(cycle / 4), data = leip))



########################################
# State exit polls
########################################
states <- data_frame(state.name, state.abb) %>% 
          rename(state = state.name) %>% print


exit.poll.data <- read_csv("data/exit poll 04-08 long.csv") %>%
																		mutate(wvote = dvote.women / (dvote.women + rvote.women),
																		       mvote = dvote.men / (dvote.men + rvote.men),
																		       gender.gap = 100 * (wvote - mvote)) %>%
																		select(-wvote, -mvote) %>% 
																		left_join(., states, by = "state") %>%
																		rename(abb = state.abb) %>%
																		mutate(abb = ifelse(is.na(abb), "DC", abb)) %>%
																		print



ggplot(data = exit.poll.data,
       aes(x = gender.gap, y = dshare)) +
		facet_grid(. ~ cycle) +
		geom_hline(yintercept = 50) +
		geom_vline(xintercept = 0) +
		geom_smooth(method = "lm",
			          color = "gray20", 
			          size = 0.75, 
			          alpha = 0.35) +
		geom_text(aes(label = abb),
		          size = 2.25) +
		scale_y_continuous(breaks = seq(10, 100, 20)) +
		coord_cartesian(ylim = c(20, 95)) +
		labs(x = "Gender Gap (%)", 
		     y = "Democratic Share of\nTwo-Party Vote (%)")


ggsave("tex/graphics/MM-cross-sectional-gap-scatter.pdf", height = 3, width = 6)
embed_fonts("tex/graphics/MM-cross-sectional-gap-scatter.pdf")




# -------------------------------------
### regression coefficients and p-values from
### state exit poll regressions, 04 and 08


# 04 regression
reg04 <- lm(dshare ~ gender.gap,
            data = filter(exit.poll.data, cycle == 2004)) %>% 
         tidy %>% 
         print

# coef, pvalue from 04
reg04 %>%
filter(term == "gender.gap") %$%
{
	# round each to 2 decimals
	round(estimate, 2) %>%
	print %>%
	write(., file = "tex/refs/exit-04-coef.tex")

	round(p.value, 2) %>%
	print %>%
	write(., file = "tex/refs/exit-04-pval.tex")
}

# correlation from 04
filter(exit.poll.data, cycle == 2004) %$%
cor(gender.gap, dshare) %>%
round(., 2) %>%
print %>%
write(., file = "tex/refs/exit-04-corr.tex")




# 08 regression
reg08 <- lm(dshare ~ gender.gap,
            data = filter(exit.poll.data, cycle == 2008)) %>%
								 tidy %>%
								 print

# coef, pvalue from 08
reg08 %>%
filter(term == "gender.gap") %$%
{
	# round each to 2 decimals
	round(estimate, 2) %>%
	print %>%
	write(., file = "tex/refs/exit-08-coef.tex")

	round(p.value, 2) %>%
	print %>%
	write(., file = "tex/refs/exit-08-pval.tex")
}

# correlation from 08
filter(exit.poll.data, cycle == 2008) %$%
cor(gender.gap, dshare) %>%
round(., 2) %>%
print %>%
write(., file = "tex/refs/exit-08-corr.tex")








# ----------------------------------------------------
#   MODEL
# ----------------------------------------------------



anes <- anes %>% 
        mutate(vote = fct_recode(VCF0706, 
                                 "dvote" = "1. Democrat", 
                                 "rvote" = "2. Republican", 
                                 "nonvote" = "7. Did not vote or voted but not for president (exc.1972)", 
                                 "other" = "3. Major third party candidate (Wallace 1968/Anderson", 
                                 "other" = "4. Other (incl. 3d/minor party candidates and write-ins)", 
                                 "invalid" = "0. DK/NA if voted; refused to say if voted; DK/NA if")) %>%
								print



#----------------------------------------
#		make model and bootstrap
#----------------------------------------

# load in functions for computing workhorse model
# (include missing vote data?)
source("gap 02 model function.r")
# source("gap 02-1 model unweight.R")

# # routines to bootstrap the model

# # totally nonparametric resampling, nonpar_boot()
# source("gap 03 pure.R")

# # blocked on year, no pre-weighting, cycle_block_boot()
# source("gap 03-1 boot cycle blocked.R") 

# # pre-weighted sample, boot_preweight()
# source("gap 03-2 boot preweighted.R") 

# # pre-weighted sample but unweighted model, boot_unweight()
# source("gap 03-3 boot unweighted model.R")



# 2. model using full data
mod.init <- run.model(anes, anes$pid.init) %>% print
mod.lean <- run.model(anes, anes$pid.sorted) %>% print



#----------------------------------------
#		Figuring out the weights.
# 	delete later
#----------------------------------------

# # nonparametric
# boot.init <- boot_nonpar(anes, leaners.as = "independents", iterations = 50)
# boot.lean <- boot_nonpar(anes, leaners.as = "partisans", iterations = 50)
# source("gap 04 extract CIs.R")


# # cycle blocked
# boot.init <- boot_block_cycle(anes, leaners.as = "independents", iterations = 50)
# boot.lean <- boot_block_cycle(anes, leaners.as = "partisans", iterations = 50)
# source("gap 04 extract CIs.R")


# # preweighted
# boot.init <- boot_preweight(anes, leaners.as = "independents", iterations = 50)
# boot.lean <- boot_preweight(anes, leaners.as = "partisans", iterations = 50)
# source("gap 04 extract CIs.R")


# # unweighted
# boot.init <- boot_unweight(anes, leaners.as = "independents", iterations = 50)
# boot.lean <- boot_unweight(anes, leaners.as = "partisans", iterations = 50)
# source("gap 04 extract CIs.R")





# mod.init <- run.model.unweight(anes, anes$pid.init) %>% print
# mod.lean <- run.model.unweight(anes, anes$pid.sorted) %>% print

#----------------------------------------
#		end messing around
#----------------------------------------








# # 4. run bootstrap functions
# start.time <- proc.time()

# boot.init <- bootstrap_model_unweight(anes, leaners.as = "independents", iterations = 100)

# boot.lean <- bootstrap_model_unweight(anes, leaners.as = "partisans", iterations = 100)


# dir.create("output")

# save(boot.init, file = "output/boot.init.rdata")
# save(boot.lean, file = "output/boot.lean.rdata")

# # load("output/boot.init.rdata")
# # load("output/boot.lean.rdata")

# proc.time() - start.time

# # mod.init
# # boot.init[[1:2]]

# # mod.lean
# # boot.lean[[1:2]]


# # create graphic with confidence intervals

# source("gap 04 extract CIs.R")

# beepr::beep(2)


 


#----------------------------------------
#		Analyze results
#----------------------------------------



# graph the RHS
rhs <- 
  bind_rows(mutate(mod.init$rhs, leaners = "Leaners as Unaffiliated"), 
            mutate(mod.lean$rhs, leaners = "Leaners as Partisans")) %>%
		# select out "mobilization" to plot loyalty instead
		select(-MD.mob, -WD.mob, -MR.mob, -WR.mob) %>%
		gather(key = Source, value = N, MD.pid:WR.loyal) %>%
		select(cycle, Source, N, leaners) %>%
		separate(Source, into = c("PG", "Source")) %>%
		left_join(., mod.init$denoms, by = "cycle") %>%
		mutate(prop = N / super.denom, 
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


rhs %>%
ggplot(aes(x = cycle, y = prop, color = PG, shape = PG)) +
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


ggsave("tex/graphics/MM-right-hand-side.pdf", height = 5, width = 8)
embed_fonts("tex/graphics/MM-right-hand-side.pdf")









#----------------------------------------
#		"net gender gap"
#----------------------------------------


# gender gap in the number of votes
# maybe should work toward a gender gap in the vote *contribution*\
# problem with that is: might have a large contribution to a smaller group!


new.gap.data <- 
		left_join(mod.lean$rhs, mod.lean$denoms, by = "cycle") %>% 
		transmute(cycle = cycle, 
		          votes.margin = 100 * (M.dem.votes + W.dem.votes - (M.rep.votes + W.rep.votes)) / super.denom, 
		          votes.gap = 100 * (((W.dem.votes - W.rep.votes) - (M.dem.votes - M.rep.votes)) / super.denom), 
		          D.votes = M.dem.votes + W.dem.votes, 
		          R.votes = M.rep.votes + W.rep.votes, 
		          est.dem.share = 100 * D.votes / (D.votes + R.votes), 
		          est.margin = est.dem.share - (100 - est.dem.share)) %>% 
		left_join(., leip, by = "cycle") %>% 
		print



# regression summary from new gap measure

# using margin over superdenominator
# (on the same scale as gap in vote count)
new.gap.data %$% {

	# r value
	cor(est.dem.share, votes.gap) %>%
	round(., 2) %>%
	print %>%
	write(., "tex/refs/new-gap-ANES-cor.tex")

	# get regression
	lm(est.dem.share ~ votes.gap) %>%
	tidy(., conf.int = TRUE) %>%
	print %>%
	# coefficient and p value
	filter(term == "votes.gap") %$% {

			# coef
			round(.$estimate, 2) %>%
			print %>%
			write(., "tex/refs/new-gap-ANES-coef.tex")

			# p val
			round(.$p.value, 2) %>%
			print %>%
			write(., "tex/refs/new-gap-ANES-pval.tex")

			# p val

	}

}


# Using margin in vote share
# For generalizability to the actual election outcome
new.gap.data %$% {

	# r value
	cor(dem.vote.share, votes.gap) %>%
	round(., 2) %>%
	print %>%
	write(., "tex/refs/new-gap-official-cor.tex")

	# get regression
	lm(dem.vote.share ~ votes.gap) %>%
	tidy(., conf.int = TRUE) %>%
	print %>%
	# coefficient and p value
	filter(term == "votes.gap") %$% {

			# coef
			new.coef <- round(.$estimate, 2) %>% 
			            print
   
   write(new.coef, "tex/refs/new-gap-official-coef.tex")

   (new.coef * 100) %>%
   print %>%
   write(., "tex/refs/new-gap-pct-gain.tex")

			# p val
			round(.$p.value, 2) %>%
			print %>%
			write(., "tex/refs/new-gap-official-pval.tex")

	}

}

#----------------------------------------


# looking at DV choice and gap choice

# new.gap.data %>%
# gather(key = dv, value = dem.share, est.dem.share, dem.vote.share) %>%
# mutate(dv = fct_recode(dv,
#                        "ANES Estimate" = "est.dem.share",
#                        "Official Count" = "dem.vote.share")) %>%
# gather(key = gap.type, value = gap.size, votes.gap, gender.gap) %>%
# mutate(gap.type = fct_recode(gap.type,
#                              "old" = "gender.gap",
#                              "new" = "votes.gap")) %>%
# ggplot(aes(x = gap.size, y = dem.share)) +
# 	facet_grid(dv ~ gap.type) +
# 	geom_text(aes(label = cycle)) +
# 	geom_smooth(method = "lm")




new.gap.data %>%
mutate(votes.margin = votes.margin,
       dem.share.margin = dem.vote.share - (100 - dem.vote.share)) %>%
gather(key = dv, value = dem.share, est.dem.share, dem.vote.share) %>%
select(cycle, votes.gap, dv, dem.share) %>%
ggplot(aes(x = votes.gap, y = dem.share)) +
		facet_wrap(~ dv) +
		geom_vline(xintercept = 0) +
		geom_hline(yintercept = 50) +
		geom_smooth(method = "lm") +
		geom_text(aes(label = cycle))

new.gap.data %>%
ggplot(aes(x = gender.gap, y = votes.gap)) +
		geom_smooth(method = "lm") +
		geom_text(aes(label = cycle)) +
		geom_abline(slope = 1, intercept = 0)


# where is this "exaggerated" effect showing up in the survey data?

	# new.gap.data %>%
	# mutate(votes.margin = votes.margin,
	#        dem.share.margin = dem.vote.share - (100 - dem.vote.share)) %>%
	# gather(key = dv, value = dem.margin, votes.margin, dem.share.margin) %>%
	# select(cycle, votes.gap, dv, dem.margin) %>%
	# ggplot(aes(x = cycle, y = dem.margin, color = dv)) +
	# 	geom_hline(yintercept = 0) +
	# 	geom_line()

# Recent years are more left-biased in the ANES (after 1980)



ggplot(data = new.gap.data, aes(x = votes.gap, y = dem.vote.share)) +
		geom_hline(yintercept = 50) +
		geom_vline(xintercept = 0) +
		geom_smooth(method = "lm",
		            color = "gray20",
		            fill = "gray50",
		            alpha = 0.3) +
		geom_point(shape = 16) +
		geom_text(aes(x = ifelse(cycle == 1992, votes.gap + 0.75, votes.gap), 
		              y = ifelse(cycle == 1992, dem.vote.share, dem.vote.share + 1), 
		              label = cycle)) +
		labs(x = "Net Gender Gap\n(Percent of Eligible Voters)",
		     y = "Democratic Two-Party Vote Share (%)")


ggsave("tex/graphics/MM-new-gap-scatter.pdf", height = 4, width = 5)
embed_fonts("tex/graphics/MM-new-gap-scatter.pdf")






# regression with time trend
summary(lm(dem.vote.share ~ votes.gap, data = new.gap.data))
summary(lm(dem.vote.share ~ votes.gap + I(cycle / 4), data = new.gap.data))




# #----------------------------------------
# # maybe this is fucked up
# #----------------------------------------

		# 		# line graph to compare the two?
		# 		compare.gap <-
		# 			lean.mod %>%
		# 			left_join(., super.denom, by = "cycle") %>%
		# 			transmute(cycle = cycle,
		# 								votes.margin = (M.dem.votes + W.dem.votes - (M.rep.votes + W.rep.votes)) / super.denom,
		# 								votes.gap = 100 * (((W.dem.votes - W.rep.votes) - (M.dem.votes - M.rep.votes)) / super.denom),
		# 								D.votes = M.dem.votes + W.dem.votes,
		# 								R.votes = M.rep.votes + W.rep.votes,
		# 								Dem.vote.share = 100 * D.votes / (D.votes + R.votes),
		# 								margin = Dem.vote.share - (100 - Dem.vote.share)) %>%
		# 			select(cycle:votes.gap, margin) %>%
		# 			# lm(margin ~ g.diff.d.margin, data = .) %>% summary
		# 			full_join(., leip, by = "cycle") %>%
		# 			gather(key = gap.strategy, value = gap, votes.gap, gender.gap) %>%
		# 			gather(key = dv, value = dvote, votes.margin, margin) %>%
		# 			mutate(dv = fct_relevel(as.factor(dv), "votes.margin", "margin"),
		# 						 dv = fct_recode(as.factor(dv),
		# 														 "ANES Reported Vote" = "votes.margin",
		# 														 "Official Democratic Vote" = "margin"),
		# 						 gap.strategy = fct_recode(as.factor(gap.strategy),
		# 						 									"Percentage Gender Gap" = "gender.gap",
		# 						 									"Gender Gap in Votes\n(New measure)" = "votes.gap")) %>%
		# 			print


		# 		new.gap.data <-
		# 			compare.gap %>%
		# 			# filters out ANES vote share and "typical" gender gap measurement
		# 			filter(dv == "Official Democratic Vote"
		# 			       & gap.strategy != "Percentage Gender Gap"
		# 			       ) %>%
		# 			print


		# 		ggplot(data = new.gap.data,
		# 		       aes(x = gap, y = dem.vote.share)) +
		# 			geom_hline(yintercept = 50) +
		# 			geom_vline(xintercept = 0) +
		# 			facet_grid(dv ~ gap.strategy) +
		# 			geom_text(aes(label = cycle),
		# 								size = 2.5) +
		# 			geom_smooth(color = "gray20",
		# 									method = "lm",
		# 									alpha = 0.3) +
		# 			labs(y = "Democratic Vote Share",
		# 					 x = "Gender Gap in Vote Count\n(% of Eligible Voters)")



		# 		ggsave("tex/graphics/new-gap.pdf", height = 5, width = 5)
		# 		embed_fonts("tex/graphics/new-gap.pdf")
		# 		# this sort of fixes a "mobilization" problem. There might be more of one group than another, so the proportion of the vote isn't informative.


		# 		# regression summary from new measure
		# 		new.gap.data %$% {

		# 			# get regression
		# 			lm(dem.vote.share ~ gap) %>%
		# 			tidy %>%
		# 			print %>%
		# 			# coefficient and p value
		# 			filter(term == "gap") %$% {
		# 				# coef
		# 				round(.$estimate, 2) %>%
		# 				print %>%
		# 				write(., "tex/refs/new-gap-coef.tex")

		# 				# p val
		# 				round(.$p.value, 2) %>%
		# 				print %>%
		# 				write(., "tex/refs/new-gap-pval.tex")

		# 				# p val
		# 			}

		# 			cor(dem.vote.share, gap) %>%
		# 			round(., 2) %>%
		# 			print %>%
		# 			write(., "tex/refs/new-gap-cor.tex")

		# 		}

#----------------------------------------
# to here
#----------------------------------------



# we should simulate the situations where this delivers more informative results?

	# gather(key = series, value = value, -cycle) %>%
	# ggplot(aes(x = cycle, y = value, color = series)) +
	# 	geom_hline(yintercept = 0) +
	# 	geom_line()









#----------------------------------------
#		partials with confidence intervals
#----------------------------------------


# combine model conditions, add super-denominators

init.merge <- mutate(mod.init$rhs, leaners = "Leaners as Unaffiliated") %>%
              left_join(., mod.init$denoms, by = "cycle") %>%
              print

lean.merge <- mutate(mod.lean$rhs, leaners = "Leaners as Partisans") %>%
              left_join(., mod.lean$denoms, by = "cycle") %>%
              print




# Make separate party frames to difference across party

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





# merge party frames to difference mechanisms across party

wide <- 
  full_join(select(dems, -combo), select(reps, -combo), 
            by = c("cycle", "super.denom", "gender", "Source", "leaners")) %>%
  # proportion CI will fail for negative numbers. Make mobilization positive for computation. Needs to be undone again.
  mutate(n.dems = ifelse(Source == "Mobilization", -1 * n.dems, n.dems),
         n.reps = ifelse(Source == "Mobilization", -1 * n.reps, n.reps),
         leaners = fct_relevel(leaners, "Leaners as Unaffiliated", "Leaners as Partisans")) %>% 
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



# gender labels for plotting 5-panel partial effects plot

gender.labels <- data_frame(x = rep(1985, 2), 
                            y = c(.18, -.05), 
                            lab = c("Women", "Men"), 
                            # leaners = "Leaners as Unaffiliated", 
                            Source = as.factor("Partisanship")) %>% 
                 mutate(Source = 
                          factor(Source, 
                                 levels = c("Partisanship", 
                                            "Mobilization", 
                                            "Persuasion", 
                                            "Unaffiliated", 
                                            "Net Democratic Votes"))) %>%
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

ggsave("tex/graphics/MM-vote-partials.pdf", height = 4.5, width = 9)
embed_fonts("tex/graphics/MM-vote-partials.pdf")





# ----------------------------------------------------
#   graphic with no intervals
# ----------------------------------------------------


# joint.partials <- 
# 	bind_rows(mutate(mod.lean$partials, leaners = "Leaners as Partisans"), 
# 	          mutate(mod.init$partials, leaners = "Leaners as Unaffiliated")) %>%
# 	mutate(Gender = ifelse(Gender == "M", "Men", "Women"),
# 	       leaners = fct_relevel(leaners, "Leaners as Unaffiliated",  "Leaners as Partisans")) %>%
# 	print




# ggplot(data = joint.partials, aes(x = cycle, y = Impact / 100)) +
# 		facet_grid(leaners ~ Source) +
# 		geom_hline(yintercept = 0) +
# 		geom_line(aes(color = Gender), 
# 		          size = 0.75, 
# 		          show.legend = FALSE) +
# 		geom_point(aes(shape = Gender, color = Gender),
# 		           fill = "white", 
# 		           size = 2, 
# 		           show.legend = FALSE
# 							 ) +
# 		scale_color_manual(values = c(mcolor, wcolor)) +
# 		scale_linetype_manual(values = c(1, 2)) +
# 		scale_shape_manual(values = c(16, 21)) +
# 		coord_cartesian(ylim = c(-0.12, 0.18)) +
# 		scale_x_continuous(breaks = seq(1956, 2012, 8)) +
# 		scale_y_continuous(labels = scales::percent) +
# 		theme(axis.text.x = element_text(angle = 45, vjust=0.75)) +
# 		labs(x = "Election Cycle", 
# 		     y = "Effect on Net Democratic Votes\n(Percent of Eligible Electorate)", 
# 		     shape = NULL) +
# 		geom_text(data = gender.labels, aes(x = x, y = y, label = lab))


# ggsave("tex/graphics/MM-vote-partials.pdf", height = 4.5, width = 9)
# embed_fonts("tex/graphics/MM-vote-partials.pdf")





