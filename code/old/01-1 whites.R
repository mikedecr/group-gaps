########################################
# revision of 'anes smarter code.r'
# reflects changes in mathematical approach as suggested by Ryan Moore
########################################

rm(list = ls())

setwd("/Users/michaeldecrescenzo/Box Sync/Barry PA/Burden-DeCrescenzo")
dir.create("tex")
dir.create("tex/graphics")
dir.create("tex/refs")
dir.create("tex/tables")

library(foreign)
library(magrittr)
library(haven)
library(forcats)
library(tidyverse)
library(ggplot2)
library(stringr)
library(broom)
library(ggthemes)
library(reshape2)
library(wesanderson)
library(viridis)
library(zeallot)
library(beepr)

# custom gg themes
source("/Users/michaeldecrescenzo/Box Sync/R learning/Custom/custom gg theme mgd.R")
theme_set(theme_mbw()) # set default theme
library(extrafont) # load font faces to graphics device
# font_import() # if first time using extrafont on machine
loadfonts() # bring font names into R

dblue <- "#259FDD"
rred <- "#FC5E47"
mcolor <- "mediumpurple"
wcolor <- "lightseagreen"



# confidence interval functions
alarm()
# are these defined in the rest of the file though (and better?)
source("/Users/michaeldecrescenzo/Box Sync/R learning/Custom/prop ci.R")



####################### #################
# Load data
########################################

# this is actually faster than haven::read_dta() ?
anes <- read.dta("anes/anes_cdf_2012_cleaned.dta",
								 convert.underscore = TRUE) %>%
				as_data_frame(.) %>%
				print


#----------------------------------------
#		run on whites only?
#----------------------------------------

anes <- filter(anes, VCF0106 == "1. White non-Hispanic")


########################################
# Helpful functions
########################################

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




# confidence intervals for a vector of differences in proportions
diff.prop.ci <- function(success1, n1, success2, n2, level=0.05) {

	# get parameters
	p1 <- success1 / n1
	q1 <- (n1 - success1) / n1
	p2 <- success2 / n2
	q2 <- (n2 - success2) / n2

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




########################################
# Party-gender-year: party ID
########################################

pid.init.tab <- anes %>%
					group_by(pid.init, gender, cycle) %>%
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


party.label.frame <-
	data_frame(cycle = rep(1980, 2),
	           perc = c(25, 65),
	           party = c("Republicans", "Democrats")) %>%
	print

{
ggplot(data =
       	filter(pid.lean.tab, pid.sorted %in% c("Dem", "Rep")),
     	 aes(x = cycle,  y = perc,
     	     fill = pid.sorted, color = pid.sorted)) +
	facet_grid(. ~ gender) +
	geom_hline(yintercept = 50) +
	geom_ribbon(aes(ymin = lower, ymax = upper,
	                color = NULL),
							alpha = 0.3,
							show.legend = FALSE) +
	geom_line(show.legend = FALSE) +
	geom_point(show.legend = FALSE) +
	geom_text(data = party.label.frame,
	          aes(x = cycle, y = perc, label = party,
	              color = NULL, fill = NULL),
	          size = 3,
	          show.legend = FALSE) +
	coord_cartesian(ylim = c(20, 75)) +
	scale_color_manual(values = c(dblue, rred)) +
	scale_fill_manual(values = c(dblue, rred)) +
	labs(
		# title="Party Identification of Men and Women, 1952-2012",
		y = "Percent Identifiers",
		x = "Election Cycle") +
	scale_x_continuous(
		breaks = seq(1956, 2012, 8)) +
	scale_y_continuous(
		breaks=seq(0, 100, 10)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75))

	# needs grayscale-friendly
	# needs party labels

	# Question: do we really need CIs here? Do they add anything to the interpretation?
}


# ggsave("tex/graphics/MM-party-ID-over-time.pdf", height = 3, width = 6)
# embed_fonts("tex/graphics/MM-party-ID-over-time.pdf")



#----------------------------------------
# gender-year defection
#----------------------------------------


# vote choice by party and gender, initial party ID
tab.loyalty.init <-
	anes %>%
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
tab.loyalty.lean <-
	anes %>%
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







########################################
# gender-year turnout
########################################

tab.turnout.gender <-
	anes %>%
	filter(got.vote.q == 1) %>% # must have been asked about voting
	group_by(gender, cycle, voted) %>%
	summarize(n = sum(wt)) %>%
	group_by(gender, cycle) %>%
	mutate(n.cycle = sum(n),
				 perc = 100 * (n / n.cycle),
				 lower = 100 * prop.ci(n, n.cycle)$lower,
				 upper = 100 * prop.ci(n, n.cycle)$upper) %>%
	ungroup() %>%
	mutate(gender = ifelse(gender == "M", "Men", "Women")) %>%
	# ungroup, refactor gender1
	print



turnout.gender.label.frame <-
	data_frame(cycle = rep(1976, 2),
	           perc = c(63, 83),
	           lab = c("Women", "Men")) %>%
	print

{
ggplot(data = filter(tab.turnout.gender, voted == 1), aes(x = cycle, perc, ymin = lower, ymax = upper,
	color = gender, fill = gender)) +
	geom_ribbon(aes(color = NULL),
	            show.legend = FALSE) +
	geom_line(show.legend = FALSE) +
	geom_point(aes(shape = gender),
	           fill = "white",
	           size = 2,
	           show.legend = FALSE) +
	geom_text(data = turnout.gender.label.frame,
	          aes(x = cycle, y = perc,
	              label = lab,
	              ymin = NULL, ymax = NULL,
	              color = NULL, fill = NULL),
	          size = 3.5,
	          show.legend = FALSE) +
	# scale_fill_manual(values = rev(wes_palette("Darjeeling", 3))) +
	# scale_color_manual(values = rev(wes_palette("Darjeeling", 3))) +
	# scale_fill_manual(values = rev(wes_palette("Rushmore", 4))) +
	# scale_color_manual(values = rev(wes_palette("Rushmore", 4))) +
	scale_color_manual(values = c(mcolor, wcolor)) +
	scale_fill_manual(values = c(mcolor, wcolor)) +
	scale_shape_manual(values = c(16, 21)) +
	labs(x = "Election Cycle",
			 y = "Percent Turnout (Self-Reported)",
			 color = NULL, fill = NULL,
			 shape = NULL) +
	coord_cartesian(ylim = c(55, 90)) +
	scale_x_continuous(
		breaks = seq(1956, 2012, 8)) +
	scale_y_continuous(
		breaks=seq(0, 100, 10)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75))

	# again we should ask if CIs are really doing much here. Do they add to the basic finding?

	# add gender labels, not legend
	# do grayscale version
}



# ggsave("tex/graphics/MM-turnout-gender.pdf", height = 3.5, width = 5)
# embed_fonts("tex/graphics/MM-turnout-gender.pdf")



# although men become more Republican, is their share of the total electorate appears to be decreasing? Might counter-act PID's negative impact on Democratic voting over this time period



alarm()
# should describe how the calculations are actually different here
# since this is kind of a weird result..?

# gender.as.pct.electorate <-
{
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
}

# Is this right???????
# Not exactly sure how this should be interpreted. Increase UNTIL 1980, but the gap continues to widen since then?
#######



# party.x.gender as pct of electorate
{
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
	mutate(
		party = ifelse(pid.sorted == "Dem", "Dem. Identifiers", "Rep. Identifiers"),
		gender = ifelse(gender == "M", "Men", "Women")
	) %>%
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
		scale_x_continuous(
			breaks = seq(1956, 2012, 8)) +
		theme(axis.text.x = element_text(angle = 45, vjust=0.75))
}

# ggsave("tex/graphics/MM-party-gender-share-electorate.pdf", height = 3.5, width = 6.5)
# embed_fonts("tex/graphics/MM-party-gender-share-electorate.pdf")



########################################
# vote-choice, gender, year
########################################

vote.label.frame <-
	data_frame(lab = c("Men", "Women"),
	           cycle = c(1991, 1984),
						 dem.share = c(37, 63)) %>%
	print

{
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
		labs(x = "Election Cycle", y = "Democratic Two-Party Vote Share (%)",
				 color = NULL, fill = NULL) +
		# scale_color_brewer(palette = "Set2") +
		# scale_fill_brewer(palette = "Set2") +
		scale_color_manual(values = c(mcolor, wcolor)) +
		scale_fill_manual(values = c(mcolor, wcolor)) +
		scale_shape_manual(values = c(16, 21)) +
		scale_x_continuous(
			breaks = seq(1956, 2012, 8)) +
		theme(axis.text.x = element_text(angle = 45, vjust=0.75))
}

# ggsave("tex/graphics/MM-gender-gap.pdf", height = 3.5, width = 5)
# embed_fonts("tex/graphics/MM-gender-gap.pdf")


########################################
# Gender gap over time
########################################
# make a list split by gender
gender.split.dshare <-
	anes %>%
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
{ m.dshare : w.dshare } %<-% gender.split.dshare

gap.frame <-
	w.dshare %>%
	rename(n.w = n, w.cycle = n.cycle, w.dem.share = dem.share) %>%
	left_join(m.dshare, ., by = "cycle") %>%
	mutate(
		gender.gap = 100 * ((n.w / w.cycle) - (n / n.cycle)),
		gap.lower = 100 * diff.prop.ci(n.w, w.cycle, n, n.cycle)$lower,
		gap.upper = 100 * diff.prop.ci(n.w, w.cycle, n, n.cycle)$upper
	) %>%
	select(cycle, gender.gap:gap.upper) %>%
	print

{
ggplot(data = gap.frame,
			 aes(x = cycle,  y = gender.gap,
					 ymin = gap.lower, ymax = gap.upper)) +
	geom_ribbon() +
	geom_hline(yintercept = 0) +
	geom_line() +
	geom_point() +
	# geom_pointrange(fatten = 1) +
	labs(x = "Election Cycle", y = "Gender Gap") +
	scale_y_continuous(breaks = seq(-10, 20, 5)) +
	scale_x_continuous(
		breaks = seq(1956, 2012, 8)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75))

}





########################################
# Longitudinal correlation, gap and vote
########################################



### WRites CSV from Leip data...

leip_data <-
	data.frame(
		cycle = seq(1952, 2016, 4),
		dem_raw_votes = c(
			27375090, # 52
			26028028, # 56
			34220984, # 60
			43129040, # 64
			31271839, # 68
			29173222, # 72
			40831881, # 76
			35480115, # 80
			37577352, # 84
			41809476, # 88
			44909806, # 92
			47400125, # 96
			51009810, # 00
			59027115, # 04
			69499428, # 08
			65918507, # 12
   65853652 # 16
			),
		rep_raw_votes = c(
			34075529, # 52
			35579180, # 56
			34108157, # 60
			27175754, # 64
			31783783, # 68
			47168710, # 72
			39148634, # 76
			43903230, # 80
			54455472, # 84
			48886597, # 88
			39104550, # 92
			39198755, # 96
			50462412, # 00
			62039572, # 04
			59950323, # 08
			60934407, # 12
   62985134 # 16
			)
		) %>%
	print

list.files("data")

write_csv(leip_data, here("data/leip-national-pop-vote-data.csv"))
# dir.create("Pop Vote Data")
# write_csv(pop.vote, "Pop Vote Data/leip-national-pop-vote-data.csv")



####
# make graphic

# read in leip data
leip <- read_csv("Pop Vote Data/leip-national-pop-vote-data.csv") %>%
					select(cycle, dem.vote.share) %>%
					left_join(., gap.frame, by = "cycle") %>% # bring in gender gap column
					select(cycle, dem.vote.share, gender.gap) %>%
					print

ggplot(data = leip, aes(x = gender.gap, dem.vote.share)) +
	geom_hline(yintercept = 50) +
	geom_vline(xintercept = 0) +
	geom_smooth(method = "lm",
		          color = "gray20", alpha = 0.3,
		          size = 0.5) +
	geom_point() +
	geom_text(aes(y = dem.vote.share + 0.9, label = cycle)) +
	labs(x = "Gender Gap (%)", y = "Democratic Share of Two-Party Vote (%)")

# ggsave("tex/graphics/MM-gap-vote-scatter.pdf", height = 4, width = 5)
# embed_fonts("tex/graphics/MM-gap-vote-scatter.pdf")


# longitudinal regression
long.reg <- lm(dem.vote.share ~ gender.gap, data = leip) %>%
						tidy %>%
						print

# store regression coefficient and p-val from longitudinal regression
filter(long.reg, term == "gender.gap") %$%
{
	# write(round(estimate, 2), file = "tex/refs/long-coef.tex")
	# write(round(p.value, 2), file = "tex/refs/long-pval.tex")
}

# store correlation
leip %$%
cor(dem.vote.share, gender.gap) %>%
round(., 2) %>%
print # %>%
# write(., "tex/refs/long-corr.tex")


#----------------------------------------
# Regression with time trend
#----------------------------------------
summary(lm(dem.vote.share ~ gender.gap, data = leip))
summary(lm(dem.vote.share ~ gender.gap + I(cycle / 2), data = leip))



########################################
# State exit polls
########################################
states <- data_frame(state.name, state.abb) %>%
					rename(state = state.name) %>% print


exit.poll.data <-
	read_csv("Exit Poll All/exit poll 04-08 long.csv") %>%
	mutate(gender.gap = 100 * (
														(dvote.women / (dvote.women + rvote.women)) -
					                  (dvote.men / (dvote.men + rvote.men))
				                  )
	) %>%
	left_join(., states, by = "state") %>%
	rename(abb = state.abb) %>%
	mutate(abb = ifelse(is.na(abb), "DC", abb)) %>%
	print



{
ggplot(data = exit.poll.data,
       aes(x = gender.gap, y = dshare)) +
	facet_grid(. ~ cycle) +
	geom_hline(yintercept = 50) +
	geom_vline(xintercept = 0) +
	geom_smooth(method = "lm",
		          color = "gray20", size = 0.75,
							alpha = 0.35) +
	# geom_point(color = "gray20",
	# 	         shape = 1,  size = 1) +
	geom_text(aes(label = abb),
	          size = 2.75) +
	scale_y_continuous(breaks = seq(10, 100, 20)) +
	coord_cartesian(ylim = c(20, 95)) +
	labs(x = "Gender Gap (%)", y = "Democratic Share of\nTwo-Party Vote (%)")
}

# ggsave("tex/graphics/MM-cross-sectional-gap-scatter.pdf", height = 3, width = 6)
# embed_fonts("tex/graphics/MM-cross-sectional-gap-scatter.pdf")




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



########################################
# We would write the "model" function somewhere else maybe and then bootstrap it here
########################################


########################################
# How would resampling work if we have survey weights?
########################################



# Dem votes = MD + WD + MRD + WRD + MOD + WOD


alarm()
beep(2)

# need to think about which vote choice variable we want to use.

anes %>% names()
anes %$% table(vote.choice)
anes %$% table(VCF0704) # NA, D, R, Other major 3rd. Don't do this?

anes %$% table(VCF0705) # NA/no vote, D, R, Other vote
anes %$% table(VCF0706) # NA, no vote, other, D, R, NA


# recode anes to include a better vote choice variable
anes <- mutate(anes,
					big.vote = VCF0706,
					big.vote = ifelse(big.vote == "0. DK/NA if voted; refused to say if voted; DK/NA if", "invalid",
										 ifelse(big.vote %in% c("3. Major third party candidate (Wallace 1968/Anderson", "4. Other (incl. 3d/minor party candidates and write-ins)"), "other",
										 	ifelse(big.vote == "7. Did not vote or voted but not for president (exc.1972)", "nonvote",
										 	ifelse(big.vote == "1. Democrat", "dvote", "rvote"))))) %>%
				print

anes %$% table(big.vote)



# choice of PID

anes %$% table(pid.init)
anes %$% table(pid.sorted)


# Raw data aggregated from "initial" PID x vote x gender





# gets n in each category
# also get: n in PID, superdenom in survey year
raw.init <-
	anes %>%
	mutate(party.id = ifelse(pid.init %in% c("Ind", "Other"), "Other", as.character(pid.init))) %>%
	filter(got.vote.q == 1) %>%
	# number in each choice category
	group_by(party.id, gender, big.vote, cycle) %>%
	summarize(n.cat = sum(wt)) %>%
	# super denominator, n total per cycle
	group_by(cycle) %>%
	mutate(super.denom = sum(n.cat)) %>%
	# number in each PID category
	group_by(cycle, party.id) %>%
	mutate(n.in.pid = sum(n.cat)) %>%
	# PID x gender category
	group_by(cycle, party.id, gender) %>%
	mutate(n.in.pid.gender = sum(n.cat)) %>%
	ungroup() %>%
	# proportion in each choice subcategory
	mutate(prop.cat = n.cat / super.denom) %>%
	print





# Raw data from "leaning" PID x vote x gender
raw.lean <-
	anes %>%
	mutate(party.id = ifelse(pid.sorted %in% c("Ind", "Other"), "Other", as.character(pid.sorted))) %>%
	filter(got.vote.q == 1) %>%
	# number in each choice category
	group_by(party.id, gender, big.vote, cycle) %>%
	summarize(n.cat = sum(wt)) %>%
	# super denominator, n total per cycle
	group_by(cycle) %>%
	mutate(super.denom = sum(n.cat)) %>%
	# number in each PID category
	group_by(cycle, party.id) %>%
	mutate(n.in.pid = sum(n.cat)) %>%
	# PID x gender category
	group_by(cycle, party.id, gender) %>%
	mutate(n.in.pid.gender = sum(n.cat)) %>%
	# proportion in each choice subcategory
	ungroup() %>%
	mutate(prop.cat = n.cat / super.denom) %>%
	print





########################################
# Make super denominator
########################################
super.denom <-
	raw.lean %>%
	group_by(cycle) %>%
	summarize(super.denom = unique(super.denom)) %>%
	# as.vector(.) %>%
	print

super.denom.tab <-
	data_frame(cycle = seq(1952, 2012, 4),
						 super.denom = super.denom$super.denom) %>% print


########################################
# Test: PID proportion looks like it should?
########################################
raw.lean %>%
	mutate(prop.pid = n.in.pid.gender / super.denom) %>%
	ggplot(aes(x = cycle, y = prop.pid, color = party.id)) +
		facet_wrap(~ gender) +
		geom_line()






########################################
# arrange raw data into columns for writing the model
########################################

make.raw.terms <- function(tab) {

		tab %>%
		unite(choice.cat, party.id, gender, big.vote, sep = ".") %>%
		select(choice.cat, cycle, n.cat) %>%
		spread(key = choice.cat, value = n.cat, fill = 0) %>%
		transmute(
			cycle = cycle,
			# party ID
			Dem.M.pid = Dem.M.dvote + Dem.M.invalid +
									Dem.M.nonvote + Dem.M.other +
									Dem.M.rvote,
			Dem.W.pid = Dem.W.dvote + Dem.W.invalid +
									Dem.W.nonvote + Dem.W.other +
									Dem.W.rvote,
			Rep.M.pid = Rep.M.dvote + Rep.M.invalid +
									Rep.M.nonvote + Rep.M.other +
									Rep.M.rvote,
			Rep.W.pid = Rep.W.dvote + Rep.W.invalid +
									Rep.W.nonvote + Rep.W.other +
									Rep.W.rvote,
			# party-loyal voting
			Dem.M.loyal = Dem.M.dvote,
			Dem.W.loyal = Dem.W.dvote,
			Rep.M.loyal = Rep.M.rvote,
			Rep.W.loyal = Rep.W.rvote,
			# Sum of all not loyal (nonvote, NA, defect)
			# PID - num.disloyal; num.disloyal = pid - loyal
			Dem.M.not.loyal = Dem.M.pid - Dem.M.loyal,
			Dem.W.not.loyal = Dem.W.pid - Dem.W.loyal,
			Rep.M.not.loyal = Rep.M.pid - Rep.M.loyal,
			Rep.W.not.loyal = Rep.W.pid - Rep.W.loyal,
			# partisan defection,
			Dem.M.defect = Dem.M.rvote,
			Dem.W.defect = Dem.W.rvote,
			Rep.M.defect = Rep.M.dvote,
			Rep.W.defect = Rep.W.dvote,
			# other stuff
			Other.M.dvote = Other.M.dvote,
			Other.W.dvote = Other.W.dvote,
			Other.M.rvote = Other.M.rvote,
			Other.W.rvote = Other.W.rvote
			) %>%
		return

}




########################################
# Use terms to build the LHS and RHS of the models
########################################

build.model <- function(raw.terms) {

	raw.terms %>%
	transmute(
		cycle = cycle,
		# Democrats, partyID, mobilization, persuasion, & independents
		MD.pid = Dem.M.pid,
		WD.pid = Dem.W.pid,
		# MD.mob = Dem.M.pid - Dem.M.not.loyal,
		# WD.mob = Dem.W.pid - Dem.W.not.loyal,
		MD.mob = -1 * Dem.M.not.loyal,
		WD.mob = -1 * Dem.W.not.loyal,
		MD.per = Rep.M.defect,
		WD.per = Rep.W.defect,
		MD.other = Other.M.dvote,
		WD.other = Other.W.dvote,
		# Republicans, partyID, mobilization, persuasion, & independents
		MR.pid = Rep.M.pid,
		WR.pid = Rep.W.pid,
		# MR.mob = Rep.M.pid - Rep.M.not.loyal,
		# WR.mob = Rep.W.pid - Rep.W.not.loyal,
		MR.mob = -1 * Rep.M.not.loyal,
		WR.mob = -1 * Rep.W.not.loyal,
		MR.per = Dem.M.defect,
		WR.per = Dem.W.defect,
		MR.other = Other.M.rvote,
		WR.other = Other.W.rvote,
		# columns for loyalty, just cuz they are useful to keep
		MD.loyal = Dem.M.loyal,
		WD.loyal = Dem.W.loyal,
		MR.loyal = Rep.M.loyal,
		WR.loyal = Rep.W.loyal,
		# build vote_{g,p}
		M.dem.votes = MD.pid + MD.mob + MD.per + MD.other,
		W.dem.votes = WD.pid + WD.mob + WD.per + WD.other,
		M.rep.votes = MR.pid + MR.mob + MR.per + MR.other,
		W.rep.votes = WR.pid + WR.mob + WR.per + WR.other,
		gap = (W.dem.votes - W.rep.votes) - (M.dem.votes - M.rep.votes),
		margin = (W.dem.votes + M.dem.votes) - (W.rep.votes + M.rep.votes),
		gap.prop = gap / super.denom.tab$super.denom,
		margin.prop = margin / super.denom.tab$super.denom
		) %>%
		return
}



########################################
# Difference across party for "net vote effects"
########################################


vote.partials <- function(model.tab) {

	local.cycle <- model.tab$cycle

	level.mechanisms <-
		model.tab %>%
		transmute(
			cycle = cycle,
			M.pid = MD.pid - MR.pid,
			W.pid = WD.pid - WR.pid,
			M.mob = MD.mob - MR.mob,
			W.mob = WD.mob - WR.mob,
			M.per = MD.per - MR.per,
			W.per = WD.per - WR.per,
			M.other = MD.other - MR.other,
			W.other = WD.other - WR.other
		)

	(100 * level.mechanisms[, -1] / super.denom.tab$super.denom ) %>%
	as_data_frame %>%
	mutate(
			W.total = W.pid + W.mob + W.per + W.other,
			M.total = M.pid + M.mob + M.per + M.other,
			gap = W.total - M.total,
			cycle = local.cycle) %>%
		gather(key = Gender.Source, value = Impact, M.pid:M.total) %>%
		separate(Gender.Source, into = c("Gender", "Source")) %>%
		mutate(
			Source = ifelse(Source == "pid", "Partisanship",
							 ifelse(Source == "mob", "Mobilization",
							 ifelse(Source == "per", "Persuasion",
							 ifelse(Source == "other", "Unaffiliated", "Net Democratic Votes")))),
			Source = factor(Source, levels = c("Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Votes"))) %>%
	return

}




# gap.partials <- function(model.tab) {

# 	local.cycle <- model.tab$cycle

# 	level.mechanisms <-
# 		lean.mod %>%
# 		transmute(
# 			cycle = cycle,
# 			D.pid = WD.pid - MD.pid,
# 			R.pid = MR.pid - WR.pid,
# 			D.mob = WD.mob - MD.mob,
# 			R.mob = MR.mob - WR.mob,
# 			D.per = WD.per - MD.per,
# 			R.per = MR.per - WR.per,
# 			D.other = WD.other - MD.other,
# 			R.other = MR.other - WR.other)

# 	(100 * level.mechanisms[, -1] / lean.super.denom$super.denom ) %>%
# 	as_data_frame %>%
# 	mutate(
# 			W.total = W.pid + W.mob + W.per + W.other,
# 			M.total = M.pid + M.mob + M.per + M.other,
# 			gap = W.total - M.total,
# 			cycle = local.cycle) %>%
# 		gather(key = Gender.Source, value = Impact, M.pid:M.total) %>%
# 		separate(Gender.Source, into = c("Gender", "Source")) %>%
# 		mutate(
# 			Source = ifelse(Source == "pid", "Partisanship",
# 							 ifelse(Source == "mob", "Mobilization",
# 							 ifelse(Source == "per", "Persuasion",
# 							 ifelse(Source == "other", "Unaffiliated", "Net Democratic Votes")))),
# 			Source = factor(Source, levels = c("Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Votes"))) %>%
# 	return

# }






# build sets of variables as model inputs
init.terms <- make.raw.terms(raw.init) %>% print
lean.terms <- make.raw.terms(raw.lean) %>% print

# process raw materials into model "parameters"
lean.mod <- build.model(lean.terms) %>% print
init.mod <- build.model(init.terms) %>% print



beep(2)
# This next graph should maybe plot the vote contributions rather than the model terms. These are slightly different for mobilization

# This means you need to track down what persuasion labels mean in the original model


# graph the RHS
rhs <-
	bind_rows(mutate(lean.mod, leaners = "Leaners as Partisans"),
						mutate(init.mod, leaners = "Leaners as Unaffiliated")) %>%
	# select out "mobilization" to plot loyalty instead
	select(-MD.mob, -WD.mob, -MR.mob, -WR.mob) %>%
	gather(key = Source, value = N, MD.pid:WR.loyal) %>%
	select(cycle, Source, N, leaners) %>%
	separate(Source, into = c("PG", "Source")) %>%
	left_join(., super.denom.tab, by = "cycle") %>%
	mutate(prop = N / super.denom,
				 Source = fct_recode(Source, "Partisanship" = "pid",
				 														 "Mobilization" = "loyal",
				 														 "Persuasion" = "per",
				 														 "Unaffiliated" = "other"),
				 Source = fct_relevel(Source, "Partisanship", "Mobilization", "Persuasion", "Unaffiliated"),
				 # ifelse(Source == "pid", "Partisanship", ifelse(Source == "mob", "Mobilization", ifelse(Source == "per", "Persuasion", "Other"))),
				 # Source = factor(Source, levels = c("Partisanship", "Mobilization", "Persuasion", "Other")),
				 PG = fct_recode(PG, "Dem. Votes - Men" = "MD",
				 										 "Dem. Votes - Women" = "WD",
				 										 "Rep. Votes - Men" = "MR",
				 										 "Rep. Votes - Women" = "WR"),
				 PG = fct_relevel(PG, "Dem. Votes - Men", "Dem. Votes - Women", "Rep. Votes - Men"),
				 party = ifelse(grepl("Dem.", PG), "Dem.", "Rep."),
				 gender = ifelse(grepl("Men", PG), "Men", "Women"),
				 leaners = fct_relevel(leaners, "Leaners as Unaffiliated")
				 # ,
				 # swap parties for Persuasion...since it's currently coded as the "benefitting" party)
				 # party = ifelse(party == "Democratic" & Source == "Persuasion", "Rswitch", party),
				 # party = ifelse(party == "Republican" & Source == "Persuasion", "Dswitch", party),
				 # party = ifelse(party == "Rswitch", "Republican", party),
				 # party = ifelse(party == "Dswitch", "Democratic", party)
				 ) %>%
	print


rhs %>%
# filter(Source != "Partisanship") %>%
ggplot(aes(x = cycle, y = 100 * prop, color = PG, shape = PG)) +
	facet_grid(leaners ~ Source) +
	geom_hline(yintercept = 0) +
	geom_line(size = 0.4, show.legend = FALSE) +
	geom_point(fill = "white") +
	# scale_color_brewer(palette = "Spectral") +
	# scale_y_continuous(expand = c(0, .05)) +
	scale_color_manual(values = c(dblue, dblue, rred, rred)) +
	scale_shape_manual(values = c(16, 21, 16, 21)) +
	# scale_shape_manual(values = c(16, 21, 16, 21)) +
	# scale_fill_manual(values = c(dblue, "white", rred, "white")) +
	# scale_color_manual(values = c(dblue, dblue, rred, rred)) +
	labs(x = "Election Cycle",
			 y = "Proportion of Eligible Electorate",
			 shape = NULL,
			 color = NULL) +
	scale_x_continuous(
		breaks = seq(1956, 2012, 8)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75),
	      legend.position = "bottom")



# ggsave("tex/graphics/MM-right-hand-side.pdf", height = 5, width = 8)
# embed_fonts("tex/graphics/MM-right-hand-side.pdf")



# although partisanship advantage among male Democrats decreases. mobilization of Democrats improves overall, and defection decreases







########################################
# Implication of a new way to measure the gender gap
########################################


# gender gap in the number of votes
# maybe should work toward a gender gap in the vote *contribution*\
	# problem with that is: might have a large contribution to a smaller group!


new.gap.data <-
	lean.mod %>%
	left_join(., super.denom, by = "cycle") %>%
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
				round(.$estimate, 2) %>%
				print %>%
				write(., "tex/refs/new-gap-official-coef.tex")

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
	# this
		# geom_text(aes(label = cycle)) +
	# or
		geom_point() +
		geom_text(
      aes(x = ifelse(cycle == 1992, votes.gap + 0.75, votes.gap),
          y = ifelse(cycle == 1992, dem.vote.share, dem.vote.share + 1),
          label = cycle)) +
	labs(x = "Net Gender Gap\n(Percent of Eligible Voters)",
	     y = "Democratic Two-Party Vote Share (%)")



# ggsave("tex/graphics/MM-new-gap-scatter.pdf", height = 4, width = 5)
# embed_fonts("tex/graphics/MM-new-gap-scatter.pdf")



#----------------------------------------
# regression with time trend
#----------------------------------------
summary(lm(dem.vote.share ~ votes.gap, data = new.gap.data))
summary(lm(dem.vote.share ~ votes.gap + I(cycle / 2), data = new.gap.data))



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



		ggsave("tex/graphics/new-gap.pdf", height = 5, width = 5)
		embed_fonts("tex/graphics/new-gap.pdf")
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



# we should simulate the situations where this delivers more informative results


# gather(key = series, value = value, -cycle) %>%
# ggplot(aes(x = cycle, y = value, color = series)) +
# 	geom_hline(yintercept = 0) +
# 	geom_line()











# compute partial impacts on LHS
lean.partials <- vote.partials(lean.mod) %>% print
init.partials <- vote.partials(init.mod) %>% print


joint.partials <-
	bind_rows(mutate(lean.partials, leaners = "Leaners as Partisans"),
						mutate(init.partials, leaners = "Leaners as Unaffiliated")) %>%
	mutate(Gender = ifelse(Gender == "M", "Men", "Women")
				 ) %>%
	print


gap.ribbon <-
	joint.partials %>%
	select(-gap) %>%
	spread(key = Gender, value = Impact) %>%
	mutate(gap.positive = Women > Men) %>%
	print


gender.labels <-
	data_frame(x = rep(1985, 2),
						 y = c(15, -3),
						 lab = c("Women", "Men"),
						 Gender = "Women",
						 # leaners = "Leaners as Unaffiliated",
						 Source = as.factor("Partisanship")) %>%
	mutate(Source = factor(Source, levels = c("Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Votes"))) %>%
	print



ggplot(data = joint.partials,
			 aes(x = cycle, y = Impact
			 		 # linetype = Gender,
			 		 )) +
	# facet_grid(. ~ Source) +
	facet_grid(leaners ~ Source) +
	# geom_area(data = gap.ribbon,
	# 					aes(x = cycle, y = Women,
	# 					fill = gap.positive),
	# 					alpha = 0.7) +
	# geom_area(data = gap.ribbon,
	# 					aes(x = cycle, y = Men),
	# 					fill = "white") +
	# geom_ribbon(data = gap.ribbon,
	# 						aes(x = cycle, ymin = Men, ymax = Women, y = Men, fill = gap.positive),
	# 						show.legend = FALSE,
	# 						alpha = 0.7) +
	geom_hline(yintercept = 0) +
	geom_line(aes(color = Gender),
	          size = 0.75,
						show.legend = FALSE) +
	geom_point(aes(shape = Gender, color = Gender),
	           size = 2,
						 fill = "white",
						 show.legend = FALSE
						 ) +
	scale_color_manual(values = c(mcolor, wcolor)) +
	# scale_fill_manual(values = c(mcolor, wcolor)) +
	scale_linetype_manual(values = c(1, 2)) +
	scale_shape_manual(values = c(16, 21)) +
	coord_cartesian(ylim = c(-12, 18)) +
	scale_x_continuous(
		breaks = seq(1956, 2012, 8)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75)) +
	labs(x = "Election Cycle",
			 y = "Effect on Net Democratic Votes",
			 shape = NULL) +
	geom_text(data = gender.labels, aes(x = x, y = y,
						label = lab))



# ggsave("tex/graphics/MM-vote-partials.pdf", height = 4.5, width = 9)
# embed_fonts("tex/graphics/MM-vote-partials.pdf")









#----------------------------------------
# Dynamics would go here...

# IF THEY WORKED

#----------------------------------------






lean.partials %>%
	spread(key = Gender, value = Impact) %>%
	arrange(cycle, Source) %>%
	mutate(Vote.Impact = M + W,
				 Gap.Impact = W - M) %>%
	group_by(cycle) %>%
	mutate(new.gap = sum(Gap.Impact)/2) %>%
	as.data.frame



########################################
# Dynamics! Difference in differences
# Might need to math this out by hand
########################################


# this arranges
lean.diffs <-
	lean.partials %>%
	arrange(cycle) %>%
	spread(key = Gender, value = Impact) %>%
	# arrange(cycle, Source) %>%
	mutate(Vote.Impact = M + W,
				 Gap.Impact = W - M) %>%
	group_by(Source) %>%
	mutate(d.mechanism = Gap.Impact - lag(Gap.Impact),
				 d.vote = Vote.Impact - lag(Vote.Impact)) %>%
	print


ggplot(lean.diffs, aes(x = d.mechanism, y = d.vote)) +
	facet_wrap(~ Source) +
	geom_hline(yintercept = 0) +
	geom_vline(xintercept = 0) +
	geom_smooth(method = "lm",
							# se = FALSE,
							size = 1) +
	geom_text(aes(label = cycle),
						size = 2.5) +
	coord_cartesian(xlim = c(-6, 11)) +
	labs(x = "Change in Net Gender Gap",
			 y = "Change in Net Democratic Advantage")


# this arranges
lean.diffs <-
	lean.partials %>%
	arrange(cycle) %>%
	spread(key = Gender, value = Impact) %>%
	# arrange(cycle, Source) %>%
	mutate(Vote.Impact = M + W,
				 Gap.Impact = W - M) %>%
	group_by(Source) %>%
	mutate(d.mechanism = Gap.Impact - lag(Gap.Impact),
				 d.vote = Vote.Impact - lag(Vote.Impact)) %>%
	print


# init.diffs <-
# 	init.partials %>%
# 	spread(key = Gender, value = Impact) %>%
# 	mutate(Vote.Impact = M + W,
# 				 Gap.Impact = W - M) %>%
# 	group_by(Source) %>%
# 	mutate(d.mechanism = Gap.Impact - lag(Gap.Impact),
# 				 d.vote = Vote.Impact - lag(Vote.Impact)) %>%
# 	print


ggplot(init.diffs, aes(x = d.mechanism, y = d.vote)) +
	facet_wrap(~ Source) +
	geom_hline(yintercept = 0) +
	geom_vline(xintercept = 0) +
	geom_smooth(method = "lm",
							# se = FALSE,
							size = 1) +
	geom_text(aes(label = cycle),
						size = 2.5) +
	coord_cartesian(xlim = c(-6, 11)) +
	labs(x = "Change in Gender Gap Since Previous Cycle",
			 y = "Change in Democratic Vote Since Previous Cycle")




init.diffs %>%
mutate(leaners = "Leaners as Unaffiliated") %>%
bind_rows(., mutate(lean.diffs, leaners = "Leaners as Partisans")) %>%
ggplot(aes(x = d.mechanism, y = d.vote)) +
	facet_grid(leaners ~ Source) +
	geom_hline(yintercept = 0) +
	geom_vline(xintercept = 0) +
	geom_smooth(method = "lm",
							# se = FALSE,
							size = 1) +
	# geom_text(aes(label = cycle),
	# 					size = 2) +
	geom_point(shape = 1, size = 0.5) +
	# coord_cartesian(xlim = c(-6, 11)) +
	labs(x = "Change in Net Gender Gap",
			 y = "Change in Net Democratic Advantage")



# ggsave("tex/graphics/MM-dynamic-effects.pdf", height = 4.5, width = 9)
# embed_fonts("tex/graphics/MM-dynamic-effects.pdf")








leip %>%
mutate(d.gap = gender.gap - lag(gender.gap),
			 d.vote = dem.vote.share - lag(dem.vote.share)) %>%
ggplot(aes(x = d.gap, y = d.vote)) +
	geom_hline(yintercept = 0) +
	geom_vline(xintercept = 0) +
	geom_text(aes(label = cycle)) +
	# coord_cartesian(ylim = c(-15, 15), xlim = c(-10, 10)) +
	geom_smooth(method = "lm",
		color = "gray20",
		size = 1) +
	labs(x = "Change in Gender Gap",
			 y = "Change in Democratic Vote")



lean.diffs <-


lean.sum.fx <-


	 %>%
	gather(key = Gender, value = Partial, M, W)

lean.partials %>%



	spread(key = Source, value = Partial) %>%
	gather(key = Source, value = Impact, Partisanship:Independents)

	group_by(Gender, Source) %>%
	mutate(d.impact = Impact - lag(Impact)) %>%
	print





