########################################
# revision of 'anes smarter code.r'
# reflects changes in mathematical approach as suggested by Ryan Moore
########################################

rm(list = ls())

setwd("/Users/michaeldecrescenzo/Box Sync/Barry PA/PA Spring 2016")

library(foreign)
library(magrittr)
library(haven)
library(tidyverse)
library(ggplot2)
library(stringr)
library(ggthemes)
library(reshape2)
library(wesanderson)
library(viridis)
library(zeallot)

# custom gg themes
source("/Users/michaeldecrescenzo/Box Sync/R learning/Custom/custom gg theme mgd.R")
theme_set(theme_mbws()) # set default theme
library(extrafont) # load font faces to graphics device
# font_import() # if first time using extrafont on machine
loadfonts() # bring font names into R

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

{
ggplot(data = filter(pid.lean.tab, pid.sorted %in% c("Dem", "Rep")), aes(x = cycle,  y = perc, ymin = lower, ymax = upper, fill = pid.sorted, color = pid.sorted)) +
	facet_grid(. ~ gender) +
	geom_hline(yintercept = 50) +
	geom_ribbon(aes(color = NULL),
							alpha = 0.3, show.legend = FALSE) +
	geom_line( show.legend = FALSE) +
	geom_point( show.legend = FALSE) +
	coord_cartesian(ylim = c(20, 80)) +
	scale_color_manual(values = c("dodgerblue", "orangered")) +
	scale_fill_manual(values = c("dodgerblue", "orangered")) +
	labs(
		# title="Party Identification of Men and Women, 1952-2012",
		y = "Percent Identifiers (Including Leaners)",
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


ggsave("MM-party-ID-over-time.pdf", height = 3.5, width = 6)
embed_fonts("MM-party-ID-over-time.pdf")




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




{
ggplot(data = filter(tab.turnout.gender, voted == 1), aes(x = cycle, perc, ymin = lower, ymax = upper,
	color = gender, fill = gender)) +
	geom_ribbon(aes(color = NULL)) +
	geom_line() +
	geom_point() +
	labs(x = "Election Cycle",
			 y = "Percent Turnout (Self-Reported)",
			 color = NULL, fill = NULL) +
	scale_x_continuous(
		breaks = seq(1956, 2012, 8)) +
	scale_y_continuous(
		breaks=seq(0, 100, 5)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75)) +
	scale_color_brewer(palette = "Set2") +
	scale_fill_brewer(palette = "Set2")

	# again we should ask if CIs are really doing much here. Do they add to the basic finding?

	# add gender labels, not legend
	# do grayscale version
}



ggsave("tex/graphics/MM-turnout-gender.pdf", height = 4, width = 6)
embed_fonts("tex/graphics/MM-turnout-gender.pdf")



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

ggsave("tex/graphics/MM-party-gender-share-electorate.pdf", height = 3.5, width = 6.5)
embed_fonts("tex/graphics/MM-party-gender-share-electorate.pdf")



########################################
# vote-choice, gender, year
########################################
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
		geom_line() +
		geom_point() +
		geom_hline(yintercept = 50) +
		labs(x = "Election Cycle", y = "Democratic Two-Party Vote Share",
				 color = NULL, fill = NULL) +
		scale_color_brewer(palette = "Set2") +
		scale_fill_brewer(palette = "Set2") +
		scale_x_continuous(
			breaks = seq(1956, 2012, 8)) +
		theme(axis.text.x = element_text(angle = 45, vjust=0.75))
}

ggsave("tex/graphics/MM-gender-gap.pdf", height = 3.5, width = 6.5)
embed_fonts("tex/graphics/MM-gender-gap.pdf")


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
read_csv("Pop Vote Data/leip-national-pop-vote-data.csv") %>%
select(cycle, dem.vote.share) %>%
left_join(., gap.frame, by = "cycle") %>% # bring in gender gap column
select(cycle, dem.vote.share, gender.gap) %>%
ggplot(aes(x = gender.gap, dem.vote.share)) +
	geom_hline(yintercept = 50) +
	geom_vline(xintercept = 0) +
	geom_smooth(method = "lm",
		          color = "gray20", alpha = 0.3,
		          size = 0.5) +
	geom_point() +
	geom_text(aes(y = dem.vote.share + 0.9, label = cycle)) +
	labs(x = "Gender Gap", y = "Democratic Share of Two-Party Vote")

ggsave("tex/graphics/MM-gap-vote-scatter.pdf", height = 4, width = 5)
embed_fonts("tex/graphics/MM-gap-vote-scatter.pdf")





########################################
# State exit polls
########################################
states <- data_frame(state.name, state.abb) %>%
					rename(state = state.name) %>% print


read_csv("Exit Poll All/exit poll 04-08 long.csv") %>%
mutate(gender.gap = 100 * (
														(dvote.women / (dvote.women + rvote.women)) -
					                  (dvote.men / (dvote.men + rvote.men))
				                  )
) %>%
left_join(., states, by = "state") %>%
ggplot(aes(x = gender.gap, y = dshare)) +
	facet_grid(. ~ cycle) +
	geom_hline(yintercept = 50) +
	geom_vline(xintercept = 0) +
	geom_smooth(method = "lm",
		          color = "gray20", size = 0.75,
							alpha = 0.35) +
	# geom_point(color = "gray20",
	# 	         shape = 1,  size = 1) +
	geom_text(aes(label = state.abb), size = 2.5) +
	scale_y_continuous(breaks = seq(10, 100, 20)) +
	coord_cartesian(ylim = c(20, 95)) +
	labs(x = "Gender Gap", y = "Democratic Two-Party Vote Share")


ggsave("tex/graphics/MM-cross-sectional-gap-scatter.pdf", height = 3, width = 6)
embed_fonts("tex/graphics/MM-cross-sectional-gap-scatter.pdf")




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
	mutate(pid.init.o = ifelse(pid.init %in% c("Ind", "Other"), "Other", as.character(pid.init))) %>%
	filter(got.vote.q == 1) %>%
	# number in each choice category
	group_by(pid.init.o, gender, big.vote, cycle) %>%
	summarize(n.cat = sum(wt)) %>%
	# super denominator, n total per cycle
	group_by(cycle) %>%
	mutate(super.denom = sum(n.cat)) %>%
	# number in each PID category
	group_by(cycle, pid.init.o) %>%
	mutate(n.in.pid = sum(n.cat)) %>%
	# PID x gender category
	group_by(cycle, pid.init.o, gender) %>%
	mutate(n.in.pid.gender = sum(n.cat)) %>%
	ungroup() %>%
	# proportion in each choice subcategory
	mutate(prop.cat = n.cat / super.denom) %>%
	print





# Raw data from "leaning" PID x vote x gender
raw.lean <-
	anes %>%
	mutate(pid.lean.o = ifelse(pid.sorted %in% c("Ind", "Other"), "Other", as.character(pid.sorted))) %>%
	filter(got.vote.q == 1) %>%
	# number in each choice category
	group_by(pid.lean.o, gender, big.vote, cycle) %>%
	summarize(n.cat = sum(wt)) %>%
	# super denominator, n total per cycle
	group_by(cycle) %>%
	mutate(super.denom = sum(n.cat)) %>%
	# number in each PID category
	group_by(cycle, pid.lean.o) %>%
	mutate(n.in.pid = sum(n.cat)) %>%
	# PID x gender category
	group_by(cycle, pid.lean.o, gender) %>%
	mutate(n.in.pid.gender = sum(n.cat)) %>%
	# proportion in each choice subcategory
	ungroup() %>%
	mutate(prop.cat = n.cat / super.denom) %>%
	print




raw.lean %>%
group_by(pid.lean.o, cycle, gender) %>%
summarize(sum.prop = sum(prop.cat)) %>%
as.data.frame




########################################
# Store vectors free floating
# might not go this route
########################################
lean.super.denom <-
	raw.lean %>%
	group_by(cycle) %>%
	summarize(super.denom = unique(super.denom)) %>%
	print

init.super.denom <-
	raw.init %>%
	group_by(cycle) %>%
	summarize(super.denom = unique(super.denom)) %>%
	print

plot(lean.super.denom$super.denom ~ init.super.denom$super.denom, type = "l")
abline(0, 1, col = "red")


########################################
# Test: PID proportion looks like it should?
########################################
raw.lean %>%
	mutate(prop.pid = n.in.pid.gender / super.denom) %>%
	ggplot(aes(x = cycle, y = prop.pid, color = pid.lean.o)) +
		facet_wrap(~ gender) +
		geom_line()





########################################
# Try to build up from raw, super-denom later
########################################
raw.init.vote <-
	raw.init %>%
	unite(choice.cat, pid.init.o, gender, big.vote, sep = ".") %>%
	select(choice.cat, cycle, n.cat) %>% # this stage might be risky
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
	print


raw.lean.vote <-
	raw.lean %>%
	unite(choice.cat, pid.lean.o, gender, big.vote, sep = ".") %>%
	select(choice.cat, cycle, n.cat) %>% # this stage might be risky
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
	print




########################################
# I should be able to rebuild the Democratic vote from either table
########################################

# Democratic vote
raw.lean.vote %>%
transmute(
	cycle = cycle,
	dem.votes = Dem.M.loyal + Dem.W.loyal +
							Rep.M.defect + Rep.W.defect +
							Other.M.dvote + Other.W.dvote,
	rep.votes = Rep.M.loyal + Rep.W.loyal +
							Dem.M.defect + Dem.W.defect +
							Other.M.rvote + Other.W.rvote,
	dem.share = 100 * dem.votes / (rep.votes + dem.votes)) %>%
ggplot(aes(x = cycle, y = dem.share)) +
	geom_line()

# gender gap
raw.lean.vote %>%
transmute(
	cycle = cycle,
	M.dem.votes = Dem.M.loyal + Rep.M.defect + Other.M.dvote,
	W.dem.votes = Dem.W.loyal + Rep.W.defect + Other.W.dvote,
	M.rep.votes = Rep.M.loyal + Dem.M.defect + Other.M.rvote,
	W.rep.votes = Rep.W.loyal + Dem.W.defect + Other.W.rvote,
	M.dem.share = 100 * M.dem.votes / (M.dem.votes + M.rep.votes),
	W.dem.share = 100 * W.dem.votes / (W.dem.votes + W.rep.votes),
	gap = W.dem.share - M.dem.share
	) %>%
ggplot(aes(x = cycle, y = gap)) +
	geom_line()


# Dem vote by gender (most useful)
raw.lean.vote %>%
transmute(
	cycle = cycle,
	M.dem.votes = Dem.M.loyal + Rep.M.defect + Other.M.dvote,
	W.dem.votes = Dem.W.loyal + Rep.W.defect + Other.W.dvote,
	M.rep.votes = Rep.M.loyal + Dem.M.defect + Other.M.rvote,
	W.rep.votes = Rep.W.loyal + Dem.W.defect + Other.W.rvote,
	M.dem.share = 100 * M.dem.votes / (M.dem.votes + M.rep.votes),
	W.dem.share = 100 * W.dem.votes / (W.dem.votes + W.rep.votes)
	) %>%
gather(key = gender, value = dvote, M.dem.share, W.dem.share) %>%
ggplot(aes(x = cycle, y = dvote, color = gender)) +
	geom_line()


########################################
# Do it with PID in the model
# loyal = pid - total disloyal
########################################

raw.lean.vote %>%
transmute(cycle = cycle,
	M.dem.votes = Dem.M.pid - Dem.M.not.loyal + Rep.M.defect + Other.M.dvote,
	W.dem.votes = Dem.W.pid - Dem.W.not.loyal + Rep.W.defect + Other.W.dvote,
	M.rep.votes = Rep.M.pid - Rep.M.not.loyal + Dem.M.defect + Other.M.rvote,
	W.rep.votes = Rep.W.pid - Rep.W.not.loyal + Dem.W.defect + Other.W.rvote,
	M.dem.share = 100 * M.dem.votes / (M.dem.votes + M.rep.votes),
	W.dem.share = 100 * W.dem.votes / (W.dem.votes + W.rep.votes)
	) %>%
gather(key = gender, value = dvote, M.dem.share, W.dem.share) %>%
ggplot(aes(x = cycle, y = dvote, color = gender)) +
	geom_line()

# sanity check: should not look any different









########################################
# Model (raw numbers)
# should control for sample size
########################################
lean.mod <-
	raw.lean.vote %>%
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
		# build vote_{g,p}
		M.dem.votes = MD.pid + MD.mob + MD.per + MD.other,
		W.dem.votes = WD.pid + WD.mob + WD.per + WD.other,
		M.rep.votes = MR.pid + MR.mob + MR.per + MR.other,
		W.rep.votes = WR.pid + WR.mob + WR.per + WR.other,
		gap = (W.dem.votes - W.rep.votes) - (M.dem.votes - M.rep.votes),
		margin = (W.dem.votes + M.dem.votes) - (W.rep.votes + M.rep.votes),
		gap.prop = gap / init.super.denom$super.denom,
		margin.prop = margin / init.super.denom$super.denom
		) %>%
		print

ggplot(data = lean.mod, aes(x = cycle, y = gap.prop)) +
	geom_line()

# Gap and vote together`
lean.mod %>%
gather(key = series, value = value, gap.prop, margin.prop) %>%
ggplot(aes(x = cycle, y = value, color = series)) +
	geom_line()



init.mod <-
	raw.init.vote %>%
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
		# build vote_{g,p}
		M.dem.votes = MD.pid + MD.mob + MD.per + MD.other,
		W.dem.votes = WD.pid + WD.mob + WD.per + WD.other,
		M.rep.votes = MR.pid + MR.mob + MR.per + MR.other,
		W.rep.votes = WR.pid + WR.mob + WR.per + WR.other,
		gap = (W.dem.votes - W.rep.votes) - (M.dem.votes - M.rep.votes),
		margin = (W.dem.votes + M.dem.votes) - (W.rep.votes + M.rep.votes),
		gap.prop = gap / init.super.denom$super.denom,
		margin.prop = margin / init.super.denom$super.denom
		) %>%
		print

ggplot(data = init.mod, aes(x = cycle, y = gap.prop)) +
	geom_line()

# Gap and vote together`
init.mod %>%
gather(key = series, value = value, gap.prop, margin.prop) %>%
ggplot(aes(x = cycle, y = value, color = series)) +
	geom_line()


########################################
# Partials
# Need to reverse men to get an effect on Gender gap?
########################################



lean.partials <-
	lean.mod %>%
	mutate(
		cycle = cycle,
		M.pid = MD.pid - MR.pid,
		W.pid = WD.pid - WR.pid,
		M.mob = MD.mob - MR.mob,
		W.mob = WD.mob - WR.mob,
		M.per = MD.per - MR.per,
		W.per = WD.per - WR.per,
		M.other = MD.other - MR.other,
		W.other = WD.other - WR.other
	) %>%
	select(cycle, M.pid:W.other) %>%
	print



lean.prop.partials <-
	# standardize across years to interpret
	(100 * lean.partials[, -1] / lean.super.denom$super.denom ) %>%
	as_data_frame %>%
	mutate(
		# maybe move "total" formula into previous function
			W.total = W.pid + W.mob + W.per + W.other,
			M.total = M.pid + M.mob + M.per + M.other,
			cycle = seq(1952, 2012, 4)) %>%
		gather(key = Gender.Source, value = Impact, M.pid:M.total) %>%
		separate(Gender.Source, into = c("Gender", "Source")) %>%
		mutate(
			Source = ifelse(Source == "pid", "Partisanship",
							 ifelse(Source == "mob", "Mobilization",
							 ifelse(Source == "per", "Persuasion",
							 ifelse(Source == "other", "Independents", "Total")))),
			Source = factor(Source, levels = c("Partisanship", "Mobilization", "Persuasion", "Independents", "Total"))) %>%
	print


ggplot(data = lean.prop.partials,
			 aes(x = cycle, y = Impact,
			 		 color = Gender)) +
	# facet_grid(. ~ Source) +
	facet_wrap(~ Source) +
	geom_hline(yintercept = 0) +
	geom_line(size = 0.5) +
	scale_color_brewer(palette = "Dark2") +
	labs(x = "Election Cycle",
			 y = "Impact on Democratic Vote",
			 title = "Leaners as Partisans")






init.partials <-
	init.mod %>%
	mutate(
		cycle = cycle,
		M.pid = MD.pid - MR.pid,
		W.pid = WD.pid - WR.pid,
		M.mob = MD.mob - MR.mob,
		W.mob = WD.mob - WR.mob,
		M.per = MD.per - MR.per,
		W.per = WD.per - WR.per,
		M.other = MD.other - MR.other,
		W.other = WD.other - WR.other
	) %>%
	select(cycle, M.pid:W.other) %>%
	print




init.prop.partials <-
	(100 * init.partials[, -1] / init.super.denom$super.denom ) %>%
	as_data_frame %>%
	mutate(
		W.total = W.pid + W.mob + W.per + W.other,
		M.total = M.pid + M.mob + M.per + M.other,
		cycle = seq(1952, 2012, 4)) %>%
	gather(key = Gender.Source, value = Impact, M.pid:M.total) %>%
	separate(Gender.Source, into = c("Gender", "Source")) %>%
	mutate(
		Source = ifelse(Source == "pid", "Partisanship",
						 ifelse(Source == "mob", "Mobilization",
						 ifelse(Source == "per", "Persuasion",
						 ifelse(Source == "other", "Independents", "Total")))),
		Source = factor(Source, levels = c("Partisanship", "Mobilization", "Persuasion", "Independents", "Total"))) %>%
	print


ggplot(data = init.prop.partials,
			 aes(x = cycle, y = Impact,
			 		 color = Gender)) +
	# facet_grid(. ~ Source) +
	facet_wrap(~ Source) +
	geom_hline(yintercept = 0) +
	geom_line(size = 0.5) +
	scale_color_brewer(palette = "Dark2") +
	labs(x = "Election Cycle",
			 y = "Impact on Democratic Vote",
			 title = "Leaners as Independents")







########################################
# WTF: how to build the model in a good way?
########################################

prop.tab.init <-
	raw.init %>%
	unite(choice.cat, pid.init.o, gender, big.vote) %>%
	select(choice.cat, cycle, prop.cat) %>% # this stage might be risky
	spread(key = choice.cat, value = prop.cat) %>%
	# group_by(cycle) %>%
	mutate(
		Dem.pid = sum(Dem_M_dvote:Dem_W_rvote, na.rm = TRUE),
		Rep.pid = sum(Rep_M_dvote:Rep_W_rvote, na.rm = TRUE)) %>%
	as.data.frame(.) %>%
	print




	split(.$choice.cat) %>%
	melt %>%
	as_data_frame %>%
	select(-choice.cat) %>%
	print






# ?
melt %>%
as_data_frame(.)




# compute Dvote for men and women, long on gender
raw.init.wide <-
	raw.init %>%
	unite(category, pid.init.o, big.vote, sep=".") %>%
	spread(key = category, value = n) %>%
	mutate(
		dvotes = Dem.dvote + Dem.dvote +
						 Rep.dvote + Rep.dvote +
						 Other.dvote + Other.dvote,
	 	rvotes = Rep.rvote + Rep.rvote +
						 Dem.rvote + Dem.rvote +
	 					 Other.rvote + Other.rvote,
	 	Dem.pid = Dem.dvote + Dem.invalid + Dem.nonvote + Dem.other + Dem.rvote,
	 	Rep.pid = Rep.dvote + Rep.invalid + Rep.nonvote + Rep.other + Rep.rvote
	) %>%
	print




# compute Dvote for men and women, long on gender
raw.lean.wide <-
	raw.lean %>%
	unite(category, pid.lean.o, big.vote, sep=".") %>%
	spread(key = category, value = n) %>%
	mutate(
		dvotes = Dem.dvote + Dem.dvote +
						 Rep.dvote + Rep.dvote +
						 Other.dvote + Other.dvote,
	 	rvotes = Rep.rvote + Rep.rvote +
						 Dem.rvote + Dem.rvote +
	 					 Other.rvote + Other.rvote,
		Dem.pid = Dem.dvote + Dem.invalid + Dem.nonvote + Dem.other + Dem.rvote,
		Rep.pid = Rep.dvote + Rep.invalid + Rep.nonvote + Rep.other + Rep.rvote
	) %>%
	print





# TEST: these two gender gap graphics should be identical, since we're summing over all party ID categories (lean/init does not matter)
{
raw.init %>%
ungroup() %>%
unite(category, pid.init.o, big.vote, sep=".") %>%
spread(key = category, value = n) %>%
mutate(dvotes = ifelse(gender == "M",
									Dem.dvote + Rep.dvote + Other.dvote,
									Dem.dvote + Rep.dvote + Other.dvote),
			 rvotes = ifelse(gender == "M",
											 Dem.rvote + Rep.rvote + Other.rvote,
											 Dem.rvote + Rep.rvote + Other.rvote)) %>%
select(cycle, dvotes, rvotes, gender) %>%
mutate(dshare = dvotes / (dvotes + rvotes)) %>%
ggplot(aes(x = cycle, y = dshare, color = gender)) +
	geom_line() +
	scale_x_continuous(breaks = seq(1956, 2012, 8)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75))
}
{
raw.lean %>%
ungroup() %>%
unite(category, pid.lean.o, big.vote, sep=".") %>%
spread(key = category, value = n) %>%
mutate(dvotes = ifelse(gender == "M",
									Dem.dvote + Rep.dvote + Other.dvote,
									Dem.dvote + Rep.dvote + Other.dvote),
			 rvotes = ifelse(gender == "M",
											 Dem.rvote + Rep.rvote + Other.rvote,
											 Dem.rvote + Rep.rvote + Other.rvote)) %>%
select(cycle, dvotes, rvotes, gender) %>%
mutate(dshare = dvotes / (dvotes + rvotes)) %>%
ggplot(aes(x = cycle, y = dshare, color = gender)) +
	geom_line() +
	scale_x_continuous(breaks = seq(1956, 2012, 8)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75))
}

# yay!







# not proportions yet

raw.init.wide %>%
gather(key = party, value = pid, Dem.pid, Rep.pid) %>%
ggplot(aes(x = cycle, y = pid)) +
	facet_wrap(~ gender) +
	geom_line(aes(color = party))






{
raw.init %>%
ungroup() %>%
unite(category, pid.init.o, big.vote, sep=".") %>%
spread(key = category, value = n) %>%
mutate(dvotes = ifelse(gender == "M",
									Dem.dvote + Rep.dvote + Other.dvote,
									Dem.dvote + Rep.dvote + Other.dvote),
			 rvotes = ifelse(gender == "M",
											 Dem.rvote + Rep.rvote + Other.rvote,
											 Dem.rvote + Rep.rvote + Other.rvote)) %>%
select(cycle, dvotes, rvotes, gender) %>%
mutate(dshare = dvotes / (dvotes + rvotes)) %>%
ggplot(aes(x = cycle, y = dshare, color = gender)) +
	geom_line() +
	scale_x_continuous(breaks = seq(1956, 2012, 8)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75))
}
{
raw.lean %>%
ungroup() %>%
unite(category, pid.lean.o, big.vote, sep=".") %>%
spread(key = category, value = n) %>%
mutate(dvotes = ifelse(gender == "M",
									Dem.dvote + Rep.dvote + Other.dvote,
									Dem.dvote + Rep.dvote + Other.dvote),
			 rvotes = ifelse(gender == "M",
											 Dem.rvote + Rep.rvote + Other.rvote,
											 Dem.rvote + Rep.rvote + Other.rvote)) %>%
select(cycle, dvotes, rvotes, gender) %>%
mutate(dshare = dvotes / (dvotes + rvotes)) %>%
ggplot(aes(x = cycle, y = dshare, color = gender)) +
	geom_line() +
	scale_x_continuous(breaks = seq(1956, 2012, 8)) +
	theme(axis.text.x = element_text(angle = 45, vjust=0.75))
}


########################################
# Need to use these data to make
# "PID", "Mobilization" and "Persuasion" measures
########################################

raw.init
raw.init.wide




cbind(
	rowSums(select(raw.init.wide, Dem.dvote:Dem.rvote), na.rm = TRUE),
	rowSums(select(raw.init.wide, Rep.dvote:Rep.rvote), na.rm = TRUE)) %>%
as.data.frame(.) %>%
rename(d = V1, r = V2) %>%
mutate(gender = rep(c("M", "W"), each = 16),
	cycle = rep(seq(1952, 2012, 4), 2)) %>%
split(.$gender) %>%
Reduce(function(dtf1,dtf2) full_join(dtf1, dtf2, by = "cycle"), .)

