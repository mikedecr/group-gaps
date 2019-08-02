########################################
# This aggregates partisanship, vote choice, and turnout data from the big ANES CDF file with recoded variables.
########################################
rm(list = ls())


setwd("/Users/michaeldecrescenzo/Box Sync/Barry PA/PA Spring 2016")

library(foreign)
library(tidyverse)
library(ggthemes)
library(reshape2)
# library(gridExtra)
# ggplot custom theme
source("/Users/michaeldecrescenzo/Box Sync/R learning/Custom/custom gg theme mgd.R")
# custom confidence interval function for vectors of successes and failures
source("/Users/michaeldecrescenzo/Box Sync/R learning/Custom/prop ci.R")
library(wesanderson)
library(viridis)

# theme_set(theme_m38())

theme_set(theme_mbws())



# load font faces to graphics device
library(extrafont)
# font_import()
loadfonts()




anes <- read.dta("ANES/ANES_CDF_2012_CLEANED.dta", convert.underscore=TRUE)
names(anes)


########################################
# maybe do a haven import, but requires converting underscore
########################################


########################################
# Party ID
########################################

# initial partisanship, numerators
tab.pid.init <-
	# aggregate long table
	aggregate(wt ~ pid.init + gender + cycle, data=anes, FUN=sum) %>%
	# combine factors into single variable for reshape/widening
	unite(party.init.gender, pid.init, gender, sep=".") %>%
	# widen by new party.x.gender variable
	spread(key=party.init.gender, value=wt) %>%
	# rename columns
	rename(
		dem.init.men = Dem.M,
		dem.init.women = Dem.W,
		ind.init.men = Ind.M,
		ind.init.women = Ind.W,
		rep.init.men = Rep.M,
		rep.init.women = Rep.W,
		other.init.men = Other.M,
		other.init.women = Other.W) %>%
	print



# sorted partisanship, numerators
tab.pid.sorted <-
	# aggregate with sorted party ID
	aggregate(wt ~ pid.sorted + gender + cycle, data=anes, FUN=sum) %>%
	# make long factor for widening
	unite(party.sorted.gender, pid.sorted, gender, sep=".") %>%
	# reshape long to wide
	spread(key=party.sorted.gender, value=wt) %>%
	rename(	dem.sorted.men = Dem.M,
			dem.sorted.women = Dem.W,
			rep.sorted.men = Rep.M,
			rep.sorted.women = Rep.W,
			true.ind.men = Ind.M,
			true.ind.women = Ind.W,
			other.sorted.men = Other.M,
			other.sorted.women = Other.W) %>%
	print


# total n. of men and women
# for dividing the party ID columns by`
tab.n.obs <-
	aggregate(wt ~ gender + cycle, data=anes, FUN=sum) %>%
	# widen
	spread(key=gender, value=wt) %>%
	rename( nmen = M,
			nwomen = W) %>%
	print




# putting together into a percentages table
tab.pid <-
	# combine data frames
	data.frame(tab.n.obs, select(tab.pid.init, -cycle), select(tab.pid.sorted, -cycle)) %>%
	# computer percentages
	mutate( pct.dem.init.men = 100 * dem.init.men / nmen,
			pct.dem.init.women = 100 * dem.init.women / nwomen,
			pct.ind.init.men = 100 * ind.init.men / nmen,
			pct.ind.init.women = 100 * ind.init.women / nwomen,
			pct.rep.init.men = 100 * rep.init.men / nmen,
			pct.rep.init.women = 100 * rep.init.women / nwomen,
			pct.dem.sorted.men = 100 * dem.sorted.men / nmen,
			pct.dem.sorted.women = 100 * dem.sorted.women / nwomen,
			pct.true.ind.men = 100 * true.ind.men / nmen,
			pct.true.ind.women = 100 * true.ind.women / nwomen,
			pct.rep.sorted.men = 100 * rep.sorted.men / nmen,
			pct.rep.sorted.women = 100 * rep.sorted.women / nwomen) %>%
	# drop raw value columns
	select(cycle, pct.dem.init.men:pct.rep.sorted.women) %>%
	print


tab.init.amg.voters <-
	# aggregate long table
	aggregate(wt ~ pid.init + gender + cycle, data=filter(anes, anes$voted==1), FUN=sum) %>%
	# combine factors into single variable for reshape/widening
	unite(party.init.gender, pid.init, gender, sep=".") %>%
	# widen by new party.x.gender variable
	spread(key=party.init.gender, value=wt) %>%
	rename(	 dem.init.men.voters = Dem.M,
			 dem.init.women.voters = Dem.W,
			 ind.init.men.voters = Ind.M,
			 ind.init.women.voters = Ind.W,
			 rep.init.men.voters = Rep.M,
			 rep.init.women.voters = Rep.W,
			 other.init.men.voters = Other.M,
			 other.init.women.voters = Other.W) %>%
	print


tab.sorted.amg.voters <-
	# aggregate long table
	aggregate(wt ~ pid.sorted + gender + cycle, data=filter(anes, anes$voted==1), FUN=sum) %>%
	# combine factors into single variable for reshape/widening
	unite(party.sorted.gender, pid.sorted, gender, sep=".") %>%
	# widen by new party.x.gender variable
	spread(key=party.sorted.gender, value=wt) %>%
	rename(	 dem.sorted.men.voters = Dem.M,
			 dem.sorted.women.voters = Dem.W,
			 ind.sorted.men.voters = Ind.M,
			 ind.sorted.women.voters = Ind.W,
			 rep.sorted.men.voters = Rep.M,
			 rep.sorted.women.voters = Rep.W,
			 other.sorted.men.voters = Other.M,
			 other.sorted.women.voters = Other.W) %>%
	print






########################################
# Turnout
########################################

# did you vote
tab.turnout.gender <-
	# select only those who got a vote q
	# aggregate by gender and turnout per cycle
	aggregate(wt ~ gender + voted + cycle, data=filter(anes, got.vote.q==1), FUN=sum) %>%
	# united factor for widening
	unite(gender.voted, gender, voted, sep=".") %>%
	# widen by gender x turnout
	spread(key = gender.voted, value=wt) %>%
	rename(
		men.voters = M.1,
		men.nonvoters = M.0,
		women.voters = W.1,
		women.nonvoters = W.0) %>%
	mutate(
		to.men = men.voters / (men.voters + men.nonvoters),
		to.women = women.voters / (women.voters + women.nonvoters),
		to.gap = to.women - to.men
		) %>%
	print



# turnout among initial partisanship groups
tab.turnout.init <-
	# select only from turnout denominator
	aggregate(wt ~ pid.init + gender + voted + cycle, data=filter(anes, got.vote.q==1 & pid.init!="Other"), FUN=sum) %>%
	# united factor
	unite(init.gender.voted, pid.init, gender, voted, sep=".") %>%
	# widen
	spread(key = init.gender.voted, value = wt) %>%
	# compute turnout
	mutate(
		pct.turnout.dem.init.men = (100 * Dem.M.1) / (Dem.M.1 + Dem.M.0),
		pct.turnout.dem.init.women = (100 * Dem.W.1) / (Dem.W.1 + Dem.W.0),
		pct.turnout.ind.init.men = (100 * Ind.M.1) / (Ind.M.1 + Ind.M.0),
		pct.turnout.ind.init.women = (100 * Ind.W.1) / (Ind.W.1 + Ind.W.0),
		pct.turnout.rep.init.men = (100 * Rep.M.1) / (Rep.M.1 + Rep.M.0),
		pct.turnout.rep.init.women = (100 * Rep.W.1) / (Rep.W.1 + Rep.W.0)) %>%
	# keep only important columns
	select(cycle, pct.turnout.dem.init.men:pct.turnout.rep.init.women) %>%
	print


# turnout among sorted party groups
tab.turnout.sorted <-
	# select only from turnout denominator
	aggregate(wt ~ pid.sorted + gender + voted + cycle, data=filter(anes, got.vote.q==1 & pid.sorted!="Other"), FUN=sum) %>%
	# united factor
	unite(sorted.gender.voted, pid.sorted, gender, voted, sep=".") %>%
	# widen
	spread(key = sorted.gender.voted, value = wt) %>%
	# compute turnout
	mutate(
		pct.turnout.dem.sorted.men = (100 * Dem.M.1) / (Dem.M.1 + Dem.M.0),
		pct.turnout.dem.sorted.women = (100 * Dem.W.1) / (Dem.W.1 + Dem.W.0),
		pct.turnout.ind.sorted.men = (100 * Ind.M.1) / (Ind.M.1 + Ind.M.0),
		pct.turnout.ind.sorted.women = (100 * Ind.W.1) / (Ind.W.1 + Ind.W.0),
		pct.turnout.rep.sorted.men = (100 * Rep.M.1) / (Rep.M.1 + Rep.M.0),
		pct.turnout.rep.sorted.women = (100 * Rep.W.1) / (Rep.W.1 + Rep.W.0)) %>%
	# keep only important columns
	select(cycle, pct.turnout.dem.sorted.men:pct.turnout.rep.sorted.women) %>%
	print



########################################
# two-party vote
########################################

tab.vote.choice.gender <-
	aggregate(wt ~ gender + voted.dem + cycle, data=filter(anes, anes$voted.maj==1), FUN=sum) %>%
	unite(gender.vote, gender, voted.dem, sep=".") %>%
	spread(key=gender.vote, value=wt) %>%
	mutate(
		# pct.dem.vote.men = (100 * M.1) / (M.0 + M.1),
		# pct.dem.vote.men.lower = 100 * prop.ci(M.1, M.0, bound="lower"),
		# pct.dem.vote.men.upper = 100 * prop.ci(M.1, M.0, bound="upper"),
		# pct.dem.vote.women = (100 * W.1) / (W.0 + W.1),
		# pct.dem.vote.women.lower = 100 * prop.ci(W.1, W.0, bound="lower"),
		# pct.dem.vote.women.upper = 100 * prop.ci(W.1, W.0, bound="upper")
		majparty.men = M.0 + M.1,
		majparty.women = W.0 + W.1
		) %>%
	rename(
		dvote.men = M.1,
		rvote.men = M.0,
		dvote.women = W.1,
		rvote.women = W.0) %>%
	print




tab.vote.choice.init <-
	# aggregate among major party voters only
	aggregate(wt ~ pid.init + gender + voted.dem + cycle, data=filter(anes, anes$voted.maj==1 & anes$pid.init!="Other"), FUN=sum) %>%
	# make wide
	unite(col=init.gender.vote, pid.init, gender, voted.dem, sep=".") %>%
	spread(key = init.gender.vote, value=wt) %>%
	# compute dem share
	mutate(	dem.share.dem.init.men = 100 * Dem.M.1 / (Dem.M.1 + Dem.M.0),
			dem.share.dem.init.women = 100 * Dem.W.1 / (Dem.W.1 + Dem.W.0),
			dem.share.ind.init.men = 100 * Ind.M.1 / (Ind.M.1 + Ind.M.0),
			dem.share.ind.init.women = 100 * Ind.W.1 / (Ind.W.1 + Ind.W.0),
			dem.share.rep.init.men = 100 * Rep.M.1 / (Rep.M.1 + Rep.M.0),
			dem.share.rep.init.women = 100 * Rep.W.1 / (Rep.W.1 + Rep.W.0)) %>%
	# drop unnecessary columns and rows
	select(cycle, dem.share.dem.init.men:dem.share.rep.init.women) %>%
	filter(!is.na(dem.share.dem.init.men))



tab.vote.choice.sorted <-
	# aggregate among major party voters only
	aggregate(wt ~ pid.sorted + gender + voted.dem + cycle, data=filter(anes, anes$voted.maj==1 & anes$pid.sorted!="Other"), FUN=sum) %>%
	# make wide
	unite(sorted.gender.vote, pid.sorted, gender, voted.dem, sep=".") %>%
	spread(key = sorted.gender.vote, value=wt) %>%
	# compute dem share
	mutate(	dem.share.dem.sorted.men = 100 * Dem.M.1 / (Dem.M.1 + Dem.M.0),
			dem.share.dem.sorted.women = 100 * Dem.W.1 / (Dem.W.1 + Dem.W.0),
			dem.share.ind.sorted.men = 100 * Ind.M.1 / (Ind.M.1 + Ind.M.0),
			dem.share.ind.sorted.women = 100 * Ind.W.1 / (Ind.W.1 + Ind.W.0),
			dem.share.rep.sorted.men = 100 * Rep.M.1 / (Rep.M.1 + Rep.M.0),
			dem.share.rep.sorted.women = 100 * Rep.W.1 / (Rep.W.1 + Rep.W.0)) %>%
	# drop unnecessary columns and rows
	select(cycle, dem.share.dem.sorted.men:dem.share.rep.sorted.women)


tab.vote.choice.amg.inits <-
	aggregate(wt ~ gender + voted.dem + cycle, data=filter(anes, rep.init==1 | dem.init==1), FUN=sum) %>%
	unite(gender.vote, gender, voted.dem, sep=".") %>%
	spread(key = gender.vote, value=wt) %>%
	mutate( dem.share.amg.men.inits = 100 * M.1 / (M.1 + M.0),
			dem.share.amg.women.inits = 100 * W.1 / (W.1 + W.0)) %>%
	select(cycle, dem.share.amg.men.inits, dem.share.amg.women.inits)


########################################
# Popular vote from Leip(?)
########################################

pop.vote <-
	data.frame(
		cycle = seq(1952, 2012, 4),
		dem.raw.vote = c(
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
			65918507
			),
		rep.raw.vote = c(
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
			60934407
			)
		) %>%
	mutate(pct.dem.pop.vote = 100 * dem.raw.vote / (dem.raw.vote + rep.raw.vote)) %>%
	select(cycle, pct.dem.pop.vote) %>%
	print


data.frame(cycle = pop.vote$cycle, Democratic.Popular.Vote = pop.vote$pct.dem.pop.vote)










########################################
# Figure 1
#  Come back to this...you have changed the DF to make it easier.
########################################

gg.pid.sort.ci <-
	# merge Ns and pid sorted table
	merge(tab.pid.sorted, tab.n.obs, by="cycle") %>%
	select(-true.ind.men, -true.ind.women) %>%
	# elongate
	gather(key = party.gender.label, value = n.pid,
		dem.sorted.men, dem.sorted.women, rep.sorted.men, rep.sorted.women) %>%
	mutate(
		gender = ifelse(party.gender.label=="dem.sorted.men" | party.gender.label=="rep.sorted.men", "Men", "Women"),
		# reorder gender factor
		gender = factor(gender, levels=c("Women", "Men")),
		party = ifelse(party.gender.label=="dem.sorted.men" | party.gender.label=="dem.sorted.women", "Democrats", "Republicans"),
		n = ifelse(gender=="Men", nmen, nwomen),
		pct.pid = 100 * (n.pid / n),
		pct.pid.lower = 100 * prop.ci(n.pid, n-n.pid)$lower,
		pct.pid.upper = 100 * prop.ci(n.pid, n-n.pid)$upper) %>%
	select(-party.gender.label, -nmen, -nwomen, -n.pid) %>%
	print



gg.pid.init.ci <-
	# merge Ns and pid sorted table
	merge(tab.pid.init, tab.n.obs, by="cycle") %>%
	select(-ind.init.men, -ind.init.women) %>%
	# elongate
	gather(key = party.gender.label, value = n.pid,
		dem.init.men, dem.init.women, rep.init.men, rep.init.women) %>%
	mutate(
		gender = ifelse(party.gender.label=="dem.init.men" | party.gender.label=="rep.init.men", "Men", "Women"),
		# reorder gender factor
		gender = factor(gender, levels=c("Women", "Men")),
		party = ifelse(party.gender.label=="dem.init.men" | party.gender.label=="dem.init.women", "Democrats", "Republicans"),
		n = ifelse(gender=="Men", nmen, nwomen),
		pct.pid = 100 * (n.pid / n),
		pct.pid.lower = 100 * prop.ci(n.pid, n-n.pid)$lower,
		pct.pid.upper = 100 * prop.ci(n.pid, n-n.pid)$upper) %>%
	select(-party.gender.label, -nmen, -nwomen, -n.pid) %>%
	print

# data frame for the labels on the graph
pid.labels <- data.frame(
				cycle=c(1980, 1980),
				pct.pid = c(65, 25),
				lab=c("Democrats", "Republicans"))



# GGPLOT function with CIs
# start by making an empty plot that we can add specific geoms to later on
empty.pid.plot <- ggplot(gg.pid.sort.ci, aes(cycle, pct.pid)) +
# empty.pid.plot <- ggplot(gg.pid.init.ci, aes(cycle, pct.pid)) +


	facet_wrap(~gender) +

	# geom_line(aes(color=party), show.legend=F) +
	# geom_point(aes(color=party), show.legend=F) +

	geom_hline(yintercept=50) +
	geom_text(data=pid.labels, aes(label=lab),
		size=4, color = "gray25") +

	scale_color_manual(values=c("Democrats" = "dodgerblue", "Republicans" = "firebrick")) +

	labs(
		# title="Party Identification of Men and Women, 1952-2012",
		y="Percent Identifiers (Including Leaners)",
		x="Election Cycle") +

	coord_cartesian(ylim=c(20,80)) +
	scale_x_continuous(
		breaks = seq(1956, 2012, 8)) +
	scale_y_continuous(
		breaks=seq(0, 100, 10)) +

	# theme_m38() +

	theme(axis.text.x = element_text(angle = 45, vjust=0.75))




########################################
# Add data to the empty plot
########################################

empty.pid.plot

pid.lines <- empty.pid.plot +
	geom_line(aes(color=party, linetype = party), show.legend=F)
pid.lines

pid.lines.with.points <- pid.lines +
	geom_point(aes(color=party), show.legend=F)
pid.lines.with.points

pid.ci.with.lines <- pid.lines.with.points +
	geom_linerange(aes(ymin=pct.pid.lower, ymax=pct.pid.upper, color=party), show.legend=FALSE)
pid.ci.with.lines

pid.ci.no.lines <- empty.pid.plot +
	geom_point(aes(color=party), show.legend=F, size=1.25) +
	geom_linerange(aes(ymin=pct.pid.lower, ymax=pct.pid.upper, color=party), show.legend=FALSE)
# geom_pointrange(aes(ymin=pct.pid.lower, ymax=pct.pid.upper, color=party), show.legend=FALSE)
pid.ci.no.lines


pid.ribbons <- pid.lines.with.points +
	geom_ribbon(
		aes(ymin=pct.pid.lower, ymax=pct.pid.upper,
			fill = party),
		alpha=0.2,
		show.legend=FALSE) +
	scale_fill_manual(values=c("Democrats" = "dodgerblue", "Republicans" = "firebrick"))
pid.ribbons


ggsave("tex/graphics/party-ID-gender-ribbon.pdf", plot = pid.ribbons, height = 3.5, width = 7)

embed_fonts("tex/graphics/party-ID-gender-ribbon.pdf")

# ggsave("graphics/gg_pid_figure_1.pdf", plot=pid.lines.with.points, height=4, width=7)
# ggsave("graphics/gg_pid_figure_1_CIs.pdf", plot=pid.ci.with.lines, height=4, width=7)
# ggsave("graphics/gg_pid_figure_1_CIs_no_lines.pdf", plot=pid.ci.no.lines, height=4, width=7)






########################################
# Dem vote by gender over time
########################################

gg.dem.vote <-
	tab.vote.choice.gender %>%
	select(-rvote.men, -rvote.women) %>%
	gather(key = gender.vote, value = n.vote,
		dvote.men, dvote.women) %>%
		mutate(
			gender = ifelse(gender.vote=="dvote.men", "Men", "Women"),
			n.majparty = ifelse(gender=="Men", majparty.men, majparty.women),
			pct.dem.vote = (100*n.vote) / n.majparty,
			pct.dem.vote.lower = 100 * prop.ci(n.vote, n.majparty - n.vote)$lower,
			pct.dem.vote.upper = 100 * prop.ci(n.vote, n.majparty - n.vote)$upper) %>%
	select(-(majparty.men:n.vote), -n.majparty) %>%
	print



# gg.dem.vote <-
# 	tab.vote.choice.gender %>%
# 	gather(gender.label, dem.vote,
# 		pct.dem.vote.men, pct.dem.vote.women) %>%
# 	mutate(	gender = ifelse(gender.label=="pct.dem.vote.men", "Men", "Women")) %>%
# 	select(-gender.label)

dem.vote.labels <- data.frame(
	cycle=c(1996, 1996),
	pct.dem.vote=c(35, 75),
	labs=c("Men", "Women"))


{
gg.dem.vote.by.gender <- ggplot(gg.dem.vote, aes(cycle, pct.dem.vote)) +

	labs(
		# title="Democratic Vote of Men and Women",
		y="Democratic Share of Two-Party Vote",
		x="Election Cycle") +

	geom_hline(yintercept=50) +

	geom_line(aes(color=gender), show.legend=FALSE) +
	geom_point(aes(color=gender), show.legend=FALSE) +

	# scale_color_brewer(palette="Set2") +
	# scale_fill_brewer(palette="Set2") +

	scale_fill_manual(values=rev(wes_palette("Darjeeling"))) +
	scale_color_manual(values=rev(wes_palette("Darjeeling"))) +


	geom_text(data=dem.vote.labels, aes(label=labs), color = "gray25") +

	coord_cartesian(ylim=c(0,100)) +
	scale_x_continuous(
		breaks = seq(1952, 2012, 8)) +
	scale_y_continuous(
		breaks=seq(0, 100, 20))
		 # +

	# geom_linerange(aes(ymin = pct.dem.vote.lower, ymax = pct.dem.vote.upper, color=gender), show.legend=FALSE) +


	# theme_m38()
}

gg.dem.vote.by.gender




# currently 95% confidence intervals

gg.dem.vote.by.gender.ci <-
	gg.dem.vote.by.gender +
	geom_ribbon(aes(x=cycle, ymin=pct.dem.vote.lower, ymax=pct.dem.vote.upper, fill=gender, color=NULL), alpha=0.5,
		show.legend=FALSE)
gg.dem.vote.by.gender.ci
# + theme_m38()


ggsave("tex/graphics/gg_gender_gap_CIs.pdf", height = 3.5, width = 4.5)

embed_fonts("tex/graphics/gg_gender_gap_CIs.pdf")

# ggsave("graphics/gg_dem_vote_gender_cycle.pdf", plot=gg.dem.vote.by.gender, height=4, width=6)
# ggsave("graphics/gg_dem_vote_gender_cycle_CIs.pdf", plot=gg.dem.vote.by.gender.ci, height=4, width=6)











########################################
# turnout plot
########################################

gg.turnout.gender.tab <-
	tab.turnout.gender %>%
	gather(key = gender, value = voters, men.voters, women.voters) %>%
	mutate(
		gender = ifelse(gender=="men.voters", "Men", "Women"),
		nonvoters = ifelse(gender=="Men", men.nonvoters, women.nonvoters),
		pct.turnout = (100 * voters) / (voters + nonvoters),
		pct.turnout.lower = 100 * prop.ci(voters, nonvoters)$lower,
		pct.turnout.upper = 100 * prop.ci(voters, nonvoters)$upper
		) %>%
	select(cycle, gender, pct.turnout, pct.turnout.lower, pct.turnout.upper) %>%
	print

# in-graph text labels
turnout.labels <- data.frame(
	cycle=c(1996, 1996),
	pct.turnout=c(65, 83),
	labs=c("Women", "Men"))

{
gg.turnout.plot <-
	ggplot(gg.turnout.gender.tab, aes(x=cycle, y=pct.turnout)) +

	coord_cartesian(ylim=c(50, 90)) +

	scale_x_continuous(
		breaks = seq(1952, 2012, 8)) +


	geom_point(aes(color = gender), size=2, show.legend = FALSE) +
	geom_ribbon(
			aes(
				ymin=pct.turnout.lower, ymax=pct.turnout.upper,
				fill=gender, color=NULL),
			alpha=0.5,
			show.legend=FALSE
			) +
	geom_line(aes(color = gender), show.legend = FALSE) +
	scale_fill_manual(values=rev(wes_palette("Darjeeling"))) +
	scale_color_manual(values=rev(wes_palette("Darjeeling"))) +
	# scale_color_manual(name=NULL, values=c("gray", "black")) +

	# theme_m38() +

	labs(y = "Percent Turnout", x="Election Cycle") +

	geom_text(data = turnout.labels, aes(label = labs), color = "gray25")
}
gg.turnout.plot



ggsave("tex/graphics/turnout-by-gender-ribbon.pdf", height = 3.5, width = 4.5)

embed_fonts("tex/graphics/turnout-by-gender-ribbon.pdf")





# ggsave("tex/graphics/turnout-by-gender.pdf", plot=gg.turnout.plot.ribbon, height=4, width=6)


# ggsave("graphics/gg_turnout_by_gender.pdf", plot=gg.turnout.plot.line, height=4, width=6)
# ggsave("graphics/gg_turnout_by_gender_CIs.pdf", plot=gg.turnout.plot.ci, height=4, width=6)





########################################
# Scatter plot
########################################

scatter.tab <-
	data.frame(pop.vote, select(tab.vote.choice.gender, -cycle)) %>%
	mutate(
		pct.dem.vote.men = 100 * dvote.men / (dvote.men + rvote.men),
		pct.dem.vote.women = 100 * dvote.women / (dvote.women + rvote.women),
		gender.gap = pct.dem.vote.women - pct.dem.vote.men,
		Era = ifelse(cycle<1980, "Before 1980", "1980 and After"),
		Era = factor(Era, levels=c("Before 1980", "1980 and After"))
	) %>%
	select(cycle, pct.dem.pop.vote, gender.gap, Era) %>%
	print


# all observations
cor(scatter.tab$gender.gap, scatter.tab$pct.dem.pop.vote)
# before 1980
cor(scatter.tab$gender.gap[scatter.tab$cycle<1980], scatter.tab$pct.dem.pop.vote[scatter.tab$cycle<1980])
# all after 1980
cor(scatter.tab$gender.gap[scatter.tab$cycle>=1980], scatter.tab$pct.dem.pop.vote[scatter.tab$cycle>=1980])
# after 1980, excluding 1996
cor(scatter.tab$gender.gap[scatter.tab$cycle>=1980 & scatter.tab$cycle!=1996], scatter.tab$pct.dem.pop.vote[scatter.tab$cycle>=1980 & scatter.tab$cycle!=1996])
# exclude 1986
cor(scatter.tab$gender.gap[scatter.tab$cycle!=1996], scatter.tab$pct.dem.pop.vote[scatter.tab$cycle!=1996])


{
ggplot(scatter.tab, aes(gender.gap, pct.dem.pop.vote)) +

	geom_hline(yintercept=50, color="gray") +
	geom_vline(xintercept=0, color="gray") +



	labs(color = NULL,
		y="Democratic Vote Share",
		x="Gender Gap") +

	geom_smooth(method = "lm", fill = "gray50", color = "gray20", alpha = 0.2) +

	geom_point(size=2.5, color = "gray25") +
	geom_text(aes(label=cycle), nudge_y=1, size=3, color = "gray25")
}


# ggsave("graphics/gg_gender_gap_fig.pdf", plot=gender.gap.fig, height=4.5, width=6)
ggsave("tex/graphics/scatter-gender-gap-dem-vote.pdf", height = 4, width= 5.5)

embed_fonts("tex/graphics/scatter-gender-gap-dem-vote.pdf")


	# coord_cartesian(
	# 	ylim=c(30,70),
	# 	xlim=c(-15,15))




{
ggplot(data = filter(scatter.tab, cycle >= 1976), aes(x = gender.gap,  y = pct.dem.pop.vote)) +

	geom_hline(yintercept = 50, color = "gray50") +
	geom_vline(xintercept = 0, color = "gray50") +

	geom_point() +

	coord_cartesian(ylim = c(40, 60)) +

	geom_text(aes(label = cycle, y = pct.dem.pop.vote + 1)) +

	geom_smooth(
		color = "gray20",
		size = 0.5,
		method = "lm",
		se = FALSE)

}








########################################
# hanger-on gender gap figure
########################################

gap.fig <-
	ggplot(scatter.tab, aes(x = cycle, y = gender.gap)) +

		geom_hline(yintercept = 0, color = "gray50") +
		geom_line(size = 1) +

		scale_x_continuous(breaks = seq(1952, 2012, 4)) +

		theme_mbws() +


		theme(
			axis.text.x = element_text(angle = 45, vjust=0.75)
		) +

		labs(
			title = "Gender Gap in Presidential Voting",
			x = "Election Cycle",
			y = "Gender Gap in Democratic Vote Share\n(Women - Men)")
gap.fig


# ggsave(plot = gap.fig, filename = "~/Box Sync/Elections Research Center/ERC www/images/gender-gap.png", height = 4, width = 6)
ggsave(plot = gap.fig, "tex/graphics/gender-gap-simple.pdf", height = 4, width = 6)

embed_fonts("tex/graphics/gender-gap-simple.pdf")





########################################
# Graph sequence
# 1. Initial party ID among everybody
# 2. party ID among voters (mobilization)
# 3. vote choice among voters (persuasion)
########################################
# major party identification in the electorate,
# voters and nonvoters,
# major party only,
# sorted ID
tab.seq.1x1.sorted <-
	tab.pid.sorted %>%
	mutate(
		majparty.men = dem.sorted.men + rep.sorted.men,
		majparty.women = dem.sorted.women + rep.sorted.women) %>%
	select(cycle, dem.sorted.men, dem.sorted.women, majparty.men, majparty.women) %>%
	gather(
		key = gender, value = n.dem.sorted, dem.sorted.men, dem.sorted.women) %>%
	mutate(
		gender = ifelse(gender=="dem.sorted.men", "Men", "Women"),
		majparty = ifelse(gender=="Men", majparty.men, majparty.women),
		pct.dem = 100 * (n.dem.sorted / majparty),
		pct.dem.lower = 100 * prop.ci(n.dem.sorted, majparty - n.dem.sorted)$lower,
		pct.dem.upper = 100 * prop.ci(n.dem.sorted, majparty - n.dem.sorted)$upper,
		tab = "Party ID - Voters and Nonvoters") %>%
	print

# sorted party ID among all voters
tab.seq.1x2.sorted <-
	tab.sorted.amg.voters %>%
	# major party ID'ers among voters
	mutate(
		majparty.men.voters = dem.sorted.men.voters + rep.sorted.men.voters,
		majparty.women.voters = dem.sorted.women.voters + rep.sorted.women.voters) %>%
	select(cycle, dem.sorted.men.voters, dem.sorted.women.voters, majparty.men.voters, majparty.women.voters) %>%
	gather(
		key = gender, value = n.dem.sorted.voters,
		dem.sorted.men.voters, dem.sorted.women.voters)	%>%
	mutate(
		gender = ifelse(gender=="dem.sorted.men.voters", "Men", "Women"),
		majparty = ifelse(gender=="Men", majparty.men.voters, majparty.women.voters),
		pct.dem = 100 * (n.dem.sorted.voters / majparty),
		pct.dem.lower = 100 * prop.ci(n.dem.sorted.voters, majparty - n.dem.sorted.voters)$lower,
		pct.dem.upper = 100 * prop.ci(n.dem.sorted.voters, majparty - n.dem.sorted.voters)$upper,
		tab = "Party ID - Voters Only") %>%
	print




tab.seq.1x3.sorted <-
	# this includes major party identifiers only as the universe
	aggregate(wt ~ gender + voted.dem + cycle, data=filter(anes, anes$voted.maj==1 & anes$pid.sorted!="Other" & anes$pid.sorted!="Ind"), FUN=sum) %>%
	# make wide
	unite(sorted.gender.vote, gender, voted.dem, sep=".") %>%
	spread(key = sorted.gender.vote, value=wt) %>%
	rename(
		dem.voters.men = M.1,
		dem.voters.women = W.1,
		rep.voters.men = M.0,
		rep.voters.women = W.0
	) %>%
	mutate(
		majparty.men = dem.voters.men + rep.voters.men,
		majparty.women = dem.voters.women + rep.voters.women
	) %>%
	gather(
		key = gender.label, value = n.dem, dem.voters.men, dem.voters.women
	) %>%
	mutate(
		gender = ifelse(gender.label=="dem.voters.men", "Men", "Women"),
		majparty.voters = ifelse(gender=="Men", majparty.men, majparty.women),
		pct.dem = 100 * (n.dem / majparty.voters),
		pct.dem.lower = 100 * prop.ci(n.dem, majparty.voters - n.dem)$lower,
		pct.dem.upper = 100 * prop.ci(n.dem, majparty.voters - n.dem)$upper,
		tab = "Vote Choice - Voters"
	) %>%
	print




tab.seq.stack.sorted.row.1 <-
	rbind(
		select(tab.seq.1x1.sorted, cycle, pct.dem, pct.dem.lower, pct.dem.upper, gender, tab),
		select(tab.seq.1x2.sorted, cycle, pct.dem, pct.dem.lower, pct.dem.upper, gender, tab),
		select(tab.seq.1x3.sorted, cycle, pct.dem, pct.dem.lower, pct.dem.upper, gender, tab))







# gender gaps
	# in party ID
tab.seq.2x1.sorted <-
	tab.pid.sorted %>%
	mutate(
		majparty.men = dem.sorted.men + rep.sorted.men,
		majparty.women = dem.sorted.women + rep.sorted.women,
		gap = 100 * diff.prop.ci(dem.sorted.women, majparty.women, dem.sorted.men, majparty.men)$estimate,
		gap.lower = 100 * diff.prop.ci(dem.sorted.women, majparty.women, dem.sorted.men, majparty.men)$lower,
		gap.upper = 100 * diff.prop.ci(dem.sorted.women, majparty.women, dem.sorted.men, majparty.men)$upper,
		tab = "Gender Gap - Party ID") %>%
	print


# in party ID among voters
tab.seq.2x2.sorted <-
	tab.sorted.amg.voters %>%
	# major party ID'ers among voters
	mutate(
		majparty.men.voters = dem.sorted.men.voters + rep.sorted.men.voters,
		majparty.women.voters = dem.sorted.women.voters + rep.sorted.women.voters,
		gap = 100 * diff.prop.ci(dem.sorted.women.voters, majparty.women.voters, dem.sorted.men.voters, majparty.men.voters)$estimate,
		gap.lower = 100 * diff.prop.ci(dem.sorted.women.voters, majparty.women.voters, dem.sorted.men.voters, majparty.men.voters)$lower,
		gap.upper = 100 * diff.prop.ci(dem.sorted.women.voters, majparty.women.voters, dem.sorted.men.voters, majparty.men.voters)$upper,
		tab = "Gender Gap - Party ID (Voters)") %>%
	print


# in voting among major party ID-ers
tab.seq.2x3.sorted <-
	aggregate(wt ~ gender + voted.dem + cycle, data=filter(anes, anes$voted.maj==1 & anes$pid.sorted!="Other" & anes$pid.sorted!="Ind"), FUN=sum) %>%
	# make wide
	unite(sorted.gender.vote, gender, voted.dem, sep=".") %>%
	spread(key = sorted.gender.vote, value=wt) %>%
	rename(
		dem.voters.men = M.1,
		dem.voters.women = W.1,
		rep.voters.men = M.0,
		rep.voters.women = W.0
	) %>%
	mutate(
		majparty.voters.men = dem.voters.men + rep.voters.men,
		majparty.voters.women = dem.voters.women + rep.voters.women,
		gap = 100 * diff.prop.ci(dem.voters.women, majparty.voters.women, dem.voters.men, majparty.voters.men)$estimate,
		gap.lower = 100 * diff.prop.ci(dem.voters.women, majparty.voters.women, dem.voters.men, majparty.voters.men)$lower,
		gap.upper = 100 * diff.prop.ci(dem.voters.women, majparty.voters.women, dem.voters.men, majparty.voters.men)$upper,
		tab = "Gender Gap - Vote Choice"
	) %>%
	print



tab.seq.stacked.sorted.row.2 <- rbind(select(tab.seq.2x1.sorted, cycle, gap:tab), select(tab.seq.2x2.sorted, cycle, gap:tab), select(tab.seq.2x3.sorted, cycle, gap:tab)) %>%
	print






tab.seq.sorted.row3 <-
	# mash data frames together
	cbind(
		select(tab.seq.2x1.sorted, cycle, dem.sorted.men, dem.sorted.women, majparty.men, majparty.women),
	 select(tab.seq.2x2.sorted, dem.sorted.men.voters, dem.sorted.women.voters, majparty.men.voters, majparty.women.voters),
	 select(tab.seq.2x3.sorted, dem.voters.men, dem.voters.women, majparty.voters.men, majparty.voters.women)
	 ) %>%
	# make diff-in-diff gaps and CIs for diff-in-diffs
	mutate(
		mobilization =
			100 * diff.diff.prop.ci(
				dem.sorted.women.voters, majparty.women.voters,
				dem.sorted.men.voters, majparty.men.voters,
				dem.sorted.women, majparty.women,
				dem.sorted.men, majparty.men)$diff.diff,
		mobilization.lower =
			100 * diff.diff.prop.ci(
				dem.sorted.women.voters, majparty.women.voters,
				dem.sorted.men.voters, majparty.men.voters,
				dem.sorted.women, majparty.women,
				dem.sorted.men, majparty.men)$lower,
		mobilization.upper =
			100 * diff.diff.prop.ci(
				dem.sorted.women.voters, majparty.women.voters,
				dem.sorted.men.voters, majparty.men.voters,
				dem.sorted.women, majparty.women,
				dem.sorted.men, majparty.men)$upper,
		persuasion =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.sorted.women.voters, majparty.women.voters,
				dem.sorted.men.voters, majparty.men.voters)$diff.diff,
		persuasion.lower =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.sorted.women.voters, majparty.women.voters,
				dem.sorted.men.voters, majparty.men.voters)$lower,
		persuasion.upper =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.sorted.women.voters, majparty.women.voters,
				dem.sorted.men.voters, majparty.men.voters)$upper,
		total.campaign =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.sorted.women, majparty.women,
				dem.sorted.men, majparty.men)$diff.diff,
		total.campaign.lower =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.sorted.women, majparty.women,
				dem.sorted.men, majparty.men)$lower,
		total.campaign.upper =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.sorted.women, majparty.women,
				dem.sorted.men, majparty.men)$upper
			) %>%
	gather( key = var, value = impact,
		mobilization, persuasion, total.campaign) %>%
	mutate(
		impact.lower =
			ifelse(var=="mobilization", mobilization.lower,
			ifelse(var=="persuasion", persuasion.lower, total.campaign.lower
				)
			),
		impact.upper =
			ifelse(var=="mobilization", mobilization.upper,
			ifelse(var=="persuasion", persuasion.upper, total.campaign.upper
				)
			),
		var =
			ifelse(var=="mobilization", "Mobilization",
				ifelse(var=="persuasion", "Persuasion", "Total Campaign"))
		) %>%
	select(cycle, impact, impact.lower, impact.upper, var) %>%
	print




# Row 1 for sorted party ID
gg.seq.sorted.row.1 <- ggplot(data=tab.seq.stack.sorted.row.1, aes(x=cycle, y=pct.dem)) +

	facet_wrap(~tab) +

	geom_hline(yintercept = 50, color="gray25") +

	geom_line(aes(color=gender)) +
	geom_ribbon(
		aes(ymin = pct.dem.lower, ymax = pct.dem.upper,
			fill=gender),
		alpha=0.4) +


	scale_color_manual(name=NULL, values=rev(wes_palette("Darjeeling"))) +
	scale_fill_manual(name=NULL, values=rev(wes_palette("Darjeeling"))) +

	labs(y="Percent Democratic", x=NULL) +

	coord_cartesian(ylim=c(0,100)) +

	# theme_mgd() +
	# theme_m38() +

	theme(
		legend.position=c(0,0),
		legend.justification=c(0,0)
		# , legend.background = element_rect(color="gray")
		)


gg.seq.sorted.row.2 <- ggplot(data=tab.seq.stacked.sorted.row.2, aes(x = cycle, y = gap)) +

	facet_wrap(~tab) +

	geom_hline(yintercept = 0, color="gray25") +

	geom_line() +
	geom_ribbon(
		aes(ymin = gap.lower, ymax = gap.upper),
		alpha=0.4,
		fill="mediumpurple") +

	labs(y="Gender Gap", x=NULL) +

	coord_cartesian(ylim=c(-25,25))
	# +

	# theme_mgd()
	# theme_m38()

gg.seq.sorted.row.3 <- ggplot(data=tab.seq.sorted.row3, aes(x=cycle, y=impact)) +

	facet_wrap(~var) +

	geom_hline(yintercept = 0, color="gray25") +

	geom_line() +

	geom_ribbon(
		aes(ymin=impact.lower, ymax=impact.upper),
		alpha=0.4,
		fill="mediumpurple") +

	coord_cartesian(ylim=c(-25, 25)) +

	labs(y="Impact on Gender Gap", x="Election Cycle")
	# +

	# theme_mgd()
	# theme_m38()



# load and unload gridExtra package for making the graphics grid.
# just don't want to overwrite other functions in dplyr
library(gridExtra)

(seq.sorted <-
	grid.arrange(
		gg.seq.sorted.row.1, gg.seq.sorted.row.2, gg.seq.sorted.row.3,
		nrow=3
		# , top="Leaners Sorted as Partisans"
		)
	)

detach(package:gridExtra, unload=TRUE)
# ggsave("tex/graphics/mobilization-persuasion-grid-sorted.pdf", plot=seq.sorted, height=8, width=10)




########################################
# Repeat above for initial party ID
########################################

tab.seq.1x1.init <-
	tab.pid.init %>%
	mutate(
		majparty.init.men = dem.init.men + rep.init.men,
		majparty.init.women = dem.init.women + rep.init.women
		) %>%
	select(cycle, dem.init.men, dem.init.women, majparty.init.men, majparty.init.women) %>%
	gather(
		key = gender.key, value = n.dem.init,
		dem.init.men, dem.init.women) %>%
	mutate(
		gender = ifelse(gender.key=="dem.init.men", "Men", "Women"),
		majparty.init = ifelse(gender=="Men", majparty.init.men, majparty.init.women),
		pct.dem = 100 * (n.dem.init/majparty.init),
		# CI for prop takes success and fail,
		# not success and n
		pct.dem.lower =
			100 * prop.ci(n.dem.init, majparty.init - n.dem.init)$lower,
		pct.dem.upper =
			100 * prop.ci(n.dem.init, majparty.init - n.dem.init)$upper,
		tab = "Party ID - Voters and Nonvoters") %>%
	print



tab.seq.1x2.init <-
	tab.init.amg.voters %>%
	# sum columns to get two-party ID
	mutate(
		majparty.init.men.voters = dem.init.men.voters + rep.init.men.voters,
		majparty.init.women.voters = dem.init.women.voters + rep.init.women.voters) %>%
	# select only numerator and denominator columns w/ cycle
	select(cycle:dem.init.women.voters, rep.init.men.voters:majparty.init.women.voters) %>%
	# make long to code gender as a factor
	gather(
		key = gender.key, value = n.dem.init.voters,
		dem.init.men.voters, dem.init.women.voters) %>%
	# gender variable, estimates, CIs, table label
	mutate(
		gender =
			ifelse(gender.key=="dem.init.men.voters", "Men", "Women"),
		majparty.init.voters =
			ifelse(gender=="Men", majparty.init.men.voters, majparty.init.women.voters),
		pct.dem =
			100 * (n.dem.init.voters / majparty.init.voters),
		pct.dem.lower =
			100 * prop.ci(n.dem.init.voters, majparty.init.voters - n.dem.init.voters)$lower,
		pct.dem.upper =
			100 * prop.ci(n.dem.init.voters, majparty.init.voters - n.dem.init.voters)$upper,
		tab = "Party ID - Voters Only") %>%
	print


tab.seq.1x3.init <-
# this includes INITIAL major party identifiers only as the universe
	aggregate(wt ~ gender + voted.dem + cycle, data=filter(anes, anes$voted.maj==1 & anes$pid.init!="Other" & anes$pid.init!="Ind"), FUN=sum) %>%
	# widen
	spread(key = voted.dem, value = wt) %>%
	mutate(
		gender = ifelse(gender=="M", "Men", "Women"),
		n.dem.voters = .$"1",
		majparty.voters = n.dem.voters + .$"0",
		pct.dem = 100 * (n.dem.voters / majparty.voters),
		pct.dem.lower =
			100 * prop.ci(n.dem.voters, .$"0")$lower,
		pct.dem.upper =
			100 * prop.ci(n.dem.voters, .$"0")$upper,
		tab = "Vote Choice - Voters"
		) %>%
	print



tab.seq.stacked.init.row.1 <-
	rbind(
		select(tab.seq.1x1.init, cycle, gender, pct.dem:tab),
		select(tab.seq.1x2.init, cycle, gender, pct.dem:tab),
		select(tab.seq.1x3.init, cycle, gender, pct.dem:tab)
		)








# gender gap row
	# going to try to put these all into a long table and get the CIs at one time.
	# which means numerator and denominator columns need to be labeled the same thing.
	# and I'm distinguishing the concepts by the factor that indicates which concept we're looking at e.g. PID, PID among voters, etc.
tab.seq.2x1.init <-
	tab.pid.init %>%
	mutate(
		num.men = dem.init.men,
		num.women = dem.init.women,
		denom.men = dem.init.men + rep.init.men,
		denom.women = dem.init.women + rep.init.women,
		tab = "Gender Gap - Party ID"
		) %>%
	select(cycle, cycle, num.men:tab) %>%
	print


tab.seq.2x2.init <-
	tab.init.amg.voters %>%
	# sum columns to get two-party ID
	mutate(
		num.men = dem.init.men.voters,
		num.women = dem.init.women.voters,
		denom.men = dem.init.men.voters + rep.init.men.voters,
		denom.women = dem.init.women.voters + rep.init.women.voters,
		tab = "Gender Gap - Party ID (Voters)") %>%
	# select only numerator and denominator columns w/ cycle
	select(cycle, num.men:tab) %>%
	print


tab.seq.2x3.init <-
	# this includes INITIAL major party identifiers only as the universe
	aggregate(wt ~ gender + voted.dem + cycle, data=filter(anes, anes$voted.maj==1 & anes$pid.init!="Other" & anes$pid.init!="Ind"), FUN=sum) %>%
	# widen
	unite(gender.vote, gender, voted.dem, sep=".") %>%
	spread(key = gender.vote, value = wt) %>%
	mutate(
		num.men = M.1,
		num.women = W.1,
		denom.men = M.1 + M.0,
		denom.women = W.1 + W.0,
		tab = "Gender Gap - Vote Choice") %>%
	select(cycle, num.men:tab) %>%
	print

tab.seq.stacked.init.row.2 <-
	rbind(
		tab.seq.2x1.init, tab.seq.2x2.init, tab.seq.2x3.init
		) %>%
	mutate(
		gap.test = 100 * ((num.women / denom.women) - (num.men / denom.men)),
		gap =
			100 * diff.prop.ci(num.women, denom.women, num.men, denom.men)$estimate,
		gap.lower =
			100 * diff.prop.ci(num.women, denom.women, num.men, denom.men)$lower,
		gap.upper =
			100 * diff.prop.ci(num.women, denom.women, num.men, denom.men)$upper
		) %>%
	select(cycle, tab:gap.upper) %>%
	print


# naming scheme not same here
tab.seq.row.3.init <-
	tab.pid.init %>%
	mutate(
		majparty.init.men = dem.init.men + rep.init.men,
		majparty.init.women = dem.init.women + rep.init.women
		) %>%
	select(cycle:dem.init.women, majparty.init.men, majparty.init.women) %>%
	print

tab.seq.row.3.init.voters <-
	tab.init.amg.voters %>%
	mutate(
		majparty.init.men.voters = dem.init.men.voters + rep.init.men.voters,
		majparty.init.women.voters = dem.init.women.voters + rep.init.women.voters) %>%
	select(cycle:dem.init.women.voters, majparty.init.men.voters, majparty.init.women.voters) %>%
	print

tab.seq.row.3.init.vote.choice <-
	aggregate(wt ~ gender + voted.dem + cycle, data=filter(anes, anes$voted.maj==1 & anes$pid.init!="Other" & anes$pid.init!="Ind"), FUN=sum) %>%
	# widen
	unite(gender.vote, gender, voted.dem, sep=".") %>%
	spread(key = gender.vote, value = wt) %>%
	mutate(
		dem.voters.men = M.1,
		dem.voters.women = W.1,
		majparty.voters.men = M.0 + M.1,
		majparty.voters.women = W.1 + W.0) %>%
	select(cycle, dem.voters.men:majparty.voters.women) %>%
	print

names(tab.seq.row.3.init)
names(tab.seq.row.3.init.voters)
names(tab.seq.row.3.init.vote.choice)

tab.seq.init.row.3 <-
	data.frame(tab.seq.row.3.init, select(tab.seq.row.3.init.voters, -cycle), select(tab.seq.row.3.init.vote.choice, -cycle)) %>%
	mutate(
		# mobilization, panel 1
		mobilization =
			100 * diff.diff.prop.ci(
				dem.init.women.voters, majparty.init.women.voters,
				dem.init.men.voters, majparty.init.men.voters,
				dem.init.women, majparty.init.women,
				dem.init.men, majparty.init.men)$diff.diff,
		mobilization.lower =
			100 * diff.diff.prop.ci(
				dem.init.women.voters, majparty.init.women.voters,
				dem.init.men.voters, majparty.init.men.voters,
				dem.init.women, majparty.init.women,
				dem.init.men, majparty.init.men)$lower,
		mobilization.upper =
			100 * diff.diff.prop.ci(
				dem.init.women.voters, majparty.init.women.voters,
				dem.init.men.voters, majparty.init.men.voters,
				dem.init.women, majparty.init.women,
				dem.init.men, majparty.init.men)$upper,
		# persuasion, panel 2
		persuasion =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.init.women.voters, majparty.init.women.voters,
				dem.init.men.voters, majparty.init.men.voters)$diff.diff,
		persuasion.lower =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.init.women.voters, majparty.init.women.voters,
				dem.init.men.voters, majparty.init.men.voters)$lower,
		persuasion.upper =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.init.women.voters, majparty.init.women.voters,
				dem.init.men.voters, majparty.init.men.voters)$upper,
		# total campaign, panel 3
		total.campaign =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.init.women, majparty.init.women,
				dem.init.men, majparty.init.men)$diff.diff,
		total.campaign.lower =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.init.women, majparty.init.women,
				dem.init.men, majparty.init.men)$lower,
		total.campaign.upper =
			100 * diff.diff.prop.ci(
				dem.voters.women, majparty.voters.women,
				dem.voters.men, majparty.voters.men,
				dem.init.women, majparty.init.women,
				dem.init.men, majparty.init.men)$upper
		) %>%
	select(cycle, mobilization:total.campaign.upper) %>%
	gather(
		key = panel, value = gap.diff,
		mobilization, persuasion, total.campaign) %>%
	mutate(
		gap.diff.lower =
			ifelse(panel=="mobilization", mobilization.lower,
				ifelse(panel=="persuasion", persuasion.lower, total.campaign.lower)),
		gap.diff.upper =
			ifelse(panel=="mobilization", mobilization.upper,
				ifelse(panel=="persuasion", persuasion.upper, total.campaign.upper)),
		tab = ifelse(panel=="mobilization", "Mobilization",
				ifelse(panel=="persuasion", "Persuasion", "Total Campaign"))) %>%
	select(cycle, panel:tab) %>%
	print




gg.seq.init.row.1 <-
ggplot(data = tab.seq.stacked.init.row.1, aes(x=cycle, y=pct.dem)) +

	geom_hline(yintercept = 50, color="gray25") +

	facet_wrap(~tab) +

	geom_line(aes(color=gender)) +
	geom_ribbon(aes(ymin=pct.dem.lower, ymax=pct.dem.upper, fill=gender),
		alpha=0.3) +

	scale_fill_manual(name=NULL, values=rev(wes_palette("Darjeeling"))) +
	scale_color_manual(name=NULL, values=rev(wes_palette("Darjeeling"))) +

	coord_cartesian(ylim=c(0, 100)) +

	labs(y="Percent Democratic", x=NULL) +

	# theme_mgd() +
	theme_m38() +

	theme(
		legend.position=c(0,0),
		legend.justification=c(0,0),
		legend.background=element_blank())




# plot for row 2 of init sequence grid
gg.seq.init.row.2 <-
ggplot(data = tab.seq.stacked.init.row.2,
	aes(x = cycle, y = gap)) +

	facet_wrap(~tab) +
	geom_hline(yintercept = 0, color="gray25") +

	geom_line() +
	geom_ribbon(
		aes(ymin = gap.lower, ymax=gap.upper),
		fill = "steelblue",
		alpha=0.2) +

	labs(x = NULL, y = "Gender Gap") +

	coord_cartesian(ylim=c(-25, 25)) +

	# theme_mgd()
	theme_m38()

gg.seq.init.row.3 <-
ggplot(data = tab.seq.init.row.3,
	aes(x = cycle, y = gap.diff)) +

	facet_wrap(~tab) +

	geom_hline(yintercept = 0, color="gray25") +

	geom_line() +
	geom_ribbon(
		aes(ymin = gap.diff.lower, ymax = gap.diff.upper),
		alpha=0.2,
		fill = "steelblue") +

	coord_cartesian(ylim=c(-25, 25)) +

	labs(x = "Election Cycle", y = "Impact on Gender Gap") +

	# theme_mgd()
	theme_m38()


library(gridExtra)
(seq.init <-
	grid.arrange(
		gg.seq.init.row.1,
		gg.seq.init.row.2,
		gg.seq.init.row.3,
		nrow=3
		# , top = "Leaners as Independents"
	))
detach(package:gridExtra, unload=TRUE)

# ggsave("tex/graphics/mobilization-persuasion-grid-init.pdf", plot=seq.init, height=8, width=10)









########################################
# Regressions: is gender gap drive more by turnout or party loyalties?


# other idea: is increase over party ID gender gap driven more by turnout or mobilization? regression context.
########################################
names(anes)

#  turnout and vote choice by gender and party ID
tab.turnout.init
tab.turnout.sorted
tab.vote.choice.init
tab.vote.choice.sorted


tab.turnout.gender
tab.pid
tab.init.amg.voters
tab.sorted.amg.voters


# turnout.men, turnout.women, dem.2p.id.men dem.2p.id.women (nonvoters)

tab.pid.init
tab.pid.sorted


reg.data.by.gender <-
	merge(tab.turnout.gender, tab.pid.init, by = "cycle", all = TRUE) %>%
	merge(., tab.pid.sorted, by = "cycle", all = TRUE) %>%
	mutate(
		gender.gap = scatter.tab$gender.gap,
		turnout.men = 100 * to.men,
		turnout.women = 100 * to.women,
		dem.pid.men.init = 100 * (dem.init.men / (dem.init.men + rep.init.men)),
		dem.pid.women.init = 100 * (dem.init.women / (dem.init.women + rep.init.women)),
		dem.pid.men.sorted = 100 * (dem.sorted.men / (dem.sorted.men + rep.sorted.men)),
		dem.pid.women.sorted = 100 * (dem.sorted.women / (dem.sorted.women + rep.sorted.women))
		) %>%
	print



reg.by.gender.init <- lm(gender.gap ~ turnout.men + turnout.women + dem.pid.men.init + dem.pid.women.init, data = reg.data.by.gender)
summary(reg.by.gender.init)

reg.by.gender.sorted <- lm(gender.gap ~ turnout.men + turnout.women + dem.pid.men.sorted + dem.pid.women.init, data = reg.data.by.gender)
summary(reg.by.gender.init)


(degrees <- reg.by.gender.init$df.residual)



library(coefplot)

coefplot(reg.by.gender.init,
	intercept = FALSE,
	title = NULL
	) +

	theme_m38()

coefplot(reg.by.gender.sorted,
	intercept = FALSE,
	title = NULL
	) +

	theme_m38()



library(broom)

broomed.mods <-
	rbind(
		cbind(tidy(reg.by.gender.init), mod = "init"),
		cbind(tidy(reg.by.gender.sorted), mod = "sorted")
	) %>%
	mutate(
		term = ifelse(term=="dem.pid.men.init" | term == "dem.pid.men.sorted", "dem.pid.men", term),
		term = ifelse(term=="dem.pid.women.init" | term == "dem.pid.women.sorted", "dem.pid.women", term)
		) %>%
	print



{
ggplot(filter(broomed.mods, term!="(Intercept)"), aes(x = term, y = estimate, ymin = (statistic - qt(.975, df = degrees)) * std.error, ymax = (statistic + qt(.975, df = degrees)) * std.error)) +

	geom_hline(yintercept = 0, color = "gray50", size = 1) +

	geom_pointrange(aes(color = mod, fill = mod), position=position_dodge(width= -0.25), shape = 21) +

	coord_flip() +

	scale_x_discrete(labels = c("Dem. Party ID\n(Men)", "Dem. Party ID\n(Women)", "Turnout\n(Men)", "Turnout\n(Women)")) +

	labs(x = NULL, y = "Percentage Point Impact\non Gender Gap", color = "Leaners coded as", fill = "Leaners coded as") +

	scale_fill_manual(
		values = c("gray25", "white"),
		labels = c("Independents", "Partisans")) +
	scale_color_manual(
		values = c("gray25", "gray25"),
		labels = c("Independents", "Partisans"))
	# +

	# theme_m38()
}


ggsave("tex/graphics/regression-coefs.pdf", height = 5, width = 4)

embed_fonts("tex/graphics/regression-coefs.pdf")



########################################
# "Net" regressions
# There are multiple denominators, some commented out.
# think about what you need.
########################################



anes <-
	anes %>%
	mutate(
		# make dummy variable where non-major party votes are NA
		voted.dem.amg.voters = ifelse(voted.maj.party==1, voted.dem, "not.party.vote")
	)

table(anes$voted.maj.party, anes$voted.dem.amg.voters, exclude=NULL)






# Initial partisanship, turnout and defection
net.turnout.init <-
	aggregate(wt ~ gender + voted + pid.init + cycle,
		data = filter(anes), FUN=sum
	) %>%
	filter(
		pid.init!="Other" & pid.init!="Ind"
	) %>%
	unite(party.gender.voted, pid.init, gender, voted, sep=".") %>%
	spread(key = party.gender.voted, value = wt) %>%
	rename(
		d.men.voted = Dem.M.1,
		d.women.voted = Dem.W.1,
		r.men.voted = Rep.M.1,
		r.women.voted = Rep.W.1
	) %>%
	mutate(
		# net votes for Democrats among men and women
		net.dem.voters.men = d.men.voted - r.men.voted,
		net.dem.voters.women = d.women.voted - r.women.voted,
		# super-denominator: all eligible men and women in the electorate
		super.denom.init = (d.men.voted + Dem.M.0 + r.men.voted + Rep.M.0 + d.women.voted + Dem.W.0 + r.women.voted + Rep.W.0),
		# first is among partisans only
		# d.men.turnout = 100 * Dem.M.1 / (Dem.M.1 + Dem.M.0),
		# second is among all men or all women
		# d.men.turnout = 100 * Dem.M.1 / (Dem.M.1 + Dem.M.0 + Rep.M.1 + Rep.M.0),
		# third is a denominator that measures all eligible men and women
		d.men.turnout.rate = 100 * d.men.voted / super.denom.init, #  = what pct of eligible electorate are men casting D votes?
		# r.men.turnout = 100 * Rep.M.1 / (Rep.M.1 + Rep.M.0),
		# r.men.turnout = 100 * Rep.M.1 / (Dem.M.1 + Dem.M.0 + Rep.M.1 + Rep.M.0),
		r.men.turnout.rate = 100 * r.men.voted / super.denom.init,
		net.men.turnout.init = d.men.turnout.rate - r.men.turnout.rate,
		# d.women.turnout = 100 * Dem.W.1 / (Dem.W.1 + Dem.W.0),
		# d.women.turnout = 100 * Dem.W.1 / (Dem.W.1 + Dem.W.0 + Rep.W.1 + Rep.W.0),
		d.women.turnout.rate = 100 * d.women.voted / super.denom.init,
		# r.women.turnout = 100 * Rep.W.1 / (Rep.W.1 + Rep.W.0),
		# r.women.turnout = 100 * Rep.W.1 / (Dem.W.1 + Dem.W.0 + Rep.W.1 + Rep.W.0),
		r.women.turnout.rate = 100 * r.women.voted / super.denom.init,

		net.women.turnout.init = d.women.turnout.rate - r.women.turnout.rate
	) %>%
	select(cycle, net.dem.voters.men, net.dem.voters.women, d.men.turnout.rate, r.men.turnout.rate, net.men.turnout.init, d.women.turnout.rate, r.women.turnout.rate, net.women.turnout.init, super.denom.init) %>%
	rename(
		net.dem.voters.men.init = net.dem.voters.men,
		net.dem.voters.women.init = net.dem.voters.women,
		dem.men.turnout.rate.init = d.men.turnout.rate,
		rep.men.turnout.rate.init = r.men.turnout.rate,
		dem.women.turnout.rate.init = d.women.turnout.rate,
		rep.women.turnout.rate.init = r.women.turnout.rate
	) %>%
	print



net.defection.init <-
	aggregate(wt ~ cycle + gender + pid.init +  voted.dem.amg.voters,
		data = anes
		# filter(anes, voted.maj.party==1)
		, FUN=sum
	) %>%
	filter(
		pid.init!="Other" & pid.init!="Ind"
	) %>%
	unite(
		party.gender.vote, pid.init, gender, voted.dem.amg.voters,
		sep="."
	) %>%
	spread(key = party.gender.vote, value = wt) %>%
	# 0 is a Rep vote, 1 is a Dem vote. Need to pay attention depending on partisanship of the voters to determine what "loyalty" and "defection" are
	rename(
		d.men.loyal = Dem.M.1,
		d.men.defect = Dem.M.0,
		r.men.loyal = Rep.M.0,
		r.men.defect = Rep.M.1,
		d.women.loyal = Dem.W.1,
		d.women.defect = Dem.W.0,
		r.women.loyal = Rep.W.0,
		r.women.defect = Rep.W.1
	) %>%
	mutate(
		men.sum.voters.init = d.men.defect + d.men.loyal + Dem.M.not.party.vote + r.men.defect + r.men.loyal + Rep.M.not.party.vote,
		women.sum.voters.init = d.women.defect + d.women.loyal + Dem.W.not.party.vote + r.women.defect + r.women.loyal + Rep.W.not.party.vote,
		all.sum.defection.init = men.sum.voters.init + women.sum.voters.init,
		# add super denominator
		# changed denominators to super.denom.init
		super.denom.init = net.turnout.init$super.denom.init,
		# dem.men.defection = 100 * d.men.defect / (d.men.defect + d.men.loyal),
		# dem.men.defection = 100 * d.men.defect / (d.men.defect + d.men.loyal + r.men.loyal + r.men.defect),
		dem.men.defection.rate = 100 * d.men.defect / (all.sum.defection.init),
		# rep.men.defection = 100 * r.men.defect / (r.men.defect + r.men.loyal),
		# rep.men.defection = 100 * r.men.defect / (d.men.defect + d.men.loyal + r.men.loyal + r.men.defect),
		rep.men.defection.rate = 100 * r.men.defect / (all.sum.defection.init),
		net.men.defection.init = rep.men.defection.rate - dem.men.defection.rate,
		net.votes.defection.men = r.men.defect - d.men.defect,
		# dem.women.defection = 100 * d.women.defect / (d.women.defect + d.women.loyal),
		# dem.women.defection = 100 * d.women.defect / (d.women.defect + d.women.loyal + r.women.defect + r.women.loyal),
		dem.women.defection.rate = 100 * d.women.defect / (all.sum.defection.init),
		# rep.women.defection = 100 * r.women.defect / (r.women.defect + r.women.loyal),
		# rep.women.defection = 100 * r.women.defect / (d.women.defect + d.women.loyal + r.women.defect + r.women.loyal),
		rep.women.defection.rate = 100 * r.women.defect / (all.sum.defection.init),
		net.votes.defection.women = r.women.defect - d.women.defect,
		net.women.defection.init = rep.women.defection.rate - dem.women.defection.rate
	) %>%
	select(cycle, dem.men.defection.rate, rep.men.defection.rate, dem.women.defection.rate, rep.women.defection.rate, net.votes.defection.men, net.votes.defection.women,
		net.men.defection.init, net.women.defection.init, super.denom.init, all.sum.defection.init) %>%
	rename(
		dem.men.defection.rate.init = dem.men.defection.rate,
		rep.men.defection.rate.init = rep.men.defection.rate,
		dem.women.defection.rate.init = dem.women.defection.rate,
		rep.women.defection.rate.init = rep.women.defection.rate,
		net.votes.defection.men.init = net.votes.defection.men,
		net.votes.defection.women.init = net.votes.defection.women
	) %>%
	print



# n.d.i.old <- net.defection.init




net.party.init <-
	aggregate(wt ~ gender + pid.init + cycle,
		data = anes, FUN=sum
	) %>%
	filter(
		pid.init!="Ind" & pid.init!="Other"
	) %>%
	unite(pid.init.gender, pid.init, gender, sep=".") %>%
	spread(key = pid.init.gender, value = wt) %>%
	mutate(
		all.sum.pid.init = Dem.M + Dem.W + Rep.M + Rep.W,
		net.dem.id.men = Dem.M - Rep.M,
		net.dem.id.women = Dem.W - Rep.W,
		d.men.pid.rate.init = (100 * Dem.M) / all.sum.pid.init,
		r.men.pid.rate.init = (100 * Rep.M) / all.sum.pid.init,
		d.women.pid.rate.init = (100 * Dem.W) / all.sum.pid.init,
		r.women.pid.rate.init = (100 * Rep.W) / all.sum.pid.init,
		net.men.pid.rate.init = d.men.pid.rate.init - r.men.pid.rate.init,
		net.women.pid.rate.init = d.women.pid.rate.init - r.women.pid.rate.init
	) %>%
	select(-(Dem.M:Rep.W), -all.sum.pid.init) %>%
	rename(
		net.dem.id.men.init = net.dem.id.men,
		net.dem.id.women.init = net.dem.id.women
	) %>%
	print









########################################
# Net measures, Leaners as partisans
########################################




# Initial partisanship, turnout and defection
net.turnout.sorted <-
	aggregate(wt ~ gender + voted + pid.sorted + cycle,
		data = filter(anes), FUN=sum
	) %>%
	filter(
		pid.sorted!="Other" & pid.sorted!="Ind"
	) %>%
	unite(party.gender.voted, pid.sorted, gender, voted, sep=".") %>%
	spread(key = party.gender.voted, value = wt) %>%
	rename(
		d.men.voted = Dem.M.1,
		d.women.voted = Dem.W.1,
		r.men.voted = Rep.M.1,
		r.women.voted = Rep.W.1
	) %>%
	mutate(
		# net votes for Democrats among men and women
		net.dem.voters.men = d.men.voted - r.men.voted,
		net.dem.voters.women = d.women.voted - r.women.voted,
		# super-denominator: all eligible men and women in the electorate
		super.denom.sorted = (d.men.voted + Dem.M.0 + r.men.voted + Rep.M.0 + d.women.voted + Dem.W.0 + r.women.voted + Rep.W.0),
		# first is among partisans only
		# d.men.turnout = 100 * Dem.M.1 / (Dem.M.1 + Dem.M.0),
		# second is among all men or all women
		# d.men.turnout = 100 * Dem.M.1 / (Dem.M.1 + Dem.M.0 + Rep.M.1 + Rep.M.0),
		# third is a denominator that measures all eligible men and women
		d.men.turnout.rate = 100 * d.men.voted / super.denom.sorted, #  = what pct of eligible electorate are men casting D votes?
		# r.men.turnout = 100 * Rep.M.1 / (Rep.M.1 + Rep.M.0),
		# r.men.turnout = 100 * Rep.M.1 / (Dem.M.1 + Dem.M.0 + Rep.M.1 + Rep.M.0),
		r.men.turnout.rate = 100 * r.men.voted / super.denom.sorted,
		net.men.turnout.sorted = d.men.turnout.rate - r.men.turnout.rate,
		# d.women.turnout = 100 * Dem.W.1 / (Dem.W.1 + Dem.W.0),
		# d.women.turnout = 100 * Dem.W.1 / (Dem.W.1 + Dem.W.0 + Rep.W.1 + Rep.W.0),
		d.women.turnout.rate = 100 * d.women.voted / super.denom.sorted,
		# r.women.turnout = 100 * Rep.W.1 / (Rep.W.1 + Rep.W.0),
		# r.women.turnout = 100 * Rep.W.1 / (Dem.W.1 + Dem.W.0 + Rep.W.1 + Rep.W.0),
		r.women.turnout.rate = 100 * r.women.voted / super.denom.sorted,

		net.women.turnout.sorted = d.women.turnout.rate - r.women.turnout.rate
	) %>%
	select(cycle, net.dem.voters.men, net.dem.voters.women, d.men.turnout.rate, r.men.turnout.rate, net.men.turnout.sorted, d.women.turnout.rate, r.women.turnout.rate, net.women.turnout.sorted, super.denom.sorted) %>%
	rename(
		net.dem.voters.men.sorted = net.dem.voters.men,
		net.dem.voters.women.sorted = net.dem.voters.women,
		dem.men.turnout.rate.sorted = d.men.turnout.rate,
		rep.men.turnout.rate.sorted = r.men.turnout.rate,
		dem.women.turnout.rate.sorted = d.women.turnout.rate,
		rep.women.turnout.rate.sorted = r.women.turnout.rate
	) %>%
	print



net.defection.sorted <-
	aggregate(wt ~ cycle + gender + pid.sorted +  voted.dem.amg.voters,
		data = anes
		# filter(anes, voted.maj.party==1)
		, FUN=sum
	) %>%
	filter(
		pid.sorted!="Other" & pid.sorted!="Ind"
	) %>%
	unite(
		party.gender.vote, pid.sorted, gender, voted.dem.amg.voters,
		sep="."
	) %>%
	spread(key = party.gender.vote, value = wt) %>%
	# 0 is a Rep vote, 1 is a Dem vote. Need to pay attention depending on partisanship of the voters to determine what "loyalty" and "defection" are
	rename(
		d.men.loyal = Dem.M.1,
		d.men.defect = Dem.M.0,
		r.men.loyal = Rep.M.0,
		r.men.defect = Rep.M.1,
		d.women.loyal = Dem.W.1,
		d.women.defect = Dem.W.0,
		r.women.loyal = Rep.W.0,
		r.women.defect = Rep.W.1
	) %>%
	mutate(
		men.sum.voters.sorted = d.men.defect + d.men.loyal + Dem.M.not.party.vote + r.men.defect + r.men.loyal + Rep.M.not.party.vote,
		women.sum.voters.sorted = d.women.defect + d.women.loyal + Dem.W.not.party.vote + r.women.defect + r.women.loyal + Rep.W.not.party.vote,
		all.sum.defection.sorted = men.sum.voters.sorted + women.sum.voters.sorted,
		# add super.denom.sorted and change denominators to it
		super.denom.sorted = net.turnout.sorted$super.denom.sorted,
		# dem.men.defection = 100 * d.men.defect / (d.men.defect + d.men.loyal),
		# dem.men.defection = 100 * d.men.defect / (d.men.defect + d.men.loyal + r.men.loyal + r.men.defect),
		dem.men.defection.rate = 100 * d.men.defect / (super.denom.sorted),
		# rep.men.defection = 100 * r.men.defect / (r.men.defect + r.men.loyal),
		# rep.men.defection = 100 * r.men.defect / (d.men.defect + d.men.loyal + r.men.loyal + r.men.defect),
		rep.men.defection.rate = 100 * r.men.defect / (super.denom.sorted),
		net.men.defection.sorted = rep.men.defection.rate - dem.men.defection.rate,
		net.votes.defection.men = r.men.defect - d.men.defect,
		# dem.women.defection = 100 * d.women.defect / (d.women.defect + d.women.loyal),
		# dem.women.defection = 100 * d.women.defect / (d.women.defect + d.women.loyal + r.women.defect + r.women.loyal),
		dem.women.defection.rate = 100 * d.women.defect / (super.denom.sorted),
		# rep.women.defection = 100 * r.women.defect / (r.women.defect + r.women.loyal),
		# rep.women.defection = 100 * r.women.defect / (d.women.defect + d.women.loyal + r.women.defect + r.women.loyal),
		rep.women.defection.rate = 100 * r.women.defect / (super.denom.sorted),
		net.votes.defection.women = r.women.defect - d.women.defect,
		net.women.defection.sorted = rep.women.defection.rate - dem.women.defection.rate
	) %>%
	select(cycle, dem.men.defection.rate, rep.men.defection.rate, dem.women.defection.rate, rep.women.defection.rate, net.votes.defection.men, net.votes.defection.women,
		net.men.defection.sorted, net.women.defection.sorted, super.denom.sorted) %>%
	rename(
		dem.men.defection.rate.sorted = dem.men.defection.rate,
		rep.men.defection.rate.sorted = rep.men.defection.rate,
		dem.women.defection.rate.sorted = dem.women.defection.rate,
		rep.women.defection.rate.sorted = rep.women.defection.rate,
		net.votes.defection.men.sorted = net.votes.defection.men,
		net.votes.defection.women.sorted = net.votes.defection.women
	) %>%
	print



net.party.sorted <-
	aggregate(wt ~ gender + pid.sorted + cycle,
		data = anes, FUN=sum
	) %>%
	filter(
		pid.sorted!="Ind" & pid.sorted!="Other"
	) %>%
	unite(pid.sorted.gender, pid.sorted, gender, sep=".") %>%
	spread(key = pid.sorted.gender, value = wt) %>%
	mutate(
		all.sum.pid.sorted = Dem.M + Dem.W + Rep.M + Rep.W,
		net.dem.id.men = Dem.M - Rep.M,
		net.dem.id.women = Dem.W - Rep.W,
		d.men.pid.rate.sorted = (100 * Dem.M) / all.sum.pid.sorted,
		r.men.pid.rate.sorted = (100 * Rep.M) / all.sum.pid.sorted,
		d.women.pid.rate.sorted = (100 * Dem.W) / all.sum.pid.sorted,
		r.women.pid.rate.sorted = (100 * Rep.W) / all.sum.pid.sorted,
		net.men.pid.rate.sorted = d.men.pid.rate.sorted - r.men.pid.rate.sorted,
		net.women.pid.rate.sorted = d.women.pid.rate.sorted - r.women.pid.rate.sorted
	) %>%
	select(-(Dem.M:Rep.W), -all.sum.pid.sorted) %>%
	rename(
		net.dem.id.men.sorted = net.dem.id.men,
		net.dem.id.women.sorted = net.dem.id.women
	) %>%
	print







########################################
# Measure net votes in the same metric
########################################


{
net.votes <-
	aggregate(wt ~ gender + cycle + voted.dem,
		data = filter(anes, voted.maj.party == 1),
		FUN = sum
	) %>%
	unite(gender.dvotes, gender, voted.dem, sep=".") %>%
	spread(key = gender.dvotes, value = wt) %>%
	mutate(
		super.denom = net.turnout.init$super.denom.init,
		# dem.votes.men = (100 * M.1) / super.denom,
		# rep.votes.men = (100 * M.0) / super.denom,
		# net.dem.votes.men = dem.votes.men - rep.votes.men
		net.dem.votes.men = (100 * (M.1 - M.0)) / super.denom,
		net.dem.votes.women = (100 * (W.1 - W.0)) / super.denom,
		n.majparty.voters = M.0 + M.1 + W.0 + W.1,
		n.majparty.men.voters = M.0 + M.1,
		n.majparty.women.voters = W.1 + W.0,
		net.gap = net.dem.votes.women - net.dem.votes.men,
		net.vote = net.dem.votes.men + net.dem.votes.women
	) %>%
	# select(cycle, net.dem.votes.men:n.majparty.women.voters) %>%
	print
}




# make into data.frame
df.net.reg <-
	merge(net.turnout.init, net.defection.init, by="cycle") %>%
	merge(., net.party.init, by="cycle") %>%
	merge(., net.turnout.sorted, by="cycle") %>%
	merge(., net.defection.sorted, by="cycle") %>%
	merge(., net.party.sorted, by = "cycle") %>%
	merge(., scatter.tab, by = "cycle") %>%
	merge(., net.votes, by = "cycle") %>%
	select(-Era) %>%
	print

names(df.net.reg)

df.net.reg


library(stargazer)


stargazer(select(df.net.reg, cycle, dem.men.turnout.rate.init, rep.men.turnout.rate.init,  dem.women.turnout.rate.init, rep.women.turnout.rate.init,  dem.men.defection.rate.init, rep.men.defection.rate.init, dem.women.defection.rate.init, rep.women.defection.rate.init, net.men.turnout.init, net.women.turnout.init, net.men.defection.init, net.women.defection.init),
	digits = 1,
	summary = FALSE,
	title = "Mobilization and Persuasion Measures (Leaners as Independents)",
	label = "descriptives-ind-leaners",
	type = "text"
	# , out="tex/tables/descriptives-leaners-as-ind.tex"
	)




stargazer(select(df.net.reg, cycle, dem.men.turnout.rate.sorted, rep.men.turnout.rate.sorted,  dem.women.turnout.rate.sorted, rep.women.turnout.rate.sorted,  dem.men.defection.rate.sorted, rep.men.defection.rate.sorted, dem.women.defection.rate.sorted, rep.women.defection.rate.sorted, net.men.turnout.sorted, net.women.turnout.sorted, net.men.defection.sorted, net.women.defection.sorted),
	digits = 1,
	summary = FALSE,
	title = "Mobilization and Persuasion Measures (Leaners as Partisans)",
	label = "descriptives-ind-partisans",
	type = "text"
	# , out="tex/tables/descriptives-leaners-as-partisans.tex"
	)







########################################
# Plot the net predictors
########################################

plot.net.vars <-
	df.net.reg %>%
	gather(key = predictor, value = net.val, net.men.turnout.init, net.men.turnout.sorted, net.women.turnout.init, net.women.turnout.sorted, net.men.defection.init, net.men.defection.sorted, net.women.defection.init, net.women.defection.sorted, net.men.pid.rate.init, net.men.pid.rate.sorted, net.women.pid.rate.init, net.women.pid.rate.sorted) %>%
	mutate(
		leaners =
			sapply(
				strsplit(predictor, split="[.]") ,
				function(x) x[length(x)]
				),
		leaners = ifelse(leaners=="init", "Leaners as\nIndependents", "Leaners as\nPartisans"),
		mechanism =
			sapply(
				strsplit(predictor, split="[.]") ,
				function(x) x[3]
				),
		mechanism = ifelse(
			mechanism=="turnout", "Mobilization", ifelse(mechanism=="defection", "Persuasion", "Party ID")),
		mechanism = factor(mechanism, levels = c("Party ID", "Mobilization", "Persuasion")),
		gender =
			sapply(
				strsplit(predictor, split="[.]") ,
				function(x) x[2]
				),
		gender = ifelse(gender=="women", "Women", "Men")
	) %>%
	print




{
ggplot(data =
	filter(
		plot.net.vars
		, mechanism!="Party ID")
	, aes(x = cycle, y = net.val)) +


	facet_grid(leaners ~ mechanism) +
	geom_hline(yintercept = 0, color = "gray50") +

	geom_line(aes(color = gender), size = 1) +

	scale_color_manual(values = rev(wes_palette("Darjeeling", 5))) +

	scale_x_continuous(breaks = seq(1952, 2012, 8)) +

	theme(
		axis.text.x = element_text(angle = 45, vjust=0.75),
		legend.position = "bottom"
		) +


	labs(x = "Election Cycle", y = "Net Democratic Advantage\n(% of Eligible Electorate)", color = NULL)
}


ggsave("tex/graphics/net-predictors-over-time.pdf", height = 5, width = 7)

embed_fonts("tex/graphics/net-predictors-over-time.pdf")




















# Init and sorted Gender Gap models

# init
gap.reg.init <-
	lm(gender.gap ~ net.men.turnout.init + net.women.turnout.init
		+ net.men.defection.init
		+ net.women.defection.init
		# + net.men.pid.rate.init
		# + net.women.pid.rate.init
		,
		data = df.net.reg)
summary(gap.reg.init)

plot(gap.reg.init)


# coefplot(gap.reg.init,
# 	intercept = FALSE,
# 	lwdOuter = 0.5,
# 	title = "Gender Gap,\nLeaners as Independents") +
# 	theme_mingd()

# coefplot() function currently broken.







# sorted
gap.reg.sorted <-
	lm(gender.gap ~ net.men.turnout.sorted + net.women.turnout.sorted
		+ net.men.defection.sorted
		+ net.women.defection.sorted
		# + net.men.pid.rate.sorted
		# + net.women.pid.rate.sorted
		,
		data = df.net.reg)
summary(gap.reg.sorted)

plot(gap.reg.sorted)


# coefplot(gap.reg.sorted,
# 	intercept = FALSE,
# 	lwdOuter = 0.5,
# 	title = "Gender Gap,\nLeaners as Partisans") +
# 	theme_mingd()


# plot predictions against actual gap
df.net.reg <-
	df.net.reg %>%
	mutate(
		gap.hat.init = predict(gap.reg.init),
		gap.hat.sorted = predict(gap.reg.sorted)
	) %>%
	print

gap.reg.long <-
	select(df.net.reg, cycle, gender.gap, gap.hat.init, gap.hat.sorted) %>%
	gather(key = series, value = gap.size, gender.gap, gap.hat.init, gap.hat.sorted) %>%
	mutate(
		series = ifelse(series=="gender.gap", "Observed Gender Gap", series),
		series = ifelse(series=="gap.hat.init", "Predicted (Independent Leaners)", series),
		series = ifelse(series=="gap.hat.sorted", "Predicted (Partisan Leaners)", series)
	) %>%
	print




{
# plot predictions of gender gap
ggplot(data = gap.reg.long, aes(x = cycle, y = gap.size)) +

	theme_mbws() +
	geom_hline(yintercept = 0, color = "gray50") +

	geom_line(aes(color = series), size = 1) +

	# scale_color_manual(values = rev(wes_palette("Rushmore", 5))) +

	# scale_color_brewer(palette = "Set1", name = NULL) +

	labs(x = "Election Cycle", y = "Gender Gap", color = NULL,
		title = "Stage 1: Gender Gap") +

	theme(legend.position = "right") +

	scale_x_continuous(breaks = seq(1952, 2012, 12))

}



ggsave("tex/graphics/predicted-gap-series.pdf", height = 3, width = 6)

embed_fonts("tex/graphics/predicted-gap-series.pdf")




cor(select(df.net.reg, -cycle))

library(corrplot)

pdf("tex/graphics/init-corr-matrix.pdf", height=10, width=10)
# par(family = "mono")

corrplot(
	cor(select(df.net.reg, net.men.turnout.init:net.women.pid.init)),
	method="number",
	type="upper",
	# diag = FALSE,
	# order="hclust",
	tl.col = "gray50")

dev.off()



pdf("tex/graphics/sorted-corr-matrix.pdf", height=10, width=10)
# par(family = "mono")

corrplot(
	cor(select(df.net.reg, -cycle, -(net.men.turnout.init:net.women.defection.init))),
	method="number",
	type="upper",
	# diag = FALSE,
	# order="hclust",
	tl.col = "gray50")

dev.off()








# Init and Sorted Dem. vote models
dvote.reg.init <-
	lm(pct.dem.pop.vote ~ net.men.turnout.init + net.women.turnout.init
		+ net.men.defection.init
		+ net.women.defection.init
		# + net.men.pid.rate.init
		# + net.women.pid.rate.init
		,
		data = df.net.reg)

summary(dvote.reg.init)

plot(dvote.reg.init)


# coefplot(dvote.reg.init,
# 	intercept = FALSE,
# 	lwdOuter = 0.5,
# 	title = "Democratic Vote,\nLeaners as Independents") +
# 	theme_mingd()





# Init and Sorted Dem. vote models
dvote.reg.sorted <-
	lm(pct.dem.pop.vote ~ net.men.turnout.sorted
		+ net.women.turnout.sorted
		+ net.men.defection.sorted
		+ net.women.defection.sorted
		# + net.men.pid.rate.sorted
		# + net.women.pid.rate.sorted
		,
		data = df.net.reg)
summary(dvote.reg.sorted)

plot(dvote.reg.sorted)


# coefplot(dvote.reg.sorted,
# 	intercept = FALSE,
# 	lwdOuter = 0.5,
# 	title = "Democratic Vote,\nLeaners as Partisans") +
# 	theme_mingd()





########################################
# Fix these into two panels in one fig??
########################################

ggplot(df.net.reg, aes(x = net.men.defection.init, y = net.women.defection.init)) +
	geom_abline(slope = 1, intercept = 0) +
	geom_vline(xintercept = 0, color = "gray") +
	geom_hline(yintercept = 0, color = "gray") +
	geom_point() +
	# theme_m38() +
	coord_cartesian(ylim = c(-15, 5), xlim = c(-15, 5))
ggsave("tex/graphics/defection-comparison-init.pdf")


ggplot(df.net.reg, aes(x = net.men.defection.sorted, y = net.women.defection.sorted)) +
	geom_abline(slope = 1, intercept = 0) +
	geom_vline(xintercept = 0, color = "gray") +
	geom_hline(yintercept = 0, color = "gray") +
	geom_point() +
	# theme_m38() +
	coord_cartesian(ylim = c(-15, 5), xlim = c(-15, 5))
ggsave("tex/graphics/defection-comparison-sorted.pdf")



summary(gap.reg.init)
summary(gap.reg.sorted)
summary(dvote.reg.init)
summary(dvote.reg.sorted)



# plot predictions against actual gap
df.net.reg <-
	df.net.reg %>%
	mutate(
		dvote.hat.init = predict(dvote.reg.init),
		dvote.hat.sorted = predict(dvote.reg.sorted)
	) %>%
	print


dvote.reg.long <-
	select(df.net.reg, cycle, pct.dem.pop.vote, dvote.hat.init, dvote.hat.sorted) %>%
	gather(key = series, value = dvote, pct.dem.pop.vote, dvote.hat.init, dvote.hat.sorted) %>%
	mutate(
		series = ifelse(series == "pct.dem.pop.vote", "Observed Democratic Vote", ifelse(series == "dvote.hat.init", "Predicted Vote, Leaners as Independents", "Predicted Vote, Leaners as Partisans"))
	)
	print



########################################
# Model of Dem vote, NOT stage 2. Not going through the gender gap
########################################


{
ggplot(data = dvote.reg.long, aes(x = cycle, y = dvote)) +

	theme_mbws() +

	geom_hline(yintercept = 50, color = "gray50") +

	geom_line(aes(color = series)) +

	labs(y = "Democratic Vote Share", x = "Election Cycle", color = NULL) +

	theme(legend.position = "right") +

	scale_x_continuous(breaks = seq(1952, 2012, 12))

}


ggsave("tex/graphics/predicted-d-vote-series.pdf", height = 3, width = 6)

embed_fonts("tex/graphics/predicted-d-vote-series.pdf")

	# theme_mbws() +
	# geom_hline(yintercept = 0, color = "gray50") +

	# geom_line(aes(color = series)) +

	# # scale_color_brewer(palette = "Set1", name = NULL) +

	# labs(x = "Election Cycle", y = "Gender Gap", color = NULL) +

	# theme(legend.position = "right") +

	# scale_x_continuous(breaks = seq(1952, 2012, 12))




net.degrees <- gap.reg.init$df.residual
alpha <- 0.05 # for getting critical values of T below

broomed.net <-
	rbind(
		cbind(
			tidy(gap.reg.init),
			dv = "Percentage Point Impact\non Gender Gap",
			leaners = "Leaners as Independents"),
		cbind(tidy(gap.reg.sorted),
			dv = "Percentage Point Impact\non Gender Gap",
			leaners = "Leaners as Partisans"),
		cbind(
			tidy(dvote.reg.init),
			dv = "Percentage Point Impact\non Democratic Vote",
			leaners = "Leaners as Independents"),
		cbind(
			tidy(dvote.reg.sorted),
			dv = "Percentage Point Impact\non Democratic Vote",
			leaners = "Leaners as Partisans")
	) %>%
	mutate(
		# better covariate labels
		predictor = "Blank",
		predictor = ifelse(term=="(Intercept)", "Intercept", predictor),
		predictor = ifelse(term=="net.men.turnout.init" | term=="net.men.turnout.sorted", "Net Dem. Turnout\n(Men)", predictor),
		predictor = ifelse(term=="net.women.turnout.init" | term=="net.women.turnout.sorted", "Net Dem. Turnout\n(Women)", predictor),
		predictor = ifelse(term=="net.men.defection.init" | term=="net.men.defection.sorted", "Net Dem. Defection\n(Men)", predictor),
		predictor = ifelse(term=="net.women.defection.init" | term=="net.women.defection.sorted", "Net Dem. Defection\n(Women)", predictor),
		predictor = ifelse(term=="net.men.pid.rate.init" | term=="net.men.pid.rate.sorted", "Net Dem. Party ID\n(Men)", predictor),
		predictor = ifelse(term=="net.women.pid.rate.init" | term=="net.women.pid.rate.sorted", "Net Dem. Party ID\n(Women)", predictor),
		# lower and upper bounds for 95% CIs
		# critical values of T done with alpha levels dynamically
		estimate.lower = (statistic - qt(1 - (alpha/2), df = net.degrees)) * std.error,
		estimate.upper = (statistic + qt(1 - (alpha/2), df = net.degrees)) * std.error
	) %>%
	print


{
ggplot(data = filter(broomed.net, predictor!="Intercept"),
	aes(x = predictor, y = estimate, ymin = estimate.lower, ymax = estimate.upper)) +



	facet_wrap( ~ dv) +

	geom_hline(yintercept = 0, color = "gray25") +

	geom_pointrange(
		aes(shape = leaners),
		position=position_dodge(width = - 0.3),
		fatten = 2.5,
		color = "gray25"
		) +


	coord_flip(ylim=c(-7,7)) +
	scale_y_continuous(breaks = seq(-6, 6, 2)) +
	labs(x = NULL, y = NULL, shape = NULL) +

	# theme_m38() +
	theme(legend.position="bottom") +

	theme(panel.border = element_rect(fill = NA, colour = "black"))
}

# ggsave("tex/graphics/net-regression-coefs.pdf", height = 3, width = 9)
ggsave("tex/graphics/net-regression-coefs-superdenominator.pdf", height = 4, width = 8)

embed_fonts("tex/graphics/net-regression-coefs-superdenominator.pdf")




########################################
# Predicted partial effects from gap model to predict democratic vote
########################################


# Leaners as independents
coefs.gap.init <- gap.reg.init$coefficients
names(coefs.gap.init)[1] <- "intercept"

gap.partials.init <-
	data.frame(
		cycle = df.net.reg$cycle,
		constant = coefs.gap.init["intercept"],
		# pid.women = df.net.reg$net.women.pid.rate.init * coefs.gap.init["net.women.pid.rate.init"],
		# pid.men = df.net.reg$net.men.pid.rate.init * coefs.gap.init["net.men.pid.rate.init"] ,
		mobilization.women =
			df.net.reg$net.women.turnout.init * coefs.gap.init["net.women.turnout.init"],
		mobilization.men =
			df.net.reg$net.men.turnout.init * coefs.gap.init["net.men.turnout.init"],
		persuasion.women =
			df.net.reg$net.women.defection.init * coefs.gap.init["net.women.defection.init"],
		persuasion.men =
			df.net.reg$net.men.defection.init * coefs.gap.init["net.men.defection.init"],
		dvote = df.net.reg$pct.dem.pop.vote
	) %>%
	mutate(
		total.mobilization = mobilization.women + mobilization.men,
		total.persuasion = persuasion.women + persuasion.men,
		total.campaign = total.mobilization + total.persuasion,
		leaners = "Leaners as Independents"
	) %>%
	print


# leaners as partisans
coefs.gap.sorted <- gap.reg.sorted$coefficients
names(coefs.gap.sorted)[1] <- "intercept"


gap.partials.sorted <-
	data.frame(
		cycle = df.net.reg$cycle,
		constant = coefs.gap.sorted["intercept"],
		# pid.women = df.net.reg$net.women.pid.rate.sorted * coefs.gap.sorted["net.women.pid.rate.sorted"],
		# pid.men = df.net.reg$net.men.pid.rate.sorted * coefs.gap.sorted["net.men.pid.rate.sorted"],
		mobilization.women =
			df.net.reg$net.women.turnout.sorted * coefs.gap.sorted["net.women.turnout.sorted"],
		mobilization.men =
			df.net.reg$net.men.turnout.sorted * coefs.gap.sorted["net.men.turnout.sorted"],
		persuasion.women =
			df.net.reg$net.women.defection.sorted * coefs.gap.sorted["net.women.defection.sorted"],
		persuasion.men =
			df.net.reg$net.men.defection.sorted * coefs.gap.sorted["net.men.defection.sorted"],
		dvote = df.net.reg$pct.dem.pop.vote
	) %>%
	mutate(
		total.mobilization = mobilization.women + mobilization.men,
		total.persuasion = persuasion.women + persuasion.men,
		total.campaign = total.mobilization + total.persuasion,
		leaners = "Leaners as Partisans"
	) %>%
	print


long.gap.impacts <-
	rbind(gap.partials.init, gap.partials.sorted) %>%
	gather(key = measure, value = fx.hat, mobilization.women, mobilization.men, persuasion.women, persuasion.men, total.mobilization, total.persuasion, total.campaign) %>%
	mutate(
		gender = ifelse(measure=="mobilization.women" | measure=="persuasion.women", "Women", ifelse(measure=="mobilization.men" | measure=="persuasion.men", "Men", NA)),
		concept = ifelse(measure=="mobilization.women" | measure=="mobilization.men", "Mobilization", ifelse(measure=="persuasion.women" | measure=="persuasion.men", "Persuasion", ifelse(measure=="total.mobilization", "Total Mobilization", ifelse(measure=="total.persuasion", "Total Persuasion", "Total Campaign")))),
		concept = factor(concept, levels = c("Mobilization", "Total Mobilization", "Persuasion", "Total Persuasion", "Total Campaign"))
	) %>%
	print


{
ggplot(data = long.gap.impacts, aes(x = cycle, y = fx.hat)) +

	facet_grid( leaners ~ concept) +
	geom_hline(yintercept = 0, color="gray50") +
	geom_line(aes(color = gender), size = 1) +

	# theme_mbws() +

	theme(legend.position="bottom",
		axis.text.x = element_text(angle = 45, vjust=0.75)
		) +
	labs(y = "Impact on Gender Gap\n(Percentage Points)", x = "Election Cycle", color = NULL)
}




ggsave("tex/graphics/partials-on-gender-gap.pdf", height = 5, width = 8)

embed_fonts("tex/graphics/partials-on-gender-gap.pdf")






########################################
# use partials to predict Dem. Vote
########################################


dvote.2s.init <- lm(dvote ~ mobilization.women + mobilization.men + persuasion.women + persuasion.men
	# + pid.women + pid.men
	, data = gap.partials.init)
summary(dvote.2s.init)


coefplot(dvote.2s.init,
	intercept = FALSE,
	lwdOuter = 0.5,
	title = "Partial Impact on Democratic Vote\n through the Gender Gap (Leaners as Independents)") +
	theme_mingd() +
	coord_cartesian(xlim = c(-1,1))



dvote.2s.sorted <- lm(dvote ~ mobilization.women + mobilization.men + persuasion.women + persuasion.men
	# + pid.women + pid.men
	, data = gap.partials.sorted)
summary(dvote.2s.sorted)


coefplot(dvote.2s.sorted,
	intercept = FALSE,
	lwdOuter = 0.5,
	outerCI = 2,
	title = "Partial Impact on Democratic Vote\nthrough the Gender Gap (Leaners as Partisans)") +
	theme_mingd() +
	coord_cartesian(xlim = c(-1,1))


df.2s <- dvote.2s.init$df.residual

broomed.2s <-
	rbind(
		cbind(tidy(dvote.2s.init), mod = "init"),
		cbind(tidy(dvote.2s.sorted), mod = "sorted")
	) %>%
	mutate(
		term = ifelse(term=="(Intercept)", "const", term),
		est.lower = (statistic - qt(.975, df = df.2s)) * std.error,
		est.upper = (statistic + qt(.975, df = df.2s)) * std.error
	) %>%
	print


########################################
# plot 2stage regression results
########################################


ggplot(data = filter(broomed.2s, term!="const"), aes(x = term, y = estimate, ymin = est.lower, ymax = est.upper)) +

	coord_flip() +
	geom_hline(yintercept = 0, color = "gray50") +
	geom_pointrange(
		aes(color = mod, fill = mod),
		position = position_dodge(width = -0.25),
		shape = 21
		) +

	scale_x_discrete(labels = c("Mobilization (Men)", "Mobilization (Women)", "Persuasion (Men)", "Persuasion (Women)")) +

	scale_fill_manual(
		values = c("gray25", "white"),
		labels = c("Independents", "Partisans")) +
	scale_color_manual(
		values = c("gray25", "gray25"),
		labels = c("Independents", "Partisans")) +

	# theme(legend.position="bottom") +

	labs(color = "Leaners Coded As", fill = "Leaners Coded As", y = "Percentage Pt. Impact on Democratic Vote\nThrough Gender Gap", x = NULL)


ggsave("tex/graphics/2sls-predicting-dvote.pdf", height = 4, width = 7)

ebed_fonts("tex/graphics/2sls-predicting-dvote.pdf")


names(broomed.net)
table(broomed.net$dv)
	# term, estimate, lower, upper, mod, leaners



tidy.stage1 <-
	filter(broomed.net, dv == "Percentage Point Impact\non Gender Gap" & term != "(Intercept)") %>%
	select(term, estimate, estimate.lower, estimate.upper, leaners) %>%
	mutate(
		term =
			ifelse(term=="net.men.turnout.init" | term=="net.men.turnout.sorted", "mobilization.men",
				ifelse(term=="net.women.turnout.init" | term=="net.women.turnout.sorted", "mobilization.women",
					ifelse(term=="net.men.defection.init" | term=="net.men.defection.sorted", "persuasion.men", "persuasion.women"
					)
				)
			),
		stage = "Stage 1: Gender Gap"
	) %>%
	print





names(broomed.2s)


tidy.stage2 <-
	select(broomed.2s, term, estimate, est.lower, est.upper, mod) %>%
	filter(term != "const") %>%
	rename(
		estimate.upper = est.upper,
		estimate.lower = est.lower
		) %>%
	mutate(
		leaners = ifelse(mod=="init", "Leaners as Independents", "Leaners as Partisans"),
		stage = "Stage 2: Democratic Vote"
	) %>%
	select(-mod) %>%
	print



tidy.tsls <-
	rbind(tidy.stage1, tidy.stage2)




{
ggplot(data = tidy.tsls, aes(x = term, y = estimate,
	ymin = estimate.lower, ymax = estimate.upper)) +

	facet_grid( . ~ stage, scales = "free_y") +

	coord_flip() +
	geom_hline(yintercept = 0, color = "gray50") +
	geom_point(aes(shape = leaners),
		position = position_dodge(width = - 0.5),
		color = "gray25",
		size = 1.75
		) +
	geom_linerange(
		aes(shape = leaners),
		position = position_dodge(width = -0.5),
		# shape = 21,
		color = "gray25",
		size = 0.75
		) +


	scale_x_discrete(labels = c("Mobilization (Men)", "Mobilization (Women)", "Persuasion (Men)", "Persuasion (Women)")) +

	scale_fill_manual(
		values = c("gray25", "white"),
		labels = c("Independents", "Partisans")) +
	scale_color_manual(
		values = c("gray25", "gray25"),
		labels = c("Independents", "Partisans")) +

	theme(legend.position = "bottom") +


	labs(shape = NULL,
		y = "Percentage Pt. Impact on Dependent Variable", x = NULL)
}

{
ggsave("tex/graphics/2sls-regression-results.pdf", height = 3.5, width = 8)

embed_fonts("tex/graphics/2sls-regression-results.pdf")
}




# ggplot(filter(broomed.mods, term!="(Intercept)"), aes(x = term, y = estimate, ymin = (statistic - qt(.975, df = degrees)) * std.error, ymax = (statistic + qt(.975, df = degrees)) * std.error)) +

# 	geom_hline(yintercept = 0, color = "gray50", size = 1) +

# 	geom_pointrange(aes(color = mod, fill = mod), position=position_dodge(width= -0.25), shape = 21) +

# 	coord_flip() +

# 	scale_x_discrete(labels = c("Dem. Party ID\n(Men)", "Dem. Party ID\n(Women)", "Turnout\n(Men)", "Turnout\n(Women)")) +

# 	labs(x = NULL, y = "Percentage Point Impact\non Gender Gap", color = "Leaners coded as", fill = "Leaners coded as") +

# 	scale_fill_manual(
# 		values = c("gray25", "white"),
# 		labels = c("Independents", "Partisans")) +
# 	scale_color_manual(
# 		values = c("gray25", "gray25"),
# 		labels = c("Independents", "Partisans")) +

# 	theme_m38()

# ggsave("tex/graphics/regression-coefs.pdf", height = 5, width = 7)







coefs.dvote.init <- coef(dvote.2s.init)[-1]
dvote.partials.init <-
	data.frame(
		leaners = "Leaners as Independents",
		cycle = df.net.reg$cycle,
		mobilization.women =
			gap.partials.init$mobilization.women * coefs.dvote.init["mobilization.women"],
		mobilization.men =
			gap.partials.init$mobilization.men * coefs.dvote.init["mobilization.men"],
			persuasion.women =
				gap.partials.init$persuasion.women * coefs.dvote.init["persuasion.women"],
			persuasion.men =
				gap.partials.init$persuasion.men * coefs.dvote.init["persuasion.men"]
	) %>%
	mutate(
		total.mobilization = mobilization.women + mobilization.men,
		total.persuasion = persuasion.women + persuasion.men,
		total.campaign = total.mobilization + total.persuasion
	) %>%
	print




coefs.dvote.sorted <- coef(dvote.2s.sorted)[-1]
dvote.partials.sorted <-
	data.frame(
		leaners = "Leaners as Partisans",
		cycle = df.net.reg$cycle,
		mobilization.women =
			gap.partials.sorted$mobilization.women * coefs.dvote.sorted["mobilization.women"],
		mobilization.men =
			gap.partials.sorted$mobilization.men * coefs.dvote.sorted["mobilization.men"],
			persuasion.women =
				gap.partials.sorted$persuasion.women * coefs.dvote.sorted["persuasion.women"],
			persuasion.men =
				gap.partials.sorted$persuasion.men * coefs.dvote.sorted["persuasion.men"]
	) %>%
	mutate(
		total.mobilization = mobilization.women + mobilization.men,
		total.persuasion = persuasion.women + persuasion.men,
		total.campaign = total.mobilization + total.persuasion
	) %>%
	print





long.vote.impacts <-
	rbind(dvote.partials.init, dvote.partials.sorted) %>%
	gather(key = measure, value = fx.hat, mobilization.women, mobilization.men, persuasion.women, persuasion.men, total.mobilization, total.persuasion, total.campaign) %>%
	mutate(
		gender = ifelse(measure=="mobilization.women" | measure=="persuasion.women", "Women", ifelse(measure=="mobilization.men" | measure=="persuasion.men", "Men", NA)),
		concept = ifelse(measure=="mobilization.women" | measure=="mobilization.men", "Mobilization", ifelse(measure=="persuasion.women" | measure=="persuasion.men", "Persuasion", ifelse(measure=="total.mobilization", "Total Mobilization", ifelse(measure=="total.persuasion", "Total Persuasion", "Total Campaign")))),
		concept = factor(concept, levels = c("Mobilization", "Total Mobilization", "Persuasion", "Total Persuasion", "Total Campaign"))
	) %>%
	print


########################################
# plot partial impacts on democratic vote over time
########################################

{
ggplot(data = long.vote.impacts, aes(x = cycle, y = fx.hat)) +
	facet_grid( leaners ~ concept) +
	geom_hline(yintercept = 0, color="gray50") +
	geom_line(aes(color = gender), size = 1) +

	theme_mbws() +

	theme(legend.position="bottom",
		axis.text.x = element_text(angle = 45, vjust=0.75)
		) +
	labs(y = "Impact on Democratic Vote\n(Percentage Points)", x = "Election Cycle", color = NULL)

}

ggsave("tex/graphics/partial-impacts-on-vote.pdf", height = 5, width = 8)

embed_fonts("tex/graphics/partial-impacts-on-vote.pdf")







########################################
# Predicted vs. observed vote (2-stage)
########################################

dvote.series <-
	df.net.reg %>%
	select(cycle, pct.dem.pop.vote) %>%
	mutate(
		dvote.hat.2s.init = predict(dvote.2s.init),
		dvote.hat.2s.sorted = predict(dvote.2s.sorted)
	) %>%
	gather(key = series, value = dvote, pct.dem.pop.vote, dvote.hat.2s.init, dvote.hat.2s.sorted) %>%
	mutate(
		series = ifelse(series == "pct.dem.pop.vote", "Observed Democratic Vote", ifelse(series == "dvote.hat.2s.init", "Predicted (Independent Leaners)", "Predicted (Partisan Leaners)"))
	) %>%
	print



{

ggplot(data = dvote.series, aes(x = cycle, y = dvote)) +

	theme_mbws() +

	geom_hline(yintercept = 50, color = "gray50") +

	geom_line(aes(color = series), size = 1) +

	labs(x = "Election Cycle", y = "Democratic Vote", color = NULL,
		title = "Stage 2: Democratic Vote Share") +

	theme(legend.position = "right") +

	scale_x_continuous(breaks = seq(1952, 2012, 12))

}


ggsave("tex/graphics/predicted-d-vote-stage-2-series.pdf", height = 3, width = 6)

embed_fonts("tex/graphics/predicted-d-vote-stage-2-series.pdf")

# {
# # plot predictions of gender gap
# ggplot(data = gap.reg.long, aes(x = cycle, y = gap.size)) +

# 	theme_mbws() +
# 	geom_hline(yintercept = 0, color = "gray50") +

# 	geom_line(aes(color = series)) +

# 	# scale_color_brewer(palette = "Set1", name = NULL) +

# 	labs(x = "Election Cycle", y = "Gender Gap", color = NULL) +

# 	theme(legend.position = "right") +

# 	scale_x_continuous(breaks = seq(1952, 2012, 12))

# }







########################################
# Try regression models with correction for party ID?
########################################


tbl_df(df.net.reg)
names(df.net.reg)


df.norm.reg <-
	df.net.reg %>%
	select(
		net.men.turnout.init, net.women.turnout.init,
		net.men.turnout.sorted, net.women.turnout.sorted,
		net.men.defection.init, net.women.defection.init,
		net.men.defection.sorted, net.women.defection.sorted,
		net.men.pid.rate.init, net.women.pid.rate.init,
		net.men.pid.rate.sorted, net.women.pid.rate.sorted,
		net.gap, net.vote
	) %>%
	mutate(
		gap.pid.init = net.women.pid.rate.init - net.men.pid.rate.init,
		gap.pid.sorted = net.women.pid.rate.sorted - net.men.pid.rate.sorted,
		sum.pid.init = net.women.pid.rate.init + net.men.pid.rate.init,
		sum.pid.sorted = net.women.pid.rate.sorted + net.men.pid.rate.sorted,
		mob.men.init = net.men.pid.rate.init - net.men.turnout.init,
		mob.women.init = net.women.pid.rate.init - net.women.turnout.init,
		mob.men.sorted = net.men.pid.rate.sorted - net.men.turnout.sorted,
		mob.women.sorted = net.women.pid.rate.sorted - net.women.turnout.sorted,
		gap.dev.init = net.gap - gap.pid.init,
		gap.dev.sorted = net.gap - gap.pid.sorted,
		vote.dev.init = net.vote - sum.pid.init,
		vote.dev.sorted = net.vote - sum.pid.sorted
	) %>%
	print








# # Init and Sorted Dem. vote models
# dvote.reg.init <-
# 	lm(pct.dem.pop.vote ~ net.men.turnout.init + net.women.turnout.init
# 		+ net.men.defection.init
# 		+ net.women.defection.init
# 		# + net.men.pid.rate.init
# 		# + net.women.pid.rate.init
# 		,
# 		data = df.net.reg)

# summary(dvote.reg.init)

# plot(dvote.reg.init)


# # coefplot(dvote.reg.init,
# # 	intercept = FALSE,
# # 	lwdOuter = 0.5,
# # 	title = "Democratic Vote,\nLeaners as Independents") +
# # 	theme_mingd()





# # Init and Sorted Dem. vote models
# dvote.reg.sorted <-
# 	lm(pct.dem.pop.vote ~ net.men.turnout.sorted
# 		+ net.women.turnout.sorted
# 		+ net.men.defection.sorted
# 		+ net.women.defection.sorted
# 		# + net.men.pid.rate.sorted
# 		# + net.women.pid.rate.sorted
# 		,
# 		data = df.net.reg)
# summary(dvote.reg.sorted)

# plot(dvote.reg.sorted)














########################################
# Is the stuff below broken?
########################################


# maybe







########################################
# Net measures over time?
########################################

df.net.long <-
	df.net.reg %>%
	gather(key = var, value = net.value, net.men.turnout.init, net.men.turnout.sorted, net.women.turnout.init, net.women.turnout.sorted, net.men.defection.init, net.men.defection.sorted, net.women.defection.init, net.women.defection.sorted, net.men.pid.rate.init, net.men.pid.rate.sorted, net.women.pid.rate.init, net.women.pid.rate.sorted) %>%
	mutate(
		gender = ifelse(var == "net.men.turnout.init" | var == "net.men.turnout.sorted" | var == "net.men.defection.init" | var == "net.men.defection.sorted" | var == "net.men.pid.rate.init" | var == "net.men.pid.rate.sorted", "Men", "Women"),
		measure =
			ifelse(var == "net.men.turnout.init" | var == "net.men.turnout.sorted" | var == "net.women.turnout.init" | var == "net.women.turnout.sorted", "Mobilization",
					ifelse(var=="net.men.defection.init" | var == "net.men.defection.sorted" | var == "net.women.defection.init" | var=="net.women.defection.sorted", "Persuasion", "Partisanship")),
		measure = factor(measure, levels = c("Partisanship", "Mobilization", "Persuasion")),
		leaners = ifelse(var=="net.men.defection.init" | var == "net.women.defection.init" | var == "net.men.turnout.init" | var=="net.women.turnout.init" | var == "net.men.pid.rate.init" | var == "net.women.pid.rate.init", "Leaners as\nIndependents", "Leaners as\nPartisans"),
		vote.impact = ifelse(measure == "Persuasion", net.value*2, net.value)
	) %>%
	print



ggplot(data = df.net.long, aes(x = cycle, y = vote.impact)) +
	facet_grid( leaners ~ measure) +

	geom_hline(yintercept = 0, color = "gray25") +
	geom_line(aes(color = gender)) +

	theme_m38() +
	theme( legend.position = "bottom") +


	scale_color_manual(values = rev(wes_palette("Darjeeling", 5)), name = NULL) +

	labs(y = "Net Democratic Advantage", x = "Election Cycle", color = NULL)


ggsave("tex/graphics/net-grid.pdf", height = 4.5, width = 7)









########################################
# Net contribution to gender gap?
########################################

# mobilization + persuasion

df.net.net <-
	df.net.reg %>%
	mutate(
		net.partisanship.init = net.women.pid.rate.init - net.men.pid.rate.init,
		net.partisanship.sorted = net.women.pid.rate.sorted - net.men.pid.rate.sorted,
		net.mobilization.init = net.women.turnout.init - net.men.turnout.init,
		net.mobilization.sorted = net.women.turnout.sorted - net.men.turnout.sorted,
		net.persuasion.init = 2 * (net.women.defection.init - net.men.defection.init),
		net.persuasion.sorted = 2 * (net.women.defection.sorted - net.men.defection.sorted),
		gap.init = net.mobilization.init + net.persuasion.init,
		gap.sorted = net.mobilization.sorted + net.persuasion.sorted,
		mobilization.impact.init = net.mobilization.init - net.partisanship.init,
		mobilization.impact.sorted = net.mobilization.sorted - net.partisanship.sorted
		# ,
		# persuasion.impact.init = net.persuasion.init - net.mobilization.init,
		# persuasion.impact.sorted = net.persuasion.sorted - net.mobilization.sorted
	) %>%
	print



df.net.net.long <-
	df.net.net %>%
	gather(key = net.var, value = net.value, net.partisanship.init, net.partisanship.sorted, net.mobilization.init, net.mobilization.sorted, net.persuasion.init, net.persuasion.sorted) %>%
	mutate(
		measure =
			ifelse(net.var == "net.mobilization.init" | net.var == "net.mobilization.sorted", "Mobilization",
				ifelse(net.var=="net.partisanship.init" | net.var=="net.partisanship.sorted", "Partisanship", "Persuasion")),
		measure = factor(measure, levels = c("Partisanship", "Mobilization", "Persuasion")),
		leaners = ifelse(net.var == "net.mobilization.init" | net.var=="net.persuasion.init" | net.var=="net.partisanship.init", "Leaners as\nIndependents", "Leaners as\nPartisans")
	) %>%
	print



ggplot(df.net.net.long, aes(x = cycle, y = net.value)) +

	facet_grid(leaners ~ measure) +

	geom_hline(yintercept = 0, color = "gray25") +
	geom_line(color = "gray25", size = 1) +

	# geom_line(data = df.net.net, aes(x = cycle, y = gap.init)) +
	# geom_line(data = df.net.net, aes(x = cycle, y = gap.sorted)) +


	theme_m38() +

	labs(y = "Gender Gap (Women - Men)", x = "Election Cycle")




df.net.impact <-
	df.net.net %>%
	gather(key = mechanism, value = impact, mobilization.impact.init, mobilization.impact.sorted, net.persuasion.init, net.persuasion.sorted) %>%
	mutate(
		measure = ifelse(mechanism=="mobilization.impact.init" | mechanism == "mobilization.impact.sorted", "Mobilization", "Persuasion"),
		leaners = ifelse(mechanism=="mobilization.impact.init" | mechanism=="net.persuasion.init", "Leaners as\nIndependents", "Leaners as\nPartisans")
	) %>%
	print


ggplot(df.net.impact, aes(x = cycle, y = impact)) +

	facet_grid(leaners ~ measure) +

	geom_hline(yintercept = 0, color = "gray25") +

	geom_line(size = 1) +

	theme_m38()

ggsave("tex/graphics/net-impact-grid.pdf", height = 4, width = 7)












