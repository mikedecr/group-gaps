rm(list=ls())

setwd("/Users/michaeldecrescenzo/Box Sync/Barry PA/PA Spring 2016")


library(tidyr)
library(dplyr)
library(ggplot2)
library(ggthemes)


# custom gg themes
source("/Users/michaeldecrescenzo/Box Sync/R learning/Custom/custom gg theme mgd.R")

# set default theme
theme_set(theme_mbws())

# load font faces to graphics device
library(extrafont)
# font_import()
loadfonts()

# custom Effects plotting package
source("/Users/michaeldecrescenzo/Box Sync/R learning/Custom/mgd-effects.R")
# confidence interval functions for vectors of successes and failures
source("/Users/michaeldecrescenzo/Box Sync/R learning/Custom/prop ci.R")

exit <- read.csv("Exit Poll All/exit poll 04-08.csv")


# making gap and change vars
names(exit)

exit <- exit%>%
	mutate(
		gender.gap08 = dvote.women08 - dvote.men08,
		gender.gap04 = dvote.women04 - dvote.men04,
		change.dshare = dshare08 - dshare04,
		change.dvote.men = dvote.men08 - dvote.men04,
		change.dvote.women = dvote.women08 - dvote.women04,
		change.rvote.men = rvote.men08 - rvote.men04,
		change.rvote.women = rvote.women08 - rvote.women04,
		change.gender.gap = gender.gap08 - gender.gap04
		) %>%
	print



exit <- mutate(exit, change.dvote.men = dvote.men08 - dvote.men04)
exit <- mutate(exit, change.dvote.women = dvote.women08 - dvote.women04)
exit <- mutate(exit, change.rvote.men = rvote.men08 - rvote.men04)
exit <- mutate(exit, change.rvote.women = rvote.women08 - rvote.women04)

# change in gender gap
exit <- mutate(exit, gender.gap08 = dvote.women08 - dvote.men08)
exit <- mutate(exit, gender.gap04 = dvote.women04 - dvote.men04)
exit <- mutate(exit, change.gender.gap = gender.gap08 - gender.gap04)

# change in vote shares
exit <- mutate(exit, change.dshare = dshare08 - dshare04)






########################################
# graph for 2008
########################################

# linear relationships
mod08 <- lm(exit$dshare08 ~ exit$gender.gap08)
summary(mod08)

cleaned.mod08 <- lm(exit$dshare08 ~ exit$gender.gap08, data=exit, subset=exit$state!="District of Columbia" & exit$state!="Hawaii")
summary(cleaned.mod08)

# correlations
cor(exit$dshare08, exit$gender.gap08)
cor(exit$dshare08[exit$state!="District of Columbia" & exit$state!="Hawaii"], exit$gender.gap08[exit$state!="District of Columbia" & exit$state!="Hawaii"])


# plot
plot(exit$dshare08 ~ exit$gender.gap08,
	xlab="Gender Gap, 2008",
	ylab="Democratic Share (% of Total Vote)",
	las=1)

abline(mod08, lwd=3)
abline(cleaned.mod08)

text(16, 52, "r = 0.14")
text(16, 59, "r = 0.35")

legend("topright",
	legend=c("All States", "Without Labeled Outliers"),
	lty=1,
	lwd=c(3,1))

# tag DC and Hawaii
identify(exit$dshare08 ~ exit$gender.gap08, labels=exit$state)






########################################
# 2004
########################################
# linear relationships
mod04 <- lm(exit$dshare04 ~ exit$gender.gap04)
summary(mod04)

cleaned.mod04 <- lm(exit$dshare04 ~ exit$gender.gap04, data=exit, subset=exit$state!="District of Columbia")
summary(cleaned.mod04)

# correlations
cor(exit$dshare04, exit$gender.gap04)
cor(exit$dshare04[exit$state!="District of Columbia"], exit$gender.gap04[exit$state!="District of Columbia"])


# plot
plot(exit$dshare04 ~ exit$gender.gap04,
	xlab="Gender Gap, 2004",
	ylab="Democratic Share (% of Total Vote)",
	las=1)

abline(mod04, lwd=3)
abline(cleaned.mod04)

text(15, 55, "r = 0.16")
text(15, 45, "r = 0.17")

legend("topright",
	legend=c("All States", "Without Labeled Outliers"),
	lty=1,
	lwd=c(3,1))

# tag DC only
identify(exit$dshare04 ~ exit$gender.gap04, labels=exit$state)




########################################
# change 04 to 08
########################################
# linear relationships
change.mod <- lm(exit$change.dshare ~ exit$change.gender.gap)
summary(change.mod)

cleaned.change.mod <- lm(exit$change.dshare ~ exit$change.gender.gap, data=exit, subset=exit$state!="Arkansas" & exit$state!="Hawaii")
summary(cleaned.change.mod)

# correlations
cor(exit$change.dshare, exit$change.gender.gap)
cor(exit$change.dshare[exit$state!="Arkansas" & exit$state!="Hawaii"], exit$change.gender.gap[exit$state!="Arkansas" & exit$state!="Hawaii"])

# plot
par(mar=c(5,5,3,1))
plot(exit$change.dshare ~ exit$change.gender.gap,
	xlab="Change in Gender Gap (2004 to 2008)",
	ylab="Change in Democratic Vote Share \n (2004 to 2008)")

abline(change.mod, lwd=3)
abline(cleaned.change.mod, col="red", lwd=2)
abline(v=0, h=0, lty=3)


text(15, 55, "r = 0.16")
text(15, 45, "r = 0.17")

legend("topright",
	legend=c("All States (r=.07)", "Without Labeled Outliers (r=.06)"),
	lty=1,
	lwd=c(3,2),
	col=c("black", "red"),
	bg="white")

# Tag Hawaii. Jury is out on AR?
identify(exit$change.dshare ~ exit$change.gender.gap, labels=exit$state)



head(exit)


long.exits <- read.csv("Exit Poll All/exit poll 04-08 long.csv")
head(long.exits)


(states <- cbind(state = state.name, abb = as.character(state.abb)))




long <-
	merge(long.exits, states, by = "state", all.x = TRUE) %>%
	mutate(
		gap = dvote.women - dvote.men,
		post = ifelse(state == "District of Columbia", "DC", as.character(abb))
	) %>%
	select(- abb) %>%
	print


str(long)
str(long.exits)
str(states)

pt.alpha <- 0.4
line.color <- "black"
axis.col <- "gray50"
rib.alpha <- 0.25



{
ggplot(data = long, aes(x = gap, y = dshare)) +
	geom_hline(yintercept = 50, color = axis.col) +
	geom_vline(xintercept = 0, color = axis.col) +

	facet_wrap( ~ cycle) +

	geom_point(alpha = pt.alpha, size = 2.5) +
	# geom_text(aes(label = post), size = 2.5) +
	geom_smooth(method = "lm", color = line.color, alpha = rib.alpha) +

	scale_y_continuous(breaks = seq(30, 90, 20)) +

	labs(x = "Gender Gap", y = "Democratic Vote Share")
	# +

	# theme_m38()
}


{
ggsave("tex/graphics/state-gender-gap-cross-sectional-scatters.pdf", height = 3, width = 6)

embed_fonts("tex/graphics/state-gender-gap-cross-sectional-scatters.pdf")

}





ggplot(data = exit, aes(x = change.gender.gap, y = change.dshare)) +
	geom_hline(yintercept = 0, color = axis.col) +
	geom_vline(xintercept = 0, color = axis.col) +

	geom_point(alpha = pt.alpha) +
	geom_smooth(method = "lm", color = line.color, alpha = rib.alpha) +

	labs(x = "Change in Gender Gap", y = "Change in Democratic Share")
	 # +

	# theme_m38()

	ggsave("tex/graphics/state-gender-gap-change-scatter.pdf", height = 4, width = 5)

	embed_fonts("tex/graphics/state-gender-gap-change-scatter.pdf")






