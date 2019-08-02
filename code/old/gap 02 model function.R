#----------------------------------------
#		
# 	FUNCTION TO BUILD THE GENDER GAP MODEL:
# 	calculates the "workhorse model" plus 
# 	important peri- and post-estimation things
#
# 	1. accepts a party ID variable to 
# 		 make the routine generic for different 
# 		 codings of party ID
# 
# 	2. aggregates individual data into counts
# 		 of voters in each party.x.gender who make
# 		 different vote choices
# 
# 	3. uses these counts to measure partisanship,
# 		 mobilization, persuasion, and "other"
# 		 to create the right-hand data from the 
# 		 workhorse model, standardized for sample sizes
# 		 (`rhs` and `denoms` list items)
# 
# 	4. differences the workhose model across party 
# 		 to show the "net partisan impact" of each
# 		 sub-gap mechanism
# 		 (`partials` list item)
# 
#----------------------------------------

run.model <- function(data.table, party.variable) {

	# create generic party variable 
	# (& keep only non-missing vote data???)
	data.table <- 
		data.table %>%
		mutate(party.id = party.variable, 
		       party.id = ifelse(party.id %in% c("Ind", "Other"), 
		                         yes = "Other", 
		                         no = as.character(party.id))) %>%
		filter(got.vote.q == 1 & vote != "invalid")

	# aggregate "choices"
	agg.table <- 
		data.table %>%
		group_by(party.id, gender, vote, cycle) %>%
		summarize(n.cat = sum(wt)) %>%  # numerator
		group_by(cycle) %>%
		mutate(super.denom = sum(n.cat)) %>% # denominator
		ungroup() %>%
		mutate(prop.cat = n.cat / super.denom) # fraction

	# super denominator, will be useful later
	super.denom.tab <- 
		agg.table %>%
		group_by(cycle) %>%
		summarize(super.denom = unique(super.denom))

	# columns for each group of voters
	wide.table <- 
		agg.table %>% 
		unite(choice.cat, party.id, gender, vote, sep = ".") %>%
		select(choice.cat, cycle, n.cat) %>%
		spread(key = choice.cat, value = n.cat, fill = 0) %>%
		transmute(
			cycle = cycle,
			# party ID
			Dem.M.pid = Dem.M.dvote + 
									# Dem.M.invalid +
									Dem.M.nonvote + Dem.M.other +
									Dem.M.rvote,
			Dem.W.pid = Dem.W.dvote + 
									# Dem.W.invalid +
									Dem.W.nonvote + Dem.W.other +
									Dem.W.rvote,
			Rep.M.pid = Rep.M.dvote + 
									# Rep.M.invalid +
									Rep.M.nonvote + Rep.M.other +
									Rep.M.rvote,
			Rep.W.pid = Rep.W.dvote + 
									# Rep.W.invalid +
									Rep.W.nonvote + Rep.W.other +
									Rep.W.rvote,
			# party-loyal voters
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
			)

	# terms in model equation
	rhs.table <- 
		wide.table %>%
		transmute(
			cycle = cycle,
			# Democrats, partyID, mobilization, persuasion, & other
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
			# Republicans, partyID, mobilization, persuasion, & other
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
			# columns for loyalty, cuz they may be useful
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
			)	

	# compute partials
	# differencing over party, divide by superdenom
	partials.table <- 
		rhs.table %>%
		left_join(., super.denom.tab, by = "cycle") %>% 
		transmute(
			cycle = cycle,
			M.pid = 100 * (MD.pid - MR.pid) / super.denom,
			W.pid = 100 * (WD.pid - WR.pid) / super.denom,
			M.mob = 100 * (MD.mob - MR.mob) / super.denom,
			W.mob = 100 * (WD.mob - WR.mob) / super.denom,
			M.per = 100 * (MD.per - MR.per) / super.denom,
			W.per = 100 * (WD.per - WR.per) / super.denom,
			M.other = 100 * (MD.other - MR.other) / super.denom,
			W.other = 100 * (WD.other - WR.other) / super.denom,
			W.total = W.pid + W.mob + W.per + W.other,
			M.total = M.pid + M.mob + M.per + M.other,
			gap = W.total - M.total) %>%
		# elongate
		gather(key = Gender.Source, value = Impact, M.pid:M.total) %>%
		separate(Gender.Source, into = c("Gender", "Source")) %>%
		mutate(
			Source = ifelse(Source == "pid", "Partisanship",
							 ifelse(Source == "mob", "Mobilization",
							 ifelse(Source == "per", "Persuasion",
							 ifelse(Source == "other", "Unaffiliated", "Net Democratic Votes")))),
			Source = factor(Source, levels = c("Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Votes")))
		

	return(list(rhs = rhs.table, 
	            partials = partials.table, 
	            denoms = super.denom.tab))

}
