#----------------------------------------
#  
#  FUNCTION TO BUILD THE GENDER GAP MODEL:
#  calculates the "workhorse model" plus 
#  important peri- and post-estimation things
#
#  1. accepts a party ID variable to 
#    make the routine generic for different 
#    codings of party ID
# 
#  2. aggregates individual data into counts
#    of voters in each party.x1.gender who make
#    different vote choices
# 
#  3. uses these counts to measure partisanship,
#    mobilization, persuasion, and "other"
#    to create the right-hand data from the 
#    workhorse model, standardized for sample sizes
#    (`rhs` and `denoms` list items)
# 
#  4. differences the workhose model across party 
#    to show the "net partisan impact" of each
#    sub-gap mechanism
#    (`partials` list item)
# 
#----------------------------------------

# testing
# dat <- anes
# party_variable <- anes$pid_lean



run_model <- function(dat, party_variable = NA) {

 # create generic party variable 
 # (& keep only non-missing vote data???)

 dat <- 
  dat %>%
  mutate(party_id = party_variable, 
         party_id = ifelse(party_id %in% c("Ind", "Other"), 
                           yes = "Other", 
                           no = as.character(party_id))) %>%
  filter(!is.na(voted) == 1 & vote != "missing")


 # --- aggregate all possible vote choices -----------------------
 
 agg_table <- 
  dat %>%
  group_by(cycle, party_id, gender, vote) %>%
  summarize(n_cat = sum(wt, na.rm = TRUE)) %>%  # numerator
  group_by(cycle) %>%
  mutate(super_denom = sum(n_cat, na.rm = TRUE)) %>% # denominator
  ungroup() %>%
  mutate(prop_cat = n_cat / super_denom) # fraction


 # super denominator, will be useful later
 super_denom_tab <- 
  agg_table %>%
  group_by(cycle) %>%
  summarize(super_denom = unique(super_denom))



 # --- compute raw data -----------------------
 
 wide_table <- 
  agg_table %>% 
  unite(choice_cat, party_id, gender, vote, sep = "_") %>%
  select(choice_cat, cycle, n_cat) %>%
  spread(key = choice_cat, value = n_cat, fill = 0) %>%
  transmute(
   cycle = cycle,
   # party ID
   Dem_M_pid = Dem_M_dvote + 
         # Dem_M_missing +
         Dem_M_nonvote + Dem_M_other +
         Dem_M_rvote,
   Dem_W_pid = Dem_W_dvote + 
         # Dem_W_missing +
         Dem_W_nonvote + Dem_W_other +
         Dem_W_rvote,
   Rep_M_pid = Rep_M_dvote + 
         # Rep_M_missing +
         Rep_M_nonvote + Rep_M_other +
         Rep_M_rvote,
   Rep_W_pid = Rep_W_dvote + 
         # Rep_W_missing +
         Rep_W_nonvote + Rep_W_other +
         Rep_W_rvote,
   # party-loyal voters
   Dem_M_loyal = Dem_M_dvote,
   Dem_W_loyal = Dem_W_dvote,
   Rep_M_loyal = Rep_M_rvote,
   Rep_W_loyal = Rep_W_rvote,
   # Sum of all not loyal (nonvote, NA, defect)
   # PID - num_disloyal; num_disloyal = pid - loyal
   Dem_M_not_loyal = Dem_M_pid - Dem_M_loyal,
   Dem_W_not_loyal = Dem_W_pid - Dem_W_loyal,
   Rep_M_not_loyal = Rep_M_pid - Rep_M_loyal,
   Rep_W_not_loyal = Rep_W_pid - Rep_W_loyal,
   # partisan defection,
   Dem_M_defect = Dem_M_rvote,
   Dem_W_defect = Dem_W_rvote,
   Rep_M_defect = Rep_M_dvote,
   Rep_W_defect = Rep_W_dvote,
   # other stuff
   Other_M_dvote = Other_M_dvote,
   Other_W_dvote = Other_W_dvote,
   Other_M_rvote = Other_M_rvote,
   Other_W_rvote = Other_W_rvote
   )


 # --- build workhorse model -----------------------
 
 rhs_table <- 
  wide_table %>%
  transmute(
   cycle = cycle,
   # Democrats, partyID, mobilization, persuasion, & other
   MD_pid = Dem_M_pid,
   WD_pid = Dem_W_pid,
   # MD_mob = Dem_M_pid - Dem_M_not_loyal,
   # WD_mob = Dem_W_pid - Dem_W_not_loyal,
   MD_mob = -1 * Dem_M_not_loyal,
   WD_mob = -1 * Dem_W_not_loyal,
   MD_per = Rep_M_defect,
   WD_per = Rep_W_defect,
   MD_other = Other_M_dvote,
   WD_other = Other_W_dvote,
   # Republicans, partyID, mobilization, persuasion, & other
   MR_pid = Rep_M_pid,
   WR_pid = Rep_W_pid,
   # MR_mob = Rep_M_pid - Rep_M_not_loyal,
   # WR_mob = Rep_W_pid - Rep_W_not_loyal,
   MR_mob = -1 * Rep_M_not_loyal,
   WR_mob = -1 * Rep_W_not_loyal,
   MR_per = Dem_M_defect,
   WR_per = Dem_W_defect,
   MR_other = Other_M_rvote,
   WR_other = Other_W_rvote,
   # columns for loyalty, cuz they may be useful
   MD_loyal = Dem_M_loyal,
   WD_loyal = Dem_W_loyal,
   MR_loyal = Rep_M_loyal,
   WR_loyal = Rep_W_loyal,
   # build vote_{g,p}
   M_dem_votes = MD_pid + MD_mob + MD_per + MD_other,
   W_dem_votes = WD_pid + WD_mob + WD_per + WD_other,
   M_rep_votes = MR_pid + MR_mob + MR_per + MR_other,
   W_rep_votes = WR_pid + WR_mob + WR_per + WR_other,
   gap = (W_dem_votes - W_rep_votes) - (M_dem_votes - M_rep_votes),
   margin = (W_dem_votes + M_dem_votes) - (W_rep_votes + M_rep_votes),
   gap_prop = gap / super_denom_tab$super_denom,
   margin_prop = margin / super_denom_tab$super_denom
   ) 


 # --- compute partials -----------------------
 
 # differencing across party
 # divide by superdenom (normalize)
 # elongate for plotting
 partials_table <- 
  rhs_table %>%
  left_join(., super_denom_tab, by = "cycle") %>% 
  transmute(
   cycle = cycle,
   M_pid = (MD_pid - MR_pid) / super_denom,
   W_pid = (WD_pid - WR_pid) / super_denom,
   M_mob = (MD_mob - MR_mob) / super_denom,
   W_mob = (WD_mob - WR_mob) / super_denom,
   M_per = (MD_per - MR_per) / super_denom,
   W_per = (WD_per - WR_per) / super_denom,
   M_other = (MD_other - MR_other) / super_denom,
   W_other = (WD_other - WR_other) / super_denom,
   W_total = W_pid + W_mob + W_per + W_other,
   M_total = M_pid + M_mob + M_per + M_other,
   gap = W_total - M_total) %>%
  gather(key = Gender_Source, value = Impact, M_pid:M_total) %>%
  separate(Gender_Source, into = c("Gender", "Source")) %>%
  mutate(
   Source = ifelse(Source == "pid", "Partisanship",
        ifelse(Source == "mob", "Mobilization",
        ifelse(Source == "per", "Persuasion",
        ifelse(Source == "other", "Unaffiliated", "Net Democratic Votes")))),
   Source = factor(Source, levels = c("Partisanship", "Mobilization", "Persuasion", "Unaffiliated", "Net Democratic Votes")))
  

 return(list(rhs = rhs_table, 
             partials = partials_table, 
             denoms = super_denom_tab))

}
