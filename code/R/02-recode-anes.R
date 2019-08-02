# ----------------------------------------------------
#   Burden & DeCrescenzo, Gender gap
#   ---
#   File 01: Cleaning ANES Cumulative File
# ----------------------------------------------------

# check for NUKE



# --- ANES raw data -----------------------

# if RDS file is absent, create it:
if ("anes-cdf-2016.RDS" %in% list.files(here("data", "ANES-2016")) == FALSE) {
  here("data", "ANES-2016", "ANES-2016/anes_timeseries_cdf.dta") %>% 
    haven::read_dta() %>%
    saveRDS(here("data", "ANES-2016", "anes-cdf-2016.RDS"))
} else {
  print("RDS file already found")
}

# 2016 RDS file, remove labels
anes <- readRDS(here("data", "ANES-2016", "anes-cdf-2016.RDS")) %>%
  mutate_all(labelled::remove_labels) %>%
  print()



# ----------------------------------------------------
#   cleaning
# ----------------------------------------------------

# things we engage with
# - cycle
# - weight
# - gender
# - PID
# - turnout
# - vote choice


# NUKE NUKE NUKE Things that used to be recoded but no longer needed

# - age (age group)
# - birth year
# - marital status
# - partisanship dummies
# - nonvoter vote intent

# --- rename variables without modifying -----------------------

anes <- rename(anes, cycle = VCF0004, wt = VCF0009z)



# --- substantive recodes -----------------------

# note:
# case_when() implicitly converts all unspecified columns to NA

# NUKE - 
# there are some inconsistencies in raw data about "Other" PID categories
# raw data collapses some "DK" responses to 0, but maybe we want that info
# check notes documents

anes %$% table(VCF0302, exclude = NULL)
anes %$% table(VCF0303, exclude = NULL)

anes <- anes %>%
  mutate(gender = case_when(VCF0104 == 1 ~ "M",
                            VCF0104 == 2 ~ "W"),
         female = case_when(gender == "M" ~ 0,
                            gender == "W" ~ 1),
         pid_init = case_when(VCF0301 %in% c(1, 2) ~ "Dem",
                              VCF0301 %in% c(3, 4, 5) ~  "Ind",
                              VCF0301 %in% c(6, 7) ~  "Rep"),

         pid_lean = case_when(VCF0301 %in% c(1, 2, 3) ~ "Dem",
                              VCF0301 %in% c(4) ~  "Ind",
                              VCF0301 %in% c(5, 6, 7) ~  "Rep"),

         pid_full = case_when(VCF0301 == 1 ~ "Strong Dem",
                              VCF0301 == 2 ~ "Weak Dem",
                              VCF0301 == 3 ~ "Lean Dem",
                              VCF0301 == 4 ~ "True Ind",
                              VCF0301 == 5 ~ "Lean Rep",
                              VCF0301 == 6 ~ "Weak Rep",
                              VCF0301 == 7 ~ "Strong Rep"),
         voted = case_when(VCF0702 == 1 ~ 0,
                           VCF0702 == 2 ~ 1),
         vote_choice = case_when(VCF0705 == 1 ~ "Dem Cand",
                                 VCF0705 == 2 ~ "Rep Cand",
                                 VCF0705 == 3 ~ "Other Cand"),
         voted_maj_party = case_when(vote_choice %in% c("Dem Cand", 
                                                        "Rep Cand") ~ TRUE,
                                     TRUE ~ NA))


anes %$% table(gender, exclude = NULL)
anes %$% table(female, exclude = NULL)
anes %$% table(pid_init, exclude = NULL)
anes %$% table(cycle, pid_init, exclude = NULL)
anes %$% table(pid_lean, exclude = NULL)
anes %$% table(pid_full, exclude = NULL)
anes %$% table(voted, exclude = NULL)
anes %$% table(vote_choice, exclude = NULL)
anes %$% table(voted_maj_party, exclude = NULL)



# --- releveling -----------------------

anes <- anes %>%
  mutate(pid_init = fct_relevel(pid_init, "Dem", "Rep", "Ind"),
         pid_lean = fct_relevel(pid_lean, "Dem", "Rep", "Ind"),
         pid_full = fct_relevel(pid_full, "Strong Dem", "Weak Dem", "Lean Dem", "True Ind", "Lean Rep", "Weak Rep", "Strong Rep"),
         vote_choice = fct_relevel(vote_choice, "Dem Cand", "Rep Cand", "Other Cand"))


anes %$% table(pid_init, exclude = NULL)
anes %$% table(pid_lean, exclude = NULL)
anes %$% table(pid_full, exclude = NULL)
anes %$% table(vote_choice, exclude = NULL)



# --- filtering -----------------------

# cycle > 1948
# presidential years only (might be worth checking this out)
# - any modifications could be done with sort(unique(tab$cycle))

anes <- anes %>%
  filter((cycle %% 4) == 0) %>% 
  filter(cycle > 1948) %>%
  filter(gender %in% c("M", "W"))


# NUKE consider adding more filters based on other code


# --- save it -----------------------
anes %>%
print() %>%
haven::write_dta(here("data", "clean", "cleaned-anes-cdf.dta"))

anes %>%
saveRDS(here("data", "clean", "cleaned-anes-cdf.RDS"))

