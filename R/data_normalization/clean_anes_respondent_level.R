# --------------------------------------------------
#  normalization of ANES data
#  raw ANES respondent level -> cleaned ANES respondent level
# --------------------------------------------------

library("conflicted")
library("here")
library("dplyr")


# --- ANES raw data -----------------------

# update this and read from some more raw format
# anes <- haven::read_dta(here("data", "ANES-2016", "anes_timeseries_cdf.dta"))
anes <- haven::read_dta(here("data", "anes_2020", "anes_timeseries_cdf_stata_20220916.dta"))

# NOTE remove labels?

# things we currently engage with

# - cycle
# - weight
# - gender
# - PID
# - turnout
# - vote choice


# things we looked at but aren't in the paper
# NOTE data prep _||_ rhetoric + findings

# - age (age group)
# - birth year
# - marital status
# - partisanship dummies
# - nonvoter vote intent


# --- rename with no transformations -----------------------

anes <- rename(
    anes,
    cycle = VCF0004,
    wt = VCF0009z
)



# --- substantive recodes -----------------------

# note:
# case_when() implicitly converts all unspecified columns to NA

# NUKE
# there are some inconsistencies in raw data about "Other" PID categories
# raw data collapses some "DK" responses to 0, but maybe we want that info
# check notes documents

with(anes, table(VCF0302, VCF0303, exclude = NULL))
attributes(anes$VCF0302)$labels
attributes(anes$VCF0303)$labels


table(anes$VCF0104, exclude = NULL)
attributes(anes$VCF0104)$labels


anes <- anes |>
    mutate(
        gender = case_match(
            VCF0104,
            1 ~ "M",
            2 ~ "W",
        ),
        female = case_match(
            gender,
            "M" ~ FALSE,
            "W" ~ TRUE
        ),
        pid_init = case_match(
            VCF0301,
            c(1, 2) ~ "Dem",
            c(3, 4, 5) ~ "Ind",
            c(6, 7) ~ "Rep"
        ),
        pid_lean = case_match(
            VCF0301,
            c(1, 2, 3) ~ "Dem",
            4 ~ "Ind",
            c(5, 6, 7) ~ "Rep"
        ),
        pid_full = case_match(
            VCF0301,
            1 ~ "Strong Dem",
            2 ~ "Weak Dem",
            3 ~ "Lean Dem",
            4 ~ "True Ind",
            5 ~ "Lean Rep",
            6 ~ "Weak Rep",
            7 ~ "Strong Rep"
        ),
        voted = case_match(
            VCF0702,
            1 ~ FALSE,
            2 ~ TRUE
        ),
        vote_choice = case_match(
            VCF0705,
            1 ~ "Dem Cand",
            2 ~ "Rep Cand",
            3 ~ "Other Cand"
        ),
        voted_maj_party = case_match(
            vote_choice,
            c("Dem Cand", "Rep Cand") ~ TRUE,
        )
)


with(anes, table(VCF0104, gender, exclude = NULL))
with(anes, table(VCF0104, female, exclude = NULL))
with(anes, table(VCF0301, pid_init, exclude = NULL))
with(anes, table(VCF0301, pid_lean, exclude = NULL))
with(anes, table(VCF0301, pid_full, exclude = NULL))
with(anes, table(pid_full, pid_init, exclude = NULL))
with(anes, table(pid_full, pid_lean, exclude = NULL))

with(anes, table(VCF0702, voted, exclude = NULL))
with(anes, table(VCF0705, vote_choice, exclude = NULL))
with(anes, table(VCF0705, voted_maj_party, exclude = NULL))



# --- releveling -----------------------

anes <- anes |>
  mutate(pid_init = forcats::fct_relevel(pid_init, "Dem", "Rep", "Ind"),
         pid_lean = forcats::fct_relevel(pid_lean, "Dem", "Rep", "Ind"),
         pid_full = forcats::fct_relevel(pid_full, "Strong Dem", "Weak Dem", "Lean Dem", "True Ind", "Lean Rep", "Weak Rep", "Strong Rep"),
         vote_choice = forcats::fct_relevel(vote_choice, "Dem Cand", "Rep Cand", "Other Cand"))


with(anes, table(pid_init, exclude = NULL))
with(anes, table(pid_lean, exclude = NULL))
with(anes, table(pid_full, exclude = NULL))
with(anes, table(vote_choice, exclude = NULL))



# --- filtering -----------------------

# cycle > 1948
# presidential years only (might be worth checking this out)
# - any modifications could be done with sort(unique(tab$cycle))

head(anes$cycle)



anes <- anes |>
    dplyr::filter((cycle %% 4) == 0) |>
    dplyr::filter(cycle >= 1952) |>
    dplyr::filter(gender %in% c("M", "W"))


# NUKE consider adding more filters based on other code


# --- save it -----------------------

print(head(anes))

clean_data = here("data", "clean")

if (file.exists(clean_data) == FALSE) {
    dir.create(clean_data, showWarnings = FALSE)
}

arrow::write_parquet(anes, here(clean_data, "anes_cdf.pq"))

