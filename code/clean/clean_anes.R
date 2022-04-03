# ----------------------------------------------------------------------------
#  Clean ANES file from original .dta format
# ----------------------------------------------------------------------------

# TODO offramps: dta -> RDS? pq?

# ---- packages -----

library("here")
library("tidyverse")


# ----------------------------------------------------------------------------
#  IO
# ----------------------------------------------------------------------------

anes <- haven::read_dta(here("data", "ANES-2016", "anes_timeseries_cdf.dta"))

# ----------------------------------------------------------------------------
#  renames
# ----------------------------------------------------------------------------

anes <- rename(
    anes,
    cycle = VCF0004,
    wt    = VCF0009z
)

# ----------------------------------------------------------------------------
#  recodes
# ----------------------------------------------------------------------------

# ---- year labels --------

anes <- anes |>
    mutate(is_presidential_cycle = (cycle %% 4) == 0)


# ---- gender -------- 

anes <- anes |>
    mutate(
        gender = case_when(
            VCF0104 == 1 ~ "M", 
            VCF0104 == 2 ~ "W"
        ),
        female = gender == "W"
    )

anes |> count(VCF0104, gender, female)


# ---- party ID --------

pid3_labels <- c("Dem", "Rep", "Ind")
pid7_labels <- c("Strong Dem", "Weak Dem", "Lean Dem",
                 "True Ind",
                 "Lean Rep", "Weak Rep", "Strong Rep")

anes <- anes |>
    mutate(
        pid_init =
            case_when(
                VCF0301 %in% c(1, 2) ~    "Dem",
                VCF0301 %in% c(3, 4, 5) ~ "Ind",
                VCF0301 %in% c(6, 7) ~    "Rep"
            ) |>
            fct_relevel(pid3_labels),
        pid_lean =
            case_when(
                VCF0301 %in% c(1, 2, 3) ~  "Dem",
                VCF0301 %in% c(4) ~        "Ind",
                VCF0301 %in% c(5, 6, 7) ~  "Rep"
            ) |>
            fct_relevel(pid3_labels),
        pid_full = case_when(
            VCF0301 == 1 ~ "Strong Dem",
            VCF0301 == 2 ~ "Weak Dem",
            VCF0301 == 3 ~ "Lean Dem",
            VCF0301 == 4 ~ "True Ind",
            VCF0301 == 5 ~ "Lean Rep",
            VCF0301 == 6 ~ "Weak Rep",
            VCF0301 == 7 ~ "Strong Rep"
        ) |>
        fct_relevel(pid7_labels)
    )

anes |> count(VCF0301, pid_init, pid_lean, pid_full)


# ---- vote choice --------

anes <- anes |>
    mutate(
        voted = case_when(
            VCF0702 == 1 ~ 0,
            VCF0702 == 2 ~ 1
        ),
        vote_choice = 
            case_when(
                VCF0705 == 1 ~ "Dem Cand",
                VCF0705 == 2 ~ "Rep Cand",
                VCF0705 == 3 ~ "Other Cand"
            ) |>
            fct_relevel("Dem Cand", "Rep Cand", "Other Cand"),
        voted_maj_party = case_when(
            vote_choice %in% c("Dem Cand", "Rep Cand") ~ TRUE, 
            TRUE ~ NA
        )
    )

anes |> count(VCF0702, VCF0705, voted, vote_choice, voted_maj_party)

# ---- summarize recodes --------

anes |> count(gender, pid_init, vote_choice) |> na.omit()


# ----------------------------------------------------------------------------
#  filter cases
# ----------------------------------------------------------------------------

min_cycle <- 1948

anes <- anes |>
    filter(is_presidential_cycle) |>
    filter(cycle > min_cycle) |>
    filter(gender %in% c("M", "W"))


# ----------------------------------------------------------------------------
#  save
# ----------------------------------------------------------------------------

write_rds(anes, here("data", "clean", "cleaned_anes_cdf.RDS"))