# ----------------------------------------------------------------------------
#  clean exit polls
# ----------------------------------------------------------------------------

library("here")
library("tidyverse")


# ----------------------------------------------------------------------------
#  read
# ----------------------------------------------------------------------------

# all files in dir
exit_dir <- here('data', 'raw', 'exit_polls')
files <- map(list.files(exit_dir), ~ here(exit_dir, .x))

# read
ex <- files |>
    map(read_csv) |>
    bind_rows()

# ---- clean names --------

names(ex)

ex <- rename_all(ex, .funs = ~ str_replace(.x, '[.]', '_'))

# ----------------------------------------------------------------------------
#  variables
# ----------------------------------------------------------------------------

# ---- two-party vote share --------
ex <- mutate(ex,
             two_party_vote = dshare + rshare,
             dem_share      = dshare / two_party_vote)



# ---- gender gap --------

ex <- ex |>
    mutate(
        two_party_vote_women    = dvote_women + rvote_women,
        two_party_vote_men      = dvote_men + rvote_men,
        dem_share_women         = dvote_women / two_party_vote_women,
        dem_share_men           = dvote_men / two_party_vote_men,
        gender_gap              = dem_share_women - dem_share_men
    )


# ---- state postal code --------

dc_name <- "District of Columbia"

postal_codes <-
    tibble(state = state.name, state_postal = state.abb) |>
    bind_rows(tibble(state = dc_name, state_postal = "DC"))
 
ex <- left_join(ex, postal_codes)

# assertion
is_missing_code <- is.na(ex$state_postal)
stopifnot(sum(is_missing_code) == 0)


# ----------------------------------------------------------------------------
#  save
# ----------------------------------------------------------------------------

write_csv(ex, here("data", "clean", "exits.csv"))
arrow::write_parquet(ex, here("data", "clean", "exits.pq"))
