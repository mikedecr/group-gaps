# raw exit poll data -> normalized exit poll data

box::use(conflicted)
box::use(dplyr[rename, rename_all, mutate, select])
box::use(readr[read_csv, write_csv])
box::use(here[here])
box::use(stringr[str_replace_all])
box::use(R/lib/state_crosswalk[state_name_to_abb])

ex_raw <- read_csv(here("data", "exits-04-08.csv"), show_col_types = FALSE)

ex <- ex_raw |>
    rename_all(str_replace_all, "[.]", "_") |>
    rename_all(str_replace_all, "dvote", "dem_vote") |>
    rename_all(str_replace_all, "rvote", "rep_vote") |>
    rename_all(str_replace_all, "dshare", "dem_vote") |>
    rename_all(str_replace_all, "rshare", "rep_vote") |>
    rename(state_name = state) |>
    mutate(
        across(.cols = contains("_vote_"), .fns = ~ .x / 100),
        state_abb = purrr::map_chr(state_name, state_name_to_abb),
        twoparty_vote = dem_vote + rep_vote,
        twoparty_vote_women = dem_vote_women + rep_vote_women,
        twoparty_vote_men = dem_vote_men + rep_vote_men,
        dem_twoparty_vote = dem_vote / twoparty_vote,
        dem_twoparty_vote_women = dem_vote_women / twoparty_vote_women,
        dem_twoparty_vote_men = dem_vote_men / twoparty_vote_men,
        gender_gap = dem_twoparty_vote_women - dem_twoparty_vote_men,
    ) |>
    select(
        cycle, state_name, state_abb,
        gender_gap, contains("twoparty"), everything(),
        -starts_with("twoparty_vote")
    ) |>
    print()

arrow::write_parquet(ex, here("data", "clean", "exits.pq"))
