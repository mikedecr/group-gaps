##############################################################################
#    fit simple regression models of the topline gap v. vote relationship    #
##############################################################################

# Longitudinal: gap (ANES) vs. vote (LEIP)
# cross-sectional: gap (exists) vs. vote (?? also exits??)
# fit models and dump summary tables to file

##############
#    libs    #
##############

library("conflicted")
library("dplyr")
library("tidyr")
library("stringr")
library("here")
library("arrow")
library("memoise")
library("purrr")
library("ggplot2")


##################
#    raw data    #
##################

clean = here("data", "clean")
national = read_parquet(here(clean, "gap_vote_x_cycle.pq"))
exits = read_parquet(here(clean, "exits.pq"))


##############################
#    a common abstraction    #
##############################

# schema: dataset = ..., y = dem_share, x = gender_gap

exit_ids = c("cycle", "state_name", "state_abb")
exits_merge = exits |>
    select(cycle, state_name, state_abb,
           dem_share = dem_twoparty_vote, gender_gap) |>
    mutate(dataset = str_glue("ExitPoll_{cycle}") |> as.character())

# drop DC
exits_merge_no_dc = exits_merge |>
    dplyr::filter(state_abb != "DC") |>
    mutate(dataset = str_glue("ExitPollNoDC_{cycle}") |> as.character())

national_merge = national |>
    select(cycle,
           gender_gap = gender_gap_estimate, dem_share = dem_share_official) |>
    mutate(dataset = "ANESxLeip")

regdata = bind_rows(national_merge, exits_merge, exits_merge_no_dc)


#############################
#    estimate regression    #
#############################

lm_with_schema = function(data) {
    lm(dem_share ~ gender_gap, data = data)
}

models = regdata |>
    nest(.by = dataset) |>
    mutate(mod = map(data, lm_with_schema),
           coefs = map(mod, broom::tidy, conf.int = TRUE),
           summary = map(mod, broom::glance),
           prediction = map2(
               mod, data,
               function(x, y) {
                   broom::augment(x, se_fit = TRUE, interval = "confidence") |>
                   full_join(y)
               }
           ))

#########################
#    save model data    #
#########################

model_dir = here("data", "models") 
if (!file.exists(model_dir)) {
    dir.create(model_dir)
}
readr::write_rds(models, here(model_dir, "simple_regressions.rds"))


#######################################################
#    stuff that should go into a playground script    #
#######################################################

#################
#    scatter    #
#################

models |>
    unnest(prediction) |>
    mutate(label = ifelse(str_detect(dataset, "ANES"), cycle, state_abb)) |>
    (function(d) {
        ggplot(d) +
        aes(x = gender_gap, y = dem_share) +
        geom_text(aes(label = label)) +
        facet_wrap(~ dataset, nrow = 1) +
        geom_line(aes(y = .fitted)) 
   })()


##################
#    analysis    #
##################

# global cor, global p.value
models |>
    unnest(summary) |>
    mutate(abs_r = sqrt(r.squared))

# gender_gap coef/stat/p
models |>
 unnest(coefs) |>
 dplyr::filter(term == "gender_gap") |>
 select(dataset, p.value)

