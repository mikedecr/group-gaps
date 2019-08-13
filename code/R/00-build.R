# ----------------------------------------------------
#   Burden & DeCrescenzo, Gender Gap
#   R master file: controls replication of all R things
# ----------------------------------------------------

# --- build it -----------------------

source(here::here("code", "R", "01-setup.R"), echo = TRUE)

source(here::here("code", "R", "02-recode-anes.R"), echo = TRUE)

source(here::here("code", "R", "03-descriptives.R"), echo = TRUE)
save.image(file = here("data", "rmd-rdata", "03-descriptives.Rdata"))

source(here::here("code", "R", "04-MCMC.R"), echo = TRUE)

source(here::here("code", "R", "05-viz.R"), echo = TRUE)
save.image(file = here("data", "rmd-rdata", "05-viz.Rdata"))

beepr::beep(2)
