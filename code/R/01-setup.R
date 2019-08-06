# ----------------------------------------------------
#   Burden & DeCrescenzo, Gender gap
#   ---
#   File 01: Project setup
# ----------------------------------------------------



# --- packages -----------------------
# library("colorout")
library("magrittr")
library("tidyverse")
library("broom")
library("beepr")
library("latex2exp")
# library("extrafont")



library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library("ggmcmc")
# --- working directory -----------------------

# set to project root
library(here)
here() # there is an invisible .here file! 


# --- subdirectories for output -----------------------

dir.create(here("tex")) # may already exist in repo
dir.create(here("tex/diagrams"))
dir.create(here("tex/graphics"))
dir.create(here("tex/tables"))
dir.create(here("tex/refs"))
dir.create(here("tex/appendix"))



# --- graphics settings -----------------------

# graphics theme
update_geom_defaults("text", list(family = "Fira Sans"))
theme_set(
  ggthemes::theme_base(base_family = "Fira Sans", base_size = 14) + 
  theme(plot.background = element_blank(), 
        axis.ticks = element_line(lineend = "square"), 
        axis.ticks.length = unit(0.25, "lines"))
)

dblue <- "#0072B2"
rred <- "#D55E00"
mcolor <- "#009E73"
wcolor <- "#E69F00"

# shorthand gray colors made available by theme_mgd download!



# --- useful functions -----------------------


# should these be mappable for nested data frames?

# confidence intervals for a vector of proportions
prop_ci <- function(successes, n, level=0.05) {

  # get parameters
  p1 <- successes/n
  q1 <- 1 - p1

  # compute standard error
  SE <- sqrt((p1*q1)/n)

  # find upper and lower bound
  lower <- p1 - qnorm(1-(level/2))*SE
  upper <- p1 + qnorm(0.975)*SE

  # return data frame
  return(data.frame(lower, upper))

}



# confidence intervals for difference in two proportions
# input 
diff_prop_ci <- function(success1, n1, success2, n2, level=0.05) {

  # get parameters
  p1 <- success1 / n1
  q1 <- 1 - p1
  p2 <- success2 / n2
  q2 <- 1 - p2

  # compute standard error
  var1 <- (p1*q1) / n1
  var2 <- (p2*q2) / n2
  se_diff <- sqrt(var1 + var2)

  # compute CI
  estimate <- p1 - p2
  lower <- estimate - qnorm(1-(level/2))*se_diff
  upper <- estimate + qnorm(1-(level/2))*se_diff

  # return data frame
  return(data.frame(estimate, lower, upper))

}



# we also want to save things using cairo by default
ggsave_cairo <- function(..., device = cairo_pdf) {
  ggsave(..., device = device)
}

