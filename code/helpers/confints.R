# ----------------------------------------------------------------------------
#  confidence intervals for a vector of proportions
# ----------------------------------------------------------------------------

box::use(stats[...])

# @export
prop_ci <- function(successes, n, level=0.05) {

    # success/fail prob
    p1 <- successes / n
    q1 <- 1 - p1

    # margin of error
    std_err <- sqrt((p1 * q1) / n)
    Z <- qnorm(1 - (level / 2))
    moe <- Z * std_err

    list(estimate = p1,
         std_err = std_err,
         lower = p1 - moe,
         upper = p1 + moe)
}



# ----------------------------------------------------------------------------
#  conf ints for differences in two proportions
# ----------------------------------------------------------------------------

# @export
diff_prop_ci <- function(success1, n1, success2, n2, level=0.05) {

  # get parameters

  p1 <- success1 / n1
  q1 <- 1 - p1

  p2 <- success2 / n2
  q2 <- 1 - p2

  # standard error & MOE
  var1 <- (p1 * q1) / n1
  var2 <- (p2 * q2) / n2
  se_diff <- sqrt(var1 + var2)
  z = qnorm(1 - (level / 2))
  MOE = z * se_diff

  estimate <- p1 - p2

  list(estimate = estimate,
       std_err = se_diff,
       lower = estimate - MOE,
       upper = estimate + MOE)

}



