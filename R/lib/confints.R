prop_ci <- function(successes, n, level = 0.05) {

    # estimates
    estimate <- successes / n
    std_err <- base::sqrt(estimate * (1 - estimate) / n)

    # MOE
    z <- stats::qnorm(1 - (level / 2))
    moe <- z * std_err

    # value: vector
    return(c(estimate = estimate,
             std_error = std_error,
             lower = estimate - moe,
             upper = estimate + moe))

}

diff_prop_ci <- function(success1, n1, success2, n2, level = 0.05) {

  # get parameters
  p1 <- success1 / n1
  p2 <- success2 / n2

  # compute standard error
  var1 = (p1 * (1 - p1) / n1)
  var2 = (p2 * (1 - p2) / n2)
  se_diff <- base::sqrt(var1 + var2)

  # compute CI
  estimate <- p1 - p2
  z = stats::qnorm(1 - (level / 2))
  moe = se_diff * z

  return(c(estimate = estimate,
           std_err = se_diff,
           lower = estimate - moe,
           upper = estimate + moe))
}
