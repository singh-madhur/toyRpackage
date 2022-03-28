#' Estimate the Power for a Likelihood Ratio Test
#'
#' This function estimates the power to reject the Null Hypothesis in a Likelihood Ratio Test
#' for two nested models fit using Maximum Likelihood Estimation.
#' The function requires as input the desired alpha (`alpha`; default = 0.05),
#' the non-centrality parameter (`diffLL`), i.e., the difference in the log-likelihood of the two models),
#' and the difference in the degrees of freedom in the two models (`diffDF`).
#'
#' @param diffLL The NCP in a given LRT
#' @param diffDF The difference in the degrees of freedom
#' @param alpha The desired alpha. Default = 0.05
#' @return The power to reject the Null Hypothesis.
#' @export
powerLRT <- function(diffLL, diffDF, alpha = 0.05) {
  crit_chisq <- qchisq(alpha, diffDF, ncp = 0, lower.tail = FALSE)	 # critical value, given alpha
  powLRT <- pchisq(crit_chisq, diffDF, ncp = diffLL, lower.tail = FALSE)
  return(powLRT)
}
