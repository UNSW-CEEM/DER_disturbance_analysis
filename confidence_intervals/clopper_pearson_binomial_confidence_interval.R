upper_bound_function <- function(x, n, p_estimate, alpha) {
  # At upper bound of Clopper-Pearson confidence interval, where alpha = 1 - confidence:
  # - observations of x successes or less should occur with probability alpha/2.
  return(pbinom(x, n, p_estimate) - alpha/2)
}

lower_bound_function <- function(x, n, p_estimate, alpha) {
  # At lower bound of Clopper-Pearson confidence interval, where alpha = 1 - confidence:
  # - observations of x successes or more should occur with probability alpha/2.
  return(1 - pbinom(x - 1, n, p_estimate) - alpha/2)
}

bisection_method <- function(func, lower_limit, upper_limit, TOL=0.001, N_max=100) {
  # Numerical method to find the root(s) of a function.
  # Lower and upper limits straddle the root, i.e. sign(lower_limit) != sign(upper_limit).

  N = 1
  while (N < N_max) {
    midpoint = (lower_limit + upper_limit)/2
    if ((upper_limit - lower_limit)/2 < TOL) {
      # Solution found, have closed in sufficiently tightly on the root of the function.
      break
    }
    if (sign(func(midpoint)) == sign(func(lower_limit))) {
      lower_limit = midpoint
    } else {
      upper_limit = midpoint
    }
    N = N + 1
  }
  if (N >= N_max) {
    stop("Convergence failure in bisection method")
  }
  return (midpoint)
}

confidence_interval <- function(x, n, confidence=0.95){
  # Computes Clopper-Pearson upper and lower bounds for a binomial proportion confidence interval
  
  # x: number of successful observations in sample
  # n: sample size
  # confidence: range in [0, 1]. Default to 95% confidence level (0.95).
  
  # Written by Alexander Makarowsky and adapted from a script by Quinn Patterson. 
  
  # Where alpha = 1 - confidence:
  # The upper bound is adjusted until the probability of the population proportion exceeding the upper
  # bound is alpha/2. Similarly, the lower bound is adjusted until the probability that the 
  # population proportion is less than the lower bound is alpha/2.
  
  # The bisection method is used to determine each interval bound.
  alpha = 1 - confidence
  TOL = 0.00001
  
  # Set upper bound such that the probability of the population proportion exceeding it is alpha/2.
  upper_bound_f <- function(p_est) upper_bound_function(x, n, p_est, alpha)
  upper_bound_left = x/n
  upper_bound_right = 1
  upper_bound = bisection_method(upper_bound_f, upper_bound_left, upper_bound_right, TOL)
  
  # Also set lower bound s.t. the probability of the population proportion exceeding it is alpha/2.
  lower_bound_f <- function(p_est) lower_bound_function(x, n, p_est, alpha)
  lower_bound_left = 0
  lower_bound_right = x/n
  lower_bound = bisection_method(lower_bound_f, lower_bound_left, lower_bound_right, TOL)
  
  return(c(lower_bound, upper_bound))
}