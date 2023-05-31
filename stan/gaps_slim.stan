// check reordering of categories
// check bounds for simplex
// add flat prior!

data {

  // bounds
  int<lower = 1> N;            // nrow
  int<lower = 1> T;            // number of cycles
  int<lower = 1> J;            // number of outcome categories

  // unit-level data
  int<lower = 1, upper = J> y[N]    // outcome code
  real<lower = 0> wt[N]             // wt on each observation
  int<lower = 1, upper = T> t[N]    // cycle[i] (cycle code)

  // priors
  vector[J] alpha;  // flat dirichlet parameter
                    // technically this could be J x T?
}

parameters {
  simplex[J] theta[T];  // theta contains T many simplexes,
                        // each with J elements
}

model {

  // one pass through the data
  for (i in 1:N) {
    target += wt[i] * categorical_lpmf(y[i] | theta[t[i]]);
  }

  // priors are independent per year
  for(cycle in 1:T) {
    theta[cycle] ~ dirichlet(alpha);
  }

}

