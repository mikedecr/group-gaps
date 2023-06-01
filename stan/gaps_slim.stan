data {

  // bounds
  int<lower = 1> N;            // nrow
  int<lower = 1> T;            // number of cycles
  int<lower = 1> J;            // number of outcome categories

  // unit-level data
  array[N] int<lower = 1, upper = J> y;    // outcome code
  array[N] real<lower = 0> wt;             // wt on each observation
  array[N] int<lower = 1, upper = T> t;    // cycle[i] (cycle code)

  // priors
  vector[J] alpha;  // prior concentration
                    // this could be J x T
}

parameters {
  array[T] simplex[J] theta;  // theta contains T many simplexes,
                        // each with J elements
}

model {

  // one pass through the data
  for (i in 1:N) {
    target += wt[i] * categorical_lpmf(y[i] | theta[t[i]]);
  }

  // draw unique theta per time period
  for(cycle in 1:T) {
    theta[cycle] ~ dirichlet(alpha);
  }

}

