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
  vector[J] alpha;  // flat dirichlet parameter
                    // technically this could be J x T?
}

// easy optimization
// outcomes are of a finite set of groups
// so just the weights in each group
transformed data {
    matrix[T, J] W = rep_matrix(0, T, J);
    for (i in 1:N) {
      W[t[i], y[i]] += wt[i];
    }
}

parameters {
  array[T] simplex[J] theta;  // theta contains T many simplexes,
                        // each with J elements
}

model {

  for(cycle in 1:T) {
    for (grp in 1:J) {
        // grouped data likelihood draw
        target += W[cycle, grp] * log(theta[cycle, grp]);
    }
    // draw unique theta per time period
    theta[cycle] ~ dirichlet(alpha);
  }

}


