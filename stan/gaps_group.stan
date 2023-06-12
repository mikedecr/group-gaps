data {

  // num. raw obs
  int<lower = 1> N;
  // time periods: t[i]
  int<lower = 1> T;
  array[N] int<lower = 1, upper = T> t;
  // outcome: y[i] (int) and wt[i] (real)
  int<lower = 1> J;            // number of outcome categories
  array[N] int<lower = 1, upper = J> y;    // outcome code
  array[N] real<lower = 0> wt;             // wt on each observation

  // priors
  vector[J] alpha;  // flat dirichlet parameter
                    // technically this could be J x T?
}

// easy optimization
// outcomes are of a finite set of groups
// so we model the sum of the weights in each group
transformed data {
    matrix[T, J] W = rep_matrix(0, T, J);
    for (i in 1:N) {
      W[t[i], y[i]] += wt[i];
    }
}

parameters {
  // T many simplexes, each with J elements
  array[T] simplex[J] theta;  
}

model {

  for(cycle in 1:T) {
    for (grp in 1:J) {
        // multinomial pseudocount
        // each outcome y has a probability p_y and a pseudocount of observations n_y,
        // but to increment the log-density accumulator, do it on the log scale:
        // log(p_{y}^n_{y}) = n_{y} * log(p_{y})
        target += W[cycle, grp] * log(theta[cycle, grp]);
    }
    // draw unique theta per time period
    theta[cycle] ~ dirichlet(alpha);
  }

}


