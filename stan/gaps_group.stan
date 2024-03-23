data {

  // num. raw obs
  int<lower = 1> n;
  // time periods: t[i]
  int<lower = 1> n_t;
  array[n] int<lower = 1, upper = n_t> t;
  // outcome: group[i] (int) and wt[i] (real)
  int<lower = 1> n_group;            // number of outcome categories
  array[n] int<lower = 1, upper = n_group> group;    // outcome code
  array[n] real<lower = 0> wt;             // wt on each observation

  // priors
  vector[n_group] alpha;  // flat dirichlet parameter
                    // technically this could be n_group x T?
}

// easy optimization
// outcomes are of a finite set of groups
// so we model the sum of the weights in each group
transformed data {
    matrix[n_t, n_group] W = rep_matrix(0, n_t, n_group);
    for (i in 1:n) {
      W[t[i], group[i]] += wt[i];
    }
}

parameters {
  // T many simplexes, each with n_group elements
  array[n_t] simplex[n_group] theta;  
}

model {

  for(cycle in 1:n_t) {
    for (grp in 1:n_group) {
        // multinomial pseudocount
        // each outcome group has a probability p_y and a pseudocount of observations n_y,
        // but to increment the log-density accumulator, do it on the log scale:
        // log(p_{group}^n_{group}) = n_{group} * log(p_{group})
        target += W[cycle, grp] * log(theta[cycle, grp]);
    }
    // draw unique theta per time period
    theta[cycle] ~ dirichlet(alpha);
  }

}


