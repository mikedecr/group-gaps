data {

  // bounds
  int J;  // outcome grps
  int T;  // time periods

  // aggregate outcomes
  array[T] vector<lower = 0>[J] W; // sum of weights per J

  // priors
  vector[J] alpha; // FOR NOW: a priori fixed dirichlet concentration across years

}


parameters {
    // one J-simplex per time unit
    array[T] simplex[J] theta;
}

model {

  for (tt in 1:T) {
    for (jj in 1:J) {
      target += log(theta[tt, jj]) * W[tt, jj];
    }
    theta[tt] ~ dirichlet(alpha);
  }


}

