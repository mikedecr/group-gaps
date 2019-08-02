// check reordering of categories
// check bounds for simplex
// add flat prior!


data {

  // metadata
  int<lower = 1> N;            // nrow
  int<lower = 1> T;            // number of cycles
  int<lower = 1> J;            // number of outcome categories
  // int<lower = 1> nt[T];        // n per cycle (do you need this?)

  // respondent-level
  int<lower = 1, upper = J> y[N];         // outcome code
  real<lower = 0> wt[N];                  // wt on each observation
  int<lower = 1, upper = T> t[N];         // cycle[i] (cycle code)

  // priors
  vector[J] alpha;  // flat dirichlet parameter

}


parameters {

  simplex[J] theta[T];   // theta contains T many simplexes, each with J elements

}


model {

  for (i in 1:N)
    target += wt[i] * categorical_lpmf(y[i] | theta[t[i]]);

  for(cycle in 1:T)
    theta[cycle] ~ dirichlet(alpha);

}


generated quantities {

  vector[T] partisanship_dem_men;
  vector[T] partisanship_dem_women;
  vector[T] partisanship_rep_men;
  vector[T] partisanship_rep_women;

  vector[T] mobilization_dem_men;
  vector[T] mobilization_dem_women;
  vector[T] mobilization_rep_men;
  vector[T] mobilization_rep_women;

  vector[T] loyal_dem_men;
  vector[T] loyal_dem_women;
  vector[T] loyal_rep_men;
  vector[T] loyal_rep_women;

  vector[T] persuasion_dem_men;
  vector[T] persuasion_dem_women;
  vector[T] persuasion_rep_men;
  vector[T] persuasion_rep_women;

  vector[T] unaffiliated_dem_men;
  vector[T] unaffiliated_dem_women;
  vector[T] unaffiliated_rep_men;
  vector[T] unaffiliated_rep_women;

  vector[T] adv_partisanship_men;
  vector[T] adv_partisanship_women;

  vector[T] adv_mobilization_men;
  vector[T] adv_mobilization_women;

  vector[T] adv_persuasion_men;
  vector[T] adv_persuasion_women;

  vector[T] adv_unaffiliated_men;
  vector[T] adv_unaffiliated_women;

  vector[T] adv_vote_men;
  vector[T] adv_vote_women;

  vector[T] adv_partisanship;
  vector[T] gap_partisanship;
  vector[T] adv_mobilization;
  vector[T] gap_mobilization;
  vector[T] adv_persuasion;
  vector[T] gap_persuasion;
  vector[T] adv_unaffiliated;
  vector[T] gap_unaffiliated;

  vector[T] adv_total;
  vector[T] gap_total;


  for (tt in 1:T) {

    // --- calculate each partial term (loop over years)
    
    // partisanship among men and women
    partisanship_dem_men[tt] = theta[tt, 1] + theta[tt, 2] + theta[tt, 3];
    partisanship_dem_women[tt] = theta[tt, 10] + theta[tt, 11] + theta[tt, 12];
    partisanship_rep_men[tt] = theta[tt, 4] + theta[tt, 5] + theta[tt, 6];
    partisanship_rep_women[tt] = theta[tt, 13] + theta[tt, 14] + theta[tt, 15];

    // all loyal for each group
    loyal_dem_men[tt] = theta[tt, 1];
    loyal_dem_women[tt] = theta[tt, 10];
    loyal_rep_men[tt] = theta[tt, 5];
    loyal_rep_women[tt] = theta[tt, 14];

    // all non-loyal for each group
    mobilization_dem_men[tt] = theta[tt, 2] + theta[tt, 3];
    mobilization_dem_women[tt] = theta[tt, 11] + theta[tt, 12];
    mobilization_rep_men[tt] = theta[tt, 4] + theta[tt, 6];
    mobilization_rep_women[tt] = theta[tt, 13] + theta[tt, 15];

    // votes for party from OTHER party
    persuasion_dem_men[tt] = theta[tt, 4];
    persuasion_dem_women[tt] = theta[tt, 13];
    persuasion_rep_men[tt] = theta[tt, 2];
    persuasion_rep_women[tt] = theta[tt, 11];

    // votes for party from unaffiliated
    unaffiliated_dem_men[tt] = theta[tt, 7];
    unaffiliated_dem_women[tt] = theta[tt, 16];
    unaffiliated_rep_men[tt] = theta[tt, 8];
    unaffiliated_rep_women[tt] = theta[tt, 17];

  }


  // now that we have T-length vectors, can do straightforward math

  // --- partisan advantage and gap in each mechanism

    // partisanship
    adv_partisanship_men = partisanship_dem_men - partisanship_rep_men;
    adv_partisanship_women = partisanship_dem_women - partisanship_rep_women;

      adv_partisanship = adv_partisanship_women + adv_partisanship_men;
      gap_partisanship = adv_partisanship_women - adv_partisanship_men;

    // mobilization
    adv_mobilization_men = -1 * (mobilization_dem_men - mobilization_rep_men);
    adv_mobilization_women = -1 * (mobilization_dem_women - mobilization_rep_women);

      adv_mobilization = adv_mobilization_women + adv_mobilization_men;
      gap_mobilization = adv_mobilization_women - adv_mobilization_men;

    // persuasion
    adv_persuasion_men = persuasion_dem_men - persuasion_rep_men;
    adv_persuasion_women = persuasion_dem_women - persuasion_rep_women;

      adv_persuasion = adv_persuasion_women + adv_persuasion_men;
      gap_persuasion = adv_persuasion_women - adv_persuasion_men;

    // unaffiliated
    adv_unaffiliated_men = unaffiliated_dem_men - unaffiliated_rep_men;
    adv_unaffiliated_women = unaffiliated_dem_women - unaffiliated_rep_women;

      adv_unaffiliated = adv_unaffiliated_women + adv_unaffiliated_men;
      gap_unaffiliated = adv_unaffiliated_women - adv_unaffiliated_men;

    // total
    adv_vote_men = adv_partisanship_men + adv_mobilization_men + adv_persuasion_men + adv_unaffiliated_men;
    adv_vote_women = adv_partisanship_women + adv_mobilization_women + adv_persuasion_women + adv_unaffiliated_women;

      adv_total = adv_partisanship + adv_mobilization + adv_persuasion + adv_unaffiliated;
      gap_total = gap_partisanship + gap_mobilization + gap_persuasion + gap_unaffiliated;

    

    // --- gender gap in each party?


    // --- first differences? easier in here?

}


