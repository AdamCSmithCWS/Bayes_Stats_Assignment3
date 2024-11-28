
data{
    int<lower=1> n_observations; // number of observations
    int<lower=1> n_actors;// number of actors
    int<lower=1> n_treatments;// number of treatments
    int<lower=1> n_blocks;// number of blocks
    
    array[n_observations] int pulled_left; // the response variable 0 or 1 (1 = pulled left)
    array[n_observations] int treatment; // the indicator for each treatment 1 through 4.
    array[n_observations] int actor; // the indicator for each chimpanzee (1 through 7 )
    array[n_observations] int block_id; // the indicator for block
}

parameters{
     vector[n_actors] a_raw; //uncentered parameterisation
     vector[n_treatments] b;
     vector[n_blocks] g;
     real<lower=0> sigma_a; // sd of actor intercepts
     real<lower=0> sigma_g; // sd of block effects
     real a_bar;
}

// including a transformed parameters block to account for
// the uncentered parameterisation
transformed parameters {
  vector[n_actors] a; //uncentered parameterisation

  a = a_bar + sigma_a * a_raw; 
  //vectorized re-scaling and centering of a_raw
  // equivalent to a ~ normal(a_bar,sigma_a);
}


model{
     vector[n_observations] p;
    b ~ normal( 0 , 0.5 );
    a_bar ~ normal( 0 , 1.5 );
    a_raw ~ std_normal(); // same as writing normal(0,1)
    sigma_a ~ exponential(1);
    sigma_g ~ exponential(1);
    g ~ normal(0,sigma_g); // centered parameterisation
    
    for ( i in 1:n_observations ) {
        p[i] = a[actor[i]] + g[block_id[i]] + b[treatment[i]];
        p[i] = inv_logit(p[i]);
    }
    pulled_left ~ binomial( 1 , p );
}
generated quantities{
    vector[n_observations] log_lik;
     vector[n_observations] p;
    for ( i in 1:n_observations ) {
        p[i] = a[actor[i]] + b[treatment[i]];
        p[i] = inv_logit(p[i]);
    }
    for ( i in 1:n_observations ) log_lik[i] = binomial_lpmf( pulled_left[i] | 1 , p[i] );
}
