
data{
    int<lower=1> n_observations; // number of observations
    int<lower=1> n_actors;// number of actors
    int<lower=1> n_treatments;// number of treatments
    array[n_observations] int pulled_left; // the response variable 0 or 1 (1 = pulled left)
    array[n_observations] int treatment; // the indicator for each treatment 1 through 4.
    array[n_observations] int actor; // the indicator for each chimpanzee (1 through 7 )
}

parameters{
     vector[n_actors] a;
     vector[n_treatments] b;
}
model{
     vector[n_observations] p;
    b ~ normal( 0 , 0.5 );
    a ~ normal( 0 , 1.5 );
    for ( i in 1:n_observations ) {
        p[i] = a[actor[i]] + b[treatment[i]];
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
