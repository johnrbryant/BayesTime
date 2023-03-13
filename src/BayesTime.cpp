
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <TMB.hpp>
#include "init.h"

using namespace Eigen;


// Helper functions -----------------------------------------------------------

// based partly on hyperhttps://discourse.mc-stan.org/t/straightforward-implementation-of-a-difference-penalty/28516/4
template <class Type>
Type logdens_rw2(vector<Type> x, Type sd) {
  int n = x.size();
  Type ans = 0;
  for (int i = 2; i < n; i++) {
    ans += dnorm(x[i], 2 * x[i - 1] - x[i - 2], sd, true);
  }
  return ans;
}

template <class Type>
Type logdens_phi_raw(vector<Type> phi_raw) {
  Type phi_prior = 2;
  Type ans = 0;
  ans += dbeta(phi_raw, phi_prior, phi_prior, true);
  ans += log(phi_raw) + log(1 - phi_raw); // Jabobian
  return ans;
}

template <class Type>
Type transform_phi_raw(vector<Type> phi_raw) {
  Type phi_min = 0.8;
  Type phi_range = 0.18;
  return phi_min + phi_range * phi_raw;
}


// Objective function ---------------------------------------------------------

template<class Type>
Type objective_function<Type>::operator() ()
{

  // inputs -------------------------------------------------------------------

  DATA_MATRIX(events);
  DATA_MATRIX(exposure);
  DATA_STRING(mod_age);
  DATA_STRING(mod_time);
  DATA_MATRIX(X_age);
  DATA_SCALAR(mean_time); /// change
  DATA_SCALAR(sd_time);   /// change

  PARAMETER_SCALAR(intercept);
  PARAMETER_VECTOR(coef_age);
  PARAMETER_SCALAR(log_sd_age);
  PARAMETER_VECTOR(par_time);


  // derived quantities -------------------------------------------------------

  int A = events.nrows();
  int T = events.ncols();
  Type sd_age = exp(log_sd_age);

  
  // log posterior ------------------------------------------------------------
  
  Type ans = 0;

  // prior model for age
  vector<Type> age_effect(A);
  ans -= log_sd_age; // Jacobian
  ans -= logdens_rw2(coef_age, sd_age);
  age_effect = X_age * coef_age;

  // prior model for time

  vector<Type> time_effect(T);
  if (mod_time == "AR1") {
    // phi
    Type logit_phi = par_time[0];
    Type phi_raw = exp(logit_phi) / (1 + exp(logit_phi));
    ans -= logdens_phi_raw(phi_raw);
    phi = transform_phi_raw(phi_raw);
    // alpha
    Type alpha = par_time[1];
    ans -= dnorm(alpha, mean_time, sd_time, true);
    // sd
    Type log_sd = par_time[2];
    Type sd = exp(log_sd);
    ans -= dnorm(sd, Type(0), Type(1), true)
      + log_sd; // Jacobian
    // time effect
    time_effect = par_time.tail(T - 3);
    ans -= dnorm(time_effect[0], (1 - phi) * alpha, sd, true);
    for (int t = 1; t < T; t++)
      ans -= dnorm(time_effect[t], phi * time[t - 1] + (1 - phi) * alpha, sd, true);
  }
  else if (mod_time = "LocalTrend") {
    // TODO - complete
  }
  else
    error("invalid value for 'mod_time'");


  // likelihood

  for (int a = 0; a < A; a++) {
    for (int t = 0; t < T; t++) {
      Type mu = intercept + age_effect[a] + time_effect[t];
      ans -= dpois(events(a,t), exposure(a,t) * exp(mu), true);
    }
  }

  return ans;
}
