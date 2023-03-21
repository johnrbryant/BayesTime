
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
  DATA_VECTOR(consts_age);
  DATA_VECTOR(consts_time);

  PARAMETER(intercept);
  PARAMETER_VECTOR(par_age);
  PARAMETER_VECTOR(par_time);


  // derived quantities -------------------------------------------------------

  int A = events.rows();
  int T = events.cols();

  
  // log posterior ------------------------------------------------------------
  
  Type ans = 0;

  // prior model for age
  
  vector<Type> age_effect(A);
  if ((mod_age == "Spline") || (mod_age == "RW2")) {
    // spline and RW2 identical apart from X_age matrix
    // (which is identity matrix for RW2)
    Type log_sd_age = par_age[0];
    vector<Type> coef_age = par_age.tail(A - 1);
    Type sd_age = exp(log_sd_age);
    ans -= log_sd_age; // Jacobian
    ans -= logdens_rw2(coef_age, sd_age);
    age_effect = X_age * coef_age;
  }
  else {
    error("invalid value for 'mod_age'");
  }

  
  // prior model for time

  vector<Type> time_effect(T);
  if (mod_time == "AR1") {
    Type phi_min = consts_time[0];
    Type phi_max = consts_time[1];
    Type alpha_mean = consts_time[2];
    Type alpha_sd = consts_time[3];
    Type scale_sd = consts_time[4];
    Type logit_phi = par_time[0];
    Type alpha = par_time[1];
    Type log_sd = par_time[2];
    time_effect = par_time.tail(T - 3);
    // phi
    Type phi_raw = exp(logit_phi) / (1 + exp(logit_phi));
    ans -= dbeta(phi_raw, Type(2), Type(2), true)
      + log(phi_raw) + log(1 - phi_raw); // Jabobian
    Type phi = phi_min + phi_raw * (phi_max - phi_min);
    // alpha
    ans -= dnorm(alpha, alpha_mean, alpha_sd, true);
    // sd
    Type sd = exp(log_sd);
    ans -= dnorm(sd, Type(0), scale_sd, true)
      + log_sd; // Jacobian
    // time effect
    ans -= dnorm(time_effect[0], (1 - phi) * alpha, sd, true);
    for (int t = 1; t < T; t++)
      ans -= dnorm(time_effect[t], phi * time_effect[t - 1] + (1 - phi) * alpha, sd, true);
  }
  else if (mod_time == "LocalTrend") {
    Type scale_sd_trend = consts_time[0];
    Type scale_sd_level = consts_time[1];
    Type scale_sd_effect = consts_time[2];
    Type log_sd_trend = par_time[0];
    Type log_sd_level = par_time[1];
    Type log_sd_effect = par_time[2];
    vector<Type> trend = par_time.segment(3, T);
    vector<Type> level = par_time.segment(T + 3, T);
    time_effect = par_time.segment(2 * T + 3, T);
    Type sd_trend = exp(log_sd_trend);
    Type sd_level = exp(log_sd_level);
    Type sd_effect = exp(log_sd_effect);
    ans -= dnorm(sd_trend, Type(0), scale_sd_trend, true)
      + log_sd_trend;
    ans -= dnorm(sd_level, Type(0), scale_sd_level, true)
      + log_sd_level;
    ans -= dnorm(sd_effect, Type(0), scale_sd_effect, true)
      + log_sd_effect;
    ans -= dnorm(trend[0], Type(0), Type(1), true);
    ans -= dnorm(level[0], Type(0), Type(1), true);
    ans -= dnorm(time_effect[0], Type(0), Type(1), true);
    for (int t = 1; t < T; t++) {
      ans -= dnorm(trend[t], trend[t - 1], sd_trend, true);
      ans -= dnorm(level[t], level[t - 1] + trend[t - 1], sd_level, true);
      ans -= dnorm(time_effect[t], level[t], sd_effect, true);
    }
  }
  else {
    error("invalid value for 'mod_time'");
  }

  
  // likelihood

  for (int a = 0; a < A; a++) {
    for (int t = 0; t < T; t++) {
      Type mu = intercept + age_effect[a] + time_effect[t];
      ans -= dpois(events(a,t), exposure(a,t) * exp(mu), true);
    }
  }

  return ans;
}
