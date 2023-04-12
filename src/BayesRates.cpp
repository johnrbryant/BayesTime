
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

  DATA_MATRIX(nevent);
  DATA_MATRIX(py);
  DATA_STRING(class_model_age);
  DATA_STRING(class_model_time);
  DATA_SPARSE_MATRIX(X_age);
  DATA_VECTOR(consts_age);
  DATA_VECTOR(consts_time);

  PARAMETER(intercept);
  PARAMETER_VECTOR(par_age);
  PARAMETER_VECTOR(par_time);


  // derived quantities -------------------------------------------------------

  int A = nevent.rows();
  int T = nevent.cols();

  
  // log posterior ------------------------------------------------------------
  
  Type ans = 0;

  // prior model for age
  
  vector<Type> age_effect(A);
  if ((class_model_age == "BayesRates_model_spline")
      || (class_model_age == "BayesRates_model_rw2")) {
    // spline and RW2 identical apart from X_age matrix
    // (which is identity matrix for RW2)
    Type scale = consts_age[0];
    Type log_sd = par_age[0];
    vector<Type> coef = par_age.tail(A - 2); // lose 2 df due to RW2 prior
    Type sd = exp(log_sd);
    ans -= dnorm(sd, Type(0), scale, true)
      + log_sd; // Jacobian
    ans -= logdens_rw2(coef, sd);
    age_effect = X_age * coef;
  }
  else {
    error("invalid value for 'class_model_age'");
  }

  
  // prior model for time

  vector<Type> time_effect(T);
  if (class_model_time == "BayesRates_model_ar1") {
    Type phi_min = consts_time[0];
    Type phi_max = consts_time[1];
    Type alpha_mean = consts_time[2];
    Type alpha_sd = consts_time[3];
    Type scale_sd = consts_time[4];
    Type logit_phi = par_time[0];
    Type alpha = par_time[1];
    Type log_sd = par_time[2];
    time_effect = par_time.tail(T);
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
  else if (class_model_time == "BayesRates_model_localtrend") {
    Type scale_sd_trend = consts_time[0];
    Type scale_sd_level = consts_time[1];
    Type scale_sd_effect = consts_time[2];
    Type phi_min = consts_time[3];
    Type phi_max = consts_time[4];
    Type log_sd_trend = par_time[0];
    Type log_sd_level = par_time[1];
    Type log_sd_effect = par_time[2];
    Type logit_phi = par_time[3];
    vector<Type> trend = par_time.segment(4, T - 1);
    vector<Type> level = par_time.segment(T + 3, T - 1);
    time_effect = par_time.segment(2 * T + 2, T);
    Type sd_trend = exp(log_sd_trend);
    Type sd_level = exp(log_sd_level);
    Type sd_effect = exp(log_sd_effect);
    ans -= dnorm(sd_trend, Type(0), scale_sd_trend, true)
      + log_sd_trend;
    ans -= dnorm(sd_level, Type(0), scale_sd_level, true)
      + log_sd_level;
    ans -= dnorm(sd_effect, Type(0), scale_sd_effect, true)
      + log_sd_effect;
    Type phi_raw = exp(logit_phi) / (1 + exp(logit_phi));
    ans -= dbeta(phi_raw, Type(2), Type(2), true)
      + log(phi_raw) + log(1 - phi_raw); // Jabobian
    Type phi = phi_min + phi_raw * (phi_max - phi_min);
    ans -= dnorm(trend[0], Type(0), Type(1), true); // TODO - CHANGE TO EQUILIBRIUM VALUE??
    for (int t = 1; t < T - 1L; t++)
      ans -= dnorm(trend[t], phi * trend[t - 1], sd_trend, true);
    ans -= dnorm(level[0], trend[0], sd_level, true); // first element of 'level' is 0
    for (int t = 1; t < T - 1; t++)
      ans -= dnorm(level[t], level[t - 1] + trend[t], sd_level, true);
    ans -= dnorm(time_effect[0], Type(0), sd_effect, true);
    for (int t = 1; t < T; t++)
      ans -= dnorm(time_effect[t], level[t - 1], sd_effect, true);
  }
  else {
    error("invalid value for 'class_model_time'");
  }

  
  // likelihood

  for (int a = 0; a < A; a++) {
    for (int t = 0; t < T; t++) {
      Type mu = intercept + age_effect[a] + time_effect[t];
      ans -= dpois(nevent(a,t), py(a,t) * exp(mu), true);
    }
  }

  return ans;
}
