
#define EIGEN_PERMANENTLY_DISABLE_STUPID_WARNINGS
#include <TMB.hpp>
#include "init.h"

using namespace Eigen;
using namespace density;

// Objective function ---------------------------------------------------------

template<class Type>
Type objective_function<Type>::operator() ()
{

  // inputs -------------------------------------------------------------------

  DATA_MATRIX(nevent);
  DATA_MATRIX(py);
  DATA_STRING(class_spec_age);
  DATA_STRING(class_spec_time);
  DATA_SPARSE_MATRIX(X_age_parfree);
  DATA_SPARSE_MATRIX(X_age_subspace);
  DATA_SPARSE_MATRIX(X_time);
  DATA_SCALAR(scale_age);
  DATA_SCALAR(scale_time);

  PARAMETER(intercept);
  PARAMETER(log_sd_age);
  PARAMETER(slope_age);
  PARAMETER(log_sd_time);
  PARAMETER(logit_rho_time);
  PARAMETER_VECTOR(parfree_age);
  PARAMETER_MATRIX(parfree_time);

  // derived quantities -------------------------------------------------------

  int A = nevent.rows();         // number of age groups
  int T = nevent.cols();         // number of periods
  int P = X_age_subspace.cols(); // dimension of age subspace
  
  // log posterior ------------------------------------------------------------
  
  Type ans = 0;

  // contribution to log-posterior from parameters for age
  
  if ((class_spec_age == "spec_spline") || (class_spec_age == "spec_rw2")) {
    // spline and RW2 identical apart from X_age matrix
    // (which is identity matrix for RW2)
    Type sd_age = exp(log_sd_age);
    ans -= dnorm(sd_age, Type(0), scale_age, true) + log_sd_age;
    ans -= dnorm(parfree_age, Type(0), sd_age, true).sum();
    ans -= dnorm(slope_age, Type(0), Type(1), true);
  }
  else {
    error("invalid value for 'class_spec_age'");
  }

  // contribution to log-posterior from parameters for time/age-time

  if (class_spec_time == "spec_timefixed") {
    Type sd_time = exp(log_sd_time);
    ans -= dnorm(sd_time, Type(0), scale_time, true) + log_sd_time;
    for (int t = 0; t < T - 1; t++)
      ans -= dnorm(parfree_time(0, t), Type(0), sd_time, true);
  }
  else if (class_spec_time == "spec_timevarying") {
    Type sd_time = exp(log_sd_time);
    ans -= dnorm(sd_time, Type(0), scale_time, true) + log_sd_time;
    Type rho = exp(logit_rho_time) / (1 + exp(logit_rho_time));
    ans -= dbeta(rho, Type(2), Type(2), true) + log(rho) + log(1 - rho);
    matrix<Type> V(A, A);
    for (int a1 = 0; a1 < A; a1++)
      for (int a2 = 0; a2 < A; a2++)
	V(a1, a2) = pow(rho, Type(abs(a1 - a2))) * sd_time * sd_time;
    MVNORM_t<Type> neg_log_density(V);
    for (int t = 0; t < T - 1; t++)
      ans += neg_log_density(parfree_time.col(t));
  }
  else if (class_spec_time == "spec_timenull") {
    // do nothing
  }
  else {
    error("invalid value for 'class_spec_time'");
  }
  
  // form age term

  vector<Type> par_age = X_age_parfree * parfree_age;
  for (int p = 0; p < P; p++) {
    par_age[p] += slope_age * (p - 0.5 * (P - 1));
  }
  vector<Type> age_term = X_age_subspace * par_age;

  // form age-time term  
  
  matrix<Type> agetime_term(A, T);
  if (class_spec_time == "spec_timefixed") {
    vector<Type> row_parfree = parfree_time.row(0);
    vector<Type> row_term = X_time * row_parfree;
    for (int a = 0; a < A; a++) {
      agetime_term.row(a) = row_term;
    }
  }
  else if (class_spec_time == "spec_timevarying") {
    for (int a = 0; a < A; a++) {
      vector<Type> row_parfree = parfree_time.row(a);
      vector<Type> row_term = X_time * row_parfree;
      agetime_term.row(a) = row_term;
    }
  }
  else if (class_spec_time == "spec_timenull") {
    for (int a = 0; a < A; a++)
      agetime_term(a, 0) = Type(0);
  }
  else {
    error("invalid value for 'class_spec_time'");
  }    

  // likelihood

  for (int a = 0; a < A; a++) {
    for (int t = 0; t < T; t++) {
      Type mu = intercept + age_term[a] + agetime_term(a, t);
      ans -= dpois(nevent(a, t), py(a, t) * exp(mu), true);
    }
  }

  return ans;
}
