
#' Fit model
#'
#' Workhorse function for main user-visible function
#' `smooth.agetime()`. Given matrices of
#' events and person-years at risk, plus prior
#' models for age and time, approximate the
#' posterior distribution.
#'
#' @param nevent A matrix of counts of events by age by time
#' @param py A matrix of person-years of exposure, by age and time
#' @param model_age An object of class `"BayesRates_model"`
#' describing the prior model for age effects
#' @param model_time An object of class `"BayesRates_model"`
#' describing the prior model for time effects
#' 
#' @returns TODO - DESCRIBE
#'
#' @noRd
smooth_agetime_inner <- function(nevent, py, model_age, model_time) {
    n_age <- nrow(nevent)
    n_time <- ncol(nevent)
    class_model_age <- class(model_age)[[1L]]
    class_model_time <- class(model_time)[[1L]]
    X_age <- get_X_age(model = model_age,
                       n_age = n_age)
    consts_age <- get_consts(model_age)
    consts_time <- get_consts(model_time)
    data <- list(nevent = nevent,
                 py = py,
                 class_model_age = class_model_age,
                 class_model_time = class_model_time,
                 X_age = X_age,
                 consts_age = consts_age,
                 consts_time = consts_time)
    ## parameters
    par_age <- get_par(model = model_age, n_effect = n_age)
    par_time <- get_par(model = model_time, n_effect = n_time)
    parameters = list(intercept = 0,
                      par_age = par_age,
                      par_time = par_time)
    ## optimisation
    f <- TMB::MakeADFun(data = data,
                        parameters = parameters,
                        DLL = "BayesRates",
                        hessian = TRUE,
                        silent = TRUE)
    stats::nlminb(start = f$par,
                  objective = f$fn,
                  gradient = f$gr,
                  hessian = f$he)
    ## extract results
    rep <- TMB::sdreport(f, getReportCovariance = TRUE)
    list(mean = as.list(rep, "Est"),
         var = rep$cov.fixed)
}
