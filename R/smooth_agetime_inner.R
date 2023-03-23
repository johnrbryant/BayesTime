
smooth_agetime_inner <- function(nevent, py, model_age, model_time) {
    ## data
    events <- stats::xtabs(count ~ age + sex + year,
                           data = nevent,
                           addNA = TRUE)
    exposure <- stats::xtabs(count ~ age + sex + year,
                             data = py,
                             addNA = TRUE)
    n_age <- nrow(nevent)
    n_time <- ncol(nevent)
    nm_model_age <- get_nm(model_age)
    nm_model_time <- get_nm(model_time)
    X_age <- get_X_age(model = model_age,
                       n_age = n_age)
    consts_age <- get_consts(model_age)
    consts_time <- get_consts(model_time)
    data <- list(events = events,
                 exposure = exposure,
                 nm_model_age = nm_model_age,
                 nm_priomod_time = nm_model_time,
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
                        hessian = TRUE)
    stats::nlminb(start = f$par,
                  objective = f$fn,
                  gradient = f$gr,
                  hessian = f$he)
    rep <- TMB::sdreport(f,
                         bias.correct = TRUE,
                         getJointPrecision = TRUE)
    ans <- new_BayesRates_out(rep = rep,
                             nevent = nevent,
                             py = py,
                             model_age = model_age,
                             model_time = model_time)
    ans
}
