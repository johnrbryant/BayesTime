
smooth_agetime_inner <- function(nevent, py, priormod_age, priormod_time) {
    ## data
    events <- stats::xtabs(count ~ age + sex + year,
                           data = nevent,
                           addNA = TRUE)
    exposure <- stats::xtabs(count ~ age + sex + year,
                             data = py,
                             addNA = TRUE)
    nm_priormod_age <- get_nm_priormod(priormod_age)
    nm_priormod_time <- get_nm_priormod(priormod_time)
    X_age <- get_X(priormod_age)
    consts_age <- get_consts(priormod_age)
    consts_time <- get_consts(priormod_time)
    data <- list(events = events,
                 exposure = exposure,
                 nm_priormod_age = nm_priormod_age
                 nm_priomod_time = nm_priormod_time,
                 X_age = X_age,
                 consts_age = consts_age,
                 consts_time = consts_time)
    ## parameters
    n_age <- nrow(nevent)
    n_time <- ncol(nevent)
    n_hyper_age <- get_n_hyper(priormod_age)
    n_hyper_time <- get_n_hyper(priormod_time)
    par_age <- rep.int(0, times = n_age + n_hyper_age - 2L) ## need n_age-2 for random walk
    par_time <- rep.int(0, times = n_time + n_hyper_time)
    parameters = list(intercept = 0,
                      par_age = par_age,n
                      par_time = par_time)
    ## optimisation
    f <- TMB::MakeADFun(data = data,
                        parameters = parameters,
                        DLL = "mod")
    stats::nlminb(start = f$par,
                  objective = f$fn,
                  gradient = f$gr)
    rep <- TMB::sdreport(f,
                         bias.correct = TRUE,
                         getJointPrecision = TRUE)
    ans <- new_BayesRates_out(rep = rep,
                             nevent = nevent,
                             py = py,
                             priormod_age = priormod_age,
                             priormod_time = priormod_time)
    ans
}
