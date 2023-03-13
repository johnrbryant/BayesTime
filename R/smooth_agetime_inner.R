
smooth_agetime_inner <- function(nevent, py, nevent, model_time) {
    df <- 5L
    events <- stats::xtabs(count ~ age + sex + year,
                           data = nevent,
                           addNA = TRUE)
    exposure <- stats::xtabs(count ~ age + sex + year,
                             data = py,
                             addNA = TRUE)
    if (model_age == "Spline") {
        param_age <- c(lambda = 0,
                       coef = rep(0, df))
    }
    else
        stop("invalid value for 'model_age'")
    if (model_time == "AR1")
        param_time <- c(alpha = 0,
                        logit_phi = 0,
                        sigma = 0)
    else
        stop("invalid value for 'model_time'")
    
    

data <- data.frame(count = 1, age = seq_along(dimnames(d)$age))
gam_setup <- gam(count ~ s(age, bs = "cs", k = n_knot),
                 data = data,
                 fit = FALSE)

X_agesex <- gam_setup$X[, -1]
S_agesex <- gam_setup$smooth[[1]]$S[[1]]
S_agesex <- as(S_agesex, "dgTMatrix")

data <- list(d = d,
             w = w,
             a_time = 2,
             m_time = 4,
             s_time = 2,
             X_agesex = X_agesex,
             S_agesex = S_agesex)

A <- dim(d)[1]
S <- dim(d)[2]
T <- dim(d)[3]
    
parameters <- list(beta_zero = -5,
                   log_lambda_agesex = 0,
                   delta_agesex = matrix(0, nrow = n_knot - 1L, ncol = S),
                   alpha_time = 0,
                   logit_phi_time = 0,
                   log_sigma_time = 0,
                   beta_time = numeric(T))

dyn.load(p_mod)

f <- MakeADFun(data = data,
               parameters = parameters,
               random = c("delta_agesex",
                          "beta_time"),
               DLL = "mod")

fit <- nlminb(start = f$par,
              objective = f$fn,
              gradient = f$gr)

rep <- sdreport(f,
                bias.correct = TRUE,
                getJointPrecision = TRUE)

mu <- unlist(as.list(rep, "Est"))
Q <- rep$jointPrecision
CH <- Cholesky(Q)
values <- rmvn.sparse(n = n_iter,
                      mu = mu,
                      CH = CH)

inv_logit <- function(x) exp(x) / (1 + exp(x))

mod <- list(beta_zero = values[, grepl("^beta_zero", names(mu))],
            lambda_agesex = exp(values[, grepl("log_lambda_agesex", names(mu))]),
            delta_agesex = values[, grepl("^delta_agesex", names(mu))],
            alpha_time = values[, grepl("^alpha_time", names(mu))],
            phi_time = inv_logit(values[, grepl("logit_phi_time", names(mu))]),
            sigma_time = exp(values[, grepl("log_sigma_time", names(mu))]),
            beta_time = values[, grepl("^beta_time", names(mu))])

delta_agesex <- array(mod$delta_agesex, dim = c(n_iter, n_knot - 1, 2))
beta_agesex <- apply(delta_agesex, c(1, 3), function(x) X_agesex %*% x)
beta_agesex <- aperm(beta_agesex, perm = c(2, 1, 3))

gamma <- array(exp(mod$beta_zero
                   + as.numeric(beta_agesex)
                   + rep(as.numeric(mod$beta_time), each = A * S)),
               dim = c(n_iter, A, S, T))

mod$beta_agesex <- beta_agesex
mod$gamma <- gamma

saveRDS(mod, file = .out)
