
## HAS_TESTS
#' Check that a scalar is greater than 0
#'
#' @param x Scalar
#' @param nm Name to be used in error messages
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_gt_zero <- function(x, nm) {
    if (x <= 0)
        stop(gettextf("'%s' is less than or equal to %d",
                      nm,
                      0L),
             call. = FALSE)
    invisible(TRUE)
}


## NO_TESTS
#' Check a data frame of events or exposures
#'
#' We assume that the name of the data frame
#' has the form "<measurevar>_df".
#' 
#' @param df The input data frame.
#' @param measurevar The name of the measurement variable.
#' @param agevar The name of the age variable.
#' @param timevar The name of the time variable.
#' @param byvar The names of additional classification variables.
#' (May have length 0.)
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_input_df <- function(df,
                           measurevar,
                           agevar,
                           timevar,
                           byvar) {
    nm_df <- paste0(measurevar, "_df")
    nms_classif_vars <- c(agevar, timevar, byvar)
    ## is data frame with no missing values,
    ## at least 1 row, and at least 3 columns
    checkmate::assert_data_frame(df,
                                 min.rows = 1L,
                                 min.cols = 3L,
                                 .var.name = nm_df)
    nms_df <- names(df)
    ## has all required variables
    for (vname in c(measurevar, nms_classif_vars)) {
        if (!(vname %in% nms_df))
            stop(gettextf("'%s' does not have a variable called \"%s\"",
                          nm_df,
                          vname),
                 call. = FALSE)
    }
    ## check age var
    agevar_val <- df[[agevar]]
    if (is.numeric(agevar_val)) {
        check_age <- checkmate::check_integerish(agevar_val,
                                                 any.missing = FALSE,
                                                 .var.name = agevar)
        if (!isTRUE(check_age))
            stop(gettextf("problem with variable '%s' in data frame '%s' : %s",
                          agevar,
                          nm_df,
                          check_age),
                 call. = FALSE)
        complete_levels <- seq(from = min(agevar_val), to = max(agevar_val))
        if (!setequal(agevar_val, complete_levels))
            stop(gettextf(paste("variable '%s' in data frame '%s' does not have",
                                "a complete set of levels"),
                          agevar,
                          nm_df),
                 call. = FALSE)
    }
    else if (is.character(agevar_val) || is.factor(agevar_val)) {
        if (anyNA(agevar_val))
            stop(gettextf("variable '%s'in data frame '%s' has NAs",
                          agevar,
                          nm_df),
                 call. = FALSE)
    }
    else
        stop(gettextf("variable '%s' in data frame '%s' has class \"%s\"",
                      agevar,
                      nm_df,
                      class(agevar_val)),
             call. = FALSE)
    ## check time var
    timevar_val <- df[[timevar]]
    check_time <- checkmate::check_integerish(timevar,
                                              any.missing = FALSE,
                                              .var.name = timevar)
    if (!isTRUE(check_time))
        stop(gettextf("problem with variable '%s' in data frame '%s' : %s",
                      timevar,
                      nm_df,
                      check_time),
             call. = FALSE)
    ## check measure var
    measurevar_val <- df[[measurevar]]
    check_measure <- checkmate::check_numeric(measurevar_val,
                                              any.missing = FALSE,
                                              lower = 0,
                                              finite = TRUE,
                                              .var.name = measurevar)
    if (!isTRUE(check_measure))
        stop(gettextf("problem with variable '%s' in data frame '%s' : %s",
                      measurevar,
                      nm_df,
                      check_measure),
             call. = FALSE)
    ## no duplicates of classification variables
    classif_vars <- df[nms_classif_vars]
    if (any(duplicated(classif_vars)))
        stop(gettextf("data frame '%s' has duplicate values for classifying variables [%s]",
                      nm_df,
                      paste(nms_classif_vars, collapse = ", ")),
             call. = FALSE)
    ## complete set of classification variables
    get_n_levels <- function(x) length(unique(x))
    n_row_expected <- prod(vapply(classif_vars, get_n_levels, 1L))
    n_row_obtained <- nrow(df)
    if (n_row_obtained != n_row_expected)
        stop(gettextf("data frame '%s' missing combinations of classifying variables [%s]",
                      nm_df,
                      paste(nms_classif_vars, collapse = ", ")),
             call. = FALSE)
    ## return TRUE
    invisible(TRUE)
}
                                
