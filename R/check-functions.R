

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
        stop("'", nm_df, "' does not have a variable called \"",
             vname, "\"")
    }
    ## check age var
    agevar_val <- df[[agevar]]
    if (is.numeric(agevar_val)) {
        val <- checkmate::check_integerish(timevar,
                                           any.missing = FALSE)
        if (!isTRUE(val))
            stop("problem with variable '", agevar, "' in data frame '",
                 nm_df, "' : ", val)
        complete_levels <- seq(from = min(val), to = max(val))
        if (!setequal(val, complete_levels))
            stop("variable '", agevar, "' in data frame '", nm_df,
                 "' does not have complete set of levels")
    }
    else if (is.character(agevar_val) || is.factor(agevar_val)) {
        if (anyNA(agevar_vl))
            stop("variable '", agevar, "' in data frame '", nm_df,
                 "' has NAs")
    }
    else
        stop("variable '", agevar, "' in data frame '", nm_df,
             "' has class \"", class(agevar_val), "\"")
    ## check time var
    timevar_val <- df[[timevar]]
    val <- checkmate::check_integerish(timevar,
                                       any.missing = FALSE)
    if (!isTRUE(val))
        stop("problem with variable '", timevar, "' in data frame '",
             nm_df, "' : ", val)
    ## check measure var
    measurevar_val <- df[[measurevar]]
    val <- checkmate::check_numeric(measurevar_val,
                                    any.missing = FALSE,
                                    lower = 0,
                                    finite = TRUE)
    if (!isTRUE(val))
        stop("problem with variable '", measurevar, "' in data frame '",
             nm_df, "' : ", val)
    ## no duplicates of classification variables
    classif_vars <- df[nms_classif_vars]
    if (any(duplicated(classif_vars)))
        stop("'", nm_df, "' has duplicate values for classifying variables [",
             paste(nms_classif_vars, collapse = ", "), "]")
    ## complete set of classification variables
    n_row_expected <- prod(vapply(classif_vars, function(x) length(unique(x)), 1L))
    n_row_obtained <- nrow(df)
    if (nrow_obtained != nrow_expected)
        stop("'", nm_df, "' missing combinations of classifying variables [",
             paste(nms_classif_vars, collapse = ", "), "]")
    ## return TRUE
    invisible(TRUE)
}
                                    
                              
    


                                 
