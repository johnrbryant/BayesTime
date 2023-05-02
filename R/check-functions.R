
## HAS_TESTS
#' Check that the 'age_min' argument
#' is supplied iff it is needed
#'
#' @param has_age_min Logical
#' @param agevar_val A vector with age labels
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_age_min_supplied <- function(has_age_min, agevar_val) {
    has_char_age_labels <- is.character(agevar_val) || is.factor(agevar_val)
    if (has_char_age_labels && !has_age_min)
        stop(gettextf("'%s' uses non-integer age labels, but no value supplied for '%s' argument",
                      "nevent_df",
                      "age_min"),
             call. = FALSE)
    if (!has_char_age_labels && has_age_min)
        stop(gettextf("'%s' uses integer age labels, but value supplied for '%s' argument",
                      "nevent_df",
                      "age_min"),
             call. = FALSE)
    invisible(TRUE)
}


## HAS_TESTS
#' Check that the 'age_width_df' argument 
#' is valid
#'
#' @param age_width_df A data frame with two columns
#' @param agevar_val A vector with age labels
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_age_width_df <- function(age_width_df, agevar_val) {
    checkmate::assert_data_frame(age_width_df,
                                 any.missing = FALSE,
                                 min.rows = 3L,
                                 ncols = 2L,
                                 col.names = "strict")
    if (!setequal(names(age_width_df), c("age", "width")))
        stop(gettextf("colnames for '%s' invalid : should be \"%s\", \"%s\"",
                      "age_width_df",
                      "age",
                      "width"),
             call. = FALSE)
    is_found <- agevar_val %in% age_width_df$age
    i_not_found <- match(FALSE, is_found, nomatch = 0L)
    if (i_not_found > 0L)
        stop(gettextf("variable '%s' in '%s' does not include age group \"%s\"",
                      "age",
                      "age_width_df",
                      agevar_val[[i_not_found]]),
             call. = FALSE)
    width <- age_width_df$width
    val <- checkmate::check_numeric(width,
                                    any.missing = FALSE,
                                    lower = 0,
                                    finite = TRUE)
    if (!isTRUE(val))
        stop(gettextf("problem with variable '%s' in '%s' : %s",
                      "width",
                      "age_width_df",
                      val),
             call. = FALSE)
    invisible(TRUE)
}


#' Check whether 'age_width_df' supplied when it is compulsory
#'
#' @param has_age_width_df Whether value supplied of age_width_df
#' @param agevar_val Values for the age variable
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_age_width_df_supplied <- function(has_age_width_df, agevar_val) {
    has_char_age_labels <- is.character(agevar_val) || is.factor(agevar_val)
    if (has_char_age_labels && !has_age_width_df)
        stop(gettextf("'%s' uses non-integer age labels, but no value supplied for '%s' argument",
                      "nevent_df",
                      "age_width_df"),
             call. = FALSE)
    invisible(TRUE)
}


## HAS_TESTS
#' Check that a data frame has all possible levels of the
#' classification variables
#'
#' @param df A data frame
#' @param nm_df The name of the data frame
#' @param nms_classif_vars
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_all_combn_classif_vars <- function(df, nm_df, nms_classif_vars) {
    classif_vars_curr <- df[nms_classif_vars]
    levels_classif_vars <- lapply(classif_vars_curr, unique)
    classif_vars_all <- expand.grid(levels_classif_vars,
                                    KEEP.OUT.ATTRS = FALSE,
                                    stringsAsFactors = FALSE)
    paste_dots <- function(...) paste(..., sep = ".")
    id_curr <- do.call(paste_dots, classif_vars_curr)
    id_all <- do.call(paste_dots, classif_vars_all)
    is_in_curr <- id_all %in% id_curr
    i_not_in_curr <- match(FALSE, is_in_curr, nomatch = 0L)
    if (i_not_in_curr > 0L) {
        str_missing <- sprintf("    %s: %s",
                               nms_classif_vars,
                               unlist(classif_vars_all[i_not_in_curr, ]))
        str_missing <- paste(str_missing, collapse = "\n")
        stop(gettextf(paste0("'%s' missing combination of classification variables:\n",
                             "%s"),
                      nm_df,
                      str_missing),
             call. = FALSE)
    }
    invisible(TRUE)
}    


#' Check that, after aggregating, 'nevent_df' and 'py_df'
#' have same levels for classification variables
#'
#' @param nevent_df A data frame
#' @param py_df A data frame
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_df_same_levels <- function(nevent_df, py_df) {
    nms_nevent <- names(nevent_df)
    nms_py <- names(py_df)
    nms_classif_vars <- setdiff(nms_nevent, "nevent")
    for (nm in nms_classif_vars) {
        if (!setequal(nevent_df[[nm]], py_df[[nm]]))
            stop(gettextf("variable '%s' in '%s' uses different categories from variable '%s' in '%s'",
                          nm,
                          "nevent_df",
                          nm,
                          "py_df"),
                 call. = FALSE)
    }
    invisible(TRUE)
}


#' Check that if 'py' is 0 or NA, then 'nevent' is also 0 or NA
#'
#' @param df A data frame formed my merging
#' nevent_df and py_df
#'
#' @returns TRUE, invisibly
#'
#' @noRd
check_df_zero_na <- function(df) {
    is_zero_na_nevent <- is.na(df$nevent) | (df$nevent == 0)
    is_zero_na_py <- is.na(df$py) | (df$py == 0)
    is_inconsistent <- !is_zero_na_nevent & is_zero_na_py
    i_inconsistent <- match(TRUE, is_inconsistent, nomatch = 0L)
    if (i_inconsistent > 0L) {
        vals <- df[i_inconsistent, , drop = FALSE]
        vals <- paste(names(vals), vapply(vals, as.character, " "), sep = " = ")
        vals <- paste0("  ", vals)
        vals <- paste(vals, collapse = "\n")
        stop(gettextf("invalid combination of values for '%s' and '%s' :\n%s",
                      "nevent",
                      "py",
                      vals),
             call. = FALSE)
    }
    invisible(TRUE)
}

            
            




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


## HAS_TESTS
#' Check a data frame of events or exposures
#' that does not include a time variable
#'
#' We assume that the name of the data frame
#' has the form "<measurevar>_df".
#' 
#' @param df The input data frame.
#' @param measurevar The name of the measurement variable.
#' @param agevar The name of the age variable.
#' @param byvar The names of additional classification variables.
#' (May have length 0.)
#'
#' @returns TRUE, invisibly.
#'
#' @noRd
check_input_notime_df <- function(df,
                                  measurevar,
                                  agevar,
                                  byvar) {
    timevars <- c("time", "year", "period")
    nm_df <- paste0(measurevar, "_df")
    nms_classif_vars <- c(agevar, byvar)
    ## is data frame with no missing values,
    ## at least 1 row, and at least 3 columns
    checkmate::assert_data_frame(df,
                                 min.rows = 1L,
                                 min.cols = 2L,
                                 .var.name = nm_df)
    nms_df <- names(df)
    ## has all required variables
    for (vname in c(measurevar, nms_classif_vars)) {
        if (!(vname %in% nms_df))
            stop(gettextf("data frame '%s' does not have a variable called \"%s\"",
                          nm_df,
                          vname),
                 call. = FALSE)
    }
    ## check age var
    agevar_val <- df[[agevar]]
    if (is.numeric(agevar_val)) {
        check_age <- checkmate::check_integerish(agevar_val, any.missing = FALSE)
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
            stop(gettextf("variable '%s' in data frame '%s' has NAs",
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
    n_val_age <- length(unique(agevar_val))
    if (n_val_age < 3L)
        stop(gettextf("%s variable ['%s'] in data frame '%s' has only %d unique value(s) : needs at least %d",
                      "age",
                      agevar,
                      nm_df,
                      n_val_age,
                      3L))
    ## check all combinations of classif vars present
    check_all_combn_classif_vars(df = df,
                                 nm_df = nm_df,
                                 nms_classif_vars = nms_classif_vars)
    ## check measure var
    measurevar_val <- df[[measurevar]]
    check_measure <- checkmate::check_numeric(measurevar_val,
                                              any.missing = FALSE,
                                              lower = 0,
                                              finite = TRUE)
    if (!isTRUE(check_measure))
        stop(gettextf("problem with variable '%s' in data frame '%s' : %s",
                      measurevar,
                      nm_df,
                      check_measure),
             call. = FALSE)
    ## check no timevar
    for (timevar in timevars) {
        p <- sprintf("^%s$", timevar)
        is_timevar <- grepl(p, nms_df, ignore.case = TRUE)
        i_timevar <- match(TRUE, is_timevar, nomatch = 0L)
        if (i_timevar > 0L)
            warning(gettextf("'%s' appears to include a time variable [\"%s\"]",
                             nm_df,
                             nms_df[[i_timevar]]))
    }
    ## return TRUE
    invisible(TRUE)
}                                


## HAS_TESTS
#' Check a data frame of events or exposures
#' that includes a time variable
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
check_input_withtime_df <- function(df,
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
            stop(gettextf("data frame '%s' does not have a variable called \"%s\"",
                          nm_df,
                          vname),
                 call. = FALSE)
    }
    ## check age var
    agevar_val <- df[[agevar]]
    if (is.numeric(agevar_val)) {
        check_age <- checkmate::check_integerish(agevar_val, any.missing = FALSE)
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
            stop(gettextf("variable '%s' in data frame '%s' has NAs",
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
    n_val_age <- length(unique(agevar_val))
    if (n_val_age < 3L)
        stop(gettextf("%s variable ['%s'] in data frame '%s' has only %d unique value(s) : needs at least %d",
                      "age",
                      agevar,
                      nm_df,
                      n_val_age,
                      3L))
    ## check time var
    timevar_val <- df[[timevar]]
    check_time <- checkmate::check_integerish(timevar_val, any.missing = FALSE)
    if (!isTRUE(check_time)) {
        if (!is_char_int(timevar_val)) {
            stop(gettextf("problem with variable '%s' in data frame '%s' : %s",
                          timevar,
                          nm_df,
                          check_time),
                 call. = FALSE)
        }
    }
    n_val_time <- length(unique(timevar_val))
    if (n_val_time < 2L)
        stop(gettextf("%s variable ['%s'] in data frame '%s' has only %d unique value(s) : needs at least %d",
                      "time",
                      timevar,
                      nm_df,
                      n_val_time,
                      2L))
    ## check all combinations of classif vars present
    check_all_combn_classif_vars(df = df,
                                 nm_df = nm_df,
                                 nms_classif_vars = nms_classif_vars)
    ## check measure var
    measurevar_val <- df[[measurevar]]
    check_measure <- checkmate::check_numeric(measurevar_val,
                                              any.missing = FALSE,
                                              lower = 0,
                                              finite = TRUE)
    if (!isTRUE(check_measure))
        stop(gettextf("problem with variable '%s' in data frame '%s' : %s",
                      measurevar,
                      nm_df,
                      check_measure),
             call. = FALSE)
    ## return TRUE
    invisible(TRUE)
}
                                
