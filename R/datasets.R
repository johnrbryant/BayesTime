
#' Divorces in China
#'
#' Counts of divorces among respondents
#' in China Family Panel Studies survey, 1980-2018
#'
#' @format A data frame with the following variables:
#' - `age`: Age, in single-year age groups
#' - `sex`: `"Female"` or `"Male"`
#' - `time`: Calendar year
#' - `nevent`: Numbers of divorces during year.
#'
#' @seealso
#' - \code{\link{cn_population}} population at risk
#' - \code{\link{nz_divorces}} New Zealand data
#'
#' @source Tabulation from China Family Panel Studies
#' (CFPS) survey data.
"cn_divorces"


#' Adult population in China
#'
#' Person-years lived by respondents in
#' China Family Panel Studies survey, 1980-2018
#'
#' @format A data frame with the following variables:
#' - `age`: Age, in single-year age groups
#' - `sex`: `"Female"` or `"Male"`
#' - `time`: Calendar year
#' - `py`: Person-years lived
#'
#' @seealso
#' - \code{\link{cn_divorces}} numbers of divorces
#' - \code{\link{nz_population}} New Zealand data
#'
#' @source Tabulation from China Family Panel Studies
#' (CFPS) survey data.
"cn_population"


#' Divorces in New Zealand
#'
#' Counts of divorces by age, sex,
#' and calendar year, in New Zealand, 1992-2021
#'
#' @format A data frame with the following variables:
#' - `age`: Age, in 5-year age groups, 15-19 to 65+
#' - `sex`: `"Female"` or `"Male"`
#' - `time`: Calendar year
#' - `nevent`: Numbers of divorces during year.
#'
#' @seealso
#' - \code{\link{nz_divorces_2020}} data for 2020 only
#' - \code{\link{nz_population}} population at risk
#' - \code{\link{cn_divorces}} Chinese data
#'
#' @source Derived from data in table "Age at divorces by
#' sex (marriages and civil unions) (Annual-Dec)"
#' in the online database Infoshare
#' on the Statistics New Zealand website.
#' Data downloaded on 22 March 2023.
"nz_divorces"


#' Adult population of New Zealand
#'
#' Person-years lived by population aged 15+ by age, sex,
#' and calendar year, in New Zealand, 1992-2021
#'
#' @format A data frame with the following variables:
#' - `age`: Age, in 5-year age groups, 15-19 to 65+
#' - `sex`: `"Female"` or `"Male"`
#' - `time`: Calendar year
#' - `py`: Person-years lived
#'
#' @seealso
#' - \code{\link{nz_population_2020}} data for 2020 only
#' - \code{\link{nz_divorces}} numbers of divorces
#' - \code{\link{cn_population}} Chinese data
#'
#' @source Derived from data in table
#' "Estimated Resident Population by Age and Sex (1991+)
#' (Annual-Dec)" in the online
#' database Infoshare
#' on the Statistics New Zealand website.
#' Data downloaded on 26 March 2023.
"nz_population"


#' Divorces in New Zealand in 2020
#'
#' Counts of divorces by age and sex,
#' in New Zealand, 2020
#'
#' @format A data frame with the following variables:
#' - `age`: Age, in 5-year age groups, 15-19 to 65+
#' - `sex`: `"Female"` or `"Male"`
#' - `nevent`: Numbers of divorces during year.
#'
#' @seealso
#' - \code{\link{nz_divorces}} data for 1992-2021
#' - \code{\link{nz_population_2020}} population at risk
#'
#' @source Derived from data in table "Age at divorces by
#' sex (marriages and civil unions) (Annual-Dec)"
#' in the online database Infoshare
#' on the Statistics New Zealand website.
#' Data downloaded on 22 March 2023.
"nz_divorces_2020"


#' Adult population of New Zealand
#'
#' Person-years lived by population aged 15+
#' by age and sex, in New Zealand, 2020
#'
#' @format A data frame with the following variables:
#' - `age`: Age, in 5-year age groups, 15-19 to 65+
#' - `sex`: `"Female"` or `"Male"`
#' - `py`: Person-years lived
#'
#' @seealso
#' - \code{\link{nz_population}} data 1992-2021
#' - \code{\link{nz_divorces_2020}} numbers of divorces
#'
#' @source Derived from data in table
#' "Estimated Resident Population by Age and Sex (1991+)
#' (Annual-Dec)" in the online
#' database Infoshare
#' on the Statistics New Zealand website.
#' Data downloaded on 26 March 2023.
"nz_population_2020"


#' Widths of age groups used with divorce data
#' for New Zealand
#'
#' Widths of age groups used by
#' \code{\link{nz_divorces}}, \code{\link{nz_population}},
#' \code{\link{nz_divorces_2020}},
#' and \code{\link{nz_population_2020}}.
#'
#' @format A data frame with the following variables:
#' - `age`: Age, in 5-year age groups, 15-19 to 65+
#' - `width`: Width of the age group
#'
#' @seealso Data frames like `nz_age_width_df` are
#' used by function [total_rates()].
"nz_age_width_df"
