
#' Rename fatty acid variables from PROMISE into scientific format.
#'
#' Fatty acids in the PROMISE cohort are in the form e.g. `ne160`. In the
#' typical data analysis pipeline, these variables may be converted into a long
#' format such that these variables are now rows in a column. These variables
#' will likely end up in plot, so it's necessary to convert them into the
#' appropriate scientific format (e.g. `NE 16:0`, or such simply `16:0`).
#'
#' @param x Vector (or column when through [dplyr::mutate]) of the original
#'   fatty acid names.
#' @param keep.fraction Logical. Whether to keep the fraction abbreviation as a
#'   prefix.
#' @param uppercase Logical. Whether to convert the fraction abbreviation to
#'   upper case.
#'
#' @return Reformatted fatty acid names to be scientifically accurate.
#' @export
#'
#' @examples
#'
#' fattyacids <- c("ne160", "tg141n7", "TotalTG")
#' renaming_fa(fattyacids)
#' renaming_fa(fattyacids, keep.fraction = FALSE)
#' extract_fa_fraction(fattyacids)
#' extract_fa_fraction(fattyacids, uppercase = TRUE)
renaming_fa <- function(fa, keep.fraction = TRUE) {
    fa <- fa %>%
        stringr::str_replace("([:digit:]{2})([:digit:])", "\\1:\\2") %>%
        stringr::str_replace("n([:digit:])(.?)$", " n-\\1\\2")

    if (keep.fraction) {
        fa <- sub("(ne|tg|pl|ce)(\\d)", "\\U\\1 \\2", fa, perl = TRUE)
    } else {
        fa <- stringr::str_replace(fa, "(ne|tg|pl|ce)([:digit:])", "\\2")
    }

    fa
}

#' @describeIn renaming_fa Extract the fatty acid fraction from the PROMISE
#'   fatty acid variables.
extract_fa_fraction <- function(x, uppercase = FALSE) {
    fraction <- stringr::str_extract(x, "ne|pl|tg|ce")
    if (uppercase)
        fraction <- stringr::str_to_upper(fraction)
    fraction
}
