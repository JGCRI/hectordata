#' Chemical symbol-aware unit conversion
#'
#' @param x Numeric value to convert
#' @param from,to Units from/to which to convert. Syntax is identical to
#'   [udunits2::ud.convert()] except that chemical symbols in hard brackets
#'   (e.g. `[N2O]`) can be converted.
#' @import udunits2
#' @return Values of `x` converted to `to` units.
#' @author Alexey Shiklomanov
#' @export
ud_convert2 <- function(x, from, to) {
    udunits2::ud.convert(x, parse_chem(from), parse_chem(to))
}

#' Chemical symbol-aware unit conversion
#'
#' @param unit a string of unit information and chemical information (C, N, N2O, ect.)
#' @return a formated unit string
#' @author Alexey Shiklomanov
#' @noRd 
parse_chem <- function(unit) {
    unit2 <- unit
    rx <- "\\[.*?\\]"
    m <- regexpr(rx, unit2)
    while (m != -1) {
        regmatches(unit2, m) <- sprintf(
            "(1/%f)",
            get_molmass(regmatches(unit2, m))
        )
        m <- regexpr(rx, unit2)
    }
    unit2
}

#' Calculate the molar mass for a chemical species
#'
#' @param s a string of chemical compound or species
#' @return the molar mass
#' @author Alexey Shiklomanov
#' @noRd
get_molmass <- function(s) {
    biogas::molMass(gsub("\\[|\\]", "", s))
}
