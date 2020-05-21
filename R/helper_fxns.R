## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

#' Chemical symbol-aware unit conversion
#'
#' @param x Numeric value to convert
#' @param from,to Units from/to which to convert. Syntax is identical to
#'   [udunits2::ud.convert()] except that chemical symbols in hard brackets
#'   (e.g. `[N2O]`) can be converted.
#' @import udunits2
#' @return Values of `x` converted to `to` units.
#' @author Alexey Shiklomanov
#' @noRd 
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

#' Fill in the missing values 
#'
#' @param data a datatable emissions or concentration data 
#' @param expected_years the number of years to ensure there data, default set to 1700 to 2500
#' @return a data table with interpolated data
complete_missing_years <- function(data, expected_years = 1700:2500){
  
  req_columns <- c('scenario', 'variable', 'units', 'year')
  missing     <- !req_columns %in% names(data)
  assertthat::assert_that(sum(missing) == 0, msg = paste0('data is missing following columns: ', paste(req_columns[missing], collapse = ', ')))
  
  # TODO this is hacky is there a  better way to do this?
  # Make a data table of the required years we want for each variable. This data table will 
  # be used to  add NA values to the data table containing the inputs. 
  data_no_years <- unique(data[ , list(scenario, variable, units)])
  required_data <- data.table::data.table(scenario = rep(data_no_years$scenario, each = length(expected_years)), 
                                          variable = rep(data_no_years$variable, each = length(expected_years)), 
                                          units = rep(data_no_years$units, each = length(expected_years)), 
                                          year = expected_years)
  
  # This data table contains the data we have values for and NA entries for the years we 
  # will need to interpolate/extrapolate values for. 
  data_NAs <- data[required_data, nomatch = NA, on = c('year', 'variable', 'scenario', 'units')]
  
  # Order and group the data frame in prepration for interpolation.
  data_NAs <- data.table::setorder(data_NAs, variable, units, scenario, year)
  completed_data <- data_NAs[ , value := ifelse(is.na(value), zoo::na.approx(value, na.rm = FALSE, rule = 2), value), keyby=c("variable", "units", "scenario")]
  return(completed_data)
}

#' Drop " " from the begning of strings
#' 
#' Because of the formating of the hector rcmip conversion table some columns begin with unexpected 
#' spaces at the start of the string, this function will remove those spaces so that the resulting 
#' data objects can be joined or merged with other data objects. 
#'
#' @param df a datatable that contains one or more vector columns. 
#' @param cols a vector of the columns that contain strings that may begin with a " " character.
#' @return a data object where if the column starts with a space in a character string it is removed.
#' @noRd 
remove_spaces <- function(df, cols){
  
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(all(cols %in% names(df)))
  
  lapply(cols, function(col){
    
    assertthat::assert_that(is.character(df[[col]]))
    df[[col]] <<- gsub(pattern = '^ ', replacement = '', x = df[[col]])
    
  })
  
  df
  
}
