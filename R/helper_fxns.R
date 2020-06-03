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
#' @return a formatted unit string
#' @author Alexey Shiklomanov
#' @noRd 
parse_chem <- function(unit) {
  # Duplicate the original unit string, one will be modifed while the
  # other retains the original information.
  unit2 <- unit
  
  # Determine if the string contains square brackets which indicates
  # a subscript of a chemical formula. 
  rx <- "\\[.*?\\]"
  m <- regexpr(rx, unit2)
  
  # Update the units by the molar mass of the chemical formula. 
  while (m != -1) {
    regmatches(unit2, m) <- sprintf("(1/%f)", get_molmass(regmatches(unit2, m)))
    m <- regexpr(rx, unit2)
  }
  return(unit2)
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
#' @importFrom zoo na.approx
#' @importFrom assertthat assert_that
complete_missing_years <- function(data, expected_years = 1700:2500){
  
  # Undefined global functions or variables
  scenario <- variable <- value <- NULL
  
  assert_that(assertthat::has_name(x = data, which = c('scenario', 'variable', 'units', 'year')))

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
  completed_data <- data_NAs[ , value:=ifelse(is.na(value), na.approx(value, na.rm = FALSE, rule = 2), value), keyby=c("variable", "units", "scenario")]
  return(completed_data)
}

#' Format the Hector input into the expected Hector input csv file
#'
#' @param x a data table containing the Hector input information
#' @param filename character path for where to save the output. 
#' @return path to the csv file formatted as a Hector input table.  
#' @export
#' @importFrom assertthat assert_that
save_hector_table <- function(x, filename){
  
  # Undefined global functions or variables:
  variable <- value <- NULL
  
  assert_that(data.table::is.data.table(x))
  req_names <- c('scenario', 'year', 'variable', 'units', 'value')
  assert_that(assertthat::has_name(x = x, which = req_names))
  assert_that(length(setdiff(names(x), req_names)) == 0, msg = 'Extra column names.')
  scn_name <- unique(x$scenario)
  assert_that(length(scn_name) == 1)
  
  # Make sure that the data frame contains emissions or concentrations but not both. 
  variable_names <- unique(x$variable)
  emis <- any(grepl(pattern = 'emissions', x = x$variable))
  conc <- any(grepl(pattern = 'constrain', x = x$variable))
  # TODO add some sort of method to make sure that the data frame contains all of the required 
  # emissions or constraints. Otherwise errors will not be triggered until trying to run the 
  # Hector core. 
  assert_that(sum(emis, conc)  == 1, msg = 'input data should include either emissions or constrained data not both.')
  
  # Transform the data frame into the wide format that Hector expects. 
  input_data <- x[ , list(Date = year, variable, value)]
  input_data <- data.table::dcast(input_data, Date ~ variable, value.var = 'value') 
  
  # TODO is there a way to skip saving this intermediate step? 
  # Save the output as csv, latter on it will be read in as a character to make 
  # is possible to add the header information required by Hector. 
  readr::write_csv(input_data, filename, append = FALSE, col_names = TRUE)
  lines <- readLines(filename)
  
  # Format a list of units that will be used to 
  var_units <- unique(x[ , list(variable, units)])
  units_list <- paste(c('; UNITS:', var_units$units), collapse = ', ')
  
  # Add the header information. 
  final_lines <- append(c(paste0('; ', scn_name),
                          # TODO would it be possible to have this print which version of hectordata was used?
                          '; created by hectordata',
                          units_list),
                        lines)
  writeLines(final_lines, filename)
  return(filename)
}
