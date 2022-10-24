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

#' Fill in the missing values 
#'
#' @param data a datatable emissions or concentration data 
#' @param expected_years the number of years to ensure there data, default set to 1700 to 2500
#' @return a data table with interpolated data
#' @importFrom zoo na.approx
#' @import data.table 
complete_missing_years <- function(data, expected_years = 1700:2500){
  
  # Undefined global functions or variables
  scenario <- variable <- value <- NULL
  
  assertthat::assert_that(assertthat::has_name(x = data, which = c('scenario', 'variable', 'units', 'year')))
  
  # Make a data table of the required years we want for each variable. This data table will 
  # be used to  add NA values to the data table containing the inputs. 
  data_no_years <- unique(data[ , list(scenario, variable, units)])
  required_data <- data.table(scenario = rep(data_no_years$scenario, each = length(expected_years)), 
                              variable = rep(data_no_years$variable, each = length(expected_years)), 
                              units = rep(data_no_years$units, each = length(expected_years)), 
                              year = expected_years)
  
  # This data table contains the data we have values for and NA entries for the years we 
  # will need to interpolate/extrapolate values for. 
  data_NAs <- data[required_data, nomatch = NA, on = c('year', 'variable', 'scenario', 'units')]
  
  # Order and group the data frame in preparation for interpolation.
  data_NAs <- setorder(data_NAs, variable, units, scenario, year)
  completed_data <- data_NAs[ , value:=ifelse(is.na(value), zoo::na.approx(value, na.rm = FALSE, rule = 2), value), keyby=c("variable", "units", "scenario")]
  return(completed_data)
}

#' Formate the carbon cycle emissions, they must be postive values 
#'
#' @param dat a data table emissions & concentration data 
#' @return emissions and concentrations data frame with the correctly formatted carbon cycle emissions aka no negative emissions
#' @importFrom assertthat assert_that
process_carbon_cycle_emissions <- function(dat){
  
  # Silence global variables 
  variable <- daccs_uptake <- ffi_emissions <- luc_uptake <- luc_emissions <- NULL
  
  # Check to make sure that the inpout had the correct names & variables. 
  assertthat::assert_that(assertthat::has_name(x = dat, which = c("year", "variable","units", "value", "scenario")))
  assertthat::assert_that(all(c("ffi_emissions", "luc_emissions") %in% unique(dat[['variable']])))
  assertthat::assert_that(is.data.table(dat))
  
  
  # Subset the input data to the two sources of carbon emissions.
  carbon_emissions <- unique(dat[dat[ , variable %in% c("ffi_emissions", "luc_emissions")]])
  wide_data <- dcast(carbon_emissions,  year + scenario + units ~ variable)
  
  # Format the fossil fuel emissions and land use change emissions so that the values 
  # are postivie, if the emissions are negative read them in as dacccs or land uptake. 
  wide_data[, daccs_uptake := ifelse(ffi_emissions <= 0, -1 * ffi_emissions, 0)]
  wide_data[, ffi_emissions := ifelse(ffi_emissions >= 0, ffi_emissions, 0)]
  wide_data[, luc_uptake := ifelse(luc_emissions <= 0, -1 * luc_emissions, 0)]
  wide_data[, luc_emissions := ifelse(luc_emissions >= 0, luc_emissions, 0)]
  
  # Add the new carbon emissions to the emissions data frame and return output. 
  rbind(melt(wide_data, id.vars = c("scenario", "year", "units")) , 
        dat[dat[ , !variable %in% c("ffi_emissions", "luc_emissions")]]) -> 
    out
  
  return(out)
}

