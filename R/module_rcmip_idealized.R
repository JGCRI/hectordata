#' Generate the idealized ini & csv files from the rcmip source
#' 
#' Creates ini and csv tables for the following scenarios based on the RCMIP protocols 
#' 1pctCO2, 1pctCO2-4xext, abrupt-0p5xCO2, abrupt-2xCO2, piControl, abrupt-4xCO2, 1pctCO2-cdr
#'
#' @return str of the ini files
#' @export
#' @importFrom assertthat assert_that
module_rcmip_idealized <- function(){
  
  # Silence package checks 
  template_ini <- value <- variable <- hector_unit <- hector_variable <- ..cols <- Region <- 
    scenarios <- ..yr_cols <- Variable <- Scenario <- NULL
  
  # Load of the data files.
  data <- load_data(c("rcmip-concentrations-annual-means-v5-1-0.csv", 
                      # The 1% CO2 CDR scenario was included in older versions of the RCMIP protocol, 
                      # use the CO2 concentrations from one of these runs to complete a Hector csv table 
                      # for this run. 
                      "rcmip_phase-1_fair-1.5-default-1pctCO2-cdr_v1-0-0.csv"))
  
  # Make a data frame of a full scenario, this scenario will be populated with the values for 
  # atmospheric CO2 concentrations from the 1pct CO2 cdr run. 
  cdr_df <- as.data.frame(data$`rcmip-concentrations-annual-means-v5-1-0.csv`[Scenario == "1pctCO2"])
  cdr_df$Scenario <- "1pctCO2-cdr"
  cols_to_keep <- names(cdr_df)
  
  cdr_co2_vals <- data$`rcmip_phase-1_fair-1.5-default-1pctCO2-cdr_v1-0-0.csv`[Variable == "Atmospheric Concentrations|CO2",]
  yr_cols <- names(cdr_co2_vals)[grepl(pattern = "X", x = names(cdr_co2_vals))]
  vals <- cdr_co2_vals[Variable == "Atmospheric Concentrations|CO2", ..yr_cols]
  cdr_df[(cdr_df$Variable == "Atmospheric Concentrations|CO2" & cdr_df$Region == "World"), names(cdr_df) %in% yr_cols] <- vals
  
  
  # The scenarios that are being processed in this module 
  scenario <- c("1pctCO2", "1pctCO2-4xext", "abrupt-0p5xCO2", "abrupt-2xCO2", "piControl", "abrupt-4xCO2", "1pctCO2-cdr")
  
  # Make sure data exists for the scenario(s) selected to process.
  data_scns <- unique(c(data$`rcmip-concentrations-annual-means-v5-1-0.csv`$Scenario, cdr_df$Scenario))
  available <- scenario %in% data_scns
  assert_that(all(available), msg = paste0('The following scenarios cannot be processed: ', paste(scenarios[!available], collapse = ', ')))
  
  # Concatenate the long emissions and concentration data tables together and subset so that
  # only the scenarios of interest will be converted.
  raw_inputs <- rbind(data$`rcmip-concentrations-annual-means-v5-1-0.csv`, cdr_df, fill = TRUE)[Scenario %in% scenario  & Region == "World"]
  
  # The idealized runs are only driven with CO2 concentrations, add the emission species that
  vars_to_add <- data.table::data.table(rcmip_variable = c("Emissions|CO2|Fossil and Industrial", "Emissions|CO2|AFOLU",
                                                           "Radiative Forcing|Anthropogenic|Albedo Change",
                                                           "Emissions|Sulfur", "Radiative Forcing|Natural|Volcanic", "Emissions|NOx", "Emissions|CO",
                                                           "Emissions|VOC", "Emissions|BC", "Emissions|NH3", "Emissions|OC",
                                                           "Atmospheric Concentrations|F-Gases|HFC|HFC125", "Atmospheric Concentrations|F-Gases|HFC|HFC134a",
                                                           "Atmospheric Concentrations|F-Gases|HFC|HFC143a",
                                                           "Atmospheric Concentrations|F-Gases|HFC|HFC227ea", "Atmospheric Concentrations|F-Gases|HFC|HFC23",
                                                           "Atmospheric Concentrations|F-Gases|HFC|HFC245fa", "Atmospheric Concentrations|F-Gases|HFC|HFC32",
                                                           "Atmospheric Concentrations|F-Gases|HFC|HFC365mfc", "Atmospheric Concentrations|F-Gases|HFC|HFC4310mee",
                                                           "Atmospheric Concentrations|F-Gases|PFC|C2F6", "Atmospheric Concentrations|F-Gases|PFC|CF4",
                                                           "Atmospheric Concentrations|F-Gases|SF6", "Atmospheric Concentrations|Montreal Gases|CCl4",
                                                           "Atmospheric Concentrations|Montreal Gases|CFC|CFC11", "Atmospheric Concentrations|Montreal Gases|CFC|CFC113",
                                                           "Atmospheric Concentrations|Montreal Gases|CFC|CFC114", 'Atmospheric Concentrations|Montreal Gases|CFC|CFC115',
                                                           'Atmospheric Concentrations|Montreal Gases|CFC|CFC12', 'Atmospheric Concentrations|Montreal Gases|CH3Br',
                                                           'Atmospheric Concentrations|Montreal Gases|CH3CCl3', 'Atmospheric Concentrations|Montreal Gases|CH3Cl',
                                                           'Atmospheric Concentrations|Montreal Gases|Halon1211', 'Atmospheric Concentrations|Montreal Gases|Halon1301',
                                                           'Atmospheric Concentrations|Montreal Gases|Halon2402', 'Emissions|F-Gases|HFC|HFC125', 'Emissions|F-Gases|HFC|HFC134a',
                                                           'Emissions|F-Gases|HFC|HFC143a', 'Emissions|F-Gases|HFC|HFC227ea', 'Emissions|F-Gases|HFC|HFC23',
                                                           'Emissions|F-Gases|HFC|HFC245fa', 'Emissions|F-Gases|HFC|HFC32', 'Emissions|F-Gases|HFC|HFC365mfc',
                                                           'Emissions|F-Gases|HFC|HFC4310mee', 'Emissions|F-Gases|PFC|C2F6', 'Emissions|F-Gases|PFC|CF4',
                                                           'Emissions|F-Gases|SF6', 'Emissions|Montreal Gases|CCl4', 'Emissions|Montreal Gases|CFC|CFC11',
                                                           'Emissions|Montreal Gases|CFC|CFC113', "Emissions|Montreal Gases|CFC|CFC114", "Emissions|Montreal Gases|CFC|CFC115",
                                                           "Emissions|Montreal Gases|CFC|CFC12", "Emissions|Montreal Gases|CH3Br", "Emissions|Montreal Gases|CH3CCl3",
                                                           "Emissions|Montreal Gases|CH3Cl", "Emissions|Montreal Gases|Halon1211", "Emissions|Montreal Gases|Halon1301",
                                                           "Emissions|Montreal Gases|Halon2402", "Atmospheric Concentrations|Montreal Gases|HCFC141b",
                                                           "Atmospheric Concentrations|Montreal Gases|HCFC142b", "Atmospheric Concentrations|Montreal Gases|HCFC22",
                                                           "Emissions|Montreal Gases|HCFC141b", "Emissions|Montreal Gases|HCFC142b", "Emissions|Montreal Gases|HCFC22",
                                                           "Emissions|N2O", "Emissions|CH4"))
  
  cols <- c("rcmip_variable", "rcmip_units")
  vars_to_add <- vars_to_add[data.table::as.data.table(hectordata::rcmipCMIP6_conversion)[ , ..cols], nomatch=0, on = "rcmip_variable" ]
  vars_to_add <- unique(vars_to_add)
  vars_to_add$join <- "on"
  
  cols <- c("Model", "Scenario", "Region", "Activity_Id", "Mip_Era")
  scn_info <- raw_inputs[, ..cols ]
  scn_info <- unique(scn_info)
  scn_info$join <- "on"
  
  rcmip_info <- merge(vars_to_add, scn_info, on ="join", all = TRUE, allow.cartesian=TRUE)[, -"join"]
  names(rcmip_info) <- c("Variable", "Unit", "Model", "Scenario", "Region", "Activity_Id", "Mip_Era")
  
  year_cols <- names(raw_inputs)[grepl(pattern = "^X[[:digit:]]{4}", x = names(raw_inputs))]
  zero_emissions <- data.table::data.table(matrix(rep(x = 0, length.out = length(year_cols)), nrow = 1, dimnames = list(NULL, year_cols)))
  zero_emissions$join <- "on"
  rcmip_info$join  <- "on"
  new_inputs <- merge(rcmip_info, zero_emissions, on ="join", all = TRUE, allow.cartesian=TRUE)[, -"join"]
  
  raw_inputs <- rbind(raw_inputs, new_inputs)
  
  
  # Determine the columns that contain identifier information, such as the model, scenario, region, variable,
  # unit, etc. These columns will be used to transform the data from being wide to long so that each row
  # corresponds to concentration for a specific year.
  id_vars <- which(!grepl(pattern = "^X[[:digit:]]{4}", x = names(raw_inputs)))
  long_inputs <- data.table::melt.data.table(data = raw_inputs, id.vars = id_vars,
                                             variable.name = "year", value.name = "value",
                                             variable.factor = FALSE)
  
  # Convert the year to an integer.
  long_inputs <- long_inputs[ , year :=  as.integer(gsub(pattern = "X", replacement = "", x = year))]
  
  # Add the conversion data table information to the raw data with an inner join so that only variables that
  # have conversion information will be converted. The raw inputs include values for variables that Hector
  # does not have that are required by other classes of simple climate models.
  conversion_table <- hectordata::rcmipCMIP6_conversion
  input_conversion_table <- stats::na.omit(long_inputs[conversion_table, on = c('Variable' = 'rcmip_variable'), nomatch=NA])
  
  # Convert the value column from RCMIP units to Hector units.
  # This step may take a while depending on the number of scenarios being
  # processed.
  unlist(mapply(ud_convert2,
         x = input_conversion_table$value,
         from = input_conversion_table$rcmip_udunits,
         to = input_conversion_table$hector_udunits,
         SIMPLIFY = FALSE)) -> 
    new_values
  
  # Create the data table of the inputs that have the Hector relevant variable, units, and values by selecting
  # and renaming the columns from the input conversion table. Then add the converted values.
  converted_idealized <- input_conversion_table[, list(Scenario, year, hector_variable, hector_unit)]
  names(converted_idealized) <- c('scenario', 'year', 'variable', 'units')
  converted_idealized[['value']] <- new_values
  
  # Add in the constant emissions
  
  # Interpolate the data over the missing years.
  complete_data <- complete_missing_years(converted_idealized, expected_years = YEARS)
  
  final_data <- process_carbon_cycle_emissions(complete_data)
  
  # From 1745 to 1850 set the values to the pre industrial concentration value. 
  abrupt_scns <- unique(final_data$scenario)[grepl(pattern = "abrupt",  x =  unique(final_data$scenario))]
  final_data <- final_data[scenario %in% abrupt_scns & year %in% 1745:1850 & variable == "CO2_constrain", value := 284.317]
  
  
  # Format and save the emissions and concentration constraints in the csv files
  # in the proper Hector table input file.
  sapply(X = split(final_data, final_data$scenario) , FUN = write_hector_csv, write_to = TABLES_DIR, source = "rcmip", USE.NAMES = TRUE) ->
    files
  
  
  make_idealized_inis <- function(f){
    
    name <- gsub(x = basename(f), pattern = "_emiss-constraints_rf.csv", replacement = "")
    
    new_path <- file.path('tables', basename(f))
    new_ini  <- replace_csv_string(template_ini, replacement_path = new_path, run_name = name)
    updated_ini_lines <-activate_input_variables(new_ini, vars = c("CO2_constrain", "CH4_constrain", "N2O_constrain"))
    
    write_to <- dirname(dirname(f))
    ini_path <- file.path(write_to, paste0(name, '.ini'))
    writeLines(updated_ini_lines, ini_path)
    
    return(ini_path)
    
  }
  
  out <- unlist(lapply(X = files, FUN = make_idealized_inis))
  
  return(out)
  
  
}

