
write_to <- here::here("inst")
# to do, need to modularlize
  # inpputs
  # outputs
  # should have some way of either checking to see if the rcmip data has already been donloaded or
  # do a remote temporarty ddownlaod...

scenario <- c("ssp370", "ssp434", "ssp460", "ssp119", "ssp126", "ssp245", "ssp534-over", "ssp585")

years <- 1750:2300

#emiss_data <- data.table::as.data.table(readr::read_csv(url(link)))
link <- "./inst/ds/rcmip-emissions-annual-means-v5-1-0.csv"
emiss_data <- data.table::as.data.table(readr::read_csv(link))


#link <- "https://zenodo.org/record/3779281/files/rcmip-concentrations-annual-means-v4-0-0.csv?download=1"
# conc_data <- data.table::as.data.table(readr::read_csv(url(link)))
link <- "./inst/ds/rcmip-concentrations-annual-means-v5-1-0.csv"
conc_data <- data.table::as.data.table(readr::read_csv(link))

link <- "./inst/ds/rcmip-radiative-forcing-annual-means-v5-1-0.csv"
rf_data <- data.table::as.data.table(readr::read_csv(link))


# Block 1: convert RCMIP emissions, concentrations & RF to the appropriate values.

# Make sure data exists for the scenario(s) selected to process.
data_scns <- unique(c(emiss_data$Scenario, conc_data$Scenario, rf_data$Scenario))
available <- scenario %in% data_scns
assert_that(all(available), msg = paste0('The following scenarios cannot be processed: ', paste(scenarios[!available], collapse = ', ')))

# Concatenate the long emissions and concentration data tables together and subset so that
# only the scenarios of interest will be converted.
raw_inputs <- rbind(conc_data, emiss_data, rf_data, fill = TRUE)[Scenario %in% scenario  & Region == "World"]

# Determine the columns that contain identifier information, such as the model, scenario, region, variable,
# unit, ect. These columns will be used to transform the data from being wide to long so that each row
# corresponds to concentration for a specific year.
id_vars <- which(!grepl(pattern = "^[[:digit:]]{4}", x = names(raw_inputs)))
long_inputs <- data.table::melt.data.table(data = raw_inputs, id.vars = id_vars,
                                           variable.name = "year", value.name = "value",
                                           variable.factor = FALSE)
# Convert the year to an integer.
long_inputs <- long_inputs[ , year :=  as.integer(year)]

# Add the conversion data table information to the raw data with an inner join so that only variables that
# have conversion information will be converted. The raw inputs include values for variables that Hector
# does not have that are required by other classes of simple climate models.
conversion_table <- hectordata::rcmipCMIP6_conversion
input_conversion_table <- stats::na.omit(long_inputs[conversion_table, on = c('Variable' = 'rcmip_variable'), nomatch=NA])

# Convert the value column from RCMIP units to Hector units.
# This step may take a while depending on the number of scenarios being
# processed.
mapply(ud_convert2,
       x = input_conversion_table$value,
       from = input_conversion_table$rcmip_udunits,
       to = input_conversion_table$hector_udunits,
       SIMPLIFY = FALSE) %>%
  unlist ->
  new_values

# Create the data table of the inputs that have the Hector relevant variable, units, and values by selecting
# and renaming the columns from the input conversion table. Then add the converted values.
converted_cmip6 <- input_conversion_table[, list(Scenario, year, hector_variable, hector_unit)]
names(converted_cmip6) <- c('scenario', 'year', 'variable', 'units')
converted_cmip6[['value']] <- new_values

# Interpolate the data over the missing years. 
complete_data <- complete_missing_years(converted_cmip6, expected_years = years)

# Format hector inputs so that negative carbon emissions are properly 
# categorized into daccs and land uptake. 
final_data <- process_carbon_cycle_emissions(complete_data) 

# Format and save the emissions and concentration constraints in the csv files 
# in the proper Hector table input file. 
split(final_data, final_data$scenario) %>%  
  sapply(write_hector_csv, write_to = file.path(write_to, 'input', 'tables'), USE.NAMES = FALSE) -> 
  files 


# Copy over the volcanic RF to create the full 
emissions_dir <- unique(dirname(files))

# Generate the ini files corresponding to the new csv files. 
inis <- make_new_ini(files)
