

#' Process the inputs materials from Hector v2.5 
#' 
#' See release Hector v2.5 commit 62381e7
#'
#' @param scenarios_to_process string vector of the scenario names to process
#' @import data.table
#' @return data frame of Hector inputs for multiple scenarios 
#' @importFrom magrittr %>%
#' @export
process_v25_data <- function(scenarios_to_process=NULL){
  
  . <- scenario <- value <- v3_variable <- variable <- Date <- SV <-  NULL
  
  v25_dir <- find_input_dir(info_source = "v25")
  
  # Import v2.5 data files change from wide to long format. 
  rcp_files <- list.files(v25_dir, pattern = "RCP", full.names = TRUE) 
  do.call(lapply(rcp_files, function(f){ 
    # Save a copy of the long data frame
    data <- rename(melt.data.table(data = as.data.table(utils::read.csv(f, comment.char = ";")),
                                   id.vars = "Date", variable.name = "variable",
                                   value.name = "value", variable.factor = FALSE), c("year" = "Date"))
    scn <- gsub(x = basename(f), pattern = "_emissions.csv", replacement = "")
    data$scenario <- tolower(scn)
    data
  }), what = "rbind") -> 
    long_data
  
  if(is.null(scenarios_to_process)){
    scenarios_to_process <- c("rcp26", "rcp45", "rcp6", "rcp85")
  }
  
  # Map from the old variable names to the new ones
  long_data %>%
    rename(cols = c("v25_variable" = "variable")) %>%
    .[hectordata::conversion_table$v25, on = "v25_variable"] %>%
    .[ , list(scenario, year, value, variable = v3_variable)] ->
    mapped_data
  
  # Save a copy of the emissions from Hector v25 that have the
  # correct name already.
  data <- long_data[long_data$variable %in% REQUIRED_EMISSIONS,  ]
  
  # Use the helper function to add the uptake columns for ffi and luc.
  data$units <- NA # Add the required columns for the helper functions.
  data <- process_carbon_cycle_emissions(data)[ , list(year, variable, value, scenario)]
  
  # Construct a data frame of 0 NH3 emissions for each scenario, since that was added to hector v3.
  missing_nh3_data <- do.call(lapply(X = scenarios_to_process, function(scn){
    data.table(year = unique(data$year),
               variable = hector::EMISSIONS_NH3(),
               value = 0, 
               scenario = scn)
  }), what = "rbind")
  
  # Read in the volcanic data and save a copy for each scenario.
  sv_data_perscn <- do.call(lapply(X = scenarios_to_process, function(scn){
    
    d <-  as.data.table(utils::read.csv(list.files(find_input_dir(info_source = "v25"),
                                            pattern = "vol", full.names = TRUE), comment.char = ";"))
    d <- d[,list(year = Date, value = SV)]
    d$scenario <- scn
    d$variable <- "SV"
    return(d)
  }), what = "rbind")
  
  # Store all of the emissions data in a single data table before loading 
  data <- rbind(mapped_data, data, missing_nh3_data, sv_data_perscn)
  data$units <- ""
  final_data <- complete_missing_years(data = data, expected_years = 1745:2500)
  
  # Save intermediate data.
  ofile <- file.path(INTERMEDIATE_DIR, "hectorv2_data.csv")
  utils::write.csv(x = final_data, file = ofile, row.names = FALSE)
  return(final_data)
  
}


#' Update the ini files and csv tables that were used with the v2.5 Hector release
#' 
#' TODO needs better way to determine dependencies 
#' 
#' @param scenarios_to_process string vector of the scenario names to process
#' @param depends_on string vector of the required intermediate data tables
#' @return nothing writes out the csv and ini files
generate_v25_files <- function(scenarios_to_process=NULL, depends_on = c("hectorv2_data.csv")){
  
  # Silence global variables
  scenario <- NULL
  
  # Check to make sure the data exists. 
  data_files <- file.path(INTERMEDIATE_DIR, depends_on)
  assertthat::assert_that(all(file.exists(data_files)), msg = "some element of depends_on does not exist")
  
  # Load the hector input data. 
  hinput_data <- as.data.table(utils::read.csv(data_files))
  
  if(is.null(scenarios_to_process)){
    scenarios_to_process <- c("rcp26", "rcp45", "rcp6", "rcp85")
  }
  
  data <- hinput_data[scenario %in% scenarios_to_process, ]
  
  
  # Format and save the emissions and concentration constraints in the csv files
  # in the proper Hector table input file. 
  use_info_source <- "v25"
  use_end_tag <-  "_emiss-rf"
  
  data_list <- split(data, data$scenario)
  lapply(data_list, FUN = write_hector_csv,  
         required = c(REQUIRED_EMISSIONS, "SV"), 
         write_to = TABLES_DIR, 
         info_source = use_info_source, 
         end_tag = use_end_tag) -> 
    csv_tables
  
  inis <- mapply(function(ofile, scn){
    # Define the new ini path
    new_path <- file.path("tables",  basename(ofile))
    new_ini <- replace_csv_string(hectordata::template_ini, replacement_path = new_path, run_name = scn)
    
    # Add albedo values into the ini file that match the old values.
    RF_index <- which(grepl(pattern = "albedo", x = new_ini))
    first_half_ini <- new_ini[1:RF_index-1]
    second_half_ini <- new_ini[(RF_index+1):length(new_ini)]
    ini <- append(append(first_half_ini, c("RF_albedo[1750]=0.0", "RF_albedo[1950]=-0.2")), second_half_ini)
    
    write_to <- gsub(pattern = "/tables", x = dirname(ofile), replacement = "")
    name <- paste0(scn, "_", use_info_source)
    ini_path <- file.path(write_to, paste0(name, ".ini"))
    writeLines(ini, ini_path)
    
  }, ofile = csv_tables, scn = names(csv_tables))
  
}

