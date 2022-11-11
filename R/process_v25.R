
generate_v25_rcps <- function(){
  v25_dir <- find_input_dir(info_source = "v25")
  
  out <- list.files(v25_dir, pattern = "RCP", full.names = TRUE) %>%
    lapply(function(f){
      
      # Save a copy of the long data frame
      long_data <- rename(melt.data.table(data = as.data.table(read.csv(f, comment.char = ";")),
                                          id.vars = "Date", variable.name = "variable",
                                          value.name = "value", variable.factor = FALSE), c("year" = "Date"))
      
      # Map from the old variable names to the new ones
      long_data %>%
        rename(cols = c("v25_variable" = "variable")) %>%
        .[conversion_table$v25, on = "v25_variable"] %>%
        .[ , list(year, value, variable = v3_variable)] ->
        mapped_data
      
      # Save a copy of the emissions from Hector v25 that have the
      # correct name already.
      data <- long_data[long_data$variable %in% REQUIRED_EMISSIONS,  ]
      
      # Use the helper function to add the uptake columns for ffi and luc.
      data$scenario <- data$units <- NA # Add the required columns for the helper functions.
      data <- process_carbon_cycle_emissions(data)[ , list(year, variable, value)]
      
      # Construct a data frame of NH3 emissions since that was added to hector v3.
      missing_nh3_data <- data.table(year = unique(data$year),
                                     variable = hector::EMISSIONS_NH3(),
                                     value = 0)
      
      # Load the volcanic data.
      read.csv(list.files(find_input_dir(info_source = "v25"),
                          pattern = "vol", full.names = TRUE), comment.char = ";") %>%
        rename(cols = c("year" = "Date", "value" = "SV")) ->
        sv_data
      sv_data$variable <- "SV"
      
      complete_data <- rbind(data, mapped_data, missing_nh3_data, sv_data)
      complete_data$scenario <- gsub(pattern = "_emissions.csv", x = basename(f),
                                     replacement = "")
      complete_data$units <- ""
      
      complete_data <- complete_missing_years(data = complete_data, expected_years = 1745:2500)
      ofile <- write_hector_csv(complete_data,
                                required = c(REQUIRED_EMISSIONS, "SV"),
                                write_to = TABLES_DIR,
                                info_source = "v25",
                                end_tag = "_emiss-rf")
      
      
      
      name <- gsub(x = basename(ofile), pattern = "_emiss-rf.csv", replacement = "")
      # TODO make this relative
      new_path <- file.path("tables",  basename(ofile))
      new_ini <- replace_csv_string(template_ini, replacement_path = new_path, run_name = name)
      
      # Add albedo values into the ini file that match the old values.
      RF_index <- which(grepl(pattern = "albedo", x = new_ini))
      first_half_ini <- new_ini[1:RF_index-1]
      second_half_ini <- new_ini[(RF_index+1):length(new_ini)]
      ini <- append(append(first_half_ini, c("RF_albedo[1750]=0.0", "RF_albedo[1950]=-0.2")), second_half_ini)
      
      write_to <- gsub(pattern = "/tables", x = dirname(ofile), replacement = "")
      ini_path <- file.path(write_to, paste0(name, ".ini"))
      writeLines(ini, ini_path)
      
      return(basename(ini_path))
      
    })
  
  return(unlist(out))
  
}


