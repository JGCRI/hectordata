## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files


scenarios <- "ssp370"



generate_inputs <- function(scenarios){
  
  # Create an empty hector_inputs data table. 
  hector_inputs <- data.table::data.table()
  
  # A list of scenarios in each data source. 
  rcmipCMIP6_scenario <- c("ssp370", "ssp434", "ssp460", "ssp119", "ssp126", "ssp245", "ssp534-over", "ssp585")
  
  # Assert that the scenarios to process are categorized scenarios. 
  assertthat::assert_that(any(scenarios %in% c(rcmipCMIP6_scenario)))
  
  # Convert the CMIP phase 6 specfic scenarios. 
  if(any(scenarios %in% rcmipCMIP6_scenario)){
    to_process <- rcmipCMIP6_scenario[rcmipCMIP6_scenario %in% scenarios]
    hector_inputs <- rbind(hector_inputs, convert_rcmipCMIP6_hector(to_process))
  }
  
  
  
  
  
}


