<<<<<<< HEAD
## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

context('convert functions')

years <- 1975:2010

test_that('convert_rcmipCMIP6_hector', {
  
  # Test specifc emission species, that have had conversion issues in the past.
  x <- 10 
  
  n2o_entry <- data.table::as.data.table(hectordata::rcmipCMIP6_conversion)[hector_variable == "N2O_emissions", ]
  ztrue <- x * (biogas::molMass("N2O") / (biogas::molMass("N") * 2))
  # Divide by 100 to force back to arbitrary units 
  z     <- ud_convert2(x, from = n2o_entry$hector_udunits, to = n2o_entry$rcmip_udunits)/1000
  testthat::expect_equal(z, ztrue)
  

  so2_entry <-   data.table::as.data.table(hectordata::rcmipCMIP6_conversion)[hector_variable == "SO2_emissions", ]
  ztrue <- x * (biogas::molMass("S") / biogas::molMass("SO2"))
  z     <- ud_convert2(x, from = so2_entry$rcmip_udunits, to = so2_entry$hector_udunits)/1000

})

||||||| e9b6ea1
=======
## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

context('convert functions')

years <- 1975:2010

test_that('convert_rcmipCMIP6_hector', {
  
  # Test specifc emission species, that have had conversion issues in the past.
  x <- 10 
  
  n2o_entry <- hectordata::rcmipCMIP6_conversion[hector_variable == "N2O_emissions", ]
  ztrue <- x * (biogas::molMass("N2O") / (biogas::molMass("N") * 2))
  # Divide by 100 to force back to arbitrary units 
  z     <- ud_convert2(x, from = n2o_entry$hector_udunits, to = n2o_entry$rcmip_udunits)/1000
  testthat::expect_equal(z, ztrue)
  

  so2_entry <-  hectordata::rcmipCMIP6_conversion[hector_variable == "SO2_emissions", ]
  ztrue <- x * (biogas::molMass("S") / biogas::molMass("SO2"))
  z     <- ud_convert2(x, from = so2_entry$rcmip_udunits, to = so2_entry$hector_udunits)/1000

  # RCMIP included inputs for the CMIP5 scenarios (the rcps) however Hector has these inputs 
  # set up from the IIASA database which therefore use the RCP45 scenario to test the RCMIP conversion.
  rcp45_data <- convert_rcmipCMIP6_hector(scenario = 'rcp45', years = years)

  # Import original Hector inputs and format for easy comparison.
  original_rcp45 <- as.data.table(read.csv(system.file('input/emissions/RCP45_emissions.csv', package = 'hector'), skip = 3))
  original_rcp45 <- data.table::melt.data.table(data = original_rcp45,
                                                id.vars = "Date",
                                                variable.name = 'variable', value.name = 'value')
  names(original_rcp45) <- c('year', 'variable', 'value_original')
  
  # Join the two data frames together to compare the original hector rcp 45 emissions with emissions
  # from the rcmip source. 
  comparison_data <- stats::na.omit(original_rcp45[rcp45_data, on = c('year', 'variable'), nomatch = NA])
  testthat::expect_equal(sum(is.na(comparison_data)), 0)
  
  # The percent difference between the two RCP inputs should be small. 
  comparison_data[ , percent_change :=  100 * (value - value_original) / value_original]
  mean_difference   <- comparison_data[ year %in% 1850:2100, .(avg = mean(percent_change, na.rm = TRUE)), by = variable]
  trivial_threshold <- 0.5 # the average percent difference should be less than half of a percent different.
  testthat::expect_true(all(abs(mean_difference$avg) < trivial_threshold))

})
>>>>>>> master
