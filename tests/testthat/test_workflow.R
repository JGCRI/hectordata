context('test full workflow')

test_that('rcp45', {
  
  # rcp45 is a test case fo
  # Create a temporary directory to store the csv & ini files in. 
  DIR <- tempdir()
  
  # First start out by making sure that the RCP emission files are equivalent 
  # 
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
  
  # Write the csv file out 
  file <- write_hector_csv(rcp45_data, write_to = DIR)
  
  # Make the ini file
  ini <- make_new_ini(file)
  expect_true(file.exists(ini))
  
  # See if the input can be read into hector! 
  core <- hector::newcore(ini)
  hector::run(core)
  new_rcp <- hector::fetchvars(core, dates = 1900:2100)
  new_rcp$run <- "rcmip"
  hector::shutdown(core)
  
  # If it can then check to see how the output compare with one another! 
  ini <- system.file("input/hector_rcp45.ini", package = "hector")
  core2 <- hector::newcore(ini)
  run(core2)
  old_rcp <- hector::fetchvars(core2, dates = 1900:2100)
  old_rcp$run <- "old"
  hector::shutdown(core2)
  
  data.table::as.data.table(rbind(new_rcp, old_rcp))[, list(year, variable, value, run)] %>% 
    data.table::dcast(year + variable ~ run) -> 
    long_comparison
 
  tol <- 1e-2
  expect_true(all((long_comparison$old - long_comparison$rcmip) <= tol))
})
