<<<<<<< HEAD
context('helper unit functions')


test_that('ud_convert2', {
  
  # make sure that the conversion from kg to g works properly, 
  # there are 1000 g in 1 kg. 
  out <- ud_convert2(x = '1', from = 'kg', 'g')
  testthat::expect_equal(out, 1000)
  
  # Make sure it works units in the per year format. 
  out <- ud_convert2(x = '10', from = 'Tg year-1', to = 'Mt year-1')
  testthat::expect_equal(out, 10)
  
  # Should throw an error with a bad unit conversion.
  testthat::expect_error(ud_convert2(x = '1', from = 'fake', 'g'))
  
})


test_that('complete_missing_years', {
  
  test_data <- rbind(data.table::data.table(scenario = 'test1', 
                                            variable = 'bc', 
                                            units = 'kg',
                                            year = c(1994:1996, 1999),
                                            value = as.double(1:4)), 
                     data.table::data.table(scenario = 'test2',
                                            variable = 'bc',  
                                            units = 'kg',
                                            year = c(1994:1996, 1998),
                                            value = as.double(4:7)))
  
  testthat::expect_error(complete_missing_years(data = test_data[, list(variable)], expected_years = 1990:2000))
  
  out <- complete_missing_years(data = test_data, expected_years = 1990:2000)
  testthat::expect_true(all(1990:2000 %in% out$year))
  
  
})


test_that('csv table fxns', {
  
  # Quickly run hector to pull generate some emissions, 
  core <- hector::newcore(system.file('input/hector_rcp45.ini', package = 'hector'))
  hector::run(core)
  emission_vars <- c(hector::EMISSIONS_BC(), hector::EMISSIONS_CO())
  emissions <- hector::fetchvars(core, vars = emission_vars, dates = 1900:2100, scenario = 'rcp45' )
  emissions <- data.table::as.data.table(emissions)
  
  # Save the output files
  temp_file <- tempfile()
  
  # Make sure that errors are thrown. 
  xx <- emissions[ , list(scenario, year)]
  testthat::expect_error(format_hector_input_table(xx, filename = temp_file))
  
  xx <- emissions
  xx$fake <- 'a column of fake data'
  testthat::expect_error(format_hector_input_table(xx, filename = temp_file), 'Extra column names.')
  
  
  # Make sure that the emissions inputs can be converted.
  format_hector_input_table(emissions, filename = temp_file)
  lines <- readLines(temp_file)
  
  testthat::expect_equal(lines[[1]], "; rcp45")
  testthat::expect_equal(lines[[2]], "; created by hectordata")
  testthat::expect_true(grepl(x = lines[[3]], pattern = "; UNITS"))
  
  data <- read.csv(temp_file, comment.char = ';')
  testthat::expect_true(all(names(data) %in% c("Date", emission_vars)))
  testthat::expect_true(is.data.frame(data))
  
  # Remove the temp file
  file.remove(temp_file)
  
  # Check how the table is written out 
  ofile <- write_hector_csv(emissions, tempdir())
  expect_true(grepl(pattern = "hectordata_outputs", ofile))
  expect_error(read.csv(ofile), "more columns than column names")
  
  dat <- read.csv(ofile, comment.char = ";")
  expect_true(is.data.frame(dat))
  
  # Remove another file 
  file.remove(ofile)
  
})


||||||| e9b6ea1
=======
## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

context('helper unit functions')


test_that('ud_convert2', {
  
  # make sure that the conversion from kg to g works properly, 
  # there are 1000 g in 1 kg. 
  out <- ud_convert2(x = '1', from = 'kg', 'g')
  testthat::expect_equal(out, 1000)
  
  # Make sure it works units in the per year format. 
  out <- ud_convert2(x = '10', from = 'Tg year-1', to = 'Mt year-1')
  testthat::expect_equal(out, 10)
  
  # Should throw an error with a bad unit conversion.
  testthat::expect_error(ud_convert2(x = '1', from = 'fake', 'g'))
  
})


test_that('complete_missing_years', {
  
  test_data <- rbind(data.table::data.table(scenario = 'test1', 
                                            variable = 'bc', 
                                            units = 'kg',
                                            year = c(1994:1996, 1999),
                                            value = as.double(1:4)), 
                     data.table::data.table(scenario = 'test2',
                                            variable = 'bc',  
                                            units = 'kg',
                                            year = c(1994:1996, 1998),
                                            value = as.double(4:7)))
  
  testthat::expect_error(complete_missing_years(data = test_data[, list(variable)], expected_years = 1990:2000))
  
  out <- complete_missing_years(data = test_data, expected_years = 1990:2000)
  testthat::expect_true(all(1990:2000 %in% out$year))
  
  
})


test_that('save_hector_table', {
  
  # Quickly run hector to pull generate some emissions, 
  core <- hector::newcore(system.file('input/hector_rcp45.ini', package = 'hector'))
  hector::run(core)
  emission_vars <- c(hector::EMISSIONS_BC(), hector::EMISSIONS_CO())
  emissions <- hector::fetchvars(core, vars = emission_vars, dates = 1900:2100, scenario = 'rcp45' )
  emissions <- data.table::as.data.table(emissions)
  
  # Save the output files
  temp_file <- tempfile()
  
  # Make sure that errors are thrown. 
  xx <- emissions[ , list(scenario, year)]
  testthat::expect_error(save_hector_table(xx, filename = temp_file))
  
  xx <- emissions
  xx$fake <- 'a column of fake data'
  testthat::expect_error(save_hector_table(xx, filename = temp_file), 'Extra column names.')
  
  
  # Make sure that the emissions inputs can be converted.
  save_hector_table(emissions, filename = temp_file)
  lines <- readLines(temp_file)
  
  testthat::expect_equal(lines[[1]], "; rcp45")
  testthat::expect_equal(lines[[2]], "; created by hectordata")
  testthat::expect_true(grepl(x = lines[[3]], pattern = "; UNITS"))
  
  data <- read.csv(temp_file, comment.char = ';')
  testthat::expect_true(all(names(data) %in% c("Date", emission_vars)))
  testthat::expect_true(is.data.frame(data))
  
  # Remove the temp file
  file.remove(temp_file)
  
})
>>>>>>> master
