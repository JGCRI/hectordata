test_that("ud_convert2", {
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


test_that('process_carbon_cycle_emissions', {

  df <- data.frame("year" = 1900, "variable"= hector::EMISSIONS_BC(),"units" = "fake", "value" = 10)
  expect_error(process_carbon_cycle_emissions(df), label = "dat does not have all of these name(s): 'year', 'variable', 'units', 'value', 'scenario'")

  df <- data.frame("year" = 1900, "variable"= hector::FFI_EMISSIONS(),"scenario" = "fake", "value" = 10, 
                   units = hector::getunits(hector::FFI_EMISSIONS()))
  expect_error(process_carbon_cycle_emissions(df),
               label = 'Elements 2 of c("ffi_emissions", "luc_emissions") %in% unique(dat[["variable"]]) are not true')
  
  df <- data.table(year = rep(1905:1910, 2), 
                   variable= rep(x = c(hector::FFI_EMISSIONS(), hector::LUC_EMISSIONS()), each = 6), 
                   scenario = "ssp119", 
                   units = hector::getunits(hector::FFI_EMISSIONS()), 
                   value = rep(2*5:10, 2)) 
  
  out <- process_carbon_cycle_emissions(df)
  expect_equal(length(unique(out$variable)), 4)
 
})


test_that("check_cols", {
  
  data <- data.frame("col1" = sample(1:10, size = 5), 
                     "col2" = sample(1:10, size = 5))
  expect_true(check_cols(dat = data, req = c("col1", "col2")))
  expect_error(check_cols(dat = data, req = c("fake")), "missing columns:  fake")
  
})
