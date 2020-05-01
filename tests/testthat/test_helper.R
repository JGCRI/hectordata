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
  
  expected_years <-  c(1994:1996, 1999)
  test_data <- rbind(data.table(scenario = 'test1', 
                     variable =  'bc', 
                     units = 'kg',
                     year = expected_years,
                     value = as.double(1:4)), 
        data.table(scenario = 'test2', 
                   variable =  'bc', 
                   units = 'kg',
                   year = c(1994:1996, 1998),
                   value = as.double(4:7)))
  
  testthat::expect_error(complete_missing_years(data = test_data[, .(variable)], expected_years = 1990:2000),
                         msg  = 'data is missing following columns: scenario, units, year')

  out <- complete_missing_years(test_data, expected_years = 1990:2000)
  testthat::expect_true(all(expected_years %in% out$year))
  
  
})
  