## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

context('generate functions')

test_that('generate_input_tables', {
  
  dir   <- tempdir()
  years <- 1800:2150
  testthat::expect_error(generate_input_tables('fake', dir, years = years), 'unrecognized scenarios')
  
  files <- generate_input_tables(scenarios = 'ssp434', output_dir = dir, years = years)
  testthat::expect_true(all(file.exists(files)))
  testthat::expect_equal(length(files), 2)
  
  file.remove(files)
  
})
