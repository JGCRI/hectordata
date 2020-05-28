## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

context('generate functions')

test_that('generate_input_tables', {
  
  dir <- tempdir()
  testthat::expect_error(generate_input_tables('fake', dir), 'unrecognized scenarios')
  
  files <- generate_input_tables(scenarios = 'ssp434', output_dir = dir)
  testthat::expect_true(all(file.exists(files)))
  testthat::expect_equal(length(files), 2)
  
  file.remove(files)
  
})
