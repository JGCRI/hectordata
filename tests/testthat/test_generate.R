## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

context('generate functions')

test_that('generate_input_tables', {
  
  dir <- tempdir()
  testthat::expect_error(generate_input_tables('fake', dir), 'unrecognized scenarios')
  
  files <- generate_input_tables('ssp434', dir)
  testthat::expect_true(all(file.exists(files)))
  testthat::expect_equal(length(files), 2)
  
  file.remove(files)
  
})


test_that("generate_volanic_inputs", {
  
  dir <- tempdir()
  
  file <- generate_volanic_inputs(dir)
  testthat::expect_true(file.exists(file))
  
  file.remove(file)
  
  
})