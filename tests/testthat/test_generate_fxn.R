## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

# context('generate functions')
# 
# test_that('generate_input_tables', {
#   
#   dir   <- tempdir()
#   years <- 1800:2150
#   testthat::expect_error(generate_csv_tables('fake', dir, years = years), 'unrecognized scenarios')
#   
#   files <- generate_csv_tables(scenarios = 'ssp434', write_to  = dir, years = years)
#   testthat::expect_true(all(file.exists(files)))
#   testthat::expect_equal(length(files), 1)
#   
#   file.remove(files)
#   
# })
