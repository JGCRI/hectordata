# TODO better way to handle dependencies 
library(hector)

DIR <- tempdir()

test_that("write_hector_csv works", {
  
  df <- data.table(scenario = "unit tests", year = 1:10, value = 1:10, 
                   variable = FFI_EMISSIONS(), units = getunits(FFI_EMISSIONS()))
  expect_error(write_hector_csv(df, required = "fake", write_to = DIR,
                                info_source = "test", end_tag = "test"), 
               label = "Missing required variable(s): fake")
  
  out <- write_hector_csv(df, required = FFI_EMISSIONS(), write_to = DIR,
                          info_source = "test", end_tag = "test")
  
  # Read in the csv file & check the contents of the data frame. 
  out_df <- utils::read.csv(out, comment.char = ";")
  expect_equal(nrow(out_df), length(df$year))
  expect_equal(ncol(out_df), (length(unique(df$variable)) + 1))
  
  # Read in the csv file as lines to check to make sure the header information exists.
  # The header information should be 5 lines long that all start with the character ;
  lines <- readLines(out)
  expect_equal(sum(grepl(x = lines, pattern = "^;")), 5)
  
  # Clean up the csv file
  file.remove(out)
  
})
