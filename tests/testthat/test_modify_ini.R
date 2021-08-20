library(data.table)

context('helper functions that modify ini files')

test_that('deactivate_variables', {
  
  # Make sure that this function works
  x <-  c('N2O_emissions', 'CFC113_emissions')
  ini_lines <- deactivate_input_variables(lines = hectordata::template_ini, vars = x)
  testthat::expect_true(all(grepl(pattern = '^;', x =  ini_lines[grepl(pattern = paste(x, collapse = '|'), x = ini_lines)])))
  
  # Make sure that it throws an error 
  testthat::expect_error(deactivate_input_variables(lines = hectordata::template_ini, vars = c(x, 'fake')))
  
}) 

test_that('activate_variables', {
  
  # Make sure that this function works
  x <-  c('atm_ocean_constrain')
  index         <- which(grepl(pattern = x, x = hectordata::template_ini))
  original_line <- template_ini[index]
  
  activated_ini  <- activate_input_variables(lines = hectordata::template_ini, vars = x)
  activated_line <- activated_ini[index]
  
  testthat::expect_true(original_line != activated_line)
  testthat::expect_true(grepl(pattern = activated_line, x = original_line))
  
  # Make sure that it throws an error 
  testthat::expect_error(activate_input_variables(lines = hectordata::template_ini, vars = c(x, 'fake')))
  
}) 

test_that('replace_csv_string', {
  
  x <- list.files(pattern = '.R')[[1]] # Select some random file from the directory to use.
  out_ini <- replace_csv_string(ini = hectordata::template_ini,  replacement_path = x, run_name = 'test')
  expect_false(any(grepl(pattern = 'template', x = tolower(out_ini))))
  
})
