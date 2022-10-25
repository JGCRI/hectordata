library(data.table)

context('test the ini helper functions')


test_that('deactivate_variables', {
  
  # Make sure that this function works
  x <-  c('N2O_emissions', 'CFC113_emissions')
  ini_lines <- deactivate_input_variables(lines = template_ini, vars = x)
  expect_true(all(grepl(pattern = '^;', x =  ini_lines[grepl(pattern = paste(x, collapse = '|'), x = ini_lines)])))
  
  # Make sure that it throws an error 
  expect_error(deactivate_input_variables(lines = template_ini, vars = c(x, 'fake')))
  
}) 


test_that('activate_variables', {
  
  # Make sure that this function works
  x <- c('CO2_constrain')
  index         <- which(grepl(pattern = x, x = template_ini))
  original_line <- template_ini[index]
  
  activated_ini  <- activate_input_variables(lines = template_ini, vars = x)
  activated_line <- activated_ini[index]
  
  expect_true(original_line != activated_line)
  expect_true(grepl(pattern = activated_line, x = original_line))
  
  # Make sure that it throws an error 
  expect_error(activate_input_variables(lines = template_ini, vars = c(x, 'fake')))
  
}) 


test_that('replace_csv_string', {
  
  out_ini <- replace_csv_string(ini = template_ini,  
                                replacement_path = "tables/unit.csv", 
                                run_name = 'test')
  expect_false(any(grepl(pattern = 'template', x = tolower(out_ini))))
  
})
