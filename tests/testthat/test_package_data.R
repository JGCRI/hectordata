## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

context('package data')

test_that('template_ini', {
  
  # Compare the dimension of the RCP ini file commited to Hector with the hectordata template_ini 
  # file. If the dimension are different the internal package data is out of date and will need to 
  # be regenerated. 
  ini_url      <- url("https://raw.githubusercontent.com/JGCRI/hector/master/inst/input/hector_rcp45.ini")
  hector_RCP45 <- readLines(ini_url)
  close(ini_url)
  
  testthat::expect_equal(length(hector_RCP45), length(template_ini))
  
  
  
})
