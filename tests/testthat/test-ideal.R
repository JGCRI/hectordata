
library(hector)

# Define the path to the first ini file listed in the Hector package. 
ini <- list.files(system.file("input", package = "hector"),
                  full.names = TRUE, pattern = "ini")[[2]]


test_that("picontrol works", {
  
  # Set up the Hector core
  hc <- newcore(ini)
  
  # Generate the pi control input data table
  input_df <-  make_hector_picontrol_df()
  # Read the pi control inputs and run Hector. 
  x <- lapply(split(input_df, input_df$variable), 
              function(x){
                setvar(hc, dates = x$year, var = x$variable, values = x$value, unit = x$units)
                reset(hc)
              })
  
  run(hc)
  
  # By the definition of pi control we expect the total RF to be 
  # held constant at 0 over the course of this run.
  out <- fetchvars(hc, dates = 1700:2100, vars = GLOBAL_TAS())
  expect_equal(sum(out$value), 0)
})



