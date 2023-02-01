library(hector)
test_that("inis run", {

  ini_files <- list.files(here::here("inst", "input"), pattern = ".ini", full.names = TRUE)
  for(ini in ini_files){
    #TODO need to figure out some way to avoid the 1pctCO2 run! because cannot run all the way to 
    # the year 2100.... 
    hc <- newcore(ini)
    expect_equal(class(hc)[1], "hcore")
    run_msg <- capture.output(run(core = hc, runtodate = 2100))
    expect_equal(length(run_msg), 5)
  }

})
