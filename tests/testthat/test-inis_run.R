library(hector)
test_that("inis run", {
  
  ini_files <- list.files(here::here("inst", "input"), pattern = ".ini", full.names = TRUE)
  for(ini in ini_files){
    hc <- newcore(ini)
    expect_equal(class(hc)[1], "hcore")
    run_msg <- capture.output(run(core = hc, runtodate = 2100)) 
    expect_equal(length(run_msg), 5)
  }
  
})
