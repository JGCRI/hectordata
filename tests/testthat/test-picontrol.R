library(hector)

test_that("picontrol concentration", {
  
  ini <- system.file("input/picontrol_concentration.ini", package = "hectordata")
  hc <- newcore(ini)
  expect_equal(class(hc), c("hcore", "environment"))
  
  run(hc)
  
  # Fetch temperature and RF output in the first model run year and final year with the 
  # pre industrial control set up these values should all be 0. 
  out <- fetchvars(core = hc, dates = c(1745, 2300), vars = c(GLOBAL_TAS(), RF_TOTAL()))
  expect_equal(out$value, rep(0, nrow(out)))
  
})
