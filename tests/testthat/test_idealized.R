test_that('idealized runs', {
  
  ini_files <- list.files(INI_DIR, pattern = ".ini")
  ini_files <- ini_files[grepl(x = ini_files, pattern = "cmip_1pctCO2-4xext|rcmip_1pctCO2|rcmip_abrupt-0p5xCO2|rcmip_abrupt-2xCO2|cmip_abrupt-4xCO2|rcmip_piControl")]
  ini_files <- file.path(INI_DIR, ini_files)
  
  # For each run, make sure that the total RF matches the CO2 RF.
  lapply(ini_files, function(f){
    core <- hector::newcore(inifile = f)
    invisible(hector::run(core = core, runtodate = 2100))
    out <- hector::fetchvars(core, dates = 1750:2100, vars = c(hector::RF_CO2(), hector::RF_TOTAL()))
    expect_equal(out[out$variable == hector::RF_TOTAL(), ]$value, out[out$variable == hector::RF_CO2(), ]$value, tolerance = 1e-4)
   
  })
})