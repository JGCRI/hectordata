test_that('inis run', {
  
  ini_files <- list.files(INI_DIR, pattern = ".ini")
  ini_files <- file.path(INI_DIR, ini_files)
  
  # For each of the ini files, check to make sure that the core can be set up 
  # and run. 
  lapply(ini_files, function(f){
    core <- hector::newcore(inifile = f)
    expect_equal(class(core)[1], "hcore")
    run_msg <- capture.output(hector::run(core = core, runtodate = 2300)) 
    expect_equal(length(run_msg), 5)
  
  })
})
