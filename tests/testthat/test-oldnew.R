# The old new test ensures that behavior is persevered during periods of rapid 
# development. Eventually this test might be deleted.

library(hector)

test_that("old new", {
  
  ini_files <-  list.files(system.file("input", package = "hectordata"), pattern = "ini", full.names = TRUE)
  old_df <- read.csv("old_new.csv")
  
  yrs <- unique(old_df$year)
  vars <- unique(old_df$variable)
    
  new_rslts <- lapply(ini_files,
                  function(ini){
                    yrs <- unique(old_df$year)
                    scn <- basename(ini)
                    hc <- hector::newcore(inifile = ini, name = scn)
                    hector::run(hc)
                    out <- hector::fetchvars(core = hc, dates = yrs, vars = vars)
                    return(out)
                  })
  names(new_rslts) <- basename(ini_files)
  
  for(name in names(new_rslts)){
    
    to_compare <- old_df[old_df$scenario == name, ]
    new_df <- new_rslts[[name]]
    
    expect_equal(to_compare$year, new_df$year)
    expect_equal(to_compare$variable, new_df$variable)
    expect_equal(to_compare$value, new_df$value)
  }
  
})
