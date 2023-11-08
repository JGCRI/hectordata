# List all of the ini files in the package we want to make sure that all the 
# ini's run and then save some selected output to make sure that we can 
# ensure that Hector behavior does not change. 

library(hector)


rslts <- lapply(list.files(file.path("inst", "input"), pattern = "ini", full.names = TRUE),
                function(ini){
                  yrs <- floor(seq(from = 1750, to = 2300, length.out = 10))
                  scn <- basename(ini)
                  hc <- newcore(inifile = ini, name = scn)
                  run(hc)
                  out <- fetchvars(core = hc, dates = yrs)
                  return(out)
                })

rslt_df <- do.call(what = "rbind", args = rslts)

ofile <- file.path("tests", "testthat", "old_new.csv")
write.csv(rslt_df, file = ofile, row.names = FALSE)


