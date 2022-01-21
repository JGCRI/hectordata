# Copy a input table from each of the models into a single data frame that will be used in an old-new test to 
# test that behavior is preserved, with that being said occasionally there may be a reason for some change 
# in the numerical out the hector input tables, when that occur this file will need to be re-run so 
# to update the old new comparison data. 

# TODO there needs to be some instruction about how to run the make function for hectordata... 

devtools::load_all() # load the hector data package 

# Select a rcmip rcp scenario and add the 
as.data.table(read.csv(file.path(TABLES_DIR, "rcmip_rcp45_emiss-constraints_rf.csv"), comment.char = ";")) %>% 
  melt(id.vars = "Date", value.name = "old_value")  ->
  rcmip_rcp45

rcmip_rcp45$scn<- "rcmip_rcp45"
  
# Select a rcmip ssp scenario 
as.data.table(read.csv(file.path(TABLES_DIR, "rcmip_ssp245_emiss-constraints_rf.csv"), comment.char = ";")) %>% 
  melt(id.vars = "Date", value.name = "old_value") -> 
  rcmip_ssp45

rcmip_ssp45$scn <- "rcmip_ssp245"


# Select the years of data to keep and subset the comparison data. 
comp_yrs  <- floor(seq(from = min(rcmip_ssp45$Date), to = max(rcmip_ssp45$Date), length.out = 10))
comp_data <- rbind(rcmip_rcp45, rcmip_ssp45)[Date %in% comp_yrs]

write.csv(comp_data, file = here::here("tests", "testthat", "comp_data", "old_new_data.csv"), row.names = FALSE)





