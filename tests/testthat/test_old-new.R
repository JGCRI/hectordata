test_that("old new test", {
  
  # Read in the old data, this is the comparison data to make sure that the 
  # behavior of the emission tables does not change with updates to hectordata. 
  comp_data <- as.data.table(read.csv("comp_data/old_new_data.csv", stringsAsFactors = FALSE))
  
  # Select the new package data. 
  scn_names <- paste0(unique(comp_data$scn), collapse = "|")
  ini_files <- list.files(TABLES_DIR, pattern = scn_names)
  ini_files <- file.path(TABLES_DIR, ini_files)
  
  # For each of the ini files, check to make sure that the core can be set up 
  # and run. 
  suppressWarnings({
    new_data <- rbindlist(lapply(ini_files, function(f){
      as.data.table(read.csv(file.path(f), comment.char = ";")) %>% 
        melt(id.vars = "Date", value.name = "new_value") -> 
        data
      
      data[["scn"]] <- gsub(x = basename(f), pattern = "_emiss-constraints_rf.csv", replacement = "")
      return(data)
    }))
  })
  
  
  comparison_df <- new_data[comp_data, on = c("variable", "Date", "scn"), ] 
  diffference <- abs(comparison_df$new_value - comparison_df$old_value)
  expect_lte(mean(diffference), 1e-5)

})