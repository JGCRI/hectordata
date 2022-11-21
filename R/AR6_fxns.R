# proccess_AR6 <- function(){
#   
#   #TODO there has to be a better way to do this
#   ar6_dir <- find_input_dir(dir = ZENODO_DIR, "AR6")
#   assertthat::assert_that(dir.exists(ar6_dir), msg = "data dir not found")
# 
#   # TODO change/exam this 
#   ar6_file <- data.table(read.csv(file.path(ar6_dir, "ERF_ssp119_1750-2500.csv")))
#   long_data <- melt.data.table(data = ar6_file,
#                                  id.vars = "year", variable.name = "variable",
#                                  value.name = "value", variable.factor = FALSE)
#   
#   # Subset the data 
#   total_rf <- long_data[variable == "total" & year  <= 2019, ]
#   total_rf$variable <- hector::FTOT_CONSTRAIN()
#   total_rf$scenario <- "historical"
#   total_rf$units <- "W/m2"
#   
#   # Save intermediate data.
#   ofile <- file.path(INTERMEDIATE_DIR, "AR6_data.csv")
#   utils::write.csv(x = total_rf, file = ofile, row.names = FALSE)
#   return(total_rf)
#   
# }
# 
# 
# 
# 
# generate_AR6_files <- function(depends_on = c("AR6_data.csv")){}
# 
# # Check to make sure the data exists. 
# data_files <- file.path(INTERMEDIATE_DIR, depends_on)
# assertthat::assert_that(all(file.exists(data_files)), msg = "some element of depends_on does not exist")
# 
# # Load the hector input data. 
# hinput_data <- as.data.table(read.csv(data_files))
# 
# 
# # Format and save the emissions and concentration constraints in the csv files
# # in the proper Hector table input file. 
# use_info_source <- "ar6"
# use_end_tag <-  "_constraint"
# 
# write_hector_csv(hinput_data, 
#                  info_source = use_info_source, 
#                  end_tag = use_end_tag, 
#                  write_to = TABLES_DIR)
# 
