# Define the hectordata constants 


ROOT <- here::here()
INPUT_DIR  <- file.path(ROOT, "inst", "ds")    
INI_DIR    <- file.path(ROOT, "inst", "input") 
TABLES_DIR <- file.path(ROOT, "inst", "input", 'tables') 
TEMP_DIR   <- tempdir() # this is where if necessary to pull from the internet the files will be stored. 

YEARS <- 1750:2300 

