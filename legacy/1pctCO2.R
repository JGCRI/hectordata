# Objective: set up 1pct CO2 runs 

# 0. Set UP --------------------------------------------------------------------

INPUT_DIR <- "inst/input/"
devtools::load_all()

# Make the data for the 1pct CO2 run 
# Args 
#   start_year: 1800 by default, this is defined as year 0 of the 1pct CO2 run
# Returns: data frame containing the 1pct CO2 x4 extension values
inputs_1pctCO2 <- function(start_year = 1800){
  
  start_year <- 1800
  pi_control_data <- read.csv(here::here("inst", "input", "tables", "hector_picontrol.csv"),
                              comment.char = ";")
  stopifnot(start_year > 1750)
  
  pi_value <- pi_control_data$CO2_constrain[1]
  df <- data.frame(Date = pi_control_data$Date, 
                   CO2_constrain = pi_control_data$CO2_constrain)
  index <- which(df$Date == start_year)
  
  t <- 1:(nrow(df) - index -1)
  vals <- pi_value * (1 + 0.01) ^ t
  df$CO2_constrain[(index+1):(nrow(df) - 1)] <- vals
  
  stop_index <- min(which(df$CO2_constrain >= (4*pi_value)))
  
  df$CO2_constrain[stop_index:nrow(df)] <- pi_value * 4  
  
  return(df)
  
}


# TODO not sure what to do about the ini functions, they are kind of hard to 
# generalize... 
# Args
#   file: character path to the csv hector input table
#   scn: str scenario name
#   fname: str name of the ini that will be saved
# Return: character path to the ini file the function produced 
specific_ini <- function(file, scn, fname){
  
  # Check the function arguments
  stopifnot(file.exists(file))
  stopifnot(grepl(pattern = "csv", x = file))
  stopifnot(grepl(pattern = "ini", x = fname))
  
  # Read in the pi control line
  ini_lines <- readLines(file.path("inst", "input", "picontrol_concentration.ini"))
  
  # replace the CO2_constrain file name with the new CO2 constraint runs
  csv_fname <- basename(file)
  constrain_index <- which(grepl(x = ini_lines, pattern = "CO2_constrain"))
  og_line <- ini_lines[constrain_index]
  new_line <- gsub(x = og_line, pattern = "hector_picontrol.csv", replacement = csv_fname)
  ini_lines[constrain_index] <- new_line
  
  # Change the config file information
  ini_lines[1] <- paste("; Config file for hector model: ", scn) 
  ofile <-  file.path("inst", "input", fname)
  writeLines(ini_lines, con = ofile)
  
  return(ofile)
  
}

# 1. Make Inputs ---------------------------------------------------------------
df_1cptCO2 <- inputs_1pctCO2()
scn <- "1pctCO2-4xext"
file <- write_input_table(x = df_1cptCO2, 
                          write_to = file.path("inst", "input", "tables"), 
                          scn = scn, 
                          fname = "hector_1pctCO2-4xext.csv")
ini <- specific_ini(file = file, 
                    scn = scn, 
                    fname = "hector_1pctCO2-4xext.ini")




# # ------------------------------------------------------------------------------------------
# # quick check to see if working as expected.
# library(ggplot2)
# library(hector)
# library(magrittr)
# 
# ini <- file.path(INPUT_DIR, "hector_1pctCO2-4xext.ini")
# hc <- newcore(ini)
# run(hc, runtodate = 2300)
# out <- fetchvars(hc, 1750:2300, vars = c(GLOBAL_TAS(), RF_TOTAL()))
# ggplot(data = out) +
#   geom_line(aes(year, value, color = variable)) +
#   facet_wrap("variable")



