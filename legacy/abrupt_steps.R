# Objective: set up the abrupt CO2 concentration step run inputs. Note that these are 
# this is probably going to need to be moved it is unclear how we want to actually 
# uses these things... 

# 0. Set UP --------------------------------------------------------------------

INPUT_DIR <- "inst/input/"
devtools::load_all()

# Make the data for the abrupt CO2 concentration step runs 
# Args 
#   step size: numeric value to multiply the pre industrial concentration CO2 value by 
#   step_year: 1800 by default, this is the year that the abrupt step takes place must be after 1750 
# Returns: data frame of CO2 concentration constraints
# TODO - possible enhancement would be generalize teh function so it worked for any emissions or 
# concentration constraint?? 
abrupt_CO2conc_step <- function(step_size, step_year = 1800){
  
  pi_control_data <- read.csv(here::here("inst", "input", "tables", "hector_picontrol.csv"),
                              comment.char = ";")
  stopifnot(step_year > 1750)
  pi_value <- pi_control_data$CO2_constrain[1]
  
  df <- data.frame(Date = pi_control_data$Date, 
                   CO2_constrain = pi_control_data$CO2_constrain)
  
  df$CO2_constrain[df$Date >= step_year] <- pi_value * step_size
  
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
df_co2X4 <- abrupt_CO2conc_step(step_size = 4)
scn <- "abruptx4CO2"
file <- write_input_table(x = df_co2X4, 
                          write_to = file.path("inst", "input", "tables"), 
                          scn = "abruptx4CO2", 
                          fname = "hector_abruptx4CO2.csv")
ini <- specific_ini(file = file, 
                    scn = scn, 
                    fname = "hector_abruptx4CO2.ini")


scn <- "abruptx2CO2"
df_co2X2 <- abrupt_CO2conc_step(step_size = 2)
file <- write_input_table(x = df_co2X2, 
                          write_to = file.path("inst", "input", "tables"), 
                          scn = scn, 
                          fname = "hector_abruptx2CO2.csv")
ini <- specific_ini(file = file, 
                    scn = scn, 
                    fname = "hector_abruptx2CO2.ini")


scn <- "abruptx0p5CO2"
df_co2Xp5 <- abrupt_CO2conc_step(step_size = 0.5)
file <- write_input_table(x = df_co2Xp5, 
                          write_to = file.path("inst", "input", "tables"), 
                          scn = scn, 
                          fname = "hector_abruptx0p5CO2.csv")
ini <- specific_ini(file = file, 
                    scn = scn, 
                    fname = "hector_abruptx0p5CO2.ini==")



# # ------------------------------------------------------------------------------------------
# # quick check to see if working as expected. 
# library(ggplot2)
# library(magrittr)
# inputs <- df_co2Xp5
# 
# ini <- file.path(INPUT_DIR, "picontrol_concentration.ini")
# hc <- newcore(ini)
# setvar(hc, dates = inputs$Date, values = inputs$CO2_constrain, var = CO2_CONSTRAIN(), 
#        unit = getunits(CO2_CONSTRAIN()))
# reset(hc)
# run(hc)
# out <- fetchvars(hc, 1750:2300, vars = c(GLOBAL_TAS(), RF_TOTAL()))
# ggplot(data = out) +
#   geom_line(aes(year, value, color = variable)) + 
#   facet_wrap("variable")
#
# hc <- newcore(ini)
# run(hc)
# out <- fetchvars(hc, 1750:2300, vars = c(GLOBAL_TAS(), RF_TOTAL()))
# ggplot(data = out) +
#   geom_line(aes(year, value, color = variable)) +
#   facet_wrap("variable")