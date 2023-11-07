# Objective: set up the abrupt CO2 concentration step run inputs. Note that these are 
# this is probably going to need to be moved it is unclear how we want to actually 
# uses these things... 

# TODO either make a helper function for the load picontrol or some sort of 
# read input_table function that could be helpful i think.. 
pi_control_data <- read.csv(here::here("inst", "input", "tables", "hector_picontrol.csv"),
         comment.char = ";")

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

# KALYN 
# Next steps are to generate the x4, x2, xp5 data frames save as csv tables... 
# Then generate the new ini files and save 
# Add to old new tests 

# ------------------------------------------------------------------------------------------
# quick check to see if working as expected. 
inputs <-  abrupt_CO2conc_step(step_size = 2, step_year = 1751)

ini <- file.path(INPUT_DIR, "picontrol_concentration.ini")
hc <- newcore(ini)
setvar(hc, dates = inputs$Date, values = inputs$CO2_constrain, var = CO2_CONSTRAIN(), unit = getunits(CO2_CONSTRAIN()))
reset(hc)
run(hc)
out <- fetchvars(hc, 1750:2300, vars = c(GLOBAL_TAS(), RF_TOTAL()))
ggplot(data = out) +
  geom_line(aes(year, value, color = variable)) + 
  facet_wrap("variable")
