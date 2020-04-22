## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

# This scripts creates a template ini file from a ini file from the Hector repository 
# that can be used to generate other ini files. 

# Start by pulling one the of ini files from the master branch of Hector (https://github.com/jgcri/hector)
# we arbitrability selected the CMIP5 RCP 4.5 scenario input file. 
ini_url      <- url("https://raw.githubusercontent.com/JGCRI/hector/master/inst/input/hector_rcp45.ini")
hector_RCP45 <- readLines(ini_url)
close(ini_url)

# The the hector ini files define the path of the emissions to scenario x. In this case all of the emissions 
# files point to the rcp45 emissions csv input. Remove the calls of the "emissions/RCP45_emissions.csv" 
# and replace the RCP45 with the string 'TEMPLATE'. 
# Note these changes should not impact the volcanic emissions. 
hector_temp <- gsub(pattern = 'emissions/RCP45_emissions.csv',  
                       replacement = 'emissions/TEMPLATE_emissions.csv',  x = hector_RCP45)


# After replacing the emissions inputs replace the CO2 concentration constraint csv files with the 
# DEFAULT file. 
hector_temp <- gsub(pattern = 'constraints/RCP45_co2ppm.csv', 
                       replacement = 'constraints/TEMPLATE_constraint.csv', x = hector_temp)

# Also update the RF constraint to the template tag. 
hector_temp <- gsub(pattern = 'constraints/MAGICC_RF_4.5.csv', 
                    replacement = 'constraints/TEMPLATE_constraint.csv', x = hector_temp)


# The default hector ini file will be puerly emission driven meaning that all of the 
# constraints should be deactivated (commented out).
constriant_index <- which(grepl(pattern = 'constraint', x = hector_temp))

# Of the constraints check to make sure that each line begins by being commented out. 
# If all of the constraints ini lines are already deactivated then continue on. 
# However any of the constraints are active (not commented out) they must be deactivated.  
deactivated_constraints <- grepl(pattern = '^;', x = hector_temp[constriant_index])

if(!any(deactivated_constraints)){
  
  activate_constraints <- which(grepl(pattern = '^;', x = hector_temp[constriant_index]))
  ini_index             <- constriant_index[activate_constraints]
  
  for(i in ini_index){
    hector_temp[i] <- paste0(';',  hector_temp[i])
      print(paste0(';',  hector_temp[i]))
  }
  
  } 

# Replace the remaining rcp45 tags with the template name. 
hector_temp  <- gsub(pattern = 'rcp45', replacement = 'TEMPLATE', x = tolower(hector_temp))
template_ini <- hector_temp

usethis::use_data(template_ini, overwrite = TRUE)
