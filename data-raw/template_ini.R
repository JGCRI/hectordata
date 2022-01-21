# This scripts creates the template ini package data. 

# The the hector ini files defines a number of parameter values that can be adjusted via ini 
# file or via the R hector interface. 
ini_lines <- readLines(file.path('data-raw', 'hector_TEMPLATE.ini'))


# Identify and deactivate all constraint lines 
constriant_index <- which(grepl(pattern = "constrain=csv", x = ini_lines))

# Of the constraints check to make sure that each line begins by being commented out. 
# If all of the constraints ini lines are already deactivated then continue on. 
# However any of the constraints are active (not commented out) they must be deactivated.  
deactivated_constraints <- grepl(pattern = '^;', x = ini_lines[constriant_index])

if(!any(deactivated_constraints)){
  
  activate_constraints <- which(grepl(pattern = '^;', x = ini_lines[constriant_index]))
  ini_index             <- constriant_index[activate_constraints]
  
  for(i in ini_index){
    ini_lines[i] <- paste0(';',  ini_lines[i])
      print(paste0(';',  ini_lines[i]))
  }
  
  } 

# Replace the remaining rcp45 tags with the template name. 
template_ini <- ini_lines

usethis::use_data(template_ini, overwrite = TRUE)
