## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files
## Function that help to modify the ini files.  

#' Identify the variables and lines of hector ini file that use csvs to read in inputs. 
#' 
#' @return a data table of the variables that use csv as inputs. 
#' @noRd 
identify_csv_inputs <- function(){
  
  # Which lines of ini use csv files to read in inputs?
  csv_input_index <- which(grepl(pattern = '*=csv:', x = template_ini))
  
  # Isolate the variable name defined in the ini file.
  names <- sapply(template_ini[csv_input_index], function(x){
    y <- strsplit(x, split = '=csv:')[[1]][1]
    y <- gsub(pattern = ';| ', replacement = '', x = y)
    return(y)
  }, USE.NAMES = FALSE)
  
  # Format a data table of the variable name and the index 
  dt <- data.table(variable_name = names, 
                   ini_index = csv_input_index)
  
  # Return output
  return(dt)
  
}

#' Deactivate variables in the Hector ini files.
#' 
#' Manipulate the ini file by activating variables. This is 
#' useful for setting up the ini files with different inputs 
#' and outputs. 
#'
#' @param lines character containing the Hector ini input. 
#' @param vars a vector of the character files. 
#' @return the lines of the modified ini file.  
#' @export
deactivate_variables <- function(lines, vars){
  
  # Make sure that lines. 
  assertthat::assert_that(is.character(lines))
  
  # Determine which inputs are using csv files from the ini file. 
  dt <- identify_csv_inputs()
  
  # Make sure that the variables to deactivate are listed in the ini file.
  missing <- !vars %in% dt[['variable_name']]
  assertthat::assert_that(sum(missing) == 0, msg = paste0('the following variables are not recognized as hector inputs: ', paste(vars[missing], collapse = ', ')))
  
  # Subset the variables and the ini lines that should be deactivate 
  # and use the comment symbol ';' to deactivate these variables aka 
  # do not read inputs from the csv file. 
  indicies        <- dt[variable_name %in% vars, ][['ini_index']]
  lines[indicies] <-  paste0(';', lines[indicies])
  
  # Return lines. 
  return(lines)
}


#' Activate variables in the Hector ini files.
#' 
#' Manipulate the ini file by activating variables. This is 
#' useful for setting up the ini files to read in constraints.
#'
#' @param lines character containing the Hector ini input. 
#' @param vars a vector of the character files. 
#' @return the lines of the modified ini file.  
#' @export
activate_variables <- function(lines, vars){
  
  # Make sure that lines. 
  assertthat::assert_that(is.character(lines))
  
  # Determine which inputs are using csv files from the ini file. 
  dt <- identify_csv_inputs()
  
  # Make sure that the variables to be activated that are listed in the ini file.
  missing <- !vars %in% dt[['variable_name']]
  assertthat::assert_that(sum(missing) == 0, msg = paste0('the following variables are not recognized as hector inputs: ', paste(vars[missing], collapse = ', ')))
  
  # Subset the variables and the ini lines that should be activated 
  # by removing the comment symbol ';' to activate these these inputs. 
  indicies        <- dt[variable_name %in% vars, ][['ini_index']]
  lines[indicies] <- gsub(pattern = '^;|^; ', x =  lines[indicies], replacement = '')
  
  # Return lines. 
  return(lines)
  
}


