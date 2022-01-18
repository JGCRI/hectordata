## Function that help to modify the ini files.  

#' Identify the variables and lines of hector ini file that use csvs to read in inputs. 
#' 
#' @return a data table of the variables that use csv as inputs. 
#' @noRd 
identify_csv_inputs <- function(){
  
  ini <- hectordata::template_ini
  
  # Which lines of ini use csv files to read in inputs?
  csv_input_index <- which(grepl(pattern = "=csv:.*.csv", x = ini))
  
  # Isolate the variable name defined in the ini file.
  names <- sapply(ini[csv_input_index], function(x){
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
deactivate_input_variables <- function(lines, vars){
  
  # Silence package checks 
  variable_name <- NULL 
  
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
activate_input_variables <- function(lines, vars){
  
  # Silence package checks 
  variable_name <- NULL
  
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

#' Replace the emissions csv file strings with the path to the new csv table
#'
#' @param ini lines of a Hector ini file. 
#' @param replacement_path the file path to the hector input csv table. 
#' @param run_name character name of run name. 
#' @param pattern  regular expression pattern of the csv paths to replace with `replacement_path`
#' the default is set to replace all emission and concentration constraints.
#' @return lines of a Hector ini file.
#' @export
#' @importFrom assertthat assert_that
replace_csv_string <- function(ini, replacement_path, run_name, pattern = "=csv:.*TEMPLATE.csv"){

  # Make sure the pattern exists. 
  assert_that(any(grepl(pattern = pattern, x = ini)))
  
  # Replace the path in the ini to the template csv file with a path to the input table. 
  new_ini <- gsub(pattern = pattern, replacement = paste0('=csv:', replacement_path), x = ini)
  new_ini <- gsub(pattern = 'TEMPLATE', replacement = run_name, x = new_ini)
  
  return(new_ini)
}

#' Generate a new ini file for a given emissions file
#'
#' @param files str vector of the new csv hector input files.
#' @return str of the ini files 
#' @export
#' @importFrom assertthat assert_that
make_new_ini <- function(files){
  
  # Silence global variables 
  template_ini <- NULL
  
  assert_that(all(file.exists(files)))
  
  out <- unlist(lapply(files, function(f){
    
    name <- gsub(x = basename(f), pattern = "_emiss-constraints_rf.csv", replacement = "")

    new_path <- file.path('tables', basename(f))
    new_ini <- replace_csv_string(template_ini, replacement_path = new_path, run_name = name)
    
    write_to <- dirname(dirname(f))
    ini_path <- file.path(write_to, paste0(name, '.ini'))
    writeLines(new_ini, ini_path)
    
    return(ini_path)
  }))
  
  return(out)
}



