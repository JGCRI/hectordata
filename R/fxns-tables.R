#' Save the hector csv files into the proper hector format 
#'
#' @param x data table containing Hector input values
#' @param required str vector of the required variables that must be included in the table
#' @param write_to str directory to write the hector csv output to 
#' @param info_source string name indicating the source part of the module name
#' @param end_tag string used at the end of of the table file, in most cases will be default name "_emiss-constraints_rf"
#' @return str file name 
#' @import assertthat 
write_hector_csv <- function(x, required, write_to, info_source, end_tag = "_emiss-constraints_rf"){
  
  # Format and save the emissions and concentration constraints in the csv files 
  # in the proper Hector table input file. 
  assert_that(dir.exists(write_to))
  assert_that(has_name(x, c("scenario", "year", "variable", "units", "value")))
  
  # Create the file name
  scn   <- unique(x[['scenario']])
  assert_that(length(scn) == 1)
  fname <- file.path(write_to, paste0(info_source, '_', scn, end_tag, '.csv'))
  
  missing <- !required %in% unique(x[["variable"]])
  assert_that(all(!missing), msg = paste("Missing required variable(s):", paste0(required[missing], collapse = ", ")))
  
  # Transform the data frame into the wide format that Hector expects. 
  input_data <- dcast(as.data.table(x)[, list(Date = year, variable, value)], Date ~ variable)
 
  # Add the header information to the csv table. 
  # TODO look into a more efficient way to do this, one that does not 
  # require intermediate products to be written out to the disk. 
  readr::write_csv(input_data, fname, append = FALSE, col_names = TRUE)
  lines <- readLines(fname)
  
  # Format a list of units that will be used in the header. 
  var_units <- unique(x[ , list(variable, units)])
  units_list <- paste(c('; UNITS:', var_units$units), collapse = ', ')
  
  git_tag <- substr(system("git rev-parse HEAD", intern=TRUE), start = 1, stop = 15)
  create_info <-  c(paste0('; created by hectordata ', date(), 
                           " commit ", git_tag))
  final_lines <- append(c(paste0('; ', scn, " from ", info_source),
                          paste0('; hectordata ', utils::packageVersion(pkg = 'hectordata')),
                          paste0("; commit ", git_tag), 
                          paste0("; date ", date()), 
                          units_list),
                        lines)
  writeLines(final_lines, fname)
  return(fname)
  
}