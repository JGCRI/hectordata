# 
# sub_dir <- '~/Desktop'
# 
# make_hector_inputs <- function(write_to, scenarios, years){
#   
#   # Make sure that the write to  directory for the ini files and that the 
#   # scenarios can be processed. 
#   assert_that(file.exists(write_to))
# 
#   # Write the input tables to a sub directory of write_to, so that the 
#   # input tables and ini files are not directory dependent.
#   files <- generate_csv_tables(scenarios = scenarios, write_to = write_to, years = years)
#   files <- normalizePath(files)
#   names <- gsub(x = basename(files), pattern = '_emiss-constraints.csv', replacement = '')
#   
#   # Replace the path to the emissions and concentration constratins in the ini files.
#   # These are the basic ini files. 
#   mapply(function(x, y){
#     out <- replace_csv_string(ini = hectordata::template_ini, replacement_path = x, run_name = y)
#     return(out)
#   }, x = files,  y = names, SIMPLIFY = FALSE) -> 
#     ini_list 
#   names(ini_list) <- names
#   
#   # TODO code needs to be added here to active the constraints whene appropriate.... 
#   # Save the ini files.
#   mapply(function(x, y){
#     
#     file <- file.path(write_to, paste0('hector_', y, '.ini'))
#     writeLines(text = x, con = file)
#     return(file)
#     
#   }, x = ini_list, y = names)
#   
#   
# }