
# TODO Ideally would not need to hard code all of the different input names here but 
# that it could be read in as package data. But there was some issue with the 
# usethis::use_data function. 
input_names <- c("Date", "ffi_emissions", "luc_emissions", "daccs_uptake", "luc_uptake", "BC_emissions",  
                 "C2F6_constrain", "C2F6_emissions", "CCl4_constrain", "CCl4_emissions", "CF4_constrain", 
                 "CF4_emissions", "CFC113_constrain", "CFC113_emissions", "CFC114_constrain", "CFC114_emissions", 
                 "CFC115_constrain", "CFC115_emissions", "CFC11_constrain", "CFC11_emissions", "CFC12_constrain", 
                 "CFC12_emissions", "CH3Br_constrain", "CH3Br_emissions", "CH3CCl3_emissions", "CH3Cl_constrain", "CH3Cl_emissions", 
                 "CH4_constrain", "CH4_emissions", "CO2_constrain", "CO_emissions", "HCFC141b_constrain", "HCFC141b_emissions", 
                 "HCFC142b_constrain", "HCFC142b_emissions", "HCFC22_constrain", "HCFC22_emissions", "HFC125_constrain", 
                 "HFC125_emissions", "HFC134a_constrain", "HFC134a_emissions", "HFC143a_constrain", "HFC143a_emissions", 
                 "HFC227ea_constrain", "HFC227ea_emissions", "HFC23_constrain", "HFC23_emissions", "HFC245_constrain", 
                 "HFC245fa_emissions", "HFC32_constrain", "HFC32_emissions", "HFC365_constrain", "HFC365_emissions", "HFC4310_constrain", 
                 "HFC4310_emissions", "N2O_constrain", "N2O_emissions", "NH3_emissions", "NMVOC_emissions", "NOX_emissions",
                 "halon1211_constrain", "halon1211_emissions", "halon1301_constrain", "halon1301_emissions", "halon2402_constrain","halon2402_emissions",
                 "OC_emissions", "RF_albedo", "SF6_constrain", "SF6_emissions", "SO2_emissions", "SV", "HFC245fa_constrain") 

# TODO similarly this would be put into some sort of internal package data. 
input_units <- c("NA", "Pg C/yr", "Pg C/yr", "Pg C/yr", "Pg C/yr", "Tg", "pptv", "Gg", "pptv", "Gg", "pptv", "Gg", "pptv",
                 "Gg", "pptv", "Gg", "pptv", "Gg", "pptv", "Gg", "pptv", "Gg", "pptv", "Gg", "Gg", "pptv",    
                 "Gg", "ppbv CH4", "Tg CH4", "ppmv CO2", "Tg CO", "pptv", "Gg", "pptv", "Gg", "pptv", "Gg", "pptv", "Gg",      
                 "pptv", "Gg", "pptv", "Gg", "pptv", "Gg", "pptv", "Gg", "NA", "Gg", "pptv", "Gg", "NA",        
                 "NA", "pptv", "Gg", "ppbv N2O", "Tg N", "Tg", "Tg NMVOC", "Tg N", "pptv", "Gg", "pptv", "Gg", "pptv",    
                 "Gg", "Tg", "W/m2", "pptv", "Gg", "Gg S", "W/m2", "pptv")

  dplyr::filter(data.frame(variable = input_names, 
                           units = input_units), variable != "Date") -> 
  var_units



#' Write a Hector input csv file out in the proper format, it can only contain 
#' results for a single scenario. 
#'
#' @param x data table containing Hector input values, may be long or wide
#' @param write_to str directory to write the hector csv output to 
#' @param scn str scenario name
#' @param fname str name for 
#' @return str path for the output file 
#' @import assertthat 
write_input_table <- function(x, write_to, scn, fname){
  
  # Check the function arguments 
  stopifnot(dir.exists(write_to))
  stopifnot(is.data.frame(x))
  stopifnot(grepl(pattern = "csv", x = fname))
  stopifnot(is.character(scn))
  
  # Check to see if in wide or long format, this will determine how the data 
  # is processed. 
  is_long <- all(hasName(x = x, name = c(c("scenario", "year", 
                                           "variable", "units", "value"))))
  
  # If long will need to change to the wide format in order to write it out. 
  if(is_long){
    # TODO need to add capabilities
    # 1) check to see if 1 scenario 
    # 2) check to make sure that variables are hector names 
    # 3) check the year info, needs to be 1 yr resolution 
    # 4) reshape the data from long to wide 
  } else {
    # If the data frame is wide minimal checks to make sure it is ready to format.
    stopifnot(hasName(x = x, name = "Date"))
    stopifnot(min(x$Date) <= 1745)
    stopifnot(unique(diff(x$Date)) == 1)
    stopifnot(all(names(x) %in% c("Date", var_units$variable)))
    xx <- x
  }
  
  # Add the header information to the csv table. 
  # TODO look into a more efficient way to do this, one that does not 
  # require intermediate products to be written out to the disk. 
  ofile <- file.path(write_to, fname)
  readr::write_csv(xx, ofile, append = FALSE, col_names = TRUE)
  lines <- readLines(fname)
  
  unit_list <- paste(c("; UNITS:", var_units$units), collapse = ", ")
  git_tag <- substr(system("git rev-parse HEAD", intern=TRUE), start = 1, stop = 15)
  final_lines <- append(c(paste0('; ', scn),
                          paste0('; hectordata ', utils::packageVersion(pkg = 'hectordata')),
                          paste0("; commit ", git_tag), 
                          paste0("; date ", date())), 
                        lines)
  writeLines(final_lines, ofile)
  
  return(ofile)
  
}