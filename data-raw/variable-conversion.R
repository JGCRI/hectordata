## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

# Save the tables that will be used to convert emission and concentration time series 
# to Hector inputs. 

# For the CMIP6 inputs we use the conversion table created by the RCMIP project.
path <- here::here('data-raw', 'RCMIP_variable-conversion.csv') 
rcmipCMIP6_conversion <- data.table::as.data.table(readr::read_csv(path))

# Because of how RCMIP conversion table were formatted make sure that the strings do not start with a space. 
cols_to_modify <- which(names(rcmipCMIP6_conversion) %in% c("Model", "Scenario", "Region", "Variable", "Unit", "Mip_Era"))
for(i in seq_along(cols_to_modify)){rcmipCMIP6_conversion[[i]] <- trimws(rcmipCMIP6_conversion[[i]], which = 'left')}

# Save the table as internal pacakge data. 
usethis::use_data(rcmipCMIP6_conversion, overwrite = TRUE, compress = "xz")