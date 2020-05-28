## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

# Save the tables that will be used to convert emission and concentration time series 
# to Hector inputs. 

# For the CMIP6 inputs we use the conversion table created by the RCMIP project.
# TODO should this conversion table be archived with the other minted data? It
# Does correspond specifically to that version of the data.  
path <- '~/Documents/2020/hectordata/data-raw/RCMIP_variable-conversion.csv'
rcmipCMIP6_conversion <- data.table::as.data.table(readr::read_csv(path))
usethis::use_data(rcmipCMIP6_conversion, overwrite = TRUE, compress = "xz")


