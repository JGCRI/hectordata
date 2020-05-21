## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

# This script imports the files that will be used convert from the raw data inputs into 
# input timeseries that can be used by Hector. 

# For the CMIP6 inputs we use the conversion table created by the RCMIP project.
# TODO should this be archived with the other minted data?
path <- '~/Documents/2020/hectordata/data-raw/variable-conversion.csv'
rcmipCMIP6_conversion <- data.table::as.data.table(readr::read_csv(path))
usethis::use_data(rcmipCMIP6_conversion, overwrite = TRUE, compress = "xz")


