library(data.table)

# Save a list of conversion tables that will be used to convert emissions and concentrations 
# to Hector inputs. 
conversion_table <- list()

# conversion table for RCMIP inputs
# ---------------------------------------------------------------------------------------------------
# For the SSP inputs we use the conversion table originally created for the RCMIP project see 
# https://github.com/ashiklom/hector-rcmip/tree/master/inst, modified for Hector v3. 
path <- here::here('data-raw', 'RCMIP_variable-conversion.csv') 
rcmip <- utils::read.csv(path, stringsAsFactors = FALSE)

# Because of how RCMIP conversion table was formatted make sure that the strings do not start with a space. 
cols_to_modify <- which(names(rcmip) %in% c("Model", "Scenario", "Region", "Variable", "Unit", "Mip_Era"))
for(i in seq_along(cols_to_modify)){rcmip[[i]] <- trimws(rcmip[[i]], which = 'left')}
conversion_table[["rcmip"]] <- rcmip


# Mapping table for the v25 to v3 
# ---------------------------------------------------------------------------------------------------
# Mapping for the variable names 
path <- here::here('data-raw', 'v25_variable-mapping.csv') 
v25 <- as.data.table(utils::read.csv(path, stringsAsFactors = FALSE))
conversion_table[["v25"]] <- v25


# ---------------------------------------------------------------------------------------------------
# Save the table as internal package data. 
usethis::use_data(conversion_table, overwrite = TRUE, compress = "xz", internal = FALSE)
