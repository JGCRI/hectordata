# # templare tof
# 
# 
# # the module naming pattern is
# # module_ the source of the data to process AR6, CMIP5, CEDS, GCAM, ect. and the class of the scenarios
# # to be processed, ssps, rcps, historical, idealized ect.
# module_source_scn <- function(command, ...) {
#   if(command == driver.DECLARE_INPUTS) {
#     return(c(FILE = "socioeconomics/USDA_GDP_MER",
#              FILE = "socioeconomics/WB_ExtraCountries_GDP_MER"))
#   } else if(command == driver.DECLARE_OUTPUTS) {
#     return(c("L100.gdp_mil90usd_ctry_Yh"))
#   } else if(command == driver.MAKE) {
# 
# 
# 
#     (ini, file)
# 
# 
#   } else {
#     stop("Unknown command")
#   }
# }
