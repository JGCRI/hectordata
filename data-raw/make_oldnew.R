library(hector)

# List all of the ini files in the package. 
# TODO this might pose as an issue in that it requires the package to be built. 
ini_files <- list.files(system.file("input", package = "hectordata"), 
                        pattern = "ini", full.names = TRUE)

comparison_dates <- seq(from = 1750, to = 2300, by = 20)
comparison_vars <- c(GLOBAL_TAS(), RF_TOTAL(), HEAT_FLUX(), CONCENTRATIONS_CO2(),
                     NPP(), OCEAN_C())

f <- ini_files[1]
name <- gsub(pattern = ".ini", x = basename(f), replacement = "")
hc <- newcore(inifile = f, name = name)
run(hc, runtodate = max(comparison_dates))
fetchvars()