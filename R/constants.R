YEARS <- 1745:2300 

SCRATCH_DIR <- tempdir()
TABLES_DIR <- here::here("inst", "input", "tables")
ZENODO_DIR <- here::here("zenodo") # TODO there is a better way to handle this. 
INTERMEDIATE_DIR <- here::here("intermediate") # TODO there has to be a better way to pass intermediate to downstream processes.

# Vector of all the emissions that must be included a Hector input table. The constraints 
# and other radiative forcing inputs may not be necessary it really depends run configuration.
REQUIRED_EMISSIONS <- c("ffi_emissions", "luc_emissions", "daccs_uptake", "luc_uptake", "BC_emissions", "C2F6_emissions", 
                        "CCl4_emissions", "CF4_emissions", "CFC113_emissions", "CFC114_emissions", "CFC115_emissions", 
                        "CFC11_emissions", "CFC12_emissions", "CH3Br_emissions", "CH3CCl3_emissions", "CH3Cl_emissions", 
                        "CH4_emissions", "CO_emissions", "HCFC141b_emissions", "HCFC142b_emissions", "HCFC22_emissions", 
                        "HFC125_emissions", "HFC134a_emissions", "HFC143a_emissions", "HFC227ea_emissions", "HFC23_emissions",
                        "HFC245fa_emissions", "HFC32_emissions", "HFC4310_emissions", "N2O_emissions", "NH3_emissions", 
                        "NMVOC_emissions", "NOX_emissions", "OC_emissions", "SF6_emissions", "SO2_emissions", 
                        "halon1211_emissions", "halon1301_emissions", "halon2402_emissions")

REQUIRED_RF <- c("RF_albedo", "SV")





