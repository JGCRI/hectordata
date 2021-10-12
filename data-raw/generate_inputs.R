# Generate the emissions tables and ini files. 

# Create the directory to write the output to. 
DIR <- here::here("inst")
dir.create(DIR, recursive = TRUE, showWarnings = FALSE)

# Generate the emissions & ini files. 
scns <- c("ssp119", "ssp126", "ssp245", "ssp370", "ssp434", "ssp460", "ssp585", "ssp534-over", 
          "rcp26", "rcp45", "rcp60", "rcp85")
scns <- c("rcp26", "rcp45", "rcp60", "rcp85")
generate_inputs(scenarios = scns, write_to = DIR)
