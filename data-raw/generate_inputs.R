# Generate the emissions tables and ini files. 
devtools::load_all()

# Create the directory to write the output to. 
DIR <- here::here("inst", "input", "tables")
dir.create(DIR, recursive = TRUE, showWarnings = FALSE)

# Run the various modules to generate the hector input tables. 
# TODO figure out some way to check to see if the data has been 
# downloaded correctly, and a better way to launch the modules. 
module_rcmip_idealized()
module_rcmip_rcp()
module_rcmip_ssp()

