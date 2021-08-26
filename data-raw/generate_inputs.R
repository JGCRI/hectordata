# Generate the emissions tables and ini files. 

# Create the directory to write the output to. 
DIR <- here::here("inst")
dir.create(DIR, recursive = TRUE, showWarnings = FALSE)


# Generate the emissions & ini files. 
scns <- c("ssp370", "ssp434", "ssp460", "ssp119", "ssp126", "ssp245", "ssp534-over", "ssp585") 
yrs  <- 1750:2100
generate_inputs(scenarios = scns, write_to = DIR, years = yrs)