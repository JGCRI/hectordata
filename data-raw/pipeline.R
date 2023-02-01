# TODO there has to be be a better way to do this may be with a drake pipeline idk
library(hectordata)
library(hector)

# Download and process data  
# NOTE: this step can take a while and only needs to be done once
fetch_minted_data()
process_rcmip_data()
process_v25_data()

generate_rcmip_submission_files()
generate_v25_files()
generate_idealized_files()
