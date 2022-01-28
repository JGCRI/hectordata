# ------------------------------------------------------------------------------
# Program Name: CEDS_hector_formatting.R
# Authors: Harrison Suchyta
# Date Last Modified: January 14, 2022
# Program Purpose: Aggregates and transforms CEDS csv data to be compatible with
# Hector
# Input Files: ~hectordata/ceds/input
# Output Files: ~hectordata/ceds/output
#-------------------------------------------------------------------------------
# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
#library(udunits2)
#library(devtools)
library(ggplot2)

line_plot <- function(emission_col, emission_row, ylabel, emi_name){
  
  em_row <- t(emission_row)
  
  #combines data from both into a dataframe 
  d1 <- data.frame(year = hector_emissions_1750$Date, type = "CEDS", val = em_row)
  d2 <- data.frame(year = hector_emissions_1750$Date, type = "hector", val = emission_col)
  both_datasets <- rbind(d1,d2)
                   
  
  dataset_plot <- ggplot(both_datasets, aes(x = year, y = val))+
    geom_line(aes(color = type))+
    labs(
      x = "year",
      y = paste0(ylabel),
      color = "Legend"
    )+
    theme(legend.position="right")+
    ggtitle(paste0(emi_name, " emissions data"))
    #title(paste0("Cumulative Emissions for ", emi_name))
  ggsave(paste0("output/",emi_name,".png"))
}

#Creates a variable for the current base ceds directory in hectordata
curr_dir <- paste0('C:/Users/Such559/Documents/hectordata/ceds')

#load in the conversion factor csv
conv_factor <- read.csv(file = paste0(curr_dir, '/input/conversion_factors.csv'), fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)

#read in hector data
hector_emissions <- read.csv(file = paste0(curr_dir, '/input/ssp245.csv'), comment.char = ";", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE) %>%
                    subset(select = c(Date,ffi_emissions,BC_emissions, CH4_emissions, CO_emissions,N2O_emissions,NH3_emissions,NMVOC_emissions,NOX_emissions,OC_emissions,SO2_emissions))

#Gets rid of emission data before 1750 and after 2019
hector_emissions_1750 <- hector_emissions[-c(1:5),]
hector_emissions_1750 <- hector_emissions_1750[-c(271:nrow(hector_emissions_1750)),]

setwd(paste0(curr_dir, '/input/emissions_global'))

#Reads in the global csv files and put them into one dataframe
temp = list.files(pattern="*.csv")
myfiles = lapply(temp, function(i){read.csv(i, header = TRUE, stringsAsFactors = FALSE)})
total_emissions <- dplyr::bind_rows(myfiles)  

      
#aggregate the data, grouping by emission type             
tot_emi <- total_emissions %>%
           subset(select = -c(sector)) %>%
           group_by(em,units) %>%
           summarise(across(where(is.numeric), ~ sum(., is.na(.), 0)))

#multiply the numerical values by the conversion constants in the conversion csv
#and change the units to hector units
for (i in  1:nrow(tot_emi)){
  tot_emi[i,(3:ncol(tot_emi))] <- tot_emi[i,(3:ncol(tot_emi))]*conv_factor$conversion_ceds_to_hector[conv_factor$em == tot_emi$em[i]]
  tot_emi$units[i] <- conv_factor$hector_unit[conv_factor$em == tot_emi$em[i]]
}
setwd(curr_dir)
line_plot(hector_emissions_1750$BC_emissions,tot_emi[1,(3:ncol(tot_emi))],"BC emissions Tg [C]","BC")
line_plot(hector_emissions_1750$CH4_emissions,tot_emi[2,(3:ncol(tot_emi))],"CH4 emissions Tg [CH4]","CH4")
line_plot(hector_emissions_1750$CO_emissions,tot_emi[3,(3:ncol(tot_emi))],"CO emissions Tg [C]","CO")
line_plot(hector_emissions_1750$ffi_emissions,tot_emi[4,(3:ncol(tot_emi))],"CO2 emissions Pg [C]","CO2")
line_plot(hector_emissions_1750$N2O_emissions,tot_emi[5,(3:ncol(tot_emi))],"N2O emissions Tg [N]","N2O")
line_plot(hector_emissions_1750$NH3_emissions,tot_emi[6,(3:ncol(tot_emi))],"NH3 emissions Tg [NH3]","NH3")
line_plot(hector_emissions_1750$NMVOC_emissions,tot_emi[7,(3:ncol(tot_emi))],"NMVOC emissions Tg [NMVOC]","NMVOC")
line_plot(hector_emissions_1750$NOX_emissions,tot_emi[8,(3:ncol(tot_emi))],"NOx emissions Tg [N]","NOx")
line_plot(hector_emissions_1750$OC_emissions,tot_emi[9,(3:ncol(tot_emi))],"OC emissions Tg [OC]","OC")
line_plot(hector_emissions_1750$SO2_emissions,tot_emi[10,(3:ncol(tot_emi))],"SO2 emissions Gg [S]","SO2")
 

#generate a new csv file
#write_csv(tot_emi,paste0(curr_dir, '/output/aggregate_emissions.csv'))

