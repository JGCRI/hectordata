

#' Make the data frame of the Hector pre industrial control inputs
#' 
#' This function produces the table of Hector inputs for the preindustrial control run 
#' inputs. Any ghg that has a preindustrial concentration must be held constant at the value 
#' in order to have zero radiative forcing effect. 
#'
#' @import hector
#' @return dataframe of the pre industrial control inputs, uses constant concentration constraints when appropriate.
make_hector_picontrol_df <- function(){
  
  # TODO figure out a flexible way to define the preindustrial values from the 
  # the ini files there also might be a way to determine the variable names 
  # from the ini files as well. 
  CH40_val <- 731.41 
  CO2_val <- 277.15
  N20_val <- 273.87
  CF40_val <- 35.0
  CH3Cl0_val <- 504.0 
  CH3Br0_val <- 5.8
  
  preindustrial_wmghgs <- c(CH40_val, CO2_val, N20_val, CF40_val, CH3Cl0_val, CH3Br0_val)
  names(preindustrial_wmghgs) <- c(CH4_CONSTRAIN(),  CO2_CONSTRAIN(), N2O_CONSTRAIN(), 
                                   CF4_CONSTRAIN(), CH3CL_CONSTRAIN(), CH3BR_CONSTRAIN())
  
  do.call(what = "rbind", 
          mapply(FUN = function(v, n){
            out <- data.frame(year = YEARS, 
                              value = v, 
                              variable = n, 
                              units = getunits(n))
            return(out)
          }, 
          v = preindustrial_wmghgs, 
          n = names(preindustrial_wmghgs), 
          USE.NAMES = FALSE, 
          SIMPLIFY = FALSE)) ->
    preindustrial_constant_values
  
  # Because one of the RFs cannot be set using the get 
  rf_inputs <- c(RF_ALBEDO(), RF_MISC(), VOLCANIC_SO2())
  do.call(what = "rbind", 
          lapply(rf_inputs, function(x){
            out <- data.frame(year = YEARS, 
                              value = 0, 
                              variable = x, 
                              units = "W/m2")
          })) -> 
    rf_zero
  
  do.call(what = "rbind", 
          lapply(REQUIRED_EMISSIONS, function(x){
            out <- data.frame(year = YEARS, 
                              value = 0, 
                              variable = x, 
                              units = getunits(x))
          })) -> 
    emissions_zero
  
  out <- rbind(preindustrial_constant_values, rf_zero, emissions_zero)
  out$scenario <- "picontrol"
  
  return(out)
  
}


#' Make the data frame of the Hector inputs for a concentration driven abrupt 2 x step 
#' 
#' This function produces the table of Hector inputs for the abrupt 2 x step test run 
#' inputs. Any ghg that has a pre-industrial concentration must be held constant at the value 
#' in order to have zero radiative forcing effect. The CO2 concentration constraint has 
#' is held constant until the \link{first_yr} when the concentration will double. 
#'
#' @param first_yr int year indicating the start year of the idealized experiment
#' @return dataframe of the inputs, using constant constant emissions and concentration constraints when appropriate.
#' @import hector
make_hector_abruptx2CO2_df <- function(first_yr){
  picontrol <- make_hector_picontrol_df()
  picontrol_nonCO2 <- picontrol[picontrol$variable != CO2_CONSTRAIN(), ]
  picontrol_CO2 <- picontrol[picontrol$variable == CO2_CONSTRAIN(), ]
  
  CO20 <- unique(picontrol_CO2$value)

  # Abrupt x 2 CO2
  abruptx2CO2_CO2 <- picontrol_CO2
  abruptx2CO2_CO2[abruptx2CO2_CO2$year >= first_yr, ]$value <- CO20 * 2
  abruptx2CO2 <- rbind(abruptx2CO2_CO2, picontrol_nonCO2)
  abruptx2CO2$scenario <- "abruptx2CO2"
  return(abruptx2CO2)
}


#' Make the data frame of the Hector inputs for a concentration driven abrupt 4 x step 
#' 
#' This function produces the table of Hector inputs for the abrupt 4 x step test run 
#' inputs. Any ghg that has a pre-industrial concentration must be held constant at the value 
#' in order to have zero radiative forcing effect. The CO2 concentration constraint has 
#' is held constant until the \link{first_yr} when the concentration quadruples 
#'
#' @param first_yr int year indicating the start year of the idealized experiment
#' @return dataframe of the inputs, using constant constant emissions and concentration constraints when appropriate.
#' @import hector
make_hector_abruptx4CO2_df <- function(first_yr){
  picontrol <- make_hector_picontrol_df()
  picontrol_nonCO2 <- picontrol[picontrol$variable != CO2_CONSTRAIN(), ]
  picontrol_CO2 <- picontrol[picontrol$variable == CO2_CONSTRAIN(), ]
  
  CO20 <- unique(picontrol_CO2$value)
  
  abruptx4CO2_CO2 <- picontrol_CO2
  abruptx4CO2_CO2[abruptx4CO2_CO2$year >= first_yr, ]$value <- CO20 * 4
  abruptx4CO2 <- rbind(abruptx4CO2_CO2, picontrol_nonCO2)
  abruptx4CO2$scenario <- "abruptx4CO2"
  return(abruptx4CO2)
  
}


#' Make the data frame of the Hector inputs for a concentration driven abrupt 1pct CO2 run
#' 
#' This function produces the table of Hector inputs for the 1pct CO2 run 
#' inputs. Any ghg that has a pre-industrial concentration must be held constant at the value 
#' in order to have zero radiative forcing effect. The CO2 concentration increases by 1% per year.
#'
#' @param first_yr int year indicating the start year of the idealized experiment
#' @return dataframe of the inputs, using constant constant emissions and concentration constraints when appropriate.
#' @import hector
make_hector_1pctCO2_df <- function(first_yr){
  
  t <- 0:535
  picontrol <- make_hector_picontrol_df()
  picontrol_nonCO2 <- picontrol[picontrol$variable != CO2_CONSTRAIN(), ]
  picontrol_CO2 <- picontrol[picontrol$variable == CO2_CONSTRAIN(), ]
  
  CO20 <- unique(picontrol_CO2$value)
  pctincrease_CO2 <- c(CO20 * (1 + 0.01)^t)
  dim_to_use <- length(YEARS[YEARS < first_yr]) + length(t)
  
  pctCO2_increase_CO2 <- picontrol_CO2[1:dim_to_use, ]
  pctCO2_increase_CO2[pctCO2_increase_CO2$year >= first_yr, ]$value <- pctincrease_CO2
  pctCO2_increase_nonCO2 <- picontrol_nonCO2[picontrol_nonCO2$year %in% pctCO2_increase_CO2$year, ]
  pctCO2_increase <- rbind(pctCO2_increase_CO2, pctCO2_increase_nonCO2)
  pctCO2_increase$scenario <- "1pctCO2"
  return(pctCO2_increase)
  
}

# make_hector_esm1pctCO2_dif <- function(first_yr){
#   
#   conc_driven <- make_hector_1pctCO2_df(first_yr)
#   conc_driven <- conc_driven[conc_driven$year <= 2000, ]
#   goal_co2 <- conc_driven[conc_driven$variable == CO2_CONSTRAIN(), ]
#   constant_conc_driven <- conc_driven[conc_driven$variable != CO2_CONSTRAIN(), ]
#   
#   ini <- list.files(system.file("input", package = "hector"), pattern = "ini", full.names = TRUE)[[2]]
#   hc <- newcore(ini)
#   lapply(split(constant_conc_driven, constant_conc_driven$variable), function(x){
#     setvar(hc, dates = x$year, var = x$variable, values = x$value, unit = x$units)
#     reset(hc)
#   })
#   
#   target_fxn <- function(emiss){
#     setvar(hc, dates = goal_co2$year, var = FFI_EMISSIONS(), values = emiss, unit = getunits(FFI_EMISSIONS()))
#     reset(hc)
#     run(hc, runtodate = max(goal_co2$year))
#     hout <- fetchvars(hc, dates = goal_co2$year, CONCENTRATIONS_CO2())
#     # return the output between the two 
#     hout$value - goal_co2$value
#     
#   }
#   
#   inital <- rep(10, length.out = nrow(goal_co2))
#   idk <- nleqslv::nleqslv(x = inital, fn = target_fxn, method = "Broyden")
#   
# }





#' A helper function that formats and write the idealized hector ini files out 
#' 
#' This works for a singel scenario at a time. If multiple scneairos are read in it will 
#' throw and error message!
#'  
#' TODO should not be writing a custom table maker function for each set of ini files but here we are
#' @param dat data fame containing some information about corresponding csv file the scenario and  variables
#' @param csv_path string vector of the required intermediate data tables
#' @return nothing writes out ini file
idealized_ini_table_maker <- function(dat, csv_path){
  assert_that(check_cols(dat, req = c("variable", "scenario")))
  assert_that(length(unique(dat$scenario)) == 1)
  assert_that(file.exists(csv_path))
  
  scn <- unique(dat$scenario)
  rel_csv_path <- file.path("tables", basename(csv_path))
  new_ini <- replace_csv_string(hectordata::template_ini, replacement_path = rel_csv_path, run_name = scn)
  
  unique_vars <- unique(dat$variable)
  to_activate <- unique_vars[grepl(pattern = "constrain", x = unique_vars)]
  new_ini <- activate_input_variables(lines = new_ini, vars = to_activate)
  
  write_to <- gsub(pattern = "/tables", x = dirname(csv_path), replacement = "")
  name <- paste0("hector_", scn)
  ini_path <- file.path(write_to, paste0(name, ".ini"))
  writeLines(new_ini, ini_path)
}



#' Module that prepares all of the Hector inputs for idealized runs
#' 
#' 
#'  
#' @param start_yr int year indicating the start year of the idealized experiment
#' @return nothing writes out ini file
#' @export
generate_idealized_files <- function(start_yr = 1760){
  
  picontrol <- make_hector_picontrol_df()
  abruptx2CO2_CO2 <- make_hector_abruptx2CO2_df(start_yr)
  abruptx4CO2_CO2 <- make_hector_abruptx4CO2_df(start_yr)
  pctIncrease_CO2 <- make_hector_1pctCO2_df(start_yr)
  
  df_list <- list(picontrol, abruptx2CO2_CO2, abruptx4CO2_CO2, pctIncrease_CO2) 
  for(x in df_list){
    
    ofile <- write_hector_csv(x = as.data.table(x), 
                              info_source = "hector", 
                              write_to = TABLES_DIR, 
                              end_tag = NULL)
    
    idealized_ini_table_maker(dat = as.data.table(x), csv_path = ofile)
    
  }
  
}




