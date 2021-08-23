context('convert functions')

years <- 1975:2010

test_that('convert_rcmipCMIP6_hector', {
  
  # Test specifc emission species, that have had conversion issues in the past.
  x <- 10 
  
  n2o_entry <- data.table::as.data.table(hectordata::rcmipCMIP6_conversion)[hector_variable == "N2O_emissions", ]
  ztrue <- x * (biogas::molMass("N2O") / (biogas::molMass("N") * 2))
  # Divide by 100 to force back to arbitrary units 
  z     <- ud_convert2(x, from = n2o_entry$hector_udunits, to = n2o_entry$rcmip_udunits)/1000
  testthat::expect_equal(z, ztrue)
  

  so2_entry <-   data.table::as.data.table(hectordata::rcmipCMIP6_conversion)[hector_variable == "SO2_emissions", ]
  ztrue <- x * (biogas::molMass("S") / biogas::molMass("SO2"))
  z     <- ud_convert2(x, from = so2_entry$rcmip_udunits, to = so2_entry$hector_udunits)/1000

})