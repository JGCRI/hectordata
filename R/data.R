#' template_ini
#'
#' List of data.tables containing information that is used to convert input data provided from 
#' various sources such as IIASA, GCAM, RCMIP and so on into the appropriate values for Hector. 
#'
#' @format character
#' \describe{ A character vector where each element vector contains a different
#' line of hector ini file.
#' }
'template_ini'


#' conversion_table
#'
#' List of data.tables containing information that is used to convert input data provided from 
#' various sources such as IIASA, GCAM, RCMIP and so on into the appropriate values for Hector. 
#'
#' @format list of data tables
#' \describe{list of data tables, each data table contains the mapping between inputs and hector. Any column with the 
#' udunits tag can be used by the ud_convert2 function
#' \item{rcmip}{data table of 74 rows with 7 columns  hector_component, hector_variable, hector_unit, hector_udunits
#'  rcmip_variable, rcmip_units, rcmip_udunits.}
#' }
'conversion_table'