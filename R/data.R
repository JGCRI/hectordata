#' template_ini
#'
#' A character vector consisting of Hector inputs. 
#'
#' @format A vector 
#' \describe{ A character vector where each element in the vector contains a different 
#' line of hector ini file. 
#' }
'template_ini'


#' rcmipCMIP6_conversion
#'
#' A data.table containing information that is used to convert the RCMIP emissions and 
#' concentration into Hector inputs.
#'
#' @format A data.table of 7 columns and 99 rows. 
#' \describe{ A character vector where each element in the vector contains a different 
#' \item{hector_component}{String character of the Hector component of the hector_variable.}
#' \item{hector_variable}{String character variable name in the Hector format.}
#' \item{hector_unit}{String character of the units expected by Hector.}
#' \item{hector_udunits}{String character of the Hector units that can be used by \code{ud_convert2}.}
#' \item{rcmip_variable}{String character variable name in the RCMIP format.}
#' \item{rcmip_units}{String character of the units of the RCMIP variable.}
#' \item{rcmip_udunits}{String character of the units of the RCMIP variable that can be used by \code{ud_convert2}.}
#' }
'rcmipCMIP6_conversion'