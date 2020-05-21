## License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

# TODO check with chris if there is a better way to do this, could we import directly 
# from a google drive? 
# Format the CMIP6 emissions and concentration inputs into the proper strucutre and format 
# to be used as Hecotr inputs. The CMIP6 scenario inputs are provided by the Reduced Complexity 
# Model Intercomparison at https://www.rcmip.org/. Due to the large size of the rcmip csv files 
# these files are not committed  to github or included in this package. Users will have to 
# downlaod the rcmip-emissions and rcmip-concentration annual mean files from https://www.rcmip.org/
# to store it in the hectordata/data-raw directory. 

install.packages("RCurl")
library(RCurl)

link <- "https://drive.google.com/file/d/1krA0lficstXqahlNCko7nbfqgjKrd_Sa"
url <- getURL( link ) 
con <- textConnection( url )
download.file(con, './test.csv')
             

download.file(url, destfile)
data <- read.csv( con )
head(data)
dim(data)

install.packages('googledrive')
library(googledrive)
dl <- drive_download(
  as_id("1AiZda_1-2nwrxI8fLD0Y6e5rTg7aocv0"), path = temp, overwrite = TRUE)
out <- unzip(temp, exdir = tempdir())
bank <- read.csv(out[14], sep = ";")
