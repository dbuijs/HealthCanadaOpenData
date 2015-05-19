#Daniel Buijs, dbuijs@gmail.com
#This script downloads and imports the Health Canada Drug Product Database
library(rvest)
library(XML)
library(httr)
library(lubridate)
library(stringr)
library(magrittr)
library(dplyr)
library(data.table)
library(digest)


#Get the DPD extract date
dpdcoverlink <- "http://www.hc-sc.gc.ca/dhp-mps/prodpharma/databasdon/dpd_bdpp_data_extract-eng.php"
dpdextractdate <- html_session(dpdcoverlink) %>%
  html_nodes("td:contains('allfiles.zip')+ td") %>%
  html_text() %>%
  parse_date_time("Ymd") %>%
  format("%Y-%m-%d")

#Download and extract the DPD extract
dpdurl <- "http://www.hc-sc.gc.ca/dhp-mps/alt_formats/zip/prodpharma/databasdon/allfiles.zip"
if(!(file.exists("../dpd"))) dir.create("../dpd")
download.file(dpdurl, "../dpd/dpdallfiles.zip")
unzip("../dpd/dpdallfiles.zip", exdir = "../dpd")

#Download and extract the DPD inactives extract
dpdiaurl <- "http://www.hc-sc.gc.ca/dhp-mps/alt_formats/zip/prodpharma/databasdon/allfiles_ia.zip"
download.file(dpdiaurl, "../dpd/dpdallfiles_ia.zip")
unzip("../dpd/dpdallfiles_ia.zip", exdir = "../dpd")


# DPD Variable Names
# libraries XML, httr, rvest, magrittr, dply and stringr loaded in project config
dpdreadme <- html_session("http://www.hc-sc.gc.ca/dhp-mps/prodpharma/databasdon/dpd_read_me-bdpp_lisez-moi-eng.php")
dpdtablenames <- dpdreadme %>% 
  html_nodes("caption") %>% 
  html_text() %>% 
  str_trim()
dpdvar <- list()
for(i in dpdtablenames){dpdcss <- paste0("table:contains('", i, "') td:nth-child(1)")
                        dpdname <- i %>%
                          tolower() %>%
                          str_extract(perl("(?<=qrym_).*$")) %>%
                          paste0("dpd_", .)
                        dpdvar[[dpdname]] <- dpdreadme %>% html_nodes(dpdcss) %>% html_text()}

dpdfiles <- list.files("../dpd", pattern = ".*txt")
dpdiafiles <- dpdfiles[grepl("_ia.txt|inactive", dpdfiles)]
dpdiafiles <- dpdiafiles[!grepl("inactive", dpdiafiles)]
dpdfiles <- dpdfiles[!grepl("_ia.txt|inactive", dpdfiles)]

for(i in dpdfiles){dpdnameroot <- i %>% tolower() %>% str_extract(perl(".*(?=\\.txt$)"))
                   varname <- paste0("dpd_", dpdnameroot)
                   dpdfile <- paste0("../dpd/", i)
                   assign(varname, fread(dpdfile, header=FALSE))}

for(i in dpdiafiles){dpdnameroot <- i %>% tolower() %>% str_extract(perl(".*(?=\\.txt$)"))
                   varname <- paste0("dpd_", dpdnameroot)
                   dpdiafile <- paste0("../dpd/", i)
                   assign(varname, fread(dpdiafile, header=FALSE))}

#Variable names
dpdtables <- sort(ls(pattern = "dpd_"))
dpdtables <- dpdtables[!grepl("_ia", dpdtables)]
dpdvarorder <- sort(names(dpdvar))[c(2:4,1,5:11)]
mapply(function(x, y) setnames(get(x), dpdvar[[y]]), dpdtables, dpdvarorder)
mapply(function(x, y) setnames(get(paste0(x, "_ia")), dpdvar[[y]]), dpdtables, dpdvarorder)

# Clean up transients
rm(list = c("dpdcoverlink", 
            "dpdurl", 
            "dpdvar", 
            "dpdtablenames", 
            "dpdreadme", 
            "dpdcss", 
            "dpdname",
            "dpdfiles",
            "dpdnameroot",
            "varname",
            "dpdfile",
            "dpdvarorder",
            "dpdtables",
            "i"))

# helper functions here?
