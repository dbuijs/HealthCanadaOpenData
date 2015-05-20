#Daniel Buijs, daniel.buijs@hc-sc.gc.ca
#This script downloads and imports the Health Canada NOC Database

library(rvest)
library(XML)
library(httr)
library(lubridate)
library(stringr)
library(magrittr)
library(dplyr)
library(data.table)


#Get the NOC db extract date
nocdbcoverlink <- "http://www.hc-sc.gc.ca/dhp-mps/prodpharma/notices-avis/noc-acc/noc_acc_data_extract-eng.php"
nocdbextractdate <- html_session(nocdbcoverlink) %>%
  html_nodes("h2:contains('Last updated')") %>%
  html_text() %>%
  str_extract(regex("(?<=Last updated:).*$")) %>%
  parse_date_time("Bdy") %>%
  format("%Y-%m-%d")

#Download and extract the NOC db extract
nocdburl <- "http://www.hc-sc.gc.ca/dhp-mps/alt_formats/txt/prodpharma/notices-avis/noc-acc/nocfiles.zip"
if(!(file.exists("../data/nocdb"))) dir.create("../data/nocdb")
nocfile <- paste0("../data/nocdb/",basename(nocdburl))
download.file(nocdburl, nocfile)
unzip(nocfile, exdir = "../data/nocdb")


# NOC Variable Names
# libraries XML, httr, rvest, magrittr, dply and stringr loaded in project config
nocreadme <- html_session("http://www.hc-sc.gc.ca/dhp-mps/prodpharma/notices-avis/noc-acc/noc_acc_data_extract_readme-lisezmoi-eng.php")
noctablenames <- nocreadme %>% 
  html_nodes("caption") %>% 
  html_text() %>% 
  str_trim()
nocvar <- list()
for(i in noctablenames){noccss <- paste0("table:contains('", i, "') td:nth-child(1)")
                        nocname <- i %>%
                                   tolower() %>%
                                   str_extract(regex("(?<=qry_).*$")) %>%
                                   paste0("noc_", .)
                        nocvar[[nocname]] <- nocreadme %>% html_nodes(noccss) %>% html_text()}

nocfiles <- list.files("../data/nocdb", pattern = "NOC|noc.*txt")

# for(i in nocfiles){varname <- i %>% tolower() %>% str_extract(regex(".*(?=\\.txt$)"))
#                    nocfilepath <- paste0("../data/nocdb/", i)
#                    assign(varname, fread(nocfilepath, header=FALSE))}
# As of May 2015, NOC extract includes a summary row at the bottom
for(i in nocfiles){varname <- i %>% tolower() %>% str_extract(regex(".*(?=\\.txt$)"))
                   nocfilepath <- paste0("../data/nocdb/", i)
                   assign(varname, read.csv(nocfilepath, 
                                            header=FALSE, 
                                            stringsAsFactors = FALSE, 
                                            fileEncoding = "ISO8859-1",
                                            strip.white = TRUE) %>%
                                   as.data.table())
                   print(paste0(varname, " read!"))}


#Fudge for extra column in noc_brand
noc_brand[, V4 := NULL]
#Variable names
for(i in ls(pattern = "noc_")){setnames(get(i), nocvar[[i]])}

#Clean up transients
rm(list= c("nocdbcoverlink",
           "noccss",
           "nocdburl",
           "nocfile",
           "nocfilepath",
           "nocfiles",
           "nocname",
           "nocreadme",
           "noctablenames",
           "nocvar",
           "varname",
           "i"))
#Helper functions?

# Need to read in files, change names, map variables, fix encodings
