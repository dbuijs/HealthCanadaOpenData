#Daniel Buijs, daniel.buijs@hc-sc.gc.ca
#This script downloads and imports the Health Canada Patent Register

library(rvest)
library(XML)
library(httr)
library(lubridate)
library(stringr)
library(magrittr)
library(dplyr)
library(data.table)

#Link to the Health Canada Patent Register download file
prurl <- "http://pr-rdb.hc-sc.gc.ca/patent/Patent.zip"

#Extract is nightly, so the date will be set to the day before today
prdate <- today() - days(1)

#Download and extract the Patent Register db extract

if(!(file.exists("../data/pr"))) dir.create("../data/pr")
prfile <- paste0("../data/pr/", basename(prurl))
download.file(prurl, prfile)
unzip(prfile, exdir = "../data/pr")

prfiles <- list.files("../data/pr", pattern = "*.txt")

for(i in prfiles){varname <- i %>% 
                    str_extract(regex(".*(?=\\.txt$)")) %>%
                    {paste0("pr_", .)} %>%
                    {ifelse(str_detect(., "-.*e$"), 
                           str_extract(., regex("^.*(?=-)")) %>% paste0("_e"),
                           .)} %>%
                    {ifelse(str_detect(., "-.*f$"), 
                            str_extract(., regex("^.*(?=-)")) %>% paste0("_f"),
                             .)}
                   prfilepath <- paste0("../data/pr/", i)
                   assign(varname, read.csv(prfilepath, 
                                            header=TRUE, 
                                            stringsAsFactors = FALSE, 
                                            fileEncoding = "UTF-8",
                                            strip.white = TRUE) %>%
                            as.data.table())
                   print(paste0(varname, " read!"))}


#Clean up transients
rm(list= c("prurl",
           "prfile",
           "prfiles",
           "i",
           "prfilepath",
           "varname"))
#Helper functions?

# Need to read in files, change names, map variables, fix encodings
