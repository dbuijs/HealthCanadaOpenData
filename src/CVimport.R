#Daniel Buijs, dbuijs@gmail.com
#This script downloads and imports the Canada Vigilance Database from Health Canada
#Returns data.tables with the cv_ prefix, and cvextractdate with the date of the extract

library(rvest)
library(lubridate)
library(stringr)
library(magrittr)
library(dplyr)
library(data.table)
library(purrr)

cvzipurl <- "http://www.hc-sc.gc.ca/dhp-mps/alt_formats/zip/medeff/databasdon/extract_extrait.zip"
cvlanding <- "http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/extract_extrait-eng.php"
cvreadmeurl <- "http://www.hc-sc.gc.ca/dhp-mps/medeff/databasdon/structure-eng.php"

#Get the CV extract date

cvextractdate <- html_session(cvlanding) %>%
  html_nodes('p:contains("time period:")') %>%
  html_text() %>%
  str_extract(regex("(?<=1965 to )\\d{4}-\\d{2}-\\d{2}")) %>%
  parse_date_time("Ymd") %>%
  format("%Y-%m-%d")

#Download and unzip the CV extract

if(!(file.exists("../data/cv"))) dir.create("../data/cv")
download.file(cvzipurl, "../data/cv/cvextract.zip")
unzip("../data/cv/cvextract.zip", exdir = "../data/cv", junkpaths = TRUE)

# CV Variable Names
# Grab the readme page
cvreadme <- html_session(cvreadmeurl)
# Extract the table names
cvtablenames <- cvreadme %>% 
  html_nodes("h2:contains('.txt')") %>% 
  html_text() %>% 
  str_trim()
# Grab the html tables with the variables
cvtables <- html_table(cvreadme)
# Put the table names and variables into a list
cvvar <- list()
for(i in 1:length(cvtablenames)){
                        cvname <- cvtablenames[i] %>%
                          tolower() %>%
                          str_extract(regex("^.*(?=.txt)")) %>%
                          ifelse(str_detect(., "_lx$"), str_extract(., "^.*(?=_lx)"), .) %>%
                          paste0("cv_", .)
                        cvvar[[cvname]] <- cvtables[[i]][,2]}
# Grab the file names
cvfiles <- list.files("../data/cv", pattern = ".*txt")
# Import the files
for(i in cvfiles){varname <- i %>% 
                    str_extract(regex(".*(?=\\.txt$)")) %>%
                    ifelse(str_detect(., "_lx$"), str_extract(., "^.*(?=_lx)"), .) %>%
                    paste0("cv_", .) %>%
                    ifelse(. == "cv_drug_products", "cv_drug_product", .)
                  
                  cvfilepath <- paste0("../data/cv/", i)

                  assign(varname, read.delim(cvfilepath, 
                         header=FALSE,
                         sep = "$",
                         stringsAsFactors = FALSE, 
                         fileEncoding = "ISO8859-1",
                         strip.white = TRUE) %>%
                  as.data.table())
                  print(paste0(varname, " read!"))}

#Fudge for cv_reports. col 1:12 are ok. Age groups are screwed up
cvvar[["cv_reports"]] <-cvvar[["cv_reports"]][-13]
cvvar[["cv_reports"]] <- c(cvvar[["cv_reports"]][1:15], "AGE_GROUP_ENG", "AGE_GROUP_FR", cvvar[["cv_reports"]][16:length(cvvar[["cv_reports"]])])

#Fudge for cv_reactions, extra columns that aren't in the data dictionary. guessing they are coding
cvvar[["cv_reactions"]] <- c(cvvar[["cv_reactions"]][1:5], "PT_NAME_CODE", cvvar[["cv_reactions"]][6:7], "SOC_NAME_CODE", cvvar[["cv_reactions"]][8:10])

#Variable names
cvtables <- sort(ls(pattern = "cv_"))
cvvarorder <- sort(names(cvvar))
mapply(function(x, y) setnames(get(x), cvvar[[y]]), cvtables, cvvarorder)

#Add 4-digit years. R assumes that 2-digits years are 00-68 = 2000-2068 and 69-99 = 1969-1999
#The following code fixes this by adding new columns with 4 digit years
#Original columns are preserved, new columns added with the dates in character and POSIX format
cv_reports %<>%
  mutate(newdate = dmy(DATRECEIVED),
         DATRECEIVEDyyyy = ifelse(year(newdate) > 2015,
                                  paste(subtract((year(newdate)), 100), format(newdate, "%m-%d"), sep = "-"),
                                  as.character(newdate)),
         newdate = dmy(DATINTRECEIVED),
         DATINTRECEIVEDyyyy = ifelse(year(newdate) > 2015,
                                     paste(subtract((year(newdate)), 100), format(newdate, "%m-%d"), sep = "-"),
                                     as.character(newdate))) %>%
  select(-newdate) %>%
  rename(DATRECEIVED_CHAR = DATRECEIVEDyyyy,
         DATINTRECEIVED_CHAR = DATINTRECEIVEDyyyy) %>%
  mutate(DATRECEIVED_CLEAN = ymd(DATRECEIVED_CHAR),
         DATINTRECEIVED_CLEAN = ymd(DATINTRECEIVED_CHAR))

#Recode Age Groups. Original Columns are preserved. New columns added.
#Decade 1: Adolescent
#Decade 6: Elderly
#Unknown: Age > 130 Years
#Elderly: 65 Years <= Age < 130 Years
#Adult: 18 Years <= Age < 65 Years
#Adolescent: 12 Years <= Age < 18 Years
#Child: 2 Years < Age <= 12 Years
#Infant: 28 Days <= Age <= 2 Years
#Neonate: Age <= 28 Days

age_groups <- c("Adult", 
                "Elderly", 
                "Neonate", 
                "Child", 
                "Infant", 
                "Adolescent")

cv_reports %<>%
  mutate(calcyear = NA,
         calcyear = ifelse(AGE_UNIT_ENG == "Years", AGE_Y, calcyear),
         calcyear = ifelse(AGE_UNIT_ENG == "Decade", AGE_Y * 10 + 5, calcyear),
         calcyear = ifelse(AGE_UNIT_ENG == "Months", AGE_Y/12, calcyear),
         calcyear = ifelse(AGE_UNIT_ENG == "Weeks", AGE_Y/52, calcyear),
         calcyear = ifelse(AGE_UNIT_ENG == "Days", AGE_Y/365, calcyear),
         AGE_GROUP_CLEAN = "Unknown",
         AGE_GROUP_CLEAN = ifelse(AGE_GROUP_ENG %in% age_groups, 
                                  AGE_GROUP_ENG, 
                                  AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(!is.na(calcyear) & calcyear > 130,
                                  "Unknown", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(!is.na(calcyear) & calcyear >= 65 & calcyear <= 130,
                       "Elderly", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(!is.na(calcyear) & calcyear >= 18 & calcyear < 65,
                                  "Adult", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(!is.na(calcyear) & calcyear >= 12 & calcyear < 18,
                                  "Adolescent", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(!is.na(calcyear) & calcyear > 2 & calcyear < 12,
                                  "Child", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(!is.na(calcyear) & calcyear >= 0.2 & calcyear <= 2,
                                  "Infant", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(!is.na(calcyear) & calcyear < 28/365,
                                  "Neonate", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(AGE_UNIT_ENG %in% c("Hours", "Minutes"),
                                  "Neonate", AGE_GROUP_CLEAN),
         AGE_GROUP_CLEAN = ifelse(is.na(AGE_GROUP_CLEAN), "Unknown", AGE_GROUP_CLEAN)) %>%
  select(-calcyear)

#Set keys for fast joins
setkey(cv_drug_product_ingredients)
setkey(cv_reactions, REPORT_ID, SOC_NAME_ENG, PT_NAME_ENG)
setkey(cv_report_drug, REPORT_ID, DRUG_PRODUCT_ID)
setkey(cv_report_drug_indication, REPORT_ID, DRUG_PRODUCT_ID)
setkey(cv_report_links, REPORT_ID, REPORT_LINK_NO)
setkey(cv_reports, REPORT_ID, DATRECEIVED_CLEAN, DATINTRECEIVED_CLEAN)

 
# Clean up transients
rm(list = c("age_groups",
            "cvfilepath", 
            "cvfiles", 
            "cvlanding", 
            "cvname", 
            "cvreadme", 
            "cvreadmeurl", 
            "cvtablenames",
            "cvtables",
            "cvzipurl",
            "varname",
            "i",
            "cvvar",
            "cvvarorder"))

# helper functions here?
