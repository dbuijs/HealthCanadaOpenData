#Daniel Buijs, dbuijs@gmail.com
#This script downloads and imports the Licensed Natural Health Products Database

library(rvest)
library(XML)
library(httr)
library(lubridate)
library(stringr)
library(magrittr)
library(dplyr)
library(data.table)


#Get the LNHPD extract date
lnhpdcoverlink <- "http://www.hc-sc.gc.ca/dhp-mps/prodnatur/applications/licen-prod/lnhpd-bdpsnh_data_extract-eng.php"
lnhpdextractdate <- html_session(lnhpdcoverlink) %>%
  html_nodes("h2:contains('Last updated')") %>%
  html_text() %>%
  str_extract(regex("(?<=Last updated:).*$")) %>%
  parse_date_time("Bdy") %>%
  format("%Y-%m-%d")

#Download and extract the LNHPD extract
lnhpdurl <- "http://www.hc-sc.gc.ca/dhp-mps/alt_formats/zip/prodnatur/applications/licen-prod/ALL_NHP_FILES.zip"
if(!(file.exists("../data/lnhpd"))) dir.create("../data/lnhpd")
lnhpdfile <- paste0("../data/lnhpd/",basename(lnhpdurl))
download.file(lnhpdurl, lnhpdfile)
unzip(lnhpdfile, exdir = "../data/lnhpd")


# LNHPD Variable Names
# libraries XML, httr, rvest, magrittr, dply and stringr loaded in project config
lnhpdreadme <- html_session("http://www.hc-sc.gc.ca/dhp-mps/prodnatur/applications/licen-prod/lnhpd-bdpsnh_readme-lisezmoi-eng.php")
lnhpdtablenames <- lnhpdreadme %>% 
  html_nodes("caption") %>% 
  html_text() %>% 
  str_trim() %>%
  str_extract(regex("(?<=Description ).*$")) 

lnhpdvar <- list()
for(i in lnhpdtablenames){lnhpdcss <- paste0("table:contains('", i, "')")
                          nhpname <- i %>%
                          str_replace_all(" ", "_") %>%
                          tolower() 
                          lnhpdvar[[nhpname]] <- lnhpdreadme %>% 
                                                  html_node(lnhpdcss) %>%
                                                  html_nodes("td:nth-child(1)") %>% 
                                                  html_text()}

lnhpdfiles <- list.files("../data/lnhpd", pattern = ".*txt$")

for(i in lnhpdfiles){varname <- i %>% tolower() %>% str_extract(regex(".*(?=\\.txt$)"))
                   lnhpdfilepath <- paste0("../data/lnhpd/", i)
                    assign(varname,
                           read.delim(lnhpdfilepath, 
                                       header = FALSE, 
                                       sep = '|', 
                                       quote = "", 
                                       fileEncoding = "ISO8859-1",
                                       stringsAsFactors = FALSE,
                                       strip.white = TRUE) %>%
                             apply(2, str_sub, 2L, -2L) %>%
                             as.data.table())
                   print(paste0(varname, " read!"))}

#Variable names
for(i in ls(pattern = "nhp_")){setnames(get(i), lnhpdvar[[i]])}

#Add 4-digit years. R assumes that 2-digits years are 00-68 = 2000-2068 and 69-99 = 1969-1999
#The following code fixes this by adding new columns with 4 digit years
nhp_products %<>%
  mutate(newdate = dmy(licence_date),
        licence_dateyyyy = ifelse(year(newdate) > 2015,
                                  paste(subtract((year(newdate)), 100), format(newdate, "%m-%d"), sep = "-"),
                                  as.character(newdate))) %>%
  select(-newdate)

#Clean up transients
rm(list= c("lnhpdcoverlink",
           "lnhpdcss",
           "lnhpdurl",
           "lnhpdfile",
           "lnhpdfilepath",
           "lnhpdfiles",
           "lnhpdreadme",
           "lnhpdtablenames",
           "lnhpdvar",
           "varname",
           "i"))
#Helper functions?

# Need to read in files, change names, map variables, fix encodings
