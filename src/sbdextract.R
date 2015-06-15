#Daniel Buijs, dbuijs@gmail.com
#This script scrapes and parses the Health Canada Summary Basis of Decisions
#All packages are from CRAN
#Resulting dataframes are sbdextractdate, sbddrugs, sbddevices
library("XML")
library("httr")
library("rvest")
library("magrittr")
library("dplyr")
library("stringr")
library("lubridate")

sbdextractdate <- today() #Today's date, for sumamry information

#Grab the list of drug links and filter out anything that isn't a Summary Basis of Decision.
#The filtering is necessary because the link page contains Notices of Decision and other gunk.
sbdlinksdrug <- html_session("http://www.hc-sc.gc.ca/dhp-mps/prodpharma/sbd-smd/drug-med/index-eng.php") %>%
                html_nodes(".date li a:contains('Summary')") %>%
                html_attr("href") %>%
                paste0("http://www.hc-sc.gc.ca", .)
#Grab the posting date for drugs. Should be possible to roll this into the statement above in the future.
#The Perl regex grabs text that follows an open bracket and has the form yyyy-mm-dd
sbdlinksdrugposted <- html_session("http://www.hc-sc.gc.ca/dhp-mps/prodpharma/sbd-smd/drug-med/index-eng.php") %>%
  html_nodes(".date li:contains('Summary')") %>%
  html_text() %>%
  str_extract(regex("(?<=\\[)\\d{4}-\\d{2}-\\d{2}"))

#Build the SBD drugs dataframe
#The Perl regexes extract the drug name and year published
#Uses the standardized naming: sbd_smd_year_drug_control#-eng.php
#Should probably add some error-checking code here
sbddrugs <- data.frame(drug=sbdlinksdrug %>%
                         str_extract(regex("(?<=sbd_smd_\\d{4}_)\\w+(?=[_-]\\d)")),
                       year=sbdlinksdrug %>%
                         str_extract(regex("(?<=sbd_smd_)\\d{4}(?=[_-])")),
                       url=sbdlinksdrug, stringsAsFactors = FALSE,
                       sbd_date_posted = sbdlinksdrugposted)

#Grab the list of medical device links and filter out anything that isn't a Summary Basis of Decision.
#The filtering is necessary because the link page contains Notices of Decision and other gunk.
sbdlinksdevice <- html_session("http://www.hc-sc.gc.ca/dhp-mps/prodpharma/sbd-smd/md-im/index-eng.php") %>%
  html_nodes(".date li a:contains('Summary')") %>%
  html_attr("href") %>%
  paste0("http://www.hc-sc.gc.ca", .)

#Grab the posting date for med devices. Should be possible to roll this into the statement above in the future.
#The Perl regex grabs text that follows an open bracket and has the form yyyy-mm-dd
sbdlinksdevposted <- html_session("http://www.hc-sc.gc.ca/dhp-mps/prodpharma/sbd-smd/md-im/index-eng.php") %>%
  html_nodes(".date li:contains('Summary')") %>%
  html_text() %>%
  str_extract(regex("(?<=\\[)\\d{4}-\\d{2}-\\d{2}"))

#Build the SBD drugs dataframe
#The Perl regexes extract the drug name and year published
#Uses the standardized naming: sbd_smd_year_drug_control#-eng.php
#Should probably add some error-checking code here
sbddevices <- data.frame(device=sbdlinksdevice %>%
                         str_extract(regex("(?<=sbd_smd_\\d{4}_)\\w+(?=[_-]\\d{5}?(-eng)?)")),
                       year=sbdlinksdevice %>%
                         str_extract(regex("(?<=sbd_smd_)\\d{4}(?=[_-])")),
                       url=sbdlinksdevice, stringsAsFactors = FALSE,
                       sbd_date_posted = sbdlinksdevposted)

#Iterate through the list of links and grab the raw html. 
#File encoding is ISO8859-1. This is very important for accented characters.
sbdhtml <- lapply(sbddrugs$url, html_session, encoding = "ISO8859-1")
#Extract the actual text of the SBD
sbdtext <- sapply(sbdhtml, function(x) x %>% html_nodes(".center") %>% html_text())
#Paste it onto the SBDdrugs table
sbddrugs <- cbind(sbddrugs, sbdtext, stringsAsFactors = FALSE)
#The next two lines detect whether the SBD is Phase I or Phase II format.
sbddrugs$prodsubinfo <- grepl("Product and Submission Information", sbddrugs$sbdtext, ignore.case = TRUE)
sbddrugs$q1what <- grepl("What was approved?", sbddrugs$sbdtext)
#Review Bureau
sbddrugs$contact <- sapply(sbdhtml, function(x) x %>% 
                                                html_nodes("p:contains('Contact:')") %>% 
                                                html_text() %>%
                                                str_replace_all("\\s{2,}", " ") %>%
                                                str_extract(regex("(?<=^Contact:).*")) %>%
                                                str_trim() %>%
                                                {ifelse(length(.) == 0, NA, .)})

sbddrugs %<>% mutate(bureau = ifelse(grepl("Biologic|Biotherapeutic", contact), "ORA", NA),
                     bureau = ifelse(grepl("Metabolism|BMORS", contact), "BMORS", bureau),
                     bureau = ifelse(grepl("Cardiology|BCANS", contact), "BCANS", bureau),
                     bureau = ifelse(grepl("Gastro|BGIVD", contact), "BGIVD", bureau))

#Fudge for Menveo
sbddrugs[sbddrugs$drug == "menveo", "bureau"] <- "ORA"

#Find numbers that look like DINs
sbddrugs$dinraw <- str_extract_all(sbddrugs$sbdtext, regex("0\\d{7}")) %>% sapply(unique) %>% paste(sep=", ")

#temporary fudge for Velcade becuase the SBD has no leading zero
sbddrugs[sbddrugs$drug == "velcade", "dinraw"] <- str_extract(sbddrugs[sbddrugs$drug == "velcade", "sbdtext"], regex("(?<=DIN: )\\d*")) %>%
                                                  as.numeric() %>%
                                                  sprintf("0%d8", .)

#Same thing for Med Devices
sbddevhtml <- lapply(sbddevices$url, html_session)
sbddevtext <- sapply(sbddevhtml, function(x) x %>% html_nodes(".center") %>% html_text())
sbddevices <- cbind(sbddevices, sbddevtext, stringsAsFactors = FALSE)
sbddevices$prodsubinfo <- grepl("Product and Submission Information", sbddevices$sbddevtext, ignore.case = TRUE)
sbddevices$q1what <- grepl("What was approved?", sbddevices$sbddevtext)
sbddevices$contact <- str_extract(sbddevices$sbddevtext, regex("(?<=Contact: ).*(?=\\n)"))
sbddevices$class <- str_extract(sbddevices$sbddevtext, regex("Class.{1,6}(?=\\s)"))
#sbddrugs$dinraw <- str_extract_all(sbddrugs$sbdtext, perl("0\\d{6}")) %>% sapply(unique) %>% paste(sep=", ")

#NOC date
sbddrugs <- sbddrugs %>%
            mutate(date_noc = str_extract(sbdtext, regex("(?<=On ).*(?=Health  ?Canada)")) %>%
                     #iconv("UTF-8", "ISO8859-1") %>% Need this line for Windows systems
                     parse_date_time("Bdy") %>%
                     format("%Y-%m-%d"),
                   firstdin = str_extract(dinraw, regex("0\\d{7}")) %>% 
                              as.numeric %>%
                              sprintf("0%d", .))
sbddrugs$firstdin[sbddrugs$firstdin == "0NA"] <- NA

#Temporary fudges for Arepanrix H1N1, Onglyza, and Tecfidera
# Radiopharmaceuticals don't have DINs. NAs expected for Cantrace, Leukoscan, Rubyfill, Xofigo and Zevalin
sbddrugs[sbddrugs$drug == "tecfidera", "date_noc"] <- "2013-04-03"
sbddrugs[sbddrugs$drug == "onglyza", "date_noc"] <- "2009-09-14"
sbddrugs[sbddrugs$drug == "arepanrix_h1n1", "date_noc"] <- "2010-05-11"
sbddrugs[sbddrugs$drug == "campral", "date_noc"] <- "2007-03-16"
sbddrugs[sbddrugs$drug == "celsentri", "firstdin"] <- "02299844"
sbddrugs[sbddrugs$drug == "edurant", "firstdin"] <- "02370603"
sbddrugs[sbddrugs$drug == "primovist", "date_noc"] <- "2010-01-14"
sbddrugs[sbddrugs$drug == "tarceva", "firstdin"] <- "02269007"
