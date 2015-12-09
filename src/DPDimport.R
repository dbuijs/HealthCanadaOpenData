#Daniel Buijs, dbuijs@gmail.com
#This script downloads and imports the Health Canada Drug Product Database
#Returns data.tables with the dpd_ prefix
library(rvest)
library(XML)
library(httr)
library(lubridate)
library(stringr)
library(magrittr)
library(dplyr)
library(data.table)
library(digest)
library(Hmisc)

#Get the DPD extract date
dpdcoverlink <- "http://www.hc-sc.gc.ca/dhp-mps/prodpharma/databasdon/dpd_bdpp_data_extract-eng.php"
dpdextractdate <- html_session(dpdcoverlink) %>%
  html_nodes('td:contains("allfiles.zip")+ td') %>%
  html_text() %>%
  parse_date_time("Ymd") %>%
  format("%Y-%m-%d")

#Download and extract the DPD extract
dpdurl <- "http://www.hc-sc.gc.ca/dhp-mps/alt_formats/zip/prodpharma/databasdon/allfiles.zip"
if(!(file.exists("../data/dpd"))) dir.create("../data/dpd")
download.file(dpdurl, "../data/dpd/dpdallfiles.zip")
unzip("../data/dpd/dpdallfiles.zip", exdir = "../data/dpd")

#Download and extract the DPD inactives extract
dpdiaurl <- "http://www.hc-sc.gc.ca/dhp-mps/alt_formats/zip/prodpharma/databasdon/allfiles_ia.zip"
download.file(dpdiaurl, "../data/dpd/dpdallfiles_ia.zip")
unzip("../data/dpd/dpdallfiles_ia.zip", exdir = "../data/dpd")

#Download and extract the DPD approved extract
dpdapurl <- "http://www.hc-sc.gc.ca/dhp-mps/alt_formats/zip/prodpharma/databasdon/allfiles_ap.zip"
download.file(dpdapurl, "../data/dpd/dpdallfiles_ap.zip")
unzip("../data/dpd/dpdallfiles_ap.zip", exdir = "../data/dpd")

# Convert all to UTF-8
sys('for a in $(find ../data/dpd -name "*.txt"); do iconv -f iso-8859-1 -t utf-8 <"$a" >"$a".utf8; done')

# Strip leading and trailing whitespace on each line with perl:  s/^\s+|\s+$//g
sys('for a in $(find ../data/dpd -name "*.txt.utf8"); do perl -lape \'s/^\\s+|\\s+$//g\' "$a" > "$a".ws1; done')

# perl to remove \r\n that is not preceded by a quote: perl -pe 's/(?<!\")\r\n//g'
# sys('perl -pe \'s/(?<!\")\\r\\n//g\' ../data/dpd/ther.txt > ../data/dpd/ther.fixed.txt')


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
                          str_extract(regex("(?<=qrym_).*$")) %>%
                          paste0("dpd_", .)
                        dpdvar[[dpdname]] <- dpdreadme %>% html_nodes(dpdcss) %>% html_text()}

dpdfiles <- list.files("../data/dpd", pattern = ".*txt.utf8.ws1")
dpdiafiles <- dpdfiles[grepl("_ia.txt|inactive", dpdfiles)]
dpdapfiles <- dpdfiles[grepl("_ap.txt", dpdfiles)]
dpdiafiles <- dpdiafiles[!grepl("inactive", dpdiafiles)]
dpdfiles <- dpdfiles[!dpdfiles %in% c(dpdiafiles, dpdapfiles, "inactive.txt.utf8.ws1")]


for(i in dpdfiles){dpdnameroot <- i %>% tolower() %>% str_extract(regex(".*(?=\\.txt.utf8.ws1$)"))
                   varname <- paste0("dpd_", dpdnameroot)
                   dpdfile <- paste0("../data/dpd/", i)
                   assign(varname, fread(dpdfile, header=FALSE))}

for(i in dpdiafiles){dpdnameroot <- i %>% tolower() %>% str_extract(regex(".*(?=\\.txt.utf8.ws1$)"))
                   varname <- paste0("dpd_", dpdnameroot)
                   dpdiafile <- paste0("../data/dpd/", i)
                   assign(varname, fread(dpdiafile, header=FALSE))}

for(i in dpdapfiles){dpdnameroot <- i %>% tolower() %>% str_extract(regex(".*(?=\\.txt.utf8.ws1$)"))
                     varname <- paste0("dpd_", dpdnameroot)
                     dpdapfile <- paste0("../data/dpd/", i)
                     assign(varname, fread(dpdapfile, header=FALSE))
                     }

#Variable names
dpdtables <- sort(ls(pattern = "dpd_"))
dpdtables <- dpdtables[!grepl("_ia|_ap", dpdtables)]
dpdvarorder <- sort(names(dpdvar))[c(2:4,1,5:11)]
mapply(function(x, y) setnames(get(x), dpdvar[[y]]), dpdtables, dpdvarorder)
mapply(function(x, y) setnames(get(paste0(x, "_ia")), dpdvar[[y]]), dpdtables, dpdvarorder)
mapply(function(x, y) setnames(get(paste0(x, "_ap")), dpdvar[[y]]), dpdtables, dpdvarorder)

#Join them all together
dpdpattern <- ls(pattern = "dpd_") %>% str_extract(regex("(?<=^dpd_)\\w*(?=_)")) %>% unique() %>% .[!is.na(.)]
for(i in dpdpattern){
  a <- paste0("dpd_", i)
  b <- paste0("dpd_", i, "_ia")
  c <- paste0("dpd_", i, "_ap")
  all <- paste0("dpd_", i, "_all")
  assign(all, bind_rows(list(active = get(a), inactive = get(b), approved = get(c)), .id = "extract") %>% as.data.table())
  print(paste("Assigned", all))
}

#Uncomment the following to write all dpd_*_all tables to an SQLite file

#library(RSQLite)
#con <- dbConnect(SQLite(), "dpdOCt2015.sqlite")
#dpdalldf <- ls(pattern = "dpd_.*_all")
#sapply(dpdalldf, function(x){dbWriteTable(con, x, as.data.frame(get(x)))})
#dbDisconnect(con)

# Clean up transients
rm(list = c("a",
            "b",
            "c",
            "all",
            "dpdapfile",
            "dpdapfiles",
            "dpdapurl",
            "dpdiafile",
            "dpdiafiles",
            "dpdiaurl",
            "dpdpattern",
            "dpdcoverlink", 
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
