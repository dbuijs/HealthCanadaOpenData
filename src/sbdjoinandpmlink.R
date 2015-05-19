library(rvest)
library(XML)
library(httr)
library(sqldf)
library(dplyr)
library(lubridate)


getdpdpmlink <- function(drug_code){
                                    dpdlanding <- "http://webprod5.hc-sc.gc.ca/dpd-bdpp/index-eng.jsp"
                                    dpdproductlink <- paste0("http://webprod5.hc-sc.gc.ca/dpd-bdpp/info.do?code=", drug_code, "&lang=eng")
                                    pmlink <- ifelse(is.na(drug_code), NA, 
                                                     html_session(dpdlanding) %>%
                                                     jump_to(dpdproductlink) %>%
                                                     html_nodes("a:contains('Product Monograph')") %>%
                                                     html_attr("href") %>%
                                                     paste0("http://webprod5.hc-sc.gc.ca", .))
                                    return(pmlink)}

nocjoin <- noc_din_product %>%
           left_join(noc_main)%>%
           mutate(date_noc = ymd(NOC_DATE) %>% format("%Y-%m-%d")) 

nocbrandjoin <- noc_brand %>% 
                group_by(NOC_NUMBER) %>% 
                mutate(noc_brands = paste(NOC_BR_BRANDNAME, collapse = ", "),
                       noc_brandcount = length(NOC_BR_BRANDNAME)) %>%
                select(NOC_NUMBER, noc_brands, noc_brandcount) %>% 
                distinct()

dpdtherjoin <- dpd_ther %>%
               group_by(DRUG_CODE) %>%
               summarize(atc_codes = paste(TC_ATC_NUMBER, collapse = ", "),
                         atc_desc = paste(TC_ATC, collapse = ", "),
                         ahfs_codes = paste(TC_AHFS_NUMBER, collapse = ", "),
                         ahfs_desc = paste(TC_AHFS, collapse = ", "),
                         atc_count = length(TC_ATC),
                         ahfs_count = length(TC_AHFS))
dpdingjoin <- dpd_ingred %>%
              group_by(DRUG_CODE) %>%
              summarize(Ingredients = paste(unique(INGREDIENT), collapse = ", "))

sbddrugjoin <- sbddrugs %>%
               left_join(nocjoin, by = c("firstdin" = "NOC_DP_DIN", "date_noc" = "date_noc")) %>%
               distinct(drug, date_noc, firstdin, NOC_NUMBER) %>%
               left_join(nocbrandjoin) %>%
               left_join(dpd_drug, by = c("firstdin" = "DRUG_IDENTIFICATION_NUMBER")) %>%
               left_join(dpdtherjoin) %>%
               left_join(dpdingjoin) %>%
               mutate(pmlink = sapply(DRUG_CODE, getdpdpmlink),
                      dpdlink = ifelse(is.na(DRUG_CODE), NA, paste0("http://webprod5.hc-sc.gc.ca/dpd-bdpp/info.do?code=", DRUG_CODE, "&lang=eng")),
                      noclink = ifelse(is.na(NOC_NUMBER), NA, paste0("http://webprod5.hc-sc.gc.ca/noc-ac/info.do?no=", NOC_NUMBER, "&lang=eng")),
                      sbdavail = TRUE,
                      nocavail = ifelse(is.na(NOC_NUMBER), FALSE, TRUE),
                      dpdavail = ifelse(is.na(DRUG_CODE), FALSE, TRUE))
sbddrugjoin <- as.data.table(sbddrugjoin)

#fudges for radiopharmaceuticals
#Need to fix Arepanrix H1N1 (DPD entry but no NOC)
#Need to fix cantrace, leukoscan, ruby-fil, xofigo, zevalin (Radiopharmaceuticals, NOC but no DIN/DPD)
# nocjoin %>% filter(date_noc %in% sbddrugjoin[is.na(NOC_NUMBER), date_noc]) %>% left_join(nocbrandjoin) %>% select(NOC_MANUFACTURER_NAME, NOC_NUMBER, date_noc, noc_brands) %>% filter(noc_brands %in% toupper(sbddrugjoin[is.na(NOC_NUMBER), drug]))

if(file.exists("../sbddrugs.db")) file.rename("../sbddrugs.db", paste0("../sbddrugs", format(today(), "%Y-%m-%d"), ".db"))
sqlitefile <- "../sbddrugs.db"
db <- dbConnect(SQLite(), dbname=sqlitefile)
dbWriteTable(conn = db, name = "drugs", value = sbddrugjoin, row.names = FALSE)
dbWriteTable(conn = db, name = "devices", value = sbddevices, row.names = FALSE)

sqldf('CREATE VIRTUAL TABLE sbdsearch USING fts4(drug, year, sbdtext)',
      dbname = sqlitefile)
sqldf('INSERT INTO sbdsearch SELECT drug, year, sbdtext FROM sbddrugs',
      dbname = sqlitefile)

sqldf('CREATE VIRTUAL TABLE sbddevsearch USING fts4(device, year, sbddevtext)',
      dbname = sqlitefile)
sqldf('INSERT INTO sbddevsearch SELECT device, year, sbddevtext FROM sbddevices',
      dbname = sqlitefile)
dbDisconnect(db)

