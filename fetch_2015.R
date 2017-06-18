# Script to fetch data from the OpenFDA api
#
# Lots of loops to cope with the limitations of only 100 results in a request, also the lack of aggregation
# exposed.
# 
# Loops over year, pulling out interesting values

library(httr)
library(readr)
library(purrr)
library(tibble)
library(dplyr)

source('api_key.R')

# build a list of dates
yr2015 <- seq.Date(as.Date("2015-01-01"), as.Date("2015-12-31"), by = 'day')

get_res <- function(res, miss_value = NA) {
  if (!is.null(res)) {
    res
  } else {
    miss_value
  }
}


results <- map(yr2015, function(date) {
  print(date)
  last_retrieved <- 100
  from_date <- map(1:10, function (n) { # up to 1000 events per day
    if (last_retrieved != 100) {
      return(list())
    } else {
      skip <- (n-1)*100
      #print(glue("https://api.fda.gov/drug/event.json?api_key={api_key}&search=receivedate:{date}&limit=100&skip={skip}"))
      req <- GET(glue("https://api.fda.gov/drug/event.json?api_key={api_key}&search=receivedate:{date}&limit=100&skip={skip}"))
      data <- content(req, as = 'parsed')
      #Sys.sleep(60/240)
      results <- data$results
      last_retrieved <<- length(results)
      
      nRes <- length(results)
      
      drugs <- vector('list', nRes)
      reacts <- vector('list', nRes)
      reports <- vector('list', nRes)
      
      
      
      for (i in seq_along(results)) {
        res <- results[[i]]
        
        patient <- res$patient
        
        reports[[i]] <- tibble(
          reportid = res$safetyreportid,
          primarysource = get_res(res$primarysource$qualification),
          patientage    = get_res(patient$patientonsetage),
          patientageunit = get_res(patient$patientonsetageunit),
          patientsex    = get_res(patient$patientsex),
          country       = get_res(res$occurcountry),
          transmission_date = get_res(res$transmissiondate),
          receipt_date     = get_res(res$receiptdate)
        )
        
        drugs[[i]] <- tibble(reportid = res$safetyreportid,
                             Drug = map_chr(patient$drug, "medicinalproduct"),
                             Indication = map_chr(patient$drug, function(x) {
                               if (is.null(x$drugindication)) {
                                 as.character(NA)
                               } else {
                                 x$drugindication 
                               }
                             })
        )
        reacts[[i]] <- tibble(reportid = res$safetyreportid, React = map_chr(patient$reaction, "reactionmeddrapt"))
        
        
      }
      
      drugs <- bind_rows(drugs)
      reacts <- bind_rows(reacts)
      reports <- bind_rows(reports)
      
      
      list(drugs = drugs, reacts = reacts, reports = reports)
    }
  }) %>% flatten()
}) %>% flatten()

reacts <- results[seq(2, length(results), 3)] %>% bind_rows()
drugs  <- results[seq(1, length(results), 3)] %>% bind_rows()
reports <- results[seq(3, length(results), 3)] %>% bind_rows()

dir.create('data/fda/raw', showWarnings = F, recursive = T, mode = "0755")

write_csv(reports, 'data/fda/raw/2015_reports.csv')
write_csv(drugs, 'data/fda/raw/2015_drugs.csv')
write_csv(reacts, 'data/fda/raw/2015_reacts.csv')
