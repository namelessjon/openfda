library(httr)
library(jsonlite)
library(tibble)
library(purrr)
library(dplyr)
library(glue)
library(ggplot2)
library(forcats)

reqs <- map(1:50, function(n) {
  skip <- (n-1)*100
  req <- GET(glue("https://api.fda.gov/drug/event.json?search=receivedate:[20040101+TO+20081231]&limit=100&skip={skip}"))
  data <- content(req, as = 'parsed')
  Sys.sleep(41/60)
  data
})

# # req <- GET(glue("https://api.fda.gov/drug/event.json?api_key={api_key}&search=receivedate:[20040101+TO+20081231]&limit=100"))
# req <- GET(glue("https://api.fda.gov/drug/event.json?search=receivedate:[20040101+TO+20081231]&limit=100"))
# 
# data <- content(req, as = 'parsed')

results <- map(reqs, "results") %>% flatten()

nRes <- length(results)

drugs <- vector('list', nRes)
reacts <- vector('list', nRes)
reports <- vector('list', nRes)

get_res <- function(res, miss_value = NA) {
  if (!is.null(res)) {
    res
  } else {
    miss_value
  }
}

for (i in seq_along(results)) {
  res <- results[[i]]
  
  patient <- res$patient
  
  reports[[i]] <- tibble(
    reportid = res$safetyreportid,
    primarysource = get_res(res$primarysource$qualification),
    patientage    = get_res(patient$patientonsetage),
    patientageunit = get_res(patient$patientonsetageunit),
    patientsex    = get_res(patient$patientsex),
    country       = get_res(res$occurcountry)
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


drugs %>% count(Drug) %>% filter(n > 9) %>% arrange(n) %>% mutate(Drug = as_factor(Drug) ) %>% ggplot(aes(x = Drug, y = n)) + geom_point()


drugs_count <- drugs %>% count(Drug) %>% filter(n > 19, n < 200)

selected_drugs <- drugs %>%
  semi_join(drugs_count, by = 'Drug')

selected_drugs %>% select(-Indication) %>% unique() %>% mutate(value = 1) %>% tidyr::spread(Drug, value, fill = 0) %>% select(-reportid) %>% as.matrix()  -> drug_matrix
 d <- dist(drug_matrix, method = 'bin')
 
 nGroup <- 80
 fit <- d %>% hclust(method = 'ward.D')
 plot(fit)
 groups <- cutree(fit, k=nGroup) # cut tree into 5 clusters
rect.hclust(fit, k=nGroup, border="red")

drug_groups <- list()
for (group in 1:nGroup) {
  indexes <- groups[groups == group] %>% names %>% as.numeric()
  if (length(indexes) > 1) {
  drugs_in_group <- drug_matrix[indexes, ] %>% colSums()
  drugs_in_group <- drugs_in_group[drugs_in_group > 0] %>% sort()
  drugs_in_group <- drugs_in_group / length(indexes)
  drug_groups[[group]] <- drugs_in_group
  } else {
    drug_groups[[group]] <- NULL
  }
}

km <- kmeans(t(drug_matrix), iter.max = 50, centers = 50)

km_groups <- list()
for (group in 1:length(km$size)) {
  indexes <- km$cluster[km$cluster == group] %>% names %>% as.numeric()
  if (length(indexes) > 1) {
    drugs_in_group <- drug_matrix[indexes, ] %>% colSums()
    drugs_in_group <- drugs_in_group[drugs_in_group > 0] %>% sort()
    drugs_in_group <- drugs_in_group / length(indexes)
    km_groups[[group]] <- drugs_in_group
  } else {
    km_groups[[group]] <- NULL
  }
}

kmt <- kmeans(t(drug_matrix), iter.max = 50, centers = 50)

kmt_groups <- list()
for (group in 1:length(kmt$size)) {
  indexes <- kmt$cluster[kmt$cluster == group] %>% names()
  if (length(indexes) > 1) {
    drugs_in_group <- drug_matrix[indexes, ] %>% colSums()
    drugs_in_group <- drugs_in_group[drugs_in_group > 0] %>% sort()
    drugs_in_group <- drugs_in_group / length(indexes)
    km_groups[[group]] <- drugs_in_group
  } else {
    km_groups[[group]] <- NULL
  }
}
 
 selected_drugs %>% unique() %>% mutate(value = 1) %>% tidyr::spread(Drug, value, fill = 0) %>% filter(`WARFERIN SODIUM)
 
 frequent_indications <- drugs %>%
   count(Indication) %>%
   filter(n >= 40, !is.na(Indication)) %>%
   semi_join(drugs, .)
 
 frequent_reacts <- reacts %>%
   count(React) %>%
   filter(n >= 40, !is.na(React), n <= 100) %>%
   semi_join(reacts, .)
 
 frequent_indications %>% left_join(frequent_reacts) -> combined
 


combined %>%
  count(Indication, React) %>%
  tidyr::spread(React, n, fill = 0) %>%
  select(-Indication) %>%
  as.matrix() %>%
  t %>%
  dist(method = 'canberra') -> indication_sideeffect_mix

indication_se_c <- hclust(indication_sideeffect_mix)
plot(indication_se_c)
groups <- cutree(indication_se_c, k=20) # cut tree into 5 clusters
rect.hclust(indication_se_c, k=20, border="red")

