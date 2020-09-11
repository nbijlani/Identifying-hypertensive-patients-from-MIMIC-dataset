library(tidyverse)
library(magrittr)
library(bigrquery)
library(caret)

con <- DBI::dbConnect(drv = bigquery(), project = "learnclinicaldatascience")

goldStd <- tbl(con, "course3_data.hypertension_goldstandard")

## getStats(df, predicted, reference)
getStats <- function(df, ...){
  df %>%
    select_(.dots = lazyeval::lazy_dots(...)) %>%
    mutate_all(funs(factor(., levels = c(1,0)))) %>% 
    table() %>% 
    confusionMatrix()
}

goldStd %>%
  collect() %>%
  filter(HYPERTENSION == 0) %>%
  summarise(count = n())

# Testing individual data types
# 1. ICD codes
diagnoses_icd <- tbl(con, "mimic3_demo.D_ICD_DIAGNOSES")
diagnoses_icd_patient <- tbl(con, "mimic3_demo.DIAGNOSES_ICD")

icd_hyp <- diagnoses_icd %>%
  filter(lower(LONG_TITLE) %like% "%hypertension%" && lower(LONG_TITLE) %not like% "%, without mention of hypertension%") %>%
  distinct(ICD9_CODE)

any_hyp_icd <- diagnoses_icd_patient %>% 
  inner_join(icd_hyp, by = c("ICD9_CODE" = "ICD9_CODE")) %>% 
  distinct(SUBJECT_ID) %>% 
  mutate(hypertension_pred = 1)

goldStd %>% 
  left_join(any_hyp_icd) %>% 
  mutate(hypertension_pred = coalesce(hypertension_pred, 0)) %>% 
  collect() %>% 
  getStats(hypertension_pred, HYPERTENSION)

# 2. Prescriptions
hyper_drugs <- tbl(con, "course3_data.D_ANTIHYPERTENSIVES")
drugs_patient <- tbl(con, "mimic3_demo.PRESCRIPTIONS")

hyper_drugs %<>%
  pull(DRUG) %>%
  tolower

hyper_drugs_patient <- drugs_patient %>%
  filter(lower(DRUG) %in% hyper_drugs) %>%
  mutate(hypertension_pred = 1) %>%
  distinct(SUBJECT_ID, hypertension_pred)

goldStd %>% 
  left_join(hyper_drugs_patient) %>% 
  mutate(hypertension_pred = coalesce(hypertension_pred, 0)) %>% 
  collect() %>% 
  getStats(hypertension_pred, HYPERTENSION)

# Manipulations of individual data types
# 1. Systolic BP >= 140 mmHg on 2 or more occasions

chartevents <- tbl(con, "mimic3_demo.CHARTEVENTS")
d_items <- tbl(con, "mimic3_demo.D_ITEMS")

sysbp_any <- d_items %>% 
  inner_join(chartevents, by = c("ITEMID" = "ITEMID"), suffix = c("_c","_d")) %>% 
  filter(tolower(LABEL) %like% '%systolic%') %>% 
  group_by(SUBJECT_ID) %>%
  mutate(sysbp_over140_marker = case_when(VALUENUM >= 140 ~ 1, TRUE ~ 0)) %>%
  summarise(sysbp_over140_count = sum(sysbp_over140_marker, na.rm = TRUE)) %>%
  mutate(hypertension_pred = case_when(sysbp_over140_count >= 2 ~ 1, TRUE ~ 0)) %>%
  select(SUBJECT_ID, hypertension_pred)

goldStd %>% 
  left_join(sysbp_any) %>% 
  mutate(hypertension_pred = coalesce(hypertension_pred, 0)) %>% 
  collect() %>% 
  getStats(hypertension_pred, HYPERTENSION)

# 2. Diastolic BP >= 90 mmHg on 2 or more occasions
chartevents <- tbl(con, "mimic3_demo.CHARTEVENTS")
d_items <- tbl(con, "mimic3_demo.D_ITEMS")

diasbp_any <- d_items %>% 
  inner_join(chartevents, by = c("ITEMID" = "ITEMID"), suffix = c("_c","_d")) %>% 
  filter(tolower(LABEL) %like% '%diastolic%') %>% 
  group_by(SUBJECT_ID) %>%
  mutate(diasbp_over90_marker = case_when(VALUENUM >= 90 ~ 1, TRUE ~ 0)) %>%
  summarise(diasbp_over90_count = sum(diasbp_over90_marker, na.rm = TRUE)) %>%
  mutate(hypertension_pred = case_when(diasbp_over90_count >= 2 ~ 1, TRUE ~ 0)) %>%
  select(SUBJECT_ID, hypertension_pred)

goldStd %>% 
  left_join(diasbp_any) %>% 
  mutate(hypertension_pred = coalesce(hypertension_pred, 0)) %>% 
  collect() %>% 
  getStats(hypertension_pred, HYPERTENSION)

# 3. ICD codes for any hypertension more than 2 times
diagnoses_icd <- tbl(con, "mimic3_demo.D_ICD_DIAGNOSES")
diagnoses_icd_patient <- tbl(con, "mimic3_demo.DIAGNOSES_ICD")

icd_hyp <- diagnoses_icd %>%
  filter(lower(LONG_TITLE) %like% "%hypertension%" && lower(LONG_TITLE) %not like% "%, without mention of hypertension%") %>%
  distinct(ICD9_CODE)

any_hyp_icd <- diagnoses_icd_patient %>% 
  inner_join(icd_hyp, by = c("ICD9_CODE" = "ICD9_CODE")) %>% 
  group_by(SUBJECT_ID) %>% 
  mutate(hypertension_prediction = 1) %>%
  summarise(hypertension_prediction_count = sum(hypertension_prediction, na.rm = TRUE)) %>%
  mutate(hypertension_pred = case_when(hypertension_prediction_count >= 2 ~ 1, TRUE ~ 0)) %>%
  select(SUBJECT_ID, hypertension_pred)

goldStd %>% 
  left_join(any_hyp_icd) %>% 
  mutate(hypertension_pred = coalesce(hypertension_pred, 0)) %>% 
  collect() %>% 
  getStats(hypertension_pred, HYPERTENSION)

# 4. ICD codes for specific hypertension codes more than 2 times
diagnoses_icd <- tbl(con, "mimic3_demo.D_ICD_DIAGNOSES")
diagnoses_icd_patient <- tbl(con, "mimic3_demo.DIAGNOSES_ICD")

cnt_hyp_icd <- diagnoses_icd_patient %>% 
  filter(ICD9_CODE %in% c("4010", "4011", "4019")) %>% 
  group_by(SUBJECT_ID) %>%
  mutate(hypertension_prediction = 1) %>%
  summarise(hypertension_prediction_count = sum(hypertension_prediction, na.rm = TRUE)) %>%
  mutate(hypertension_pred = case_when(hypertension_prediction_count >= 2 ~ 1, TRUE ~ 0)) %>%
  select(SUBJECT_ID, hypertension_pred)

goldStd %>% 
  left_join(cnt_hyp_icd) %>% 
  mutate(hypertension_pred = coalesce(hypertension_pred, 0)) %>% 
  collect() %>% 
  getStats(hypertension_pred, HYPERTENSION)

# Combinations of data types
# NOTE: This one below has been included but is not a combination of data types. 
# Both systolic and diastolic BP are derived from the chart events data type

# 1. Systolic BP >= 140 and Diastolic BP >= 90 mmHg on 2 or more occasions
chartevents <- tbl(con, "mimic3_demo.CHARTEVENTS")
d_items <- tbl(con, "mimic3_demo.D_ITEMS")

sysbp_any <- d_items %>% 
  inner_join(chartevents, by = c("ITEMID" = "ITEMID"), suffix = c("_c","_d")) %>% 
  filter(tolower(LABEL) %like% '%systolic%') %>% 
  group_by(SUBJECT_ID) %>%
  mutate(sysbp_over140_marker = case_when(VALUENUM >= 140 ~ 1, TRUE ~ 0)) %>%
  summarise(sysbp_over140_count = sum(sysbp_over140_marker, na.rm = TRUE)) %>%
  mutate(sysbp_over140_2ormore = case_when(sysbp_over140_count >= 2 ~ 1, TRUE ~ 0)) %>%
  select(SUBJECT_ID, sysbp_over140_2ormore)

diasbp_any <- d_items %>% 
  inner_join(chartevents, by = c("ITEMID" = "ITEMID"), suffix = c("_c","_d")) %>% 
  filter(tolower(LABEL) %like% '%diastolic%') %>% 
  group_by(SUBJECT_ID) %>%
  mutate(diasbp_over90_marker = case_when(VALUENUM >= 90 ~ 1, TRUE ~ 0)) %>%
  summarise(diasbp_over90_count = sum(diasbp_over90_marker, na.rm = TRUE)) %>%
  mutate(diasbp_over90_2ormore = case_when(diasbp_over90_count >= 2 ~ 1, TRUE ~ 0)) %>%
  select(SUBJECT_ID, diasbp_over90_2ormore)

goldStd %>% 
  left_join(sysbp_any) %>% 
  left_join(diasbp_any) %>% 
  mutate(sysbp_over140_2ormore = coalesce(sysbp_over140_2ormore, 0),
         diasbp_over90_2ormore = coalesce(diasbp_over90_2ormore, 0)) %>% 
  mutate(sysbp_or_diasbp = case_when(sysbp_over140_2ormore == 1 |
                                       diasbp_over90_2ormore == 1 ~ 1,
                                     TRUE ~ 0)) %>% 
  collect() %>% 
  getStats(sysbp_or_diasbp, HYPERTENSION)


# 2. Hypertension medication and (Systolic BP >= 140 or Diastolic BP >= 90 mmHg on 2 or more occasions)
hyper_drugs <- tbl(con, "course3_data.D_ANTIHYPERTENSIVES")
drugs_patient <- tbl(con, "mimic3_demo.PRESCRIPTIONS")

chartevents <- tbl(con, "mimic3_demo.CHARTEVENTS")
d_items <- tbl(con, "mimic3_demo.D_ITEMS")

hyper_drugs %<>%
  pull(DRUG) %>%
  tolower

hyper_drugs_patient <- drugs_patient %>%
  filter(lower(DRUG) %in% hyper_drugs) %>%
  mutate(hyper_drugs_prescribed = 1) %>%
  distinct(SUBJECT_ID, hyper_drugs_prescribed)

sysbp_any <- d_items %>% 
  inner_join(chartevents, by = c("ITEMID" = "ITEMID"), suffix = c("_c","_d")) %>% 
  filter(tolower(LABEL) %like% '%systolic%') %>% 
  group_by(SUBJECT_ID) %>%
  mutate(sysbp_over140_marker = case_when(VALUENUM >= 140 ~ 1, TRUE ~ 0)) %>%
  summarise(sysbp_over140_count = sum(sysbp_over140_marker, na.rm = TRUE)) %>%
  mutate(sysbp_over140_2ormore = case_when(sysbp_over140_count >= 2 ~ 1, TRUE ~ 0)) %>%
  select(SUBJECT_ID, sysbp_over140_2ormore)

diasbp_any <- d_items %>% 
  inner_join(chartevents, by = c("ITEMID" = "ITEMID"), suffix = c("_c","_d")) %>% 
  filter(tolower(LABEL) %like% '%diastolic%') %>% 
  group_by(SUBJECT_ID) %>%
  mutate(diasbp_over90_marker = case_when(VALUENUM >= 90 ~ 1, TRUE ~ 0)) %>%
  summarise(diasbp_over90_count = sum(diasbp_over90_marker, na.rm = TRUE)) %>%
  mutate(diasbp_over90_2ormore = case_when(diasbp_over90_count >= 2 ~ 1, TRUE ~ 0)) %>%
  select(SUBJECT_ID, diasbp_over90_2ormore)

goldStd %>% 
  left_join(sysbp_any) %>% 
  left_join(diasbp_any) %>% 
  left_join(hyper_drugs_patient) %>%
  mutate(sysbp_over140_2ormore = coalesce(sysbp_over140_2ormore, 0),
         diasbp_over90_2ormore = coalesce(diasbp_over90_2ormore, 0),
         hyper_drugs_prescribed = coalesce(hyper_drugs_prescribed, 0)
  ) %>% 
  mutate(drugs_and_sysbp_or_diasbp = case_when(hyper_drugs_prescribed == 1 && (sysbp_over140_2ormore == 1 |
                                                                                 diasbp_over90_2ormore == 1) ~ 1,
                                               TRUE ~ 0)) %>% 
  collect() %>% 
  getStats(drugs_and_sysbp_or_diasbp, HYPERTENSION)



# 3. ICD code >=3 times or last systolic BP >= 140 or last diastolic BP >= 90
diagnoses_icd <- tbl(con, "mimic3_demo.D_ICD_DIAGNOSES")
diagnoses_icd_patient <- tbl(con, "mimic3_demo.DIAGNOSES_ICD")

chartevents <- tbl(con, "mimic3_demo.CHARTEVENTS")
d_items <- tbl(con, "mimic3_demo.D_ITEMS")

cnt_hyp_icd <- diagnoses_icd_patient %>% 
  filter(ICD9_CODE %in% c("4010", "4011", "4019")) %>% 
  group_by(SUBJECT_ID) %>%
  mutate(hypertension_prediction = 1) %>%
  summarise(hypertension_prediction_count = sum(hypertension_prediction, na.rm = TRUE)) %>%
  mutate(hyp_icd_3ormore = case_when(hypertension_prediction_count >= 3 ~ 1, TRUE ~ 0)) %>%
  select(SUBJECT_ID, hyp_icd_3ormore)

sysbp_over140_last <- chartevents %>%
  inner_join(d_items, by=c("ITEMID" = "ITEMID"), suffix=c("_c", "_d")) %>%
  filter(tolower(LABEL) %like% '%systolic%') %>% 
  group_by(SUBJECT_ID) %>%
  mutate(last_sysbp = max(CHARTTIME, na.rm = TRUE)) %>%
  filter(CHARTTIME == last_sysbp) %>%
  mutate(sysbp_over140_last = case_when(VALUENUM >= 140 ~ 1, TRUE ~ 0)) %>%
  select(SUBJECT_ID, sysbp_over140_last)

diasbp_over90_last <- chartevents %>%
  inner_join(d_items, by=c("ITEMID" = "ITEMID"), suffix=c("_c", "_d")) %>%
  filter(tolower(LABEL) %like% '%diastolic%') %>% 
  group_by(SUBJECT_ID) %>%
  mutate(last_diasbp = max(CHARTTIME, na.rm = TRUE)) %>%
  filter(CHARTTIME == last_diasbp) %>%
  mutate(diasbp_over90_last = case_when(VALUENUM >= 90 ~ 1, TRUE ~ 0)) %>%
  select(SUBJECT_ID, diasbp_over90_last)

goldStd %>% 
  left_join(cnt_hyp_icd) %>% 
  left_join(sysbp_over140_last) %>% 
  left_join(diasbp_over90_last) %>%
  mutate(sysbp_over140_last = coalesce(sysbp_over140_last, 0),
         diasbp_over90_last = coalesce(diasbp_over90_last, 0),
         hyp_icd_3ormore = coalesce(hyp_icd_3ormore, 0)
  ) %>% 
  mutate(icd_and_sysbp_or_diasbp = case_when(hyp_icd_3ormore == 1 | sysbp_over140_last == 1 |
                                               diasbp_over90_last == 1 ~ 1,
                                             TRUE ~ 0)) %>% 
  collect() %>% 
  getStats(icd_and_sysbp_or_diasbp, HYPERTENSION)
