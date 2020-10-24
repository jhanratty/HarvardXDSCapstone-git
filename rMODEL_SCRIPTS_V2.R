# THIS SCRIPT CREATES TRAIN AND TESTS SETS FROM THE JSON DOWNLOADS
#   Use rJSON_SCRIPTS and rJASON_FUNCTIONS to download file information
#   and creates a Model Data Set (new_model) that is partitioned into train and test sets
# ##########################################

library(tidyverse)
library(dplyr)
library(stringr)
library(formatR)
library(tibble)
library(lubridate)
# some functions from this file are used
source(file="rCapFUNCTIONS.R")

print("**************************************************")
print("**THIS SCRIPT MAY TAKE 5+ MINUTES WITH NO OUTPUT TO COMPLETE**")
print("**************************************************")

# Note: the _msu_model is included in the distribution and 
#       configured below so you can build _msu_train and _msu_test
#       for training and testing a model.
#EDIT THESE - Configure your input and output files
# INFILE_RL  <- "_newset_rl"    # input file with data created by rJSON_SCIPTS.R
# INFILE_MWP <- "_newset_mwp"   # input file with data created by rJSON_SCIPTS.R
OUT_MODEL  <- "_msu_model" # file to store model data set
OUT_TEST   <- "_msu_test"  # file to store model data set
OUT_TRAIN  <- "_msu_train" # file to store model data set

## NO MORE EDITING
## Build Training & Testing Dataset
if (exists("INFILE_RL") & exists("INFILE_MWP")) {   # TRUE if you have defined _rl & _mwp datafiles
  d_rl <- readRDS(file = INFILE_RL)
  d_mwp <- readRDS(file = INFILE_MWP)
  d_model <- makeModelDF(new_rl, new_mwp)
  saveRDS(d_model, OUT_MODEL)
}

## Begin by extracting attributes directly from the d_model
d_model <- readRDS(OUT_MODEL)
d_results <- makeResultsDF(d_model)
ds_TnT <- d_model %>% left_join(d_results, by="sha1") %>% 
  select(sha1, malicious, file_type, file_subtype, ident_name, size, m1st_seen) %>%
  mutate(first_seen = ifelse(is.na(m1st_seen), 0, as_date(m1st_seen)))

# Parent data (count of parents referenced in each file)
tmp_p <- d_model %>% select(sha1, parent, children) %>% 
  mutate(parent = as.character(parent)) %>%
  separate_rows(parent) %>% 
  filter(!is.na(str_match(parent, "^[a-fA-F0-9]{40}$" ))) %>% 
  group_by(sha1) %>% summarize(parents = n()) %>%
  right_join(d_results, by="sha1") %>%
  mutate(parents = ifelse(is.na(parents), 0, parents)) %>%
  select(sha1, parents)
ds_TnT <- left_join(ds_TnT, tmp_p, by="sha1")

# Children data (count of children referenced in each file)
tmp_c <- d_model %>% select(sha1, parent, children) %>% 
  mutate(children = as.character(children)) %>%
  separate_rows(children) %>% 
  filter(!is.na(str_match(children, "^[a-fA-F0-9]{40}$" ))) %>% 
  group_by(sha1) %>% summarize(children = n()) %>%
  right_join(d_results, by="sha1") %>%
  mutate(children = ifelse(is.na(children), 0, children)) %>%
  select(sha1, children)
ds_TnT <- left_join(ds_TnT, tmp_c, by="sha1")

# Strings data (count of various string types referenced in each file)
tmp_s <- d_model %>%    
  select(sha1, strings) %>% unnest(strings) %>% 
  mutate(category = as.character(category), values = as.character(values)) %>%
  separate_rows(category, values) %>%
  group_by(sha1) %>% 
  dplyr::summarize(
    ip = sum(str_count(values, "\\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\b")),
    web = sum(str_count(values, "^((ftp|http|https):\\/\\/)?(www.)?(?!.*(ftp|http|https|www.))[a-zA-Z0-9_-]+(\\.[a-zA-Z]+)+((\\/)[\\w#]+)*(\\/\\w+\\?[a-zA-Z0-9_]+=\\w+(&[a-zA-Z0-9_]+=\\w+)*)?$")),
    email = sum(str_count(values, "^([a-zA-Z0-9_\\-\\.]+)@([a-zA-Z0-9_\\-\\.]+)\\.([a-zA-Z]{2,5})$")),
    all = sum(!is.na(values)),
    rest = sum(all - ip - web - email)
  ) %>% right_join(d_results, by="sha1") %>%
  mutate(str_ip = ifelse(is.na(ip), 0, ip),
         str_web = ifelse(is.na(web), 0, web),
         str_email = ifelse(is.na(email), 0, email),
         str_all = ifelse(is.na(all), 0, all), 
         str_rest = ifelse(is.na(rest), 0, rest), 
         Class=ifelse(malicious==0, "Unrated", "Malicious")) %>%   
  select(sha1, str_ip, str_web, str_email, str_all, str_rest)
ds_TnT <- left_join(ds_TnT, tmp_s, by="sha1")

# Extract File Attributes From ReversingLabs Summary Narrative (story)
dsa_story <- d_model %>% select(sha1, story) %>%
  mutate(
    p_rha =  ifelse(is.na(story),0, str_detect(story, "[Rr][Hh][Aa]")),
    p_run_proc =  ifelse(is.na(story), 0, str_detect(story, "running proc")),
    p_undoc_api = ifelse(is.na(story), 0, str_detect(story, "undocumented API")),
    p_china =     ifelse(is.na(story), 0, str_detect(story, "[Cc]hin")),
    p_russia =    ifelse(is.na(story), 0, str_detect(story, "[Rr]ussia")),
    p_korea =    ifelse(is.na(story), 0, str_detect(story, "[Kk]orea")),
    p_dev_id =    ifelse(is.na(story), 0, str_detect(story, "device ident")),
    p_dev_cf =    ifelse(is.na(story), 0, str_detect(story, "device config")),
    p_monitor =    ifelse(is.na(story), 0, str_detect(story, "monitor")),
    p_crypt =     ifelse(is.na(story), 0, str_detect(story, "[Cc]ryptography")),
    p_protect =   ifelse(is.na(story), 0, str_detect(story, "protection")),
    p_self_sign = ifelse(is.na(story), 0, str_detect(story, "self-signed")),
    p_append_dat = ifelse(is.na(story), 0, str_detect(story, "[Aa]ppended "))
  ) 
ds_TnTa <- left_join(ds_TnT, dsa_story, by="sha1") %>% select(-story) 
## BRANCH TnTa also used below to create a Markdown dataset for the Capstone Summary

#################################
# Final Touches  
ds_TnT <-  ds_TnTa %>%
  mutate(malicious = factor(malicious, labels = c("UNRATED", "MALICIOUS"))) %>%
  select(-str_ip, -str_web, -str_email)

## Clean Up File Types
ds_TnT <- ds_TnT %>% mutate(
     file_type =    ifelse(is.na(file_type), "None", file_type),
     file_type =    ifelse(file_type == "PE+", "PEplus", file_type),
     file_type =    ifelse(file_type %in% c("PE", "PEplus","Binary","Document", "Text", "None"), file_type, "Other"),
     file_type =    as.factor(file_type),
     file_subtype = as.factor(ifelse(is.na(file_subtype), "None", file_subtype)),
     ident_name =   as.factor(ifelse(is.na(ident_name), "None", ident_name))
     ) %>% filter(file_type != "None") %>% select(-sha1, -m1st_seen)

## Partition Data for training and testing
set.seed(1958, sample.kind="Rounding")
test_index <- createDataPartition(y = ds_TnT$malicious, times = 1, p = 0.1, list = FALSE)
d_train <- ds_TnT[-test_index,]
d_test <- ds_TnT[test_index,]

saveRDS(d_test,  file = OUT_TEST)
saveRDS(d_train, file = OUT_TRAIN)

