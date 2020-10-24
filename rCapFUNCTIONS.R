#########################################################################
# FUNCTIONS MODULE
# This module is designed for loading via source() for use by scripting files.
# It contains functions that are used to download files, create a modeling
# data sets, test models and create a performance results database for analysis.
# The Functions are group below as follows:
#   - GET TRAINING RESULTS
#   - CREATE MODEL DATA SET
#   - JSON / DOWNLOAD FILE INFORMATION
######################################################################## 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tibble)) install.packages("tibble", repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("httr", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(tibble)
library(jsonlite)
library(httr)
library(formatR)
library(caret)

#######################################################################
# GET TRAINING RESULTS (one function)
#######################################################################
# This script grabs the training objects created by the 
# the script rTRAIN_MODEL for testing and creating a performance database.  
# rTRAIN_MODEL uses a Batch id (e.g. "ztm") to signify the training set and
# the variable set used.  The batch id also act as the prefix on the filename
# for the training objects created. The file name body identifies the modeling
# tool (e.g. rpart)  and the suffix identifies the file type (e.g. ALL, PE).
# An example:
#   .ztmtrain_rpart_PE - designates "ztm" training set, "rpart" model for "PE" files.
#
# The GetTrainResults() funtion below loads the training objects for the specified 
# batch (e.g. "ztm"), tests them with the specified test set (TestSet), and
# returns the results in a data frame.  The data frame has one row per test
# identified by batch, file type, testset and model.
# 
# An example output ("RESULTSDAT") is included with this submission for use by the 
# Capstone_Report Markdown document to create graphs and tables.
# 
# GetTrainResults()
# The function creates a data frame table with test results for a training object
# set (one per file type and modeling tool).
#
GetTrainResults <- function(Batch, TestSet) {
  Model <-tibble()
  TEST1  <- readRDS(TestSet) %>% 
    filter(!is.na(size)) %>% filter(file_type != "None")

  # Consolidate levels in factor variable (file_subtype) to match training data set.
  if(Batch %in% c("ztm", "zum", "zvm", "zwm", "zxm")) {
    subtype_list = c(".Net Exe", "Archive", "Dll", "Exe", "HTML", "JavaScript", "None", "Other", "PowerShell", "VBS", "XML")
    TEST1 <- TEST1 %>% mutate(file_subtype = as.factor(ifelse(file_subtype %in% subtype_list, 
                                                              as.character(file_subtype), "Other")))
  }
  # The objects that resulted from training are stored with file format:
  #   {batch}train_{model}_{file_type}  e.g. zttrain_rpart_Document
  # The script looks in the current working directory to create a
  # list of train object files starting with the specified BATCH prefix (e.g. "zt").
  # It then uses file name suffix to derive their 
  #    model type e.g. rpart, xbg, rfc 
  #    file type e.g. PE Document
  #  and create the list model/file type pairs that are stored in mod_comp for processing
  fregex <- paste(sep="", "^", Batch, "train_")
  file_list <- list.files(".", fregex)
  TRAINOBJECTS <- str_match(file_list,".*train_((.*?)_(.*?))$")
  
  # The data set divided for separate training
  # by file type (PE, PEplus, Binary, Text, Document, Other)
  # Each file type is trained separate modeling technologies 
  # including rpart, xgb, randam forest.
  # Below we loop through model/file type pairs to 
  # create and measure the results of each
  ModCompare <- list()
  print(paste("Num Objs", length(TRAINOBJECTS[,1])))
  for(x in 1:length(TRAINOBJECTS[,1])) {
      print(paste("----", TRAINOBJECTS[x,4],TRAINOBJECTS[x,3], x,length(TRAINOBJECTS[,1])))
      # segment test data for the file type in this loop
      ifelse("ALL" %in% TRAINOBJECTS[x,4],
           index <- 1:nrow(TEST1),
           index <- which(TEST1$file_type %in% TRAINOBJECTS[x,4]))
      if(length(index) >0) {
           test1 <- TEST1[index,]
      
      # ModCompare dataframe contain the reference test data set and
      # prediction results for all model/file type pairs.
      # This section loads the model object trained on the appropriate file_type, 
      # creates prediction seta and store them in the ModCompare data.frame
      # with all model/file type  predictions 
      ModCompare[paste(sep="","test_",TRAINOBJECTS[x,4])] <- list(test1$malicious)
      train <- readRDS(file=TRAINOBJECTS[x,1])
      y_hat <- tryCatch(predict(train, newdata=test1, type='raw'),
                        warning=function(w) {print("predict:FAIL! (warning)"); print(RAINOBJECTS[x,1]); print(w);return(c("-"))},
                        error=function(e) {print("predict:FAIL! (error)"); print(TRAINOBJECTS[x,1]); print(e); return(c("-"))}
      )
      if(y_hat[1] == "-") {next}
      
      ModCompare[TRAINOBJECTS[x,2]] <- list(y_hat)
      # compute accuracy, fp, and fn for predicion set 
      # against test set (test1$malicious)
      cnt <- length(test1$malicious)
      cm <- confusionMatrix(y_hat, test1$malicious)
      Accuracy <- 100*(cm$table[1,1] + cm$table[2,2]) / cnt
      FalsePos <- 100*cm$table[2,1] / cnt
      FalseNeg <-100*cm$table[1,2] / cnt
      # grab variable importance info from the training object
      imp <- tryCatch(varImp(train)$importance %>% rownames_to_column(var="attr"),
                      warning=function(w) {print("FAIL! (warning)");return(c("-"))},
                      error=function(e) {print("FAIL! (error)");return(c("-"))}
      )
      # store information in dataframe for model and file type
      M <- tibble(
        batch      = Batch,
        testset    = TestSet,
        ftype      = TRAINOBJECTS[x,4],
        model      = TRAINOBJECTS[x,3],
        Accuracy   = Accuracy,
        FalsePos   = FalsePos,
        FalseNeg   = FalseNeg,
        weight     = nrow(test1) / nrow(TEST1),
        Variables  = length(train$coefnames),  # Num vars for this batch,
        Objects    = length(train$control$index$Resample01),
        trAccuracy   = train$results$Accuracy %>% head(1),
        trAccuracySD = train$results$AccuracySD %>% head(1),
        trKappa      = train$results$Kappa %>% head(1),
        trKappaSD    = train$results$KappaSD %>% head(1),
        trBestTune   = paste(colnames(train$bestTune)[1], "=", round(train$bestTune[1],4)),
        varimp       = list(imp),
        final        = "-"
      )
      # add results as a row in the Model summary data.frame
      Model <- rbind(Model, M)
    } 
  } # loop back for the next file type / model tool combination
  
 ## Model data frame - now contains the results for model/file type predictions
 ## ModCompare dataframe - now contains predictions for the reference set and 
 ## for each model/file type.
  
  ### ADD ENSEMBLES #####
  # Use the predictions stored ModCompare to create ensemble predictions
  # based on voting 1, 2, or 3 correct predictions
  
  # PerfTest calculates and formats for prediction against test set 
  PerfTest <- function(y, ref, model =" ", ftype = " ", best = " ", wt = 0, obj= "--", vars="--") {
    cnt <- length(ref)
    cm <- confusionMatrix(y, ref)
    data.frame(
      batch    = Batch,
      testset  = TestSet,
      ftype    = ftype,
      model    = model,
      Accuracy = 100*(cm$table[1,1] + cm$table[2,2]) / cnt,
      FalsePos = 100*cm$table[2,1] / cnt,
      FalseNeg = 100*cm$table[1,2] / cnt,
      weight = wt,
      Variables = vars,
      Objects = obj,
      trAccuracy   = "-",
      trAccuracySD = "-",
      trKappa      = "-",
      trKappaSD    = "-",
      trBestTune   = best,
      varimp       = "-",
      final        = "-"
    )
  }
  # End functionPerfTest()
  #Create Ensemble Data and store all results data in Model data frame
  type_list <- unique(Model$ftype)
  for(t in type_list) {
    print(paste("ensemble:", t))
    trObjects <- Model %>% 
      filter(batch == Batch & ftype == t) %>% 
      slice(1) %>% pull(Objects)
    trVariables <- Model %>% 
      filter(batch == Batch & ftype == t) %>% 
      slice(1) %>% pull(Variables)
    print(paste("obj=", trObjects, "vars=", trVariables))
    ifelse("ALL" == t,
           test1 <- TEST1,
           test1 <- TEST1[which(TEST1$file_type == t),])
    wt <-  nrow(test1)/nrow(TEST1)
    nobj <- nrow(TEST1)
    if(nrow(test1) > 0) {
      ensembles <- data.frame(ref = test1$malicious)
      cidx <- TRAINOBJECTS[which(TRAINOBJECTS[,4] == t),2]
      for(c in cidx) {
        if(length(ModCompare[[c]]) == 0) {
          print(paste("** ensemble: MISSING SET", c))
          next 
        }
        ensembles <- eval(parse(text=paste(sep="","data.frame(ensembles, \'", c, "\'= ModCompare[[c]])")))
      }
      ensembles <- ensembles %>% mutate(sum = rowSums(.== "MALICIOUS"),
                                        e_ens1x = factor(levels = c("UNRATED", "MALICIOUS"), 
                                                         ifelse(sum >= 1, "MALICIOUS", "UNRATED")),
                                        e_ens2x = factor(levels = c("UNRATED", "MALICIOUS"), 
                                                         ifelse(sum >= 2, "MALICIOUS", "UNRATED")),
                                        e_ens3x = factor(levels = c("UNRATED", "MALICIOUS"), 
                                                         ifelse(sum >= 3, "MALICIOUS", "UNRATED")))
      Model <- rbind(Model, PerfTest(ensembles$e_ens1x, ensembles$ref, 
                                     "ens1x", t, "match=1", wt, 
                                     obj=trObjects, vars=trVariables))
      Model <- rbind(Model, PerfTest(ensembles$e_ens2x, ensembles$ref, 
                                     "ens2x", t, "match=2", wt, 
                                     obj=trObjects, vars=trVariables))
      Model <- rbind(Model, PerfTest(ensembles$e_ens3x, ensembles$ref, 
                                     "ens3x", t, "match=3", wt, 
                                     obj=trObjects, vars=trVariables))
    }
  }
  # Calculate a composite results for the the sub-models in this batch
  #  Adds {results for each file type} * weight (% of files for the file type)
  #  to create a composite Accuracy, FalsePos and FalseNeg result 
  AComp <- Model %>% filter(batch == Batch & ftype != "ALL") %>%
    mutate(Acc_wt = Accuracy * weight, FP_wt = FalsePos * weight, FN_wt = FalseNeg * weight) %>%
    group_by(model) %>% 
    summarize(Accuracy     = sum(Acc_wt), 
              FalsePos     = sum(FP_wt), 
              FalseNeg     = sum(FN_wt), 
              weight       = sum(weight),
              Variables    = "-",
              Objects      = "-",
              trAccuracy   = "-",
              trAccuracySD = "-",
              trKappa      = "-",
              trKappaSD    = "-",
              trBestTune = paste(sprintf("%.2f%%",sum(weight))),
              varimp       = "-",
              final        = "-"
    )
  r <- nrow(AComp)
  ma <- data.frame(batch= rep(Batch,r), 
                   testset = rep(TestSet, r), 
                   ftype = rep("composite",r), 
                   AComp)
  Model <- rbind(Model, ma)
}
## Model now contains all test results and is the 
## returned data fram from this function
########################################


#########################################################
# Functions: CREATE MODEL DATA SET
#########################################################
# makeModelDF FUNCTION CREATE 
# ds_dl and ds_mwp data downloaded and stored as data frames
# by JSON scripts below
#     OUTPUT
#        ds_model contains the objects used to create the 
#            final training and test sets
#
########################################################

makeModelDF <- function(df_rl, df_mwp) {
  dset <- df_rl %>% unnest(analysis, keep_empty = TRUE) %>% flatten() %>% 
    select(sha1, size,
           parent = contains("parent"),       #added not tested
           children = contains("children"),     #added not tested
           story = contains("tc_report.story"), 
           strings = contains("tc_report.interesting_strings"),
           file_type = contains("tc_report.info.file.file_type"),
           file_subtype = contains("tc_report.info.file.file_subtype"),
           ident_name = contains("tc_report.info.identification.name"),
           cert_certs = contains("tc_report.metadata.certificate.certificates"),
           cert_version = contains("tc_report.metadata.certificate.signer_info.version"),
           cert_issuer = contains("tc_report.metadata.certificate.signer_info.issuer"),
           cert_serialnum = contains("tc_report.metadata.certificate.signer_info.serial_number"),
           cert_digest_algorithm = contains("tc_report.metadata.certificate.signer_info.digest_algorithm"),
           cert_digest_ecryption = contains("tc_report.metadata.certificate.signer_info.digest_encrypt_algorithm"),
           cert_encrypted_digest = contains("tc_report.metadata.certificate.signer_info.encrypted_digest"),
           cert_authenticated_attr = contains("tc_report.metadata.certificate.signer_info.authenticated_attributes"),
           cert_unauthenticated_attr = contains("tc_report.metadata.certificate.signer_info.unauthenticated_attributes")
    )
  left_join(dset, df_mwp, by = "sha1") %>%
    mutate(m1st_seen = as_date(m1st_seen), mLast_seen = as_date(mLast_seen)) %>%
    filter(!is.na(size)) %>% distinct(sha1, .keep_all = TRUE)  #this line is new and untested
}

###############
# makeResultsDF() creates the ds_results dataframe with the ratings for each file
#   might be depricated already
##############
makeResultsDF <- function(df_model) {
  df_model %>% select(sha1, mStatus) %>% 
    mutate(malicious = ifelse(mStatus == "MALICIOUS",1,0),
           mal_sus = ifelse(malicious == 1 | mStatus == "SUSPTICIOUS",1,0)) %>%
    select(-mStatus)
}



####################################################################
#  Functions:  JSON / DOWNLOAD FILE INFORMATION
#################################################################
#  This module does not execute any scripts but contains functions 
#  used by other modules to download file information
#  from the ReversingLabs TiCloud database. These functions are called
#  by scripts in 'rJSON Download Scripts.R' that actual orchestrate
#  the download process. 
#
#  NOTE: RUNNING THE DOWNLOAD SCRIPTS BELOW WILL NOT RUN WITHOUT
#   VALID CREDENTIALS FROM REVERSINGLABS
####################################################################

#########################################################
# FAST.RBIND function
#   adds missing columns with NA Values
#   x,y - datatables for bind()
########################################################
fast.rbind <- function(x,y,value=NA){
  x[setdiff(colnames(y),colnames(x))] <- value
  y[setdiff(colnames(x),colnames(y))] <- value
  return(rbind(x,y))
}

#######################################################
# GET FILE DETAILS FROM WITH HASH LIST 
#   - POST Bulk Request to TiCloud
#  Input: 
#     hash_list = list of hasshes to download
#     exist_list = list of hashes already downloaded. These will not be
#             downloaded again 
#     orig = flag added to each file entry to track origian hashes and hashes from files
# Output:
#     dataframe of the data from the downloads, one row per hash
#  
#######################################################
downloadFileInfo <- function(hash_list,          # list of hashes in " "  character list
                             exist_list = c("0"),  # df with $sha1 for checking for already downloaded info
                             md5 = FALSE,        # TRUE for md5 hashes, else sha1
                             orig = FALSE) {     # TRUE if original load set, e.g. not derived from parent/child
  hlist <- processHashList(hash_list, exist_list, md5)
  if(is.null(hlist)) {return(NULL)}
  print(paste("downloadFileInfo: Adding ", length(hlist), " Files"))
  
  ## FORM AND SENT QUERY FROM TO REVERSINGLABS
  rlpayload <- paste('{ "rl": { "query":{
                     "hash_type":"sha1", "hashes": [', toString(shQuote(hlist, type="cmd")), '] }}}')
  rlurl <- "https://data.reversinglabs.com/api/databrowser/rldata/bulk_query/json"  # Reversinglabs RL database
  r <- POST(rlurl, body = rlpayload, encode = "json", authenticate("e/avonhill", "PeemZ4GM", type = "basic"))
  if(r$status_code != 200) {
    print(paste("status code:", r$status_code))
  }
  ## Convert from JSON to dataframe
  dset <- fromJSON(content(r, as="text",encoding = "UTF-8"))
  if(!is_empty(dset$rl$unknown_hashes)) {
    print(paste("unknown hash", dset$rl$unknown_hashes))
  }
  if(is_empty(dset$rl$entries)) {
    print("downloadFileInfo: no entries found in POST")
    return(NULL)
  }
  dset <-data.frame(entries = dset$rl$entries)
  dset <- flatten(dset)
  dset <- dset %>% mutate(orig = orig) %>% 
    select(
      sha1 =      contains("entries.sha1"),
      md5 =       contains("entries.md5"),
      sha256 =    contains("entries.sha256"),
      size =      contains("entries.sample_size"),
      parent =    contains("entries.relationships.parent_sample_sha1"),
      children =  contains("entries.relationships.container_sample_sha1"),
      analysis =  contains("entries.analysis.entries"),
      xref =      contains("entries.xref.entries"),
      orig =      contains("orig")
    )
}

#######################################################
# GET SUMMARY DATA FROM MAlWARE PRESENCE DATABASE (MWP)
#     POST Bulk Request to TiCloud
#  Input: 
#     hash_list = list of hasshes to download
#     exist_list = list of hashes already downloaded, ignores these hashese
#     orig = flag added to each file entry to track origianl hashes and hashes from files
# Output:
#     dataframe of the data from the downloads, one row per hash
#  
#######################################################
downloadFileMWP <- function(hash_list,          # list of hashes in " "  character list
                            exist_list = c("0")) { # df with $sha1 for checking for already downloaded info
#  hlist <- processHashList(hash_list, exist_list)
  hlist <- unique(hash_list)
  if(is.null(hlist)) {return(NULL)}
  ## Convert from JSON to dataframe
  rlpayload <- paste('{ "rl": { "query":{
                     "hash_type":"sha1", "hashes": [', toString(shQuote(hlist, type="cmd")), '] }}}')
  rlurl <- "https://data.reversinglabs.com/api/databrowser/malware_presence/bulk_query/json?extended=true&show_hashes=true"  # Reversinglabs RL database
  r <- POST(rlurl, body = rlpayload, encode = "json", authenticate("e/avonhill", "PeemZ4GM", type = "basic"))
  if(r$status_code != 200) {
    print(paste("status code:", r$status_code))
  }
  mpset <-as.data.frame(fromJSON(content(r, as="text",encoding = "UTF-8")))
  mpset <- flatten(mpset) %>% select(
    sha1 =          contains("rl.entries.query_hash.sha1"),
    md5 =           contains("rl.entries.md5"),
    sha256 =        contains("rl.entries.sha256"),
    mStatus =       contains("rl.entries.status"),
    mRL_name =      contains("rl.entries.threat_name"),
    mAV_count =     contains("rl.entries.scanner_count"),
    mAV_percent =   contains("rl.entries.scanner_percent"),
    mAV_match =     contains("rl.entries.scanner_match"),
    mPlatform =     contains("rl.entries.classification.platform"),
    mType =         contains("rl.entries.classification.type"),
    mIs_generic =   contains("rl.entries.classification.is_generic"),
    mFamily =       contains("rl.entries.classification.family_name"),
    m1st_seen =     contains("rl.entries.first_seen"),
    mThreat_level = contains("rl.entries.threat_level"),
    mTrust_factor = contains("rl.entries.trust_factor"),
    mLast_seen =    contains("rl.entries.last_seen")
  )
}

#######################################################
# CLEAN HASH LIST - check that hashes are valid and new
#  Input: 
#     hash_list = list of hashes to download
#     exist_list = list of hashes already downloaded, ignores these hashese
# Output:
#     list of good, new hashes
#  
#######################################################
processHashList <- function(hash_list,          # list of hashes, character list
                            exist_list = c("0"), # df with $sha1 for checking for already downloaded info
                            md5 = FALSE) {     # TRUE if original load set, e.g. not derived from parent/child
  ## Check exist_list format 
  if(!is.character(exist_list)) {
    print("downloadFileInfo: input exist_list not list of strings")
    return(NULL)
  }
  ## Remove bad hashes (not sha1 or md5) from list
  ifelse(md5,
         hlist <- as_tibble(hash_list) %>% filter(!is.na(str_match(value, "^[a-fA-F0-9]{16}$" ))) %>% pull(value),
         hlist <- as_tibble(hash_list) %>% filter(!is.na(str_match(value, "^[a-fA-F0-9]{40}$" ))) %>% pull(value)
  )
  ## Remove hashs that are in  existing_list (we already have these)
  hlist <- setdiff(hlist, exist_list)
  ## Chech whether any hashses left in list, return NULL if none
  if(is_empty(hlist)) { 
    print("downLoadFileInfo: All hashes exist already")
    return(NULL)
  }
  hlist <- unique(hlist)
  print(paste("processHashList: New hashes ", length(hlist), " Files"))
  hlist
} 
###### END HASH CLEAN


#######################################################
# DOWNLOAD LARGE BATCHES (thousands) OF RL DATA -
#  Input: 
#     hash_list = list of hashes to download
#     batch = size of chucks (RL limit = 100)
# Output:
#     dataframe of RL data for hash_list
#  
#######################################################
batchRLDownload <- function (
  hash_list, 
  batch) {
  
  options(echo=FALSE)
  d_out <- data.frame()
  for(x in  0:((length(hash_list)-1)%/%batch)) {
    print(" ")
    print(paste("***BATCH: ", x+1))
    idx <- (x*batch+1):(ifelse((x+1)*batch > length(hash_list), length(hash_list), (x+1)*batch))
    d <- downloadFileInfo(hash_list[idx], orig = TRUE)
#    d <- downloadFileInfo(hash_list[idx], ds_model$sha1, orig = TRUE)
    ## checking for all columns so rbind() doen't fail 
#    cols <- c("sha1","md5","sha256","size","parent","children", "analysis","xref", "orig")
#    cnam <- setdiff(cols, names(d))
#    for(cc in cnam) {
#      d[cc] <- rep(0, nrow(dset))
#      print(paste("Missing Columns: ", cc))
#    }
    ## end all columns
    if(!is.null(d_out)) {
      d_out <- rbind(d_out, d)
      saveRDS(d_out, file="d_out")
    } else {
      print(paste("    done batch: ", x+1, "objects:", nrow(d)))
    }
    print(paste("------DONE: Files downloaded: ", nrow(d_out)))
  }
  options(echo=TRUE)
  d_out
}

#######################################################
# DOWNLOAD LARGE BATCHES OF MWP DATA -
#  Input: 
#     hash_list = list of hashes to download
#     batch = size of chucks (RL limit = 100)
# Output:
#     dataframe of RL data for hash_list
#  
#######################################################
batchMWPDownload <- function (
  hash_list, 
  batch) {
  
  options(echo=FALSE)
  d_out <- data.frame()
  for(x in  0:((length(hash_list)-1)%/%batch)) {
    print(" ")
    print(paste("***BATCH: ", x+1))
    idx <- (x*batch+1):(ifelse((x+1)*batch > length(hash_list), length(hash_list), (x+1)*batch))
    d <- downloadFileMWP(hash_list[idx])
#    d <- downloadFileMWP(hash_list[idx], ds_model$sha1)
    ## end all columns
    if(!is.null(d_out)) {
      d_out <- rbind(d_out, d)
      saveRDS(d_out, file="d_out_mwp")
    } else {
      print(paste("    done batch: ", x+1, "objects:", nrow(d)))
    }
    print(paste("------DONE: Files downloaded: ", nrow(d_out)))
  }
  options(echo=TRUE)
  d_out
}

########################################################
# GetRelatedFiles - searches ds for parent/children and looks them up
#   Returns a dataframe with new parent file entries
#   Avoids file entries that duplicate entries already in ds
######################################################
GetRelatedFiles <- function(ds, exist_list = dataset_raw$sha1) {
  if(!is.data.frame(ds)) {
    print("GetRelatedFiles: ds argument must be a dataframe with columns 'parent' and 'children")
    return(NULL)
  }
  rp_hash <- ds %>% unnest(parent) %>% select(parent) %>% pull(parent)
  rc_hash <- ds %>% unnest(children) %>% select(children) %>% pull(children)
  r_hash <- append(rp_hash, rc_hash)
  r_hash <- setdiff(r_hash, exist_list)
  r_hash
}
