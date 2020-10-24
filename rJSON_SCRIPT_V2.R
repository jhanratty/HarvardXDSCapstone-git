#  rJSON Download Scripts
#  This module contains scripts to download file information
#  from the ReversingLabs TiCloud database. These scripts call 
#  functions defined in the module 'rJSON Functions.R'.
#  The scripts in this module were hand crafted for each download 
#  project so are not completely generic. 
#
#  NOTE: RUNNING THE DOWNLOAD SCRIPTS BELOW WILL NOT RUN WITHOUT
#   VALID CREDENTIALS FROM REVERSINGLABS
####################################################################

library(tidyverse)
library(dplyr)
library(stringr)
library(tibble)
library(jsonlite)
library(httr)
library(caret)
source(file="rCapFUNCTIONS.R")  # Load required functions

if(TRUE) {
  print("This script is included with the Capstone submission for")
  print("informational purposes.  It will not run without credentials")
  print("for the ReversingLabs TitaniumCloud services")
  quit()
}
################################################################################
# DOWNLOAD FILES FROM REVERSINGLABS BASED ON LIST OF HASHES
#  NOTE: These scripts were crafted and tweaked by hand depending on the download.
#        Your results may vary.
################################################################################
# EDIT THIS TO SPECIFY INPUT/OUTPUT FILES
IN_HASHES <- "random_hash_list3.txt"  # CVS file with list of hashes
OUT_RL    <- "_newset_rl"             # Filename for saving rl data frame
OUT_MWP   <- "_newset_mwp"            # Filename for saving mwp data frame
OUT_MODEL <- "_newset_model"          # Filename for saving model data frame

# reading the hash list for download
hashes_new <- read.csv2(file=IN_HASHES, header = FALSE)
hashes_new <- hashes_new$V1  #convert to char list from dataframe

# Checking new list against hashes contained in data sets that were previously download.
#   We don't want to download the same information multiple times, it's expensive.  :)
mst_model <- readRDS("_mst_model")      # a previously processed model file
txu_model <- readRDS("_txu_model")      # another previously processed model file
exist_list <- c(mst_model$sha1, txu_model$sha1)
hlist <- setdiff(hashes_new, exist_list) # list hashes in hashes_new that are not in exist_list

# Downloading and saving RL and MWP data for the hashes in hlist.  
# Note batchRLDownload saves the downloaded along the way for recovery 
#   from lockup or communication errors
d_rl <- batchRLDownload(hlist, 100)    # new RL data downloaded 
saveRDS(d_rl, OUT_RL)

hlist <- d_rl$sha1
d_mwp <- batchMWPDownload(hlist, 100) 
saveRDS(d_mwp, OUT_MWP)

d_model <- makeModelDF(d_rl, d_mwp)
saveRDS(d_mwp, OUT_MODEL)



################################
# DERIVE AND DOWNLOAD PARENT/CHILD FILES FOR AN EXISTING DATASET
################################
# RECURSIVELY LOOP TO DOWNLOAD PARENT/CHILD FILES for IN_RLDATASET.
#  NOTE: This is a handcrafted script the was customized and baby sat for this process.
#        Don't expect to run it "as is."
####################################################################################### 

# The loop starts with the IN_RLDATASET to extract parent/child hashes 
#  and then download the files.  
OUT_RL <- "_newpc_rl"
OUT_MWP <- "_newpc_mwp"
OUT_MODEL <- "_newpc_model"

d1_rl <- readRDS("_ext_model")      # The staring dataset for parent / child extraction

# The loop grabs the hashes from d1_rl and downloads them. The next loop interatsion repeats
# this with the data set with the new files.   It takes 30-40 loops to follow most parent/child 
# paths
unknown_hashes <- c("49f3acab24814dd6839e9264f3088dd6d4f8a1e7","d1534e349a04bb7e9cac0125797369ceeaac8f97")
for(x in 1:5) {
  print(paste("####### New data LOOP: ", x))
  exist_list <- c(ds_model$sha1, d1_rl$sha1, unknown_hashes)
  rlist <- GetRelatedFiles(d1_rl, exist_list) # extract hash list and ignore exist_list
  print(length(rlist))
  length(setdiff(rlist, exist_list))
  d1_out_rl <- batchRLDownload(rlist, 100)   ## get the files
  d1_rl <- rbind(d1_rl, d1_out_rl)
  saveRDS(d1_rl, "_d1_rl")                   # Saving the cummulated results in case of a crash
} 

# Now down load the MWP data from ReversingLabs
# and create a _model dataset for testing and/or training
 
  rlist <- d1_rl$sha1
  d1_mwp <- batchMWPDownload(rlist, 100)
  d1_model <- makeModelDF(d1_rl, d1_mwp, d1_model)
  saveRDS(d1_ml, file=OUT_RL)
  saveRDS(d1_mwp, file=OUT_MWP)
  saveRDS(d1_mwp, file=OUT_MODEL)
  
  # binding to dataset  
  

######### END LOOP - finding relations



