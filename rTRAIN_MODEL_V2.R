# rTRAIN_MODEL_2 #################################################################
# This SCRIPT runs model training batches that are usually executed with Rscript 
# and run for many hours. The script performs the following:
#  1. Loads appropriate _test and _train data sets for the batch (BATCH).
#  2. Selects the appropriate variables (columns) from the training / testing data sets
#  3. Runs a training session for each specified file_type in the list FORMATS. 
#     The file_formats are ALL, PE, PEplus, Binary, Document, Text, and Other.
#      a) Filters ojects in the input data set for training / testing by file_types
#         specified in FORMATS.
#      b) Trains and tests sets for each file_type by using the tools specified by
#          the variables RPART, RFC and XGB.
#      c) Stores the training object in the file with a file name:
#           {batch}train_{model}_{file_type}   e.g B1train_rpart_Document
#
#######################################################################################
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(doParallel)) install.packages("doParallel", repos = "http://cran.us.r-project.org")

library(caret)
library(dplyr)
library(e1071)
library(randomForest)
library(xgboost)
library(doParallel)


# Configure batch indicator (BATCH)
# this is configured for the training set _msu_train which is included with
# the Capstone submission
BATCH <- "zum"  # 
RPART <- TRUE   # run rpart
XGB <- FALSE     # run XGB
RF <- FALSE      # run random forest
FORMATS <- c("ALL")  # options: "ALL", "PE","PEplus", "Binary","Text","Document","Other"

# load test and train data sets based on batch designation 
#  (using BATCH avoids misconfiguration - the batch ID indicates the data set, variables).
if(BATCH %in% c("z2","zt",  "ztm")) {f_train = "_mst_train"; f_test = "_mst_test"}
if(BATCH %in% c("zu","zus", "zum")) {f_train = "_msu_train"; f_test = "_msu_test"}
if(BATCH %in% c("zv","zvs", "zvm")) {f_train = "_meq_train"; f_test = "_meq_test"}
if(BATCH %in% c("zw","zws", "zwm")) {f_train = "_msw_train"; f_test = "_msw_test"}
D_train <-readRDS(file = f_train) %>% select(-starts_with("f_")) %>% filter(!is.na(size))
D_test <- readRDS(file = f_test) %>% select(-starts_with("f_")) %>% filter(!is.na(size))

print("*********************************************")
print("**THIS SCRIPT MAY TAKE 10-15 MINUTES COMPLETE RPART TRAINING**")
print("**THIS SCRIPT MAY TAKE HOURS COMPLETE RF or XGB TRAINING**")
print("*********************************************")

# select variables for training
if(BATCH %in% c("z2", "zu", "zv", "zw")) {      # BIG SET VARIABLES
  RFMTRY = mtry = seq(32, 36, 2)
  vlist <- c("malicious", 
           "file_type", 
           "file_subtype", 
        #    "ident_name", 
        "size", "first_seen",
        "parents", "children", "str_all", "str_rest", "p_rha", "p_run_proc", 
        "p_undoc_api", "p_china", "p_russia", "p_korea", "p_dev_id", "p_dev_cf", 
        "p_monitor", "p_crypt", "p_protect", "p_self_sign", "p_append_dat"
  )
} else if(BATCH %in% c("ztm", "zum", "zvm", "zwm")) {  # MEDIUM NUMBER OF VARIABLES
    RFMTRY = mtry = seq(18, 22, 2)
    subtype_list = c(".Net Exe", "Archive", "Dll", "Exe", "HTML", "JavaScript", "None", "Other", "PowerShell", "VBS", "XML")
    D_train <- D_train %>% mutate(file_subtype = as.factor(ifelse(file_subtype %in% subtype_list, 
                                                             as.character(file_subtype), "Other")))
    D_test <- D_test %>% mutate(file_subtype = as.factor(ifelse(file_subtype %in% subtype_list, 
                                                              as.character(file_subtype), "Other")))
    vlist <- c("malicious", 
             "file_type", 
             "file_subtype", 
          #    "ident_name", 
             "size", "first_seen",
             "parents", "children", "str_all", "str_rest", "p_rha", "p_run_proc", 
             "p_undoc_api", "p_china", "p_russia", "p_korea", "p_dev_id", "p_dev_cf", 
             "p_monitor", "p_crypt", "p_protect", "p_self_sign", "p_append_dat"
  )  
} else {
  RFMTRY = mtry = seq(16, 20, 2)              # SMALL NUMBER OF VARIABLES/NO SUBTYPES
  vlist <- c("malicious", 
             "file_type", 
             #    "file_subtype", 
             #    "ident_name", 
             "size", "first_seen",
             "parents", "children", "str_all", "str_rest", "p_rha", "p_run_proc", 
             "p_undoc_api", "p_china", "p_russia", "p_korea", "p_dev_id", "p_dev_cf", 
             "p_monitor", "p_crypt", "p_protect", "p_self_sign", "p_append_dat"
  )   
}

PARALLEL <- FALSE  # make TRUE to turn on parallel processing 
DESCRIPTION <- ""
print(paste("train row ", nrow(D_train), "batch=", BATCH))

modelScore <- function(y_hat, ref, mod_name = " ") {
  cnt <- length(ref)
  cm <- confusionMatrix(y_hat, ref)
  cat(sprintf("%s Accuracy: %.1f %%  False Pos: %.1f %%  False Neg: %.1f %%  %s\n",
              Sys.Date(),
              100*(cm$table[1,1] + cm$table[2,2]) / cnt, 
              100*cm$table[2,1] / cnt,
              100*cm$table[1,2] / cnt,
              mod_name))
}

#start looping through training sessions for each file_format specified in FORMATS
for(ftype in FORMATS) {
  print("################################")
  Sys.time()
  print(paste("STARTING PROCESSING FORMAT:", ftype, "Prefix: ", BATCH))

    # Filter the datasets for only the specified file_type
    if(ftype == "ALL") {
    d_test  <- D_test %>% filter(file_type != "None")
    d_train <- D_train %>% filter(file_type != "None")
  } else {
    d_test  <- D_test  %>% filter(file_type %in% ftype)
    d_train <- D_train %>% filter(file_type %in% ftype)
  }
  d_train <- select(d_train, all_of(vlist)) 
  print(paste("TRAIN rows", ftype, nrow(d_train)))
  
  # Train the file_type model with the specified tools.
  if(RPART) {
    print(paste("****STARTING RPART Training", ftype, "prefix=", BATCH))
    rgrid <- data.frame(cp = seq(0, 0.1, len = 10)) # data.frame(cp = 0)
    print(sprintf("%s, train objects: %d, test: %d",DESCRIPTION, nrow(d_train), nrow(d_test)))
    if(PARALLEL) {  
      cl <- makePSOCKcluster(6)
      registerDoParallel(cl)
      }
    trainctrl <- trainControl(verboseIter = TRUE, trim = TRUE, returnData = FALSE)
    set.seed(1958, sample.kind="Rounding")
    train_rpart <- train(malicious ~ ., 
                       method = "rpart", 
                       data = d_train, 
                       tuneGrid = rgrid,
                       trControl = trainctrl,
                       na.action = na.omit)
    ## Training object saved under file name constucted from BATCH, train_rpart_, and file_type
    saveRDS(train_rpart, file = paste(sep="",BATCH, "train_rpart_", ftype))
    y_rpart <- predict(train_rpart, newdata=d_test, type="raw")
    if (PARALLEL) {stopCluster(cl)}

    # print results to the console
    cat(sprintf("TRAIN OBJECTS: %d\n", nrow(d_train)," ftype=", ftype, "prefix=", BATCH))
    modelScore(y_rpart, d_test$malicious, paste("TRAIN_RPART", ftype))
    print(train_rpart)
    print(varImp(train_rpart))
    print(paste("****FINISHED RPART Training", ftype, "prefix=", BATCH, "Dataset:",nrow(D_train)))
    print(cat("\n\n"))
    }
  ####END RPART###

  ### RANDOMFOREST for mtry = range 
  if (RF) {
    print(paste("****STARTING RANDOM FOREST Training", ftype, "prefix=", BATCH))
    ns <- 20
    mtry = RFMTRY #seq(32, 36, 2)
    print(paste("mtry = ", mtry))
    print(sprintf("%s, train objects: %d",DESCRIPTION, nrow(d_train)))
    if(PARALLEL) {
      cl <- makePSOCKcluster(6)  
      registerDoParallel(cl)
      }
    trainctrl <- trainControl(verboseIter = TRUE, trim=TRUE, returnData = FALSE)
    set.seed(1958, sample.kind="Rounding")
    train_rfc <- train(malicious ~ ., 
                   method = "rf", 
                   data = d_train,
                   tuneGrid = data.frame(mtry = mtry),
                   nodesize = ns,
                   trControl = trainctrl,
                   na.action = na.omit)
    ## Training object saved under file name constucted from BATCH, train_xrfc_, and file_type
    saveRDS(train_rfc, file = paste(sep="", BATCH, "train_rfc_", ftype))
    y_rfc <- predict(train_rfc, newdata = d_test, type="raw")
    if(PARALLEL) {stopCluster(cl)}
    
    # print results to the console
    cat(sprintf("TRAIN OBJECTS: %d\n", nrow(d_train)," ftype=", ftype, "prefix=", BATCH))
    modelScore(y_rfc, d_test$malicious, paste("TRAIN_RFc", ftype))
    print(train_rfc)
    print(varImp(train_rfc))
    print(paste("****FINISHED RANDOM FOREST Training", ftype, "Dataset:",nrow(D_train)))
    print(cat("\n\n"))
    }
  ####END Random Forest###

  ####BOOST
  if(XGB) {  #no doParallel needed
    print(paste("****STARTING XGB BOOST TREE Training", ftype, "prefix=", BATCH))
    print(sprintf("%s, train objects: %d",DESCRIPTION, nrow(d_train)))
    trainctrl <- trainControl(verboseIter = TRUE, trim=TRUE, returnData = FALSE)
    set.seed(1958, sample.kind="Rounding")
    train_xgb <- train(malicious ~ ., 
                     method = "xgbTree", 
                     data = d_train,
                     trControl = trainctrl,
                     na.action = na.omit)
    ## Training object saved under file name constucted from BATCH, train_xgb_, and file_type
    saveRDS(train_xgb, file = paste(sep="", BATCH, "train_xgb_", ftype))
    y_xgb <- predict(train_xgb, newdata = d_test, type="raw")

    # print results to the console
    cat(sprintf("TRAIN OBJECTS: %d\n", nrow(d_train)," ftype=", ftype, "prefix=", BATCH))
    modelScore(y_xgb, d_test$malicious, paste("TRAIN_XGB", ftype))
    print(train_xgb)
    print(varImp(train_xgb))
    print(paste("****FINISHED XGB BOOST TREE Training", ftype, "prefix=", BATCH, "Dataset:",nrow(D_train)))
    print(cat("\n\n"))
    }
  ####END Random Forest###
}
warnings()

