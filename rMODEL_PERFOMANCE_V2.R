
library(tidyverse)
library(dplyr)
library(stringr)
library(formatR)
library(tibble)
library(lubridate)
library(caret)

source(file="rCapFUNCTIONS.R")
###############
## Read training file and create batch statistics
#################
## get test data set
HIST_VERSION = "v25"
HISTFILE = paste("z_ModelData_", HIST_VERSION, sep="")

Results <- data.frame()
Results <- rbind(Results, GetTrainResults("zum", "_msu_test"))



saveRDS(Results, HISTFILE)

####### TABLE OF TRAINING INFORMATION
tab1 <- Results %>% 
  mutate(Accuracy = sprintf("%.2f%%", Accuracy),
         FalsePos = sprintf("%.2f%%", FalsePos),
         FalseNeg = sprintf("%.2f%%", FalseNeg),
         BestTune = trBestTune) %>%
  select(batch, Type = ftype, Model = model, Testset = testset, Accuracy, FalsePos, FalseNeg, Variables, Objects, trBestTune) %>% arrange(batch, Type, Model)

tab1

#cap_out <- paste("Model Training Information")
#knitr::kable(tab1, align = c("c"), caption = cap_out) %>%
#  kable_styling(latex_options = 
#                  c("striped", "left"),
#                full_width = F)

##############################################



