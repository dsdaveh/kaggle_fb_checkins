# settings based on Study_Grid_vga1.R
# currently produces a chunk with LB score ~ 

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(xgboost)
library(lubridate)

source('fb_checkins_util.R')

ichunk=1
hp_classify <- hp_classify_xgb

tcheck(0)
#train <- fread('../input/train.csv', integer64='character') %>% global_features() tcheck(desc='build train w global features')
load("~/GitHub/kaggle/Facebook_Checkins/data/train_w_global.RData")
test <- fread('../input/test.csv', integer64='character') %>% global_features(); tcheck(desc='build test w global features')

xgb_params <- list( 
    eta = 0.1,      #
    #     max_depth = 6,   # 
    #     gamma = 0.5,     # 
    #     min_child_weight = 5, #
    #     subsample = 0.5,
    #     colsample_bytree = 0.5, 
    eval_metric = "merror", #"mlogloss",   #map@3",
    objective = "multi:softprob",
    # num_class = 12,
    nthreads = 4,
    # maximize = TRUE
    verbose = 1
)

chunk_size = 10
grid_nx = 50
grid_ny = 50 ;tcheck(desc='vga start grid 50x50')

chunk_size <- 250
xgb_nrounds = 60
tcheck(desc='vga start')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# [1] "17824.200000 elapsed for vga complete"  

## there could be duplicate row_id's so they need to be resolved
## method below uses the row with the lowest X1 (probability) since  
## in most cases place_id1 is identical and a lower X1 probability
## results in a higher X2, X3 probability
setkey(hp_results, row_id, X1)
hp_results <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
result <- hp_results[,.(row_id, place_id=predictions)]
result_p <- hp_results[,.(row_id, X1, X2, X3)]

blanks <- setdiff( test$row_id, hp_results$row_id) 
result <- rbind(result, data.frame( row_id=blanks, place_id = ""))
if (! exists("submit_name")) submit_name <- 
    sprintf("../submissions/xgb_vga50x50_chunk%d_%s.csv", ichunk, format(Sys.time(), "%Y_%m_%d_%H%M%S"))
cat('writing', submit_name, EOL)
write.csv(result, file=submit_name, row.names=FALSE) #
if (! exists("submit_name_p")) submit_name_p <- gsub("vga50x50_", "vga50x50_probs_", submit_name)
cat('writing', submit_name_p, EOL)
write.csv(result_p, file=submit_name_p, row.names=FALSE) #

data_template <- "../data/xgb_results_50x50_chunk%d.RData"
data_name <- sprintf(data_template, ichunk)
save(hp_results, file=data_name) 
rm(ichunk) #to prevent accidental overwrite
tcheck(desc='complete')
