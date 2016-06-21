# settings based on Study_new_features_w_xgb.R
# currently produces a chunk with LB score ~ 0.5699

stopifnot(exists("df_train"))

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(xgboost)
library(lubridate)

source('fb_checkins_util.R')

ichunk=1
hp_classify <- hp_classify_xgb

global_features <- function(dt) {
    
    time0 <- ymd_hms('2014-01-01 00:00:00')
    dt$ltime <- dt[, time0 + minutes(time) ]
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    
    dt <- dt[,
              c(.SD, .(
                  abs_hr = as.integer(floor(time/60)), 
                  hour = hour(ltime),
                  weekday = wday(ltime),
                  mday = day(ltime),
                  month = month(ltime),
                  year = (year(ltime) - 2013),
                  quarter_period_of_day = as.integer(hour(ltime) / 6),
                  rating_history= log10(3+month(ltime)) )) ]
    
    n_this_hr <-  dt[, .(g_n_this_hr = .N), abs_hr]
    n_this_hr$g_hr_chg <- c(0, diff( n_this_hr$g_n_this_hr)) / n_this_hr$g_n_this_hr
    n_this_hr$g_n_this_hr <- NULL
    
    #JOIN n_this_hr to fea
    setkey(dt, abs_hr); setkey(n_this_hr, abs_hr); dt <- dt[n_this_hr]

    setkey(dt, time)
    dt$g_time_diff <- c(0, diff(dt$time))

    #remove time absolute time vars (except original)
    dt$ltime <- NULL
    
    return(dt)
    
}

create_features <- function(dt) {
    
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    
    n_this_hr <- dt[, .(n_this_hr = .N), abs_hr]

    setkey(dt, abs_hr); setkey(n_this_hr, abs_hr); dt <- dt[n_this_hr]

    setkey(dt, time)
    dt$time_diff <- c(0, diff(dt$time))
    dt$time <- NULL
    dt$abs_hr <- NULL

    dt$rat_hr_chg <- dt$n_this_hr * dt$g_hr_chg
    
    return(dt)
}

tcheck(0)
train <- global_features(df_train); tcheck(desc='add global features') # "310.900000 elapsed

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


#validate the run
# ..[1] 0.55297889 0.09148562 elapsed 82.24 
# hp_classify <- hp_classify_xgb
# chunk_size <- 10
# xgb_nrounds = 54
# tcheck()
# source('hectare_analysis3.R')
# tcheck()
# # ..[1] 0.55126368 0.08894244 elapsed  76.55
# 
# hp_classify <- hp_classify_imp
# load("~/GitHub/kaggle/Facebook_Checkins/code/train_w_global.RData")
# chunk_size <- 3
# source('hectare_analysis3.R')
# 
# impdf <- data.frame()
# for (i in 1:length(imp)) impdf <- rbind(impdf, imp[[i]] %>% mutate(cell=i))
# 
# imp_order <- impdf %>% group_by(Feature) %>% summarize(Gain = mean(Gain)) %>% arrange(Gain) %>% .[[1]]
# impdf %>% 
#     ggplot(aes(Feature, Gain)) + 
#     geom_bar( data=impdf %>% mutate(Gain=Gain/length(imp)), stat="identity") +
#     geom_bar( stat="identity", position="dodge", aes(fill=as.factor(cell))) + 
#     scale_x_discrete(limits = imp_order) +
#     coord_flip() 
# 
# load("~/GitHub/kaggle/Facebook_Checkins/code/train_w_global.RData")
# train$g_time_diff <- NULL
# chunk_size <- 10
# source('hectare_analysis3.R')
# #..[1] 0.55192029 0.08986413  <<< NOT Getting there, but settling for this;

df_test <- fread('../input/test.csv', integer64='character')
test <- global_features(df_test); tcheck(desc='add global features (test)') # "
hp_classify <- hp_classify_xgb
chunk_size <- 1000
xgb_nrounds = 54
source('hectare_analysis3.R');tcheck()

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
    sprintf("../submissions/xgb_ha3_chunk%d_%s.csv", ichunk, format(Sys.time(), "%Y_%m_%d_%H%M%S"))
cat('writing', submit_name, EOL)
write.csv(result, file=submit_name, row.names=FALSE) #
if (! exists("submit_name_p")) submit_name_p <- gsub("ha3_", "ha3_probs_", submit_name)
cat('writing', submit_name_p, EOL)
write.csv(result_p, file=submit_name_p, row.names=FALSE) #

data_template <- "../data/xgb_results_chunk%d.RData"
data_name <- sprintf(data_template, ichunk)
save(hp_results, file=data_name) 
rm(ichunk) #to prevent accidental overwrite

