if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')
tcheck(0)

## load data  (allow for pre-loaded subsets)
if (! exists("train")) {
    #train <- fread('../input/train.csv', integer64='character') %>% global_features() tcheck(desc='build train w global features')
    load("~/GitHub/kaggle/Facebook_Checkins/data/train_w_global.RData")
}

setorder(train, time)
train_cut_ix <- as.integer(nrow(train) * .80)
test <- train[(train_cut_ix+1):nrow(train), ]
train <- train[1:train_cut_ix, ]

xgb_params <- list( 
    eta = 0.1,      #
    #     max_depth = 6,   # 
    #     gamma = 0.5,     # 
    #     min_child_weight = 5, #
    #     subsample = 0.5,
    #     colsample_bytree = 0.5, 
    eval_metric = "merror", #mlogloss",  #map@3",
    objective = "multi:softprob",
    # num_class = 12,
    nthreads = 4,
    # maximize = TRUE
    verbose = 1
)
hp_classify <- hp_classify_xgb

chunk_size = 10
grid_nx = 50
grid_ny = 50 ;tcheck(desc='vga start grid 50x50')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
#..[1] 0.54046043 0.04439034 [1] "423.030000 elapsed for vga complete" (fixed error)
est_time <- ( 423 * grid_nx * grid_ny / chunk_size ) / 3600.  # 29.4 hrs

############### end baseline
chunk_size = 25
grid_nx = 100
grid_ny = 100 ;tcheck(desc='vga start grid 100x100')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 21.6
# [1] 0.53622982 0.08620633 [1] "194.590000 elapsed for vga complete"

# train$hour_sin <- sin(train$hour * pi/24)
# test$hour_sin <- sin(test$hour * pi/24)
train[ , hour_sin := sin(hour * pi/24) ]
test[ , hour_sin := sin(hour * pi/24) ]

chunk_size = 25
grid_nx = 100
grid_ny = 100 ;tcheck(desc='vga start hour_sin')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.5373656 0.0856017 [1] "199.840000 elapsed

train$hour_cos <- cos(train$hour * pi/24)
test$hour_cos <- cos(test$hour * pi/24)
tcheck(desc='vga start hour_cos')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.5385796 0.0849331 [1] "191.640000 elapsed

train$hour_sincos <- train$hour_sin * train$hour_cos
test$hour_sincos <- test$hour_sin * test$hour_cos
tcheck(desc='vga start hour_sincos')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53874442 0.08572107 [1] "192.930000 elapsed

train[ , accuracy := log10(accuracy)]
test[ , accuracy := log10(accuracy)]
tcheck(desc='vga start log10acc')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.5392312 0.0862352 [1] "187.980000 elapsed

######## 50 x 50
chunk_size = 10
grid_nx = 50
grid_ny = 50 ;tcheck(desc='vga start grid 50x50')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53852774 0.04341558 [1] "432.830000 elapsed
est_time <- ( 423 * grid_nx * grid_ny / chunk_size ) / 3600.  # 29.4 hrs

#Maybe this needs more tuning for the 50 x 50?
# random guess to get a feel for sensitivity
xgb_params$eta = 0.07
xgb_nrounds = 100
tcheck(desc='vga start grid 50x50 eta=0.07')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.54194361 0.04320954 [1] "876.920000 elapsed
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 61 hrs OUCH

#Tuning looks like it would help, but at quite a price.  Let's try original eta with more rounds
xgb_params$eta = 0.1
tcheck(desc='vga start grid 50x50 eta=0.1')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# [1] 0.54066897 0.04250652 [1] "866.450000 elapsed
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 60.2

#Well of course that didn't work (duh) raise eta lower nrounds to speed up and find accuracy loss
xgb_params$eta = 0.15
xgb_nrounds = 50
tcheck(desc='vga start grid 50x50 eta=0.15')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53555739 0.04380698 [1] "452.630000 elapsed
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 31
