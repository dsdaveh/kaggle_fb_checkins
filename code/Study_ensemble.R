if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')
tcheck(0)

## load data  (allow for pre-loaded subsets)
if (! exists("train")) {
    #train <- fread('../input/train.csv', integer64='character') %>% global_features(); tcheck(desc='build train w global features')
    load("../data/train_w_global.RData")
}

train[ , hour_sin := sin(hour * pi/24) ]
train[ , hour_sincos := hour_sin * cos(hour * pi/24) ]
train[ , accuracy := log10(accuracy)]

train[ ,rating_history := NULL ]
train[ ,year := NULL ]
train[ ,month := NULL ]

setorder(train, time)
train_cut_ix <- as.integer(nrow(train) * .80)
test <- train[(train_cut_ix+1):nrow(train), ]
train <- train[1:train_cut_ix, ]

knn_weights_xgb <- c(0.233083074,0.460971279,0.052850552,0.070733799,0.027843511,
                     0.019624543,0.009489949,0.019791895,0.037008454,0.026524299,0.006716613,0.018984853,0.016377178)
names(knn_weights_xgb) <- names( create_features_safe(train[1:10]) %>% select( -c(row_id, place_id)))
### Baseline runs
chunk_size = 10

knn_k = 30
knn_norm = TRUE
chunk_size = 10
grid_nx = 50
grid_ny = 50 
min_occ = 10
expand_margin = .005

knn_weights <- knn_weights_xgb
knn_probs = TRUE
tcheck(desc='vga start grid 50x50 knn baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53124656 0.05166494 [1] "241.220000 elapsed
# hp_results %>% group_by(grid_x,grid_y) %>% summarize( score=calculate_map_score( data.frame(truth=truth, predictions=predictions) ))
setkey(hp_results, row_id, X1)
hp_results_knn <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
calculate_map_score(hp_results_knn) # 0.5336533


xgb_params <- list( 
    eta = 0.1,      #
    #     max_depth = 6,   # 
    #     gamma = 0.5,     # 
    #     min_child_weight = 5, #
    #     subsample = 0.5,
    #     colsample_bytree = 0.5, 
    eval_metric = "mlogloss", #merror",  #map@3",
    objective = "multi:softprob",
    # num_class = 12,
    nthreads = 4,
    # maximize = TRUE
    verbose = 1
)
xgb_nrounds = 50

hp_classify <- hp_classify_xgb
tcheck(desc='vga start grid 50x50 xgb baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.54083016 0.05065228 [1] "410.670000 elapsed 
setkey(hp_results, row_id, X1)
hp_results_xgb <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
calculate_map_score(hp_results_xgb) # 0.5426339



