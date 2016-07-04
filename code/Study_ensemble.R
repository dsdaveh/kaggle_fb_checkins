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

knn_norm = TRUE
chunk_size = 10
grid_nx = 50
grid_ny = 50 
min_occ = 10
expand_margin = .005
tbound_lower = 0

knn_weights <- knn_weights_xgb
hp_classify <- hp_classify_knn_RANN

# k_vals <- numeric();    tcheck(desc='start K sensitivity study')
# knn_probs = FALSE
# for (knn_k in seq(10,50,5)) {
#     source('variable_grid_analysis1.R'); tcheck(desc=sprintf('knn k=%d complete', knn_k))
#     k_vals <- c(k_vals, mean(chunk$score))
# }
# plot( seq(10,50,5), k_vals, type="b")
#  0.5243515 0.5301292 0.5318033 0.5315496 0.5312466 0.5310014 0.5304353 0.5292710 0.5275573
# seq(10,50,5)[which.max(k_vals)] # k = 20

knn_probs = TRUE
knn_k = 20
tcheck(desc='vga start grid 50x50 knn baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53180332 0.05090504 [1] "252.800000 elapsed
# hp_results %>% group_by(grid_x,grid_y) %>% summarize( score=calculate_map_score( data.frame(truth=truth, predictions=predictions) ))
setkey(hp_results, row_id, X1)
hp_results_knn.k20 <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
calculate_map_score(hp_results_knn.k20) # 0.5342708

knn_probs = TRUE
knn_k = 10
tcheck(desc='vga start grid 50x50 knn baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.52435149 0.04871733 [1] "240.440000 elapsed
# hp_results %>% group_by(grid_x,grid_y) %>% summarize( score=calculate_map_score( data.frame(truth=truth, predictions=predictions) ))
setkey(hp_results, row_id, X1)
hp_results_knn.k10 <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
calculate_map_score(hp_results_knn.k10) # 0.5268155

knn_probs = TRUE
knn_k = 40
tcheck(desc='vga start grid 50x50 knn baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53043531 0.05063838 [1] "244.580000 elapsed
# hp_results %>% group_by(grid_x,grid_y) %>% summarize( score=calculate_map_score( data.frame(truth=truth, predictions=predictions) ))
setkey(hp_results, row_id, X1)
hp_results_knn.k40 <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
calculate_map_score(hp_results_knn.k40) # 0.5327158

knn_probs = TRUE
knn_k = 20
tbound_lower = 1/3
tcheck(desc='vga start grid 50x50 knn baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.52895638 0.05897209 [1] "239.700000 elapsed
# hp_results %>% group_by(grid_x,grid_y) %>% summarize( score=calculate_map_score( data.frame(truth=truth, predictions=predictions) ))
setkey(hp_results, row_id, X1)
hp_results_knn.k20.t33 <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
calculate_map_score(hp_results_knn.k20.t33) # 0.5329836

knn_probs = TRUE
knn_k = 20
tbound_lower = 2/3
tcheck(desc='vga start grid 50x50 knn baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.50724944 0.06590111 [1] "310.520000 elapsed
# hp_results %>% group_by(grid_x,grid_y) %>% summarize( score=calculate_map_score( data.frame(truth=truth, predictions=predictions) ))
setkey(hp_results, row_id, X1)
hp_results_knn.k20t67 <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
calculate_map_score(hp_results_knn.k20t67) # 0.5126786

# k_vals <- numeric();    tcheck(desc='start K sensitivity study')
# knn_probs = FALSE
# for (knn_k in seq(10,50,5)) {
#     source('variable_grid_analysis1.R'); tcheck(desc=sprintf('knn k=%d complete', knn_k))
#     k_vals <- c(k_vals, mean(chunk$score))
# }
# plot( seq(10,50,5), k_vals, type="b")
# #[1] 0.5034148 0.5062157 0.5072494 0.5054332 0.5037054 0.5015990 0.4992912 0.4974070 0.4950117 # max k=20

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
tbound_lower <- 0
tcheck(desc='vga start grid 50x50 xgb baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.54083016 0.05065228 [1] "410.670000 elapsed 
setkey(hp_results, row_id, X1)
hp_results_xgb <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
calculate_map_score(hp_results_xgb) # 0.5426339

tbound_lower <- 1/3
hp_classify <- hp_classify_xgb
tcheck(desc='vga start grid 50x50 xgb baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.52844662 0.05947499 [1] "258.880000 elapsed
setkey(hp_results, row_id, X1)
hp_results_xgb.t33 <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
calculate_map_score(hp_results_xgb.t33) # 0.5316815

tbound_lower <- 2/3
hp_classify <- hp_classify_xgb
tcheck(desc='vga start grid 50x50 xgb baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.50283709 0.06676792 [1] "144.420000 elapsed
setkey(hp_results, row_id, X1)
hp_results_xgb.t67 <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
calculate_map_score(hp_results_xgb.t67) # 0.5078497