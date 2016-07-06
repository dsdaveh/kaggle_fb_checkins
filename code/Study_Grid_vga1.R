if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')
tcheck(0)

## load data  (allow for pre-loaded subsets)
if (! exists("train")) {
    #train <- fread('../input/train.csv', integer64='character') %>% global_features() tcheck(desc='build train w global features')
    load("../data/train_w_global.RData")
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
xgb_nrounds = 54
ichunk = 1
chunk_size = 10
grid_nx = 100
grid_ny = 100
hp_classify <- hp_classify_xgb

tcheck(desc='run vga')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# ..[1] 0.55192029 0.08986413 "82.010000 elapsed for vga complete" (validation ) 
est_time <- ( 82. * grid_nx * grid_ny / chunk_size ) / 3600.  # 22.8 hrs

chunk_size = 10
grid_nx = 50
grid_ny = 50 ;tcheck(desc='vga start grid 50x50')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
#..[1] 0.501964 0.139210 [1] "67.580000 elapsed for vga complete"  (with known error in grid)
#..[1] 0.54046043 0.04439034 [1] "423.030000 elapsed for vga complete" (fixed error)
est_time <- ( 423 * grid_nx * grid_ny / chunk_size ) / 3600.  # 29.4 hrs

# compare to splitting grid into hectares
gs <- 100/grid_nx  #grid_scale
chunk2 <- chunk  %>% transmute( x=x*gs-1, y=y*gs-1 ); chunkx <- chunk2
chunk3 <- chunk2 %>% transmute( x=x+1, y=y      ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x   , y=y+1 ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+1   , y=y+1 ); chunkx <- rbind(chunkx, chunk3)

chunk_size=nrow(chunkx)
chunk = chunkx

for( i in 1:chunk_size) with(chunk[i, ], { rect(x/gs-1/gs, y/gs-1/gs, x/gs, y/gs, col="red") } )
grid_nx = grid_ny = 100
# run variable_grid_analysis1.R  lines 52:99 (was 55:93
# 0.51785481 0.09435137

chunk_size = 10
grid_nx = 25
grid_ny = 25; tcheck(desc='vga start grid 25x25')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# ..[1] 0.52924896 0.08412564  [1] "139.640000 elapsed for vga complete" (with known error...)
# ..[1] 0.52979404 0.02285734 [1] "7342.250000 elapsed for vga complete"
est_time <- ( 7342 * grid_nx * grid_ny / chunk_size ) / 3600.  # 127.5 hrs

gs <- 100/grid_nx  #grid_scale
chunk2 <- chunk  %>% transmute( x=x*gs-(gs-1), y=y*gs-(gs-1) ); chunkx <- chunk2
chunk3 <- chunk2 %>% transmute( x=x+1, y=y       ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+2, y=y       ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+3, y=y       ); chunkx <- rbind(chunkx, chunk3)

chunk3 <- chunk2 %>% transmute( x=x   , y=y+1 ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+1   , y=y+1 ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+2   , y=y+1 ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+3   , y=y+1 ); chunkx <- rbind(chunkx, chunk3)

chunk3 <- chunk2 %>% transmute( x=x   , y=y+2 ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+1   , y=y+2 ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+2   , y=y+2 ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+3   , y=y+2 ); chunkx <- rbind(chunkx, chunk3)

chunk3 <- chunk2 %>% transmute( x=x   , y=y+3 ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+1   , y=y+3 ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+2   , y=y+3 ); chunkx <- rbind(chunkx, chunk3)
chunk3 <- chunk2 %>% transmute( x=x+3   , y=y+3 ); chunkx <- rbind(chunkx, chunk3)

chunk_size=nrow(chunkx)
chunk = chunkx

for( i in 1:chunk_size) with(chunk[i, ], { rect(x/gs-1/gs, y/gs-1/gs, x/gs, y/gs, col="red") } )
grid_nx = grid_ny = 100
# run variable_grid_analysis1.R  lines 52:99 (was55:93
# 0.5186515 0.0974449

chunk_size = 10
grid_nx = 40
grid_ny = 40; tcheck(desc='vga start grid 40x40')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# ..[1] 0.51800689 0.03273349 [1] "976.340000 elapsed for vga complete"
est_time <- ( 976 * grid_nx * grid_ny / chunk_size ) / 3600.  # 43.4 hrs

chunk_size = 3
grid_nx = 40
grid_ny = 40; tcheck(desc='gross optimize xgb_nrounds')

scores <- data.frame()
for(nrnd in seq(45,65,5)) {
    xgb_nrounds <- nrnd
    source('variable_grid_analysis1.R')
    scores <- rbind(scores, data.frame( xgb_nrounds = xgb_nrounds,
                                        score = mean(chunk$score),
                                        time =  diff(tcheck.df$elapsed) %>% tail(1)))
}

#rectangles instead of squares
xgb_nrounds = 54
ichunk = 1
chunk_size = 10
grid_nx = 50
grid_ny = 100
tcheck(desc='vga start grid 50x100')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 
#0.53362387 0.08470319 [1] "162.680000 elapsed for vga complete"

# compare to splitting grid into hectares
gsx <- 100/grid_nx  #grid_scale
gsy <- 100/grid_ny  #grid_scale
chunk2 <- chunk  %>% transmute( x=x*gsx-1, y=y*gsy ); chunkx <- chunk2
chunk3 <- chunk2 %>% transmute( x=x+1, y=y      ); chunkx <- rbind(chunkx, chunk3)

chunk_size=nrow(chunkx)
chunk = chunkx

for( i in 1:chunk_size) with(chunk[i, ], { rect(x/gsx-1/gsx, y/gsy-1/gsy, x/gsx, y/gsy, col="red") } )
grid_nx = grid_ny = 100
# run variable_grid_analysis1.R  lines 52:99
# 0.5210167 0.1097545

chunk_size = 10
grid_nx = 100
grid_ny = 50
tcheck(desc='vga start grid 100x50')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 20.9 hrs
# 0.52544344 0.08698849 [1] "149.810000 elapsed for vga complete"

# compare to splitting grid into hectares
gsx <- 100/grid_nx  #grid_scale
gsy <- 100/grid_ny  #grid_scale
chunk2 <- chunk  %>% transmute( x=x*gsx, y=y*gsy-1 ); chunkx <- chunk2
chunk3 <- chunk2 %>% transmute( x=x, y=y+1      ); chunkx <- rbind(chunkx, chunk3)

chunk_size=nrow(chunkx)
chunk = chunkx

for( i in 1:chunk_size) with(chunk[i, ], { rect(x/gsx-1/gsx, y/gsy-1/gsy, x/gsx, y/gsy, col="red") } )
grid_nx = grid_ny = 100
# run variable_grid_analysis1.R  lines 58:99
# 0.5214679 0.1053201

######################  


xgb_fea_g2 <- function(dt) {
    
    dt[, c( "hour", "weekday", "mday", "month", "year", 
            "quarter_period_of_day", "rating_history"  ) := NULL]
    
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    
    dt <- dt[, c(.SD, .(
        min5_rad = 2 * (pi * floor(time/5) %% 288 ) / 288,
        yday_rad  = 2 * (pi * floor(time/1440) %% 365) / 365,
        wday_rad = 2 * (pi * floor(time/1440) %% 7) / 7,
        year = floor( time/(24*60*365))  ))][ ,c(.SD, .(
            min5_sin = sin( min5_rad ),
            min5_cos = cos( min5_rad ),
            yday_sin = sin( yday_rad ),
            yday_cos = cos( yday_rad ),
            wday_sin = sin( wday_rad ),
            wday_cos = cos( wday_rad )   ))][ 
                , c( "min5_rad", "yday_rad", "wday_rad") := NULL ]
    dt[, accuracy := log10(accuracy)]
    
    return(dt)
}

train <- xgb_fea_g2(train)
test <-  xgb_fea_g2(test)

chunk_size = 10
grid_nx = 50
grid_ny = 50
hp_classify <- hp_classify_xgb

tcheck(desc='vga start grid 50x50 newg2')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 26 hr
# 0.53423168 0.05717505 [1] "392.740000 elapsed

skip_xgb_weight_calc <- TRUE
create_features <- create_features_safe
fea_names <- names( create_features(train[1:10]) %>% select( -c(row_id, place_id)))

if (! skip_xgb_weight_calc) {
    #use the importances from xgb
    hp_classify <- hp_classify_xgb_imp
    xgb_importance <- list()
    
    tcheck(desc='vga start grid 50x50 xgb importance')
    source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
    # 0.52251821 0.04142637
    
    impdf <- data.frame()
    for (i in 1:length(xgb_importance)) impdf <- rbind(impdf, xgb_importance[[i]] %>% mutate(cell=i))
    
    imp_order <- impdf %>% group_by(Feature) %>% summarize(Gain = mean(Gain)) %>% arrange(Gain) %>% .[[1]]
    impdf %>% 
        ggplot(aes(Feature, Gain)) + 
        geom_bar( data=impdf %>% mutate(Gain=Gain/length(xgb_importance)), stat="identity") +
        geom_bar( stat="identity", position="dodge", aes(fill=as.factor(cell))) + 
        scale_x_discrete(limits = imp_order) +
        coord_flip() 
    
    avg_gain <- impdf %>% group_by(Feature) %>% summarize(Gain=mean(Gain))
    # Feature        Gain
    # (chr)       (dbl)
#     1           y 0.439015591
#     2           x 0.220155413
#     3    min5_cos 0.066788562
#     4    min5_sin 0.073974979
#     5    accuracy 0.042271521
#     6    yday_cos 0.039280065
#     7    yday_sin 0.037667020
#     8    wday_sin 0.022556942
#     9   time_diff 0.013269391
#     10 rat_hr_chg 0.010621939
#     11   g_hr_chg 0.011340718
#     12   wday_cos 0.008482786
#     13       year 0.010149469
#     14  n_this_hr 0.004425603
    
    sorted_weights <- avg_gain$Gain
    names(sorted_weights) <- avg_gain$Feature
    knn_weights_xgb <- rep(.0001, length(fea_names))
    names(knn_weights_xgb) <- as.character(fea_names)
    for (i in 1:length(fea_names))  knn_weights_xgb[i] <- round(sorted_weights[ fea_names[i] ] , 6)
    
} else {
    knn_weights_xgb <- c(0.220155, 0.439016, 0.042272, 0.011341, 0.010149, 0.073975, 0.066789, 
                         0.037667, 0.03928, 0.022557, 0.008483, 0.004426, 0.013269, 0.010622)
    names(knn_weights_xgb) <- as.character(fea_names)
}

knn_weights <- knn_weights_xgb
hp_classify <- hp_classify_knn_RANN
knn_k = 30
expand_margin = .005
knn_norm = TRUE
knn_probs = FALSE
tcheck(desc='vga start grid 50x50 knn g2 xgb weights k30')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53501859 0.04569957 [1] "139.610000 elapsed
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 

# knn_k = 20
# tcheck(desc='vga start grid 50x50 knn g2 xgb weights k 20')
# source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# # 0.53523070 0.04553918 [1] "132.020000 elapsed
# 
# knn_k = 0
# tcheck(desc='vga start grid 50x50 knn g2 xgb weights cte=5.6')
# source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# # 0.5329532 0.0458874 [1] "132.730000 elapsed
# 
# knn_cte = 3
# tcheck(desc='vga start grid 50x50 knn g2 xgb weights cte=3')
# source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# # 0.53444747 0.04597214 [1] "134.560000 elapsed
# 
# knn_cte = 2.5
# tcheck(desc='vga start grid 50x50 knn g2 xgb weights cte=2.5')
# source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# # 0.53310436 0.04718606 [1] "131.550000 elapsed
# 
# knn_cte = 3.5
# tcheck(desc='vga start grid 50x50 knn g2 xgb weights cte=3.5')
# source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 

train <- train[ , c(.SD, .N), by=place_id ][ N > 5][ , N := NULL ]
min_occ = 8
tcheck(desc='vga start grid 50x50 knn g2 xgb weights th_opt')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53563752 0.04511383
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 17hr

## full data
#train <- fread('../input/train.csv', integer64='character') %>% global_features() tcheck(desc='build train w global features')
load("../data/train_w_global.RData")
test <- fread('../input/test.csv', integer64='character') %>% global_features()
train <- xgb_fea_g2(train)
test <-  xgb_fea_g2(test)

train <- train[ , c(.SD, .N), by=place_id ][ N > 5][ , N := NULL ]
min_occ = 8

chunk_size <- grid_ny * grid_nx
tcheck(desc='vga start')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')

## there could be duplicate row_id's so they need to be resolved
## method below uses the row with the lowest X1 (probability) since  
## in most cases place_id1 is identical and a lower X1 probability
## results in a higher X2, X3 probability
setkey(hp_results, row_id, X1)
hp_results <- hp_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
result <- hp_results[,.(row_id, place_id=predictions)]

blanks <- setdiff( test$row_id, hp_results$row_id) 
result <- rbind(result, data.frame( row_id=blanks, place_id = ""))
if (! exists("submit_name")) submit_name <- 
    sprintf("../submissions/SG_vga1_50x50_KNN.csv", ichunk, format(Sys.time(), "%Y_%m_%d_%H%M%S"))
cat('writing', submit_name, EOL)
write.csv(result, file=submit_name, row.names=FALSE) #

tcheck(desc='complete')

