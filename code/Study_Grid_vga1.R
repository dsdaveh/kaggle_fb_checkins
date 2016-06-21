if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')
tcheck(0)

## load data  (allow for pre-loaded subsets)
if (! exists("train")) {
    #train <- fread('../input/train.csv', integer64='character') %>% global_features() tcheck(desc='build train w global features')
    load("~/GitHub/kaggle/Facebook_Checkins/data/train_w_global.RData")
}

train.orig <- train
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
# run variable_grid_analysis1.R  lines 55:93
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
# run variable_grid_analysis1.R  lines 55:93
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

