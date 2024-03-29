if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))
stopifnot (exists ('hp_classify'))

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(xgboost)
library(lubridate)

source('fb_checkins_util.R')

## load data  (allow for pre-loaded subsets)
if (! exists("train")) {
    #train <- fread('../input/train.csv', integer64='character') %>% global_features() tcheck(desc='build train w global features')
    load("~/GitHub/kaggle/Facebook_Checkins/data/train_w_global.RData")
    train$g_n_this_hr <- NULL
    train$g_n_this_day <- NULL
    train$g_n_this_15m <- NULL
}
if (! exists("test")) {
    test <- fread('../input/test.csv', integer64='character') %>% global_features(); tcheck(desc='build test w global features')
}

if (! exists("xgb_params")) xgb_params <- list( 
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

#rm(ichunk, chunk_size, grid_nx, grid_ny)
if (! exists("ichunk")) ichunk = 1
if (! exists("chunk_size")) chunk_size = 10
if (! exists("grid_nx")) grid_nx = 50
if (! exists("grid_ny")) grid_ny = 50
if (! exists("min_occ")) min_occ = 10
if (! exists("expand_margin")) expand_margin = 0.00 
if (! exists("tbound_lower")) tbound_lower = 0   
if (! exists("skip_x")) skip_x = 0   

set.seed(48)
h_scramble <- expand.grid( x=1:grid_nx, y=1:grid_ny) %>% sample_frac(size=1)

chunk <- h_scramble[ ((ichunk-1) * chunk_size + 1):(ichunk * chunk_size),]

gx_scale <- grid_nx / 10.
gy_scale <- grid_ny / 10.

plot(c(0,grid_nx), c(0,grid_ny), type="n")
for( i in 1:chunk_size) with(chunk[i, ], { rect(x-1, y-1, x, y) } )

hp_results <- data.table()
ih <- ncum <- 0

grpx <- chunk %>% count(x)
chunk <- chunk %>% arrange (x)
chunk$score <- NA

cat( "ichunk, chunk_size, grid_nx, grid_ny:", ichunk, chunk_size, grid_nx, grid_ny, EOL)
t0 <- proc.time()
i=j=1
for (i in 1:nrow(grpx)) {
    xmin <- (chunk[ih+1, ]$x - 1) / gx_scale
    xmax <- xmin + 1./gx_scale
    xeps <- expand_margin * ( xmax - xmin )
    trnx <- train %>% filter( x >= xmin - xeps, x <= xmax + xeps,
                              time >= as.integer(tbound_lower * max(train$time)) )
    tstx <- test %>% filter( x >= xmin, x <= xmax )
    
    ny <- grpx[i, ]$n
    ncum <- ncum + ny
    for(j in 1:ny){
        grid <- data.frame( grid_x = chunk[ih+1, ]$x,  grid_y = chunk[ih+1, ]$y)
        ih <- ih + 1
        
        if (i <= skip_x ) {
            with(chunk[ih, ], { rect(x-1, y-1, x, y, col="yellow") } )
            next
        }
    
        tx <- proc.time()
        if(ih %% 5 == 0) cat('.') 
        if(ih %% 100 == 0) cat( sprintf('  elapsed=%f\n', (tx-t0)[3]))

        ymin <- (chunk[ih, ]$y - 1) / gy_scale
        ymax <- ymin + 1./gy_scale
        yeps <- expand_margin * ( ymax - ymin )
        with(chunk[ih, ], { rect(x-1, y-1, x, y, col="green") } )
        trn <- trnx %>% filter( y >= ymin - yeps, y <= ymin + 1./gy_scale )
        tst <- tstx %>% filter( y >= ymin, y <= ymax )
        
        ## xgb classifier
        if (! exists("xgb_nrounds")) xgb_nrounds <- 50 #   
        if (! exists("verbose")) verbose <- 0 #   
        preds <- hp_classify( trn, tst, min_occ = min_occ, verbose=verbose) %>% data.table()
        #xgb_preds$truth <- NULL
 
        hp_results <- rbind( hp_results, cbind(preds, grid) )
        
        if (test$place_id[1] != "TBD") chunk[ih,"score"] <- calculate_map_score( preds )
    }
}
cat (sprintf("...total elapsed = %f\n", (proc.time()-t0)[3]))
if(test$place_id[1] != "TBD") print(c( mean(chunk$score), sd(chunk$score)))
    



    

