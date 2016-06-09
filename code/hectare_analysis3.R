# rm(ichunk)
# source('hectare_analysis2b.R')

if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(xgboost)
library(lubridate)

source('fb_checkins_util.R')

create_features <- function(dt) {
    
    time0 <- ymd_hms('2014-01-01 00:00:00')
    dt[, time:= time0 + minutes(time) ]
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    
    dt[,
       .(row_id,
         x, y, accuracy,
          hour = hour(time) *4,
          weekday = wday(time) *3,
          mday = day(time) *1./22.,
          month = month(time) *2,
          year = (year(time) - 2013) *10,
          quarter_period_of_day = as.integer(hour(time) / 6),
          rating_history= log10(3+month(time)),
          place_id),
       ]
}

xgb_params <- list( 
    #     eta = 0.01,      #
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
    verbose = 0
)

if (! exists("df_train")) df_train <- fread('../input/train.csv', integer64='character')
#if (! exists("df_test")) df_test <- fread('../input/test.csv', integer64='character')

ichunk = 1
set.seed(48)
h_scramble <- expand.grid( x=1:100, y=1:100) %>% sample_frac(size=1)
chunk_size = 100
chunk <- h_scramble[ ((ichunk-1) * chunk_size + 1):(ichunk * chunk_size),]

plot(c(0,100), c(0,100), type="n")
for( i in 1:chunk_size) with(chunk[i, ], { rect(x-1, y-1, x, y) } )

# method 1: split based on time
train <-df_train

setorder(train, time)
train_cut_ix <- as.integer(nrow(train) * .80)
test <- train[(train_cut_ix+1):nrow(train), ]
train <- train[1:train_cut_ix, ]

t0 <- proc.time()

xgb_results <- data.table()
ih <- ncum <- 0

grpx <- chunk %>% count(x)
chunk <- chunk %>% arrange (x)
chunk$score <- NA

i=j=1
for (i in 1:nrow(grpx)) {
    xmin <- (chunk[ih+1, ]$x - 1) / 10.
    trnx <- train %>% filter( x >= xmin, x <= xmin + 0.1 )
    tstx <- test %>% filter( x >= xmin, x <= xmin + 0.1 )
    
    ny <- grpx[i, ]$n
    ncum <- ncum + ny
    for(j in 1:ny){
        ih <- ih + 1
        
        tx <- proc.time()
        if(ih %% 5 == 0) cat('.') 
        if(ih %% 100 == 0) cat( sprintf('  elapsed=%f\n', (tx-t0)[3]))

        ymin <- (chunk[ih, ]$y - 1) / 10.
        trn <- trnx %>% filter( y >= ymin, y <= ymin + 0.1 )
        tst <- tstx %>% filter( y >= ymin, y <= ymin + 0.1 )
        
        ## xgb classifier
        xgb_nrounds <- 50 #   
        xgb_preds <- hp_classify( trn, tst, min_occ = 10, verbose=0) %>% data.table()
        #xgb_preds$truth <- NULL
        
        chunk[ih,"score"] <- calculate_map_score( xgb_preds )
        xgb_results <- rbind( xgb_results, xgb_preds) 
    }
}
print(c( mean(chunk$score), sd(chunk$score)))
    
t1 <- proc.time()
print((t1-t0)[3])




