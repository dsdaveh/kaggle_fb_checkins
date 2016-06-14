ichunk=1
chunk_size = 10
hp_classify <- hp_classify_xgb
source('hectare_analysis3.R')
# ..[1] 0.54594484 0.08941181  elapsed 67.39 

global_features <- function(dt) {
    fea <- dt[ ,.(row_id, x, y, accuracy, time, place_id,
                  abs_day = as.integer(floor(time/(60*24))), 
                  abs_hr = as.integer(floor(time/60)), 
                  abs_15m = as.integer(floor(time/15)) ) ]
    
    n_this_day <- fea[, .(g_n_this_day = .N), abs_day]
    n_this_15m <- fea[, .(g_n_this_15m = .N), abs_15m]
    n_this_hr <-  fea[, .(g_n_this_hr = .N), abs_hr]
    n_this_hr$g_hr_chg <- c(0, diff( n_this_hr$g_n_this_hr)) / n_this_hr$g_n_this_hr
    hr_change <- 
    
    setkey(fea, abs_day); setkey(n_this_day, abs_day); fea <- fea[n_this_day]
    setkey(fea, abs_hr); setkey(n_this_hr, abs_hr); fea <- fea[n_this_hr]
    setkey(fea, abs_15m); setkey(n_this_15m, abs_15m); fea <- fea[n_this_15m]
    
    setkey(fea, time)
    fea$abs_day <- NULL
    fea$abs_hr <- NULL
    fea$abs_15m <- NULL
    
    return(fea)
}

create_features <- function(dt) {
    
    time0 <- ymd_hms('2014-01-01 00:00:00')
    dt$ltime <- dt[, time0 + minutes(time) ]
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    
    wt <- c(1, 1, 4, 3, 1./22, 2, 10, 1, 1)
    wt <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
    fea <- dt[,
       .(row_id,
         x, y, accuracy, time,
         abs_day = as.integer(floor(time/(60*24))), 
         abs_hr = as.integer(floor(time/60)), 
         abs_15m = as.integer(floor(time/15)),
         hour = hour(ltime) *wt[3]  ,
         weekday = wday(ltime) *wt[4],
         mday = day(ltime) *wt[5],
         month = month(ltime) *wt[6],
         year = (year(ltime) - 2013) *wt[7],
         quarter_period_of_day = as.integer(hour(ltime) / 6) *wt[8],
         rating_history= log10(3+month(ltime)) *wt[9],
         place_id) ]

    n_this_day <- fea[, .(n_this_day = .N), abs_day]
    n_this_hr <- fea[, .(n_this_hr = .N), abs_hr]
    n_this_15m <- fea[, .(n_this_15m = .N), abs_15m]
    
    setkey(fea, abs_day); setkey(n_this_day, abs_day); fea <- fea[n_this_day]
    setkey(fea, abs_hr); setkey(n_this_hr, abs_hr); fea <- fea[n_this_hr]
    setkey(fea, abs_15m); setkey(n_this_15m, abs_15m); fea <- fea[n_this_15m]
    
    setkey(fea, time)
    fea$time_diff <- c(0, diff(fea$time))
    fea$time <- NULL
    fea$abs_day <- NULL
    fea$abs_hr <- NULL
    fea$abs_15m <- NULL
    
    return(fea)
}

source('hectare_analysis3.R')
# ..[1] 0.53679427 0.08655373 elapsed 76.71 

chunk_size <- 10
imp <- list()
hp_classify <- function(trn, val, min_occ=2, verbose=0) {
    trn <- create_features(trn)
    val <- create_features(val)
    
    places <- trn %>% count(place_id, sort=TRUE) %>% filter(n >= min_occ) %>% .[[1]]
    trn <- trn %>% filter(place_id %in% places)
    trn$place_id <- as.factor(trn$place_id)
    xgb_params$num_class <- length(places)
    
    xx = trn %>% select(-c(row_id, place_id)) %>% as.matrix()
    yy = as.integer( trn$place_id ) - 1
    
    xgb.train <- xgb.DMatrix(xx, label = yy)
    model <- xgboost( xgb.train,
                      nrounds = xgb_nrounds,
                      params = xgb_params, verbose = verbose )
    pred <- predict( model, val %>% select(-c(row_id, place_id)) %>% as.matrix() )
    
    #dev only
    #imp <<- c(imp, list(xgb.importance( colnames(xx), model=model )))
    
    top3 <- predict( model, xgb.DMatrix(val %>% select(-c(row_id, place_id)) %>% as.matrix() )) %>%
        top3_preds( levels(trn$place_id) ) 
    preds <- val %>% select( row_id, truth=place_id ) %>% 
        bind_cols( data.frame(predictions = apply(top3[,1:3], 1, paste, collapse=" "),
                              top3[4:6]) )
    
    return(preds)
}

source('hectare_analysis3.R')

# ..[1] 0.53279591 0.06987748  elapsed 268.83
# 
xgb_params <- list( 
    #     eta = 0.01,      #
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

cv <- xgb.cv( xgb.train,
                  nrounds = 1000, early.stop.round = 15, nfold=5, 
                  params = xgb_params, verbose = verbose )
xgb_nrounds = 38

source('hectare_analysis3.R')
# .[1] 0.53119636 0.06969433
# > chunk
# x  y     score
# 1 28  1 0.6137410
# 2 32 25 0.4406728
# 3 39  4 0.4964052
# 4 73 38 0.5864636
# 5 92 58 0.5186992

xgb_params$eta=0.1
xgb_nrounds = 63
# [1] 0.53927314 0.07437021
# > chunk
# x  y     score
# 1 28  1 0.6226167
# 2 32 25 0.4425076
# 3 39  4 0.5098039
# 4 73 38 0.6055838
# 5 92 58 0.5158537

xgb_params$eta=0.2
xgb_nrounds = 21
# [1] 0.53148441 0.06987854

xgb_params$eta=0.1
xgb_nrounds = 63
# [1] 0.54942476 0.06345821
# 1 28  1 0.6226167
# 2 39  4 0.5098039
# 3 92 58 0.5158537

chunk_size = 10
#[1] 0.54032516 0.08919399

# fea$abs_day <- NULL
# fea$abs_15m <- NULL
# ..[1] 0.54282665 0.08922342 
# fea$abs_hr <- NULL
# ..[1] 0.55266318 0.09179917 elapsed 69.53 

#eta = default (0.3 ?)
# ..[1] 0.55058580 0.09160343

train <- global_features(df_train)
#..[1] 0.55268049 0.09224294 elapsed 72.03 