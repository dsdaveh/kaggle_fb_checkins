rm(ichunk)
source('hectare_analysis2b.R')

ichunk=1
chunk_size = 10
#hp_classify <- hp_classify_xgb
source('hectare_analysis3.R')
# ..[1] 0.54594484 0.08941181  elapsed 67.39 

global_features <- function(dt) {
    fea <- dt[ , c( .SD, .(
                  abs_day = as.integer(floor(time/(60*24))), 
                  abs_hr = as.integer(floor(time/60)), 
                  abs_15m = as.integer(floor(time/15)) )) ]
    
    n_this_day <- fea[, .(g_n_this_day = .N), abs_day]
    n_this_15m <- fea[, .(g_n_this_15m = .N), abs_15m]
    n_this_hr <-  fea[, .(g_n_this_hr = .N), abs_hr]
    n_this_hr$g_hr_chg <- c(0, diff( n_this_hr$g_n_this_hr)) / n_this_hr$g_n_this_hr

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
       c(.SD, .(
         abs_day = as.integer(floor(time/(60*24))), 
         abs_hr = as.integer(floor(time/60)), 
         abs_15m = as.integer(floor(time/15)),
         hour = hour(ltime) *wt[3]  ,
         weekday = wday(ltime) *wt[4],
         mday = day(ltime) *wt[5],
         month = month(ltime) *wt[6],
         year = (year(ltime) - 2013) *wt[7],
         quarter_period_of_day = as.integer(hour(ltime) / 6) *wt[8],
         rating_history= log10(3+month(ltime)) *wt[9] )) ]

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
    fea$ltime <- NULL
    
    #fea$rat_n_day <- fea$n_this_day / fea$g_n_this_day 
    #fea$rat_n_hr <- fea$n_this_hr / fea$g_n_this_hr 
    #fea$rat_n_15m <- fea$n_this_15m / fea$g_n_this_15m
    fea$rat_hr_chg <- fea$n_this_hr * fea$g_hr_chg
    
    return(fea)
}

source('hectare_analysis3.R')
# ..[1] 0.53679427 0.08655373 elapsed 76.71 

chunk_size <- 10
imp <- list()
hp_classify_imp <- function(trn, val, min_occ=2, verbose=0) {
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
    imp <<- c(imp, list(xgb.importance( colnames(xx), model=model )))
    
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
#..[1] 0.55268049 0.09224294 elapsed 72.03  !!!!!!!!! can't reproduce this
#..[1] 0.54506876 0.08662349 elapsed 110.36 

load('../data/train_w_global.RData')
train$g_n_this_day <- NULL
train$g_n_this_hr <- NULL
train$g_n_this_15m <- NULL
source('hectare_analysis3.R')
# ..[1] 0.55320273 0.09266388

chunk_history <- data.frame()
for (ieta in c(.1, 0.07, .13)) {
    xgb_params$eta = ieta
    for(jz in 1:50) {
        xgb_nrounds <- 50 + jz*2 
        load('../data/train_w_global.RData')
        train$g_n_this_day <- NULL
        train$g_n_this_hr <- NULL
        train$g_n_this_15m <- NULL
        source('hectare_analysis3.R')
        chunk_scores <- cbind(chunk, data.frame( eta = ieta, xgb_nrounds = xgb_nrounds))
        print(chunk_scores)
        chunk_history <- rbind(chunk_history, chunk_scores)
    }
}

chunk_history <- chunk_history %>% bind_cols( data.frame(chunk= rep(1:10, 50)))
write.csv(chunk_history, '../data/Study_new_features_chunk_history.csv', row.names = F)
chunk_history %>% filter(eta==.1) %>% group_by(chunk) %>%
    mutate(score.norm = normalize(score)) %>% 
    ggplot(aes(x=xgb_nrounds, y=score, group=chunk, color=as.factor(chunk))) + geom_line()

#6/14
load('../data/train_w_global.RData')
train$g_n_this_day <- NULL
train$g_n_this_hr <- NULL
train$g_n_this_15m <- NULL
train_xx <- train
xgb_nrounds = 54
source('hectare_analysis3.R')
# ..[1] 0.55012904 0.09434786
train <- train_xx
xgb_params$eta = 0.1
# ..[1] 0.55473793 0.09173208
train <- train_xx
xgb_params$eta = 0.3
# ..[1] 0.55012904 0.09434786  ### confirming the default. 
load('../data/train_w_global.RData')

### with the other 3 g_n... features
train_xx <- train
xgb_params$eta = 0.1 
#elapsed 85.64 
#..[1] 0.54386388 0.08707054   ## expected this drop (the point of todays exercise is restore with tuning)
train <- train_xx

## add_rat_hr_chg
# ..[1] 0.55297889 0.09148562 elapsed 82.24 <<<<<<  BEST !!

## add  rat_n_day     rat_n_hr   rat_n_15m   
# ..[1] 0.54297402 0.08907978    ### only a mild drop, so these additional vars could help
# elapsed 94.78 

chunk_history <- data.frame()
#for (ieta in c(.1, 0.07, .13)) {
#    xgb_params$eta = ieta
xgb_params$eta = 0.07 # .08, 0.1
xgb_params$max_depth = 8   #6 default=6 

    for(jz in seq(190,230,20)) {
        xgb_nrounds <- jz
        train <- train_xx
        source('hectare_analysis3.R')
        chunk_scores <- cbind(chunk, data.frame( eta = xgb_params$eta,
                                                 xgb_nrounds = xgb_nrounds,
                                                 max_depth = xgb_params$max_depth,
                                                 chunk = 1:chunk_size))
        print(chunk_scores)
        chunk_history <- rbind(chunk_history, chunk_scores)
    }
#}
chunk_history %>% group_by(eta, xgb_nrounds) %>%
    summarize (score = mean(score)) %>% 
    ggplot(aes(x=xgb_nrounds, y=score, group=eta, color=as.factor(eta))) + geom_line()

#write.csv(chunk_history, file="../Study_new_features_chunk_history2.csv", row.names = FALSE)
# chunk_history <- read.csv(file="../data/Study_new_features_chunk_history2.csv") %>% tbl_df

#with importance
hp_classify <- hp_classify_imp
impdf <- data.frame()
for (i in 1:length(imp)) impdf <- rbind(impdf, imp[[i]] %>% mutate(cell=i))

imp_order <- impdf %>% group_by(Feature) %>% summarize(Gain = mean(Gain)) %>% arrange(Gain) %>% .[[1]]
impdf %>% 
    ggplot(aes(Feature, Gain)) + 
    geom_bar( data=impdf %>% mutate(Gain=Gain/length(imp)), stat="identity") +
    geom_bar( stat="identity", position="dodge", aes(fill=as.factor(cell))) + 
    scale_x_discrete(limits = imp_order) +
    coord_flip() 
hp_classify <- hp_classify_xgb
    
