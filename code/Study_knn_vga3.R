# deviate from ...vga1&2 : throw everything at it

if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')
tcheck(0)

g3_features <- function(dt) {
    
    dt[, c( "hour", "month", "year", 
            "quarter_period_of_day", "rating_history" ) := NULL]
    
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    
    dt <- dt[, c(.SD, .(
        min5_rad = 2 * (pi * floor(time/5) %% 288 ) / 288,
        yday_rad  = 2 * (pi * floor(time/1440) %% 365) / 365,
        wday_rad = 2 * (pi * floor(time/1440) %% 7) / 7 ))][ ,c(.SD, .(
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

## load data  (allow for pre-loaded subsets)
if (! exists("train")) {
    train <- fread('../input/train.csv', integer64='character') 
    train <- global_features(train)
    #load("../data/train_w_global.RData")
    train <- g3_features(train); tcheck(desc='build train w global features')
}

setorder(train, time)
train_cut_ix <- as.integer(nrow(train) * .80)
test <- train[(train_cut_ix+1):nrow(train), ]
train <- train[1:train_cut_ix, ]

# copy function locally in order to set importance = TRUE
xgb_importance <- list()
hp_classify_xgb_imp <- function(trn, val, min_occ=2, verbose=0, importance = TRUE) {
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
    
    #massively increases run time 
    if (importance) {
        cat ('capturing xgb_importance a (be patient)... ', EOL)
        xgb_importance <<- c(xgb_importance, list(xgb.importance( colnames(xx), model=model )))
    }
    
    top3 <- predict( model, xgb.DMatrix(val %>% select(-c(row_id, place_id)) %>% as.matrix() )) %>%
        top3_preds( levels(trn$place_id) ) 
    preds <- val %>% select( row_id, truth=place_id ) %>% 
        bind_cols( data.frame(predictions = apply(top3[,1:3], 1, paste, collapse=" "),
                              top3[4:6]) )
    
    return(preds)
}

knn_norm = TRUE
knn_k = 25
knn_probs <- FALSE

hp_classify <- hp_classify_knn_RANN

grid_nx = 50
grid_ny = 50
ichunk = 1
chunk_size = 10
tbound_lower = 0
expand_margin = .005

#now something more scientific:
skip_xgb_weight_calc <- TRUE
fea_names <- names( create_features(train[1:10]) %>% select( -c(row_id, place_id)))

if (! skip_xgb_weight_calc) {
    #use the importances from xgb
    hp_classify <- hp_classify_xgb_imp
    xgb_importance <- list()
    
    tcheck(desc='vga start grid 50x50 xgb importance')
    source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
    
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
#     1           y 0.438449922
#     2           x 0.220056783
#     3    min5_cos 0.066338591
#     4    min5_sin 0.073741291
#     5    accuracy 0.041734677
#     6    yday_cos 0.040526546
#     7    yday_sin 0.036155550
#     8    g_hr_chg 0.012086119
#     9   time_diff 0.012777505
#     10 rat_hr_chg 0.010643747
#     11    weekday 0.011588220
#     12   wday_sin 0.014311321
#     13       mday 0.010204182
#     14   wday_cos 0.007062294
#     15  n_this_hr 0.004323252    
    sorted_weights <- avg_gain$Gain
    names(sorted_weights) <- avg_gain$Feature
    knn_weights_xgb <- rep(.0001, length(fea_names))
    names(knn_weights_xgb) <- as.character(fea_names)
    for (i in 1:length(fea_names))  knn_weights_xgb[i] <- sorted_weights[ fea_names[i] ] 
    
} else {
    #paste(round(knn_weights_xgb,6), collapse=", ")
    knn_weights_xgb <- c(0.220057, 0.43845, 0.041735, 0.011588, 0.010204, 0.012086, 0.073741, 0.066339,
                         0.036156, 0.040527, 0.014311, 0.007062, 0.004323, 0.012778, 0.010644)
    names(knn_weights_xgb) <- as.character(fea_names)
}
hp_classify <- hp_classify_knn_RANN
knn_weights <- knn_weights_xgb

tcheck(desc='vga start grid 50x50 knn xgb weights')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53347775 0.04614819 [1] "127.200000 elapsed

#curious what happens if I remove <= 5 occurances from the global set
train <- train[ , c(.SD, .N), by=place_id ][ N > 5][ , N := NULL ]
tcheck(desc='vga start grid 50x50 knn xgb weights - reduced set')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53352418 0.04615007 [1] "135.490000 elapsed

min_occ = 8
tcheck(desc='vga start grid 50x50 knn xgb weights - reduced set')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53440516 0.04543136 [1] "130.420000 elapsed
 
min_occ = 5
tcheck(desc='vga start grid 50x50 knn xgb weights - reduced set')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53454820 0.04463414 [1] "131.070000 elapsed