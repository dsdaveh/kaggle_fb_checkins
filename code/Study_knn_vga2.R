# deviate from ...vga1 : use vars from best py script

if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')
tcheck(0)

g2_features <- function(dt) {
    
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

## load data  (allow for pre-loaded subsets)
if (! exists("train")) {
    train <- fread('../input/train.csv', integer64='character') %>% g2_features(); tcheck(desc='build train w global features')
    #load("../data/train_w_global.RData")
}

setorder(train, time)
train_cut_ix <- as.integer(nrow(train) * .80)
test <- train[(train_cut_ix+1):nrow(train), ]
train <- train[1:train_cut_ix, ]

top3_knn <- function( nn.index.row, place_ids, prob=FALSE ) {
    top3_places <- data.frame( place_id = place_ids[ nn.index.row ]) %>% 
        count(place_id, sort=TRUE) %>% 
        mutate( prob = n/sum(n)) %>% 
        dplyr::slice(1:3)
    if (prob) {
        out <- top3_places$prob
    } else {
        out <- top3_places$place_id %>% as.character()
    }
    if ( length(out) < 3) {
        fill = ifelse( prob, 0, levels(place_ids)[1])
        out <- c(out, rep(fill, 3 - length(out)))
    }
    return(out)
}

knn_norm = FALSE
knn_weights = 1
knn_k = 25

library(RANN.L1)
knn_probs <- TRUE
hp_classify_knn_RANN <- function(trn, val, min_occ=2, verbose=0, norm=knn_norm, w=knn_weights) {
    # uses RANN.L1::nn (manhattan distance)
    #w is a constant or vector of length ncol(trn2)-2 to multiply features by
    trn2 <- create_features(trn)
    val2 <- create_features(val)
    
    places <- trn2 %>% count(place_id, sort=TRUE) %>% filter(n >= min_occ) %>% .[[1]]
    trn2 <- trn2 %>% filter(place_id %in% places)
    trn2.place_id <- as.factor(trn2$place_id)
    
    if (norm) {
        trn2 <- apply( trn2 %>% select(-c(row_id, place_id)), 2 , normalize)
        val2 <- apply( val2 %>% select(-c(row_id, place_id)), 2 , normalize)
        
        constants <- c( which( is.nan(trn2[1, ])), which( is.nan(val2[1, ])) ) %>% unique
        if (length(constants > 0)) {
            trn2 <- trn2[, -constants]
            val2 <- val2[, -constants]
            w    <- w   [  -constants]
        }
    } else {
        trn2 <- apply( trn2 %>% select(-c(row_id, place_id)), 2 , identity)
        val2 <- apply( val2 %>% select(-c(row_id, place_id)), 2 , identity)
    }
    
    cte <- 5.6
    k <- ifelse(knn_k > 0, knn_k, integer( sqrt(nrow(trn2)) / cte ) )
    cat('k=',k)
    
    #RANN.L1  (manhattan distances)
    nn <- nn2( t(t( trn2 ) * w),  #train
               t(t( val2 ) * w),  #test
               k = k )
    
    top3_places <- apply(nn$nn.idx, 1, top3_knn, trn2.place_id ) %>% t() %>% as.data.frame %>% tbl_df()
    
    preds <- val %>% select( row_id, truth=place_id)
    preds$predictions <- with(top3_places, paste(V1,V2,V3))
    
    if (knn_probs == TRUE) {
        top3_probs  <- apply(nn$nn.idx, 1, top3_knn, trn2.place_id, prob=TRUE ) %>% t() %>% as.data.frame %>% tbl_df()
        names(top3_probs) <- c('X1', 'X2', 'X3')
        preds <- cbind(preds, top3_probs)
    }
    
    return(preds)
}
hp_classify <- hp_classify_knn_RANN


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

create_features <- function(dt) if('time' %in% names(dt)) dt[ , time := NULL ]  #override


# Need to work on weighting before proceeding 
# gut feel based on ranges (  lapply( create_features(train), range)   )
fw = c(0.613,0.3235, 0.56535, 0.2671, 20, 50, 0.51786)

knn_weights <- c( 20, 50, 0.613, 0.51786, 0.56535, 0.56535, 0.3235, 0.3235, 0.2671, 0.2671)
knn_norm = TRUE
knn_k = 30
knn_probs = FALSE
grid_nx = 50
grid_ny = 50
ichunk = 1
chunk_size = 10
tbound_lower = 0
expand_margin = .005
tcheck(desc='vga start grid 50x50 knn weights from py')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.4925692 0.0479884 [1] "123.440000 elapsed 
# 0.49362224 0.04824496 [1] "131.150000 elapsed   #mystery (unreproducible)
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 9 hr
# 

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
    # 1         y 0.447210333
    # 2         x 0.225823935
    # 3  min5_cos 0.070690576
    # 4  min5_sin 0.077711951
    # 5  accuracy 0.046762824
    # 6  yday_cos 0.043910566
    # 7  yday_sin 0.043362221
    # 8  wday_sin 0.024558173
    # 9      year 0.011004490
    # 10 wday_cos 0.008964929
    
    sorted_weights <- avg_gain$Gain
    names(sorted_weights) <- avg_gain$Feature
    knn_weights_xgb <- rep(.0001, length(fea_names))
    names(knn_weights_xgb) <- as.character(fea_names)
    for (i in 1:length(fea_names))  knn_weights_xgb[i] <- sorted_weights[ fea_names[i] ] 
    
} else {
    
    knn_weights_xgb <- c(0.225823935, 0.447210333, 0.046762824, 0.011004490, 0.077711951, 0.070690576, 
                         0.043362221, 0.043910566, 0.024558173, 0.008964929)
    names(knn_weights_xgb) <- as.character(fea_names)
}
hp_classify <- hp_classify_knn_RANN
knn_weights <- knn_weights_xgb

tcheck(desc='vga start grid 50x50 knn xgb weights')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.53447783 0.04625566 [1] "126.890000 elapsed

