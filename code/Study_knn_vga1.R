if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')
tcheck(0)

## load data  (allow for pre-loaded subsets)
if (! exists("train")) {
    #train <- fread('../input/train.csv', integer64='character') %>% global_features() tcheck(desc='build train w global features')
    load("~/GitHub/kaggle/Facebook_Checkins/data/train_w_global.RData")
}

train[ , hour_sin := sin(hour * pi/24) ]
train[ , hour_cos := cos(hour * pi/24) ]
train[ , hour_sincos := hour_sin * hour_cos ]
train[ , accuracy := log10(accuracy)]

setorder(train, time)
train_cut_ix <- as.integer(nrow(train) * .80)
test <- train[(train_cut_ix+1):nrow(train), ]
train <- train[1:train_cut_ix, ]

library(class)
library(FNN)

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
hp_classify_knn <- function(trn, val, min_occ=2, verbose=0, norm=knn_norm, w=knn_weights) {
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
    
    knn_fit <- knn( t(t( trn2 ) * w),  #train
                    t(t( val2 ) * w),  #test
                    trn2.place_id, #cl
                    k = knn_k )
    top3_places <- apply(attr(knn_fit,'nn.index'), 1, top3_knn, trn2.place_id ) %>% t() %>% as.data.frame %>% tbl_df()
    top3_probs  <- apply(attr(knn_fit,'nn.index'), 1, top3_knn, trn2.place_id, prob=TRUE ) %>% t() %>% as.data.frame %>% tbl_df()
    
    preds <- val %>% select( row_id, truth=place_id)
    preds$predictions <- with(top3_places, paste(V1,V2,V3))
    names(top3_probs) <- c('X1', 'X2', 'X3')
    preds <- cbind(preds, top3_probs)
    
    #     #row <- 1
    #     print(preds[row, ])
    #     calculate_map_score( preds[row, ])
    #     with(trn2, plot(x, y, type="n"))
    #     with(val2[row,], points(x, y, pch=16))
    #     mask <- trn2 %>% filter( place_id %in% unlist( top3_places[row,1:3] ) )
    #     mask$color <- as.integer(as.factor(as.character(mask$place_id))) +2
    #     with(mask, points(x, y, col=color))
    #     with(trn[place_id == preds[row,]$truth ], points(x, y, col="red", pch=3))
    #     
    #     neighbors <- trn2[ attr(knn_fit,'nn.index')[row, ]]
    #     neighborsplace_id <- lapply(neighbors$place_id,as.character) %>% unlist()
    #     
    #     neighbors$color <- as.integer(as.factor(as.character(neighbors$place_id))) +2
    #     with(trn2, plot(x, y, type="n"))
    #     with(val2[row,], points(x, y, pch=16))
    #     with(neighbors, points(x, y, col=color))
    #     #neighbors_top3 <- neighbors %>% filter( place_id %in% unlist( top3_places[row,1:3] ) )
    #     neighbors_top3$place_id <- lapply(neighbors_top3$place_id,as.character) %>% unlist()
    #     neighbors_top3$nn_rank <- ifelse(neighbors_top3$place_id == as.character(top3_places[row, ]$V1), 1,
    #                                      ifelse(neighbors_top3$place_id == as.character(top3_places[row, ]$V2), 2, 3))
    #     with(neighbors_top3, text(x, y, labels=nn_rank, col=color))
    #     
    #     neighbors$nn_rank <- ifelse(neighbors$place_id == as.character(top3_places[row, ]$V1), 1,
    #                                      ifelse(neighbors$place_id == as.character(top3_places[row, ]$V2), 2,
    #                                             ifelse(neighbors$place_id == as.character(top3_places[row, ]$V3), 3, 0)))
    #     
    #     with(neighbors, text(x,y,labels=nn_rank, col=color, pos=1))
    
    return(preds)
}
hp_classify <- hp_classify_knn

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

### Baseline runs
chunk_size = 10
grid_nx = 50
grid_ny = 50 ;tcheck(desc='vga start grid 50x50 knn')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.15167882 0.02511116 [1] "245.300000 elapsed
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 17 hr
### Baseline end

# Need to work on weighting before proceeding 
# gut feel based on ranges (  lapply( create_features(train), range)   )
knn_weights <- c( 500, 1000, 100, 100, 10, 3, 5, 10, 20, 1, 10, 10, 10, 10, 1/3, 3, .1)
tcheck(desc='vga start grid 50x50 knn weights')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.3162225 0.0388140 [1] "250.140000 elapsed

#adding a normalizing option
knn_norm = TRUE
tcheck(desc='vga start grid 50x50 knn norm')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.33578480 0.02929445 [1] "248.040000


#now something more scientific:
#use the importances from xgb
hp_classify <- hp_classify_xgb_imp
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

hp_classify <- hp_classify_knn
avg_gain <- impdf %>% group_by(Feature) %>% summarize(Gain=mean(Gain))
# Feature  mean(Gain)
# (chr)       (dbl)
# 1                      y 0.448362679
# 2                      x 0.223796547
# 3                   hour 0.066934625
# 4               accuracy 0.047252332
# 5                  month 0.040894129
# 6               hour_sin 0.035452052
# 7            hour_sincos 0.025322823
# 8                weekday 0.024921063
# 9                   mday 0.016844978
# 10             time_diff 0.015801287
# 11              g_hr_chg 0.014285368
# 12            rat_hr_chg 0.015801287
# 13                  year 0.012640366
# 14 quarter_period_of_day 0.009145116
# 15             n_this_hr 0.005309761

knn_weights <- c(0.223796547, 0.448362679, 0.047252332, 0.066934625, 0.024921063, 0.016844978, 
                   0.040894129, 0.012640366, 0.009145116, .0001, 0.014285368, 0.035452052, .0001, 
                   0.025322823, 0.005309761, 0.015801287, 0.015801287)
tcheck(desc='vga start grid 50x50 knn xgb weights')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.51154663 0.04746262 [1] "241.370000 elapsed

chunk_size = 10
grid_nx = 25
grid_ny = 25
tcheck(desc='vga start grid 25x25 knn xgb weights')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
#  0.48823496 0.03172931 [1] "1009.450000 elapsed
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 17.5 hr

grid_nx = 50
grid_ny = 50
knn_k = 15
tcheck(desc='vga start grid 50x50 knn xgb weights k=15')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.50489313 0.04555412 [1] "243.110000 elapsed 
est_time <- ( (diff(tcheck.df$elapsed) %>% tail(1)) * grid_nx * grid_ny / chunk_size ) / 3600.  # 16.9

knn_k = 20
tcheck(desc='vga start grid 50x50 knn xgb weights k=20')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.50879085 0.04767399 [1] "265.890000 elapsed

knn_k = 30
tcheck(desc='vga start grid 50x50 knn xgb weights k=30')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 

### Lost script records !!!! (crash)  -- lessons captured in notes

knn_weights_xgb <- c(0.223796547, 0.448362679, 0.047252332, 0.066934625, 0.024921063, 0.016844978, 
                 0.040894129, 0.012640366, 0.009145116, .0001, 0.014285368, 0.035452052, .0001, 
                 0.025322823, 0.005309761, 0.015801287, 0.015801287)
knn_k = 25
knn_norm = TRUE
chunk_size = 10
grid_nx = 50
grid_ny = 50 
min_occ = 10

fea_names <- names( create_features_safe(train[1:10]) %>% select( -c(row_id, place_id)))
rm_idx <- integer()

train[ ,rating_history := NULL ]
test[ ,rating_history := NULL ]
rm_idx <- c(rm_idx, which(fea_names == 'rating_history'))
# no need to run, since this weight was set to .0001

train[ ,hour_cos := NULL ]
test[ ,hour_cos := NULL ]
rm_idx <- c(rm_idx, which(fea_names == 'hour_cos'))
# no need to run, since this weight was set to .0001

train[ ,year := NULL ]
test[ ,year := NULL ]
rm_idx <- c(rm_idx, which(fea_names == 'year'))
# no need to run, since year is constant in test and gets auto removed

# but lets run it anyway to validate we're getting the same score as our baseline
knn_weights <- knn_weights_xgb[ -rm_idx]
tcheck(desc='vga start grid 50x50 knn base - sans3')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.51154663 0.04746262 [1] "247.890000 elapsed  (checks out)

train[ ,month := NULL ]
test[ ,month := NULL ]
rm_idx <- c(rm_idx, which(fea_names == 'month'))
knn_weights <- knn_weights_xgb[ -rm_idx]
tcheck(desc='vga start grid 50x50 knn sns3 - month')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.52388404 0.05180941 [1] "241.550000

train[ ,hour_sincos := NULL ]
test[ ,hour_sincos := NULL ]
rm_idx <- c(rm_idx, which(fea_names == 'hour_sincos'))
knn_weights <- knn_weights_xgb[ -rm_idx]
tcheck(desc='vga start grid 50x50 knn sans4 - sincos')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.5220355 0.0529259 [1] "258.910000 elapsed

train[ ,hour_sin := NULL ]
test[ ,hour_sin := NULL ]
rm_idx <- c(rm_idx, which(fea_names == 'hour_sin'))
knn_weights <- knn_weights_xgb[ -rm_idx]
tcheck(desc='vga start grid 50x50 knn sans5 - sin')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# 0.51616076 0.05300311 [1] "250.140000

# reset train
# recalc xgb weights
train[ ,rating_history := NULL ]
test[ ,rating_history := NULL ]

train[ ,hour_cos := NULL ]
test[ ,hour_cos := NULL ]

train[ ,year := NULL ]
test[ ,year := NULL ]

train[ ,month := NULL ]
test[ ,month := NULL ]

# hp_classify <- hp_classify_xgb_imp
# tcheck(desc='vga start grid 50x50 xgb importance')
# #[1] "217.540000 elapsed for vga start grid 50x50 xgb importance"
# source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
# ichunk, chunk_size, grid_nx, grid_ny: 1 10 50 50 
# capturing xgb_importance a (be patient)...  
# capturing xgb_importance a (be patient)...  
# capturing xgb_importance a (be patient)...  
# capturing xgb_importance a (be patient)...  
# .capturing xgb_importance a (be patient)...  
# capturing xgb_importance a (be patient)...  
# capturing xgb_importance a (be patient)...  
# capturing xgb_importance a (be patient)...  
# capturing xgb_importance a (be patient)...  
# .capturing xgb_importance a (be patient)...  
# ...total elapsed = 2306.160000
# [1] 0.53766370 0.05166226
# [1] "2306.280000 elapsed for vga complete"
# > 

# 1                      y 0.460971279
# 2                      x 0.233083074
# 3               accuracy 0.052850552
# 4                   hour 0.070733799
# 5               hour_sin 0.037008454
# 6                weekday 0.027843511
# 7            hour_sincos 0.026524299
# 8               g_hr_chg 0.019791895
# 9                   mday 0.019624543
# 10             time_diff 0.018984853
# 11            rat_hr_chg 0.016377178
# 12 quarter_period_of_day 0.009489949
# 13             n_this_hr 0.006716613

sorted_weights <- avg_gain$Gain
names(sorted_weights) <- avg_gain$Feature
fea_names <- names( create_features_safe(train[1:10]) %>% select( -c(row_id, place_id)))
knn_weights_xgb <- rep(.0001, length(fea_names))
names(knn_weights_xgb) <- as.character(fea_names)
for (i in 1:length(fea_names))  knn_weights_xgb[i] <- sorted_weights[ fea_names[i] ] 

# to reproduce
# knn_weights_xgb <- c(0.233083074,0.460971279,0.052850552,0.070733799,0.027843511,
# 0.019624543,0.009489949,0.019791895,0.037008454,0.026524299,0.006716613,0.018984853,0.016377178)

hp_classify <- hp_classify_knn
knn_weights <- knn_weights_xgb  
tcheck(desc='vga start grid 50x50 knn sans4 - new xgb weights')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
#0.52419918 0.05223014 [1] "243.480000 elapsed  ## only minor (.001) improvement

##
##  Try copying features from best script
##

library(knnGarden)
hp_classify_knng <- function(trn, val, min_occ=2, verbose=0, norm=knn_norm, w=knn_weights) {
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
    
    knn_fit <- knnVCN( t(t( trn2 ) * w),  #TrnX
                       trn2.place_id, #OrigTrng
                       t(t( val2 ) * w),  #TstX
                       K = knn_k,
                       method = "manhattan")
    top3_places <- apply(attr(knn_fit,'nn.index'), 1, top3_knn, trn2.place_id ) %>% t() %>% as.data.frame %>% tbl_df()
    top3_probs  <- apply(attr(knn_fit,'nn.index'), 1, top3_knn, trn2.place_id, prob=TRUE ) %>% t() %>% as.data.frame %>% tbl_df()
    
    preds <- val %>% select( row_id, truth=place_id)
    preds$predictions <- with(top3_places, paste(V1,V2,V3))
    names(top3_probs) <- c('X1', 'X2', 'X3')
    preds <- cbind(preds, top3_probs)
    
    return(preds)
}
hp_classify <- hp_classify_knng
