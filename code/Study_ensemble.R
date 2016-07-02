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
    
    #RANN.L1  (manhattan distances)
    nn <- nn2( t(t( trn2 ) * w),  #train
               t(t( val2 ) * w),  #test
               k = knn_k )
    
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




knn_weights_xgb <- c(0.233083074,0.460971279,0.052850552,0.070733799,0.027843511,
                     0.019624543,0.009489949,0.019791895,0.037008454,0.026524299,
                     0.006716613,0.018984853,0.016377178)
names(knn_weights_xgb) <- names( create_features_safe(train[1:10]) %>% select( -c(row_id, place_id)))
### Baseline runs
knn_k = 30
knn_norm = TRUE
chunk_size = 10
grid_nx = 50
grid_ny = 50 
min_occ = 10
expand_margin = .005

hp_classify <- hp_classify_knn_RANN
knn_weights <- knn_weights_xgb  
knn_probs = FALSE
tcheck(desc='vga start grid 50x50 knn baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
#  0.53113145 0.05173669 [1] "303.530000 elapsed for vga complete"

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
hp_classify <- hp_classify_xgb

tcheck(desc='vga start grid 50x50 xgb baseline')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')
#  0.5379553 0.0516012 [1] "1643.650000 elapsed (laptop)

xgb_params$eval_metric = 'merror'
xgb_nrounds = 50
tcheck(desc='vga start grid 50x50 xgb merror @ 50')
source('variable_grid_analysis1.R'); tcheck(desc='vga complete')

