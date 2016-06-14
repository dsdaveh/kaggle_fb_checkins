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

normalize <- function(x) (x - min(x)) / diff(range(x))

create_features_ha3 <- function(dt) {
    
    time0 <- ymd_hms('2014-01-01 00:00:00')
    if( all(class(dt$time) == "integer"))  dt[, time := time0 + minutes(time) ]
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    
    wt <- c(1, 1, 4, 3, 1./22, 2, 10, 1, 1)
    wt <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)
    dt[,
       .(row_id,
          x, y, accuracy,
          hour = hour(time) *wt[3],
          weekday = wday(time) *wt[4],
          mday = day(time) *wt[5],
          month = month(time) *wt[6],
          year = (year(time) - 2013) *wt[7],
          quarter_period_of_day = as.integer(hour(time) / 6) *wt[8],
          rating_history= log10(3+month(time)) *wt[9],
          place_id),
       ]
}
if (! exists("create_features")) create_features <- create_features_ha3

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

hp_classify_knn <- function(trn, val, min_occ=2, verbose=0) {
    trn2 <- create_features(trn)
    val2 <- create_features(val)
    
    places <- trn2 %>% count(place_id, sort=TRUE) %>% filter(n >= min_occ) %>% .[[1]]
    trn2 <- trn2 %>% filter(place_id %in% places)
    trn2$place_id <- as.factor(trn2$place_id)

    knn_fit <- knn( trn2 %>% select(-c(row_id, place_id)),  #train
                    val2 %>% select(-c(row_id, place_id)),  #test
                    trn2$place_id, #cl
                    k = 25 )
    top3_places <- apply(attr(knn_fit,'nn.index'), 1, top3_knn, trn2$place_id ) %>% t() %>% as.data.frame %>% tbl_df()
    top3_probs  <- apply(attr(knn_fit,'nn.index'), 1, top3_knn, trn2$place_id, prob=TRUE ) %>% t() %>% as.data.frame %>% tbl_df()

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
if (! exists("hp_classify")) hp_classify <- hp_classify_knn

if (! exists("xgb_params")) xgb_params <- list( 
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
    verbose = 1
)

if (! exists("df_train")) df_train <- fread('../input/train.csv', integer64='character')
#if (! exists("df_test")) df_test <- fread('../input/test.csv', integer64='character')

if (! exists("ichunk")) ichunk = 1
if (! exists("chunk_size")) chunk_size = 10
    
set.seed(48)
h_scramble <- expand.grid( x=1:100, y=1:100) %>% sample_frac(size=1)
chunk <- h_scramble[ ((ichunk-1) * chunk_size + 1):(ichunk * chunk_size),]

plot(c(0,100), c(0,100), type="n")
for( i in 1:chunk_size) with(chunk[i, ], { rect(x-1, y-1, x, y) } )

# method 1: split based on time
if( ! exists("train")) train <-df_train

setorder(train, time)
train_cut_ix <- as.integer(nrow(train) * .80)
test <- train[(train_cut_ix+1):nrow(train), ]
train <- train[1:train_cut_ix, ]

t0 <- proc.time()

hp_results <- data.table()
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
        if (! exists("xgb_nrounds")) xgb_nrounds <- 50 #   
        if (! exists("verbose")) verbose <- 0 #   
        preds <- hp_classify( trn, tst, min_occ = 10, verbose=verbose) %>% data.table()
        #xgb_preds$truth <- NULL
        
        chunk[ih,"score"] <- calculate_map_score( preds )
        hp_results <- rbind( hp_results, preds) 
    }
}
print(c( mean(chunk$score), sd(chunk$score)))
    
t1 <- proc.time()
print((t1-t0)[3])


    
    

