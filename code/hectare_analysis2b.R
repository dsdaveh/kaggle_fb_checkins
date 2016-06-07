stopifnot( exists("ichunk"))  # ichunk = 1 thru 10 (100000 / chunk_size)

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(xgboost)
if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')

if (! exists("df_train")) df_train <- fread('../input/train.csv', integer64='character')
if (! exists("df_test")) df_test <- fread('../input/test.csv', integer64='character')

create_features <- function(dt) {
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    dt[,
       .(row_id,
         x, y, accuracy,
         hour = as.integer(floor(time/60) %% 24),
         weekday = as.integer(floor(time/(60 * 24)) %% 7),
         quarter_period_of_day = as.integer(floor((time + 120) / (6*60)) %% 4),
         rating_history= log10(3+((time + 120.0) / (60 * 24 * 30))),
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
xgb_nrounds <- 50 #   
###
top3_preds <- function (pred, place_ids) {
    predictions <- as.data.frame(matrix(pred, ncol=length(place_ids), byrow=TRUE ))
    colnames(predictions) <- place_ids
    
    pred3 <- predictions %>% apply(1, function(x) names( sort( desc(x))[1:3])) %>%
        as.vector() %>% matrix(ncol=3, byrow=TRUE) %>% data.frame() 
    prob3 <- predictions %>% apply(1, function(x)        sort( desc(x))[1:3])  %>%
        as.vector() %>% matrix(ncol=3, byrow=TRUE) %>% data.frame() 
    cbind( pred3, -prob3)
}

hp_classify <- function(trn, val, min_occ=2) {
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
                      params = xgb_params, verbose = 0 )
    pred <- predict( model, val %>% select(-c(row_id, place_id)) %>% as.matrix() )
    
    top3 <- predict( model, xgb.DMatrix(val %>% select(-c(row_id, place_id)) %>% as.matrix() )) %>%
        top3_preds( levels(trn$place_id) ) 
    preds <- val %>% select( row_id, truth=place_id ) %>% 
        bind_cols( data.frame(predictions = apply(top3[,1:3], 1, paste, collapse=" "),
                              top3[4:6]) )
    
    return(preds)
}

set.seed(48)
h_scramble <- expand.grid( x=1:100, y=1:100) %>% sample_frac(size=1)
chunk_size = 1000
chunk <- h_scramble[ ((ichunk-1) * chunk_size + 1):(ichunk * chunk_size),]

plot(c(0,100), c(0,100), type="n")
for( i in 1:chunk_size) with(chunk[i, ], { rect(x-1, y-1, x, y) } )
#for( i in 1:100) with(chunk[i, ], { rect(x-1, y-1, x, y, col="red") } )

train <- df_train
test <- df_test

t0 <- proc.time()

xgb_results <- data.table()
ih <- ncum <- 0

grpx <- chunk %>% count(x)
chunk <- chunk %>% arrange (x)

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
        xgb_preds <- hp_classify( trn, tst, min_occ = 10) %>% data.table()
        xgb_preds$truth <- NULL
        xgb_results <- rbind( xgb_results, xgb_preds) 
    }
}

    
t1 <- proc.time()
print((t1-t0)[3])

## there could be duplicate row_id's so they need to be resolved
## method below uses the row with the lowest X1 (probability) since  
## in most cases place_id1 is identical and a lower X1 probability
## results in a higher X2, X3 probability
setkey(xgb_results, row_id, X1)
xgb_results <- xgb_results[, n := 1:.N , by=row_id][n==1][ ,n := NULL][, chunk := ichunk]
result <- xgb_results[,.(row_id, place_id=predictions)]
result_p <- xgb_results[,.(row_id, X1, X2, X3)]

blanks <- setdiff( test$row_id, xgb_results$row_id) 
result <- rbind(result, data.frame( row_id=blanks, place_id = ""))
submit_name <- sprintf("../submissions/xgb_ha2b_chunk%d_%s.csv", ichunk, format(Sys.time(), "%Y_%m_%d_%H%M%S"))
write.csv(result, file=submit_name, row.names=FALSE) #
submit_name_p <- gsub("2b_", "2b_probs_", submit_name)
write.csv(result_p, file=submit_name_p, row.names=FALSE) #

data_name <- sprintf("../data/xgb_results_chunk%d.RData", ichunk)
save(xgb_results, file=data_name) 
rm(ichunk) #to prevent accidental overwrite
