library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(xgboost)
if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')

read_class <- c("integer", "numeric", "numeric", "integer", "integer", "character")
if (! exists("df_train")) df_train <- fread('../input/train.csv', integer64='character')
if (! exists("df_test")) df_test <- fread('../input/train.csv', integer64='character')

set.seed(48)

h_backlog <- expand.grid( x=1:100, y=1:100) %>% sample_frac(size=1)
chunk_size = 1000
chunk <- h_backlog[1:1000,]
h_backlog <- h_backlog[-(1:1000),]

plot(c(0,100), c(0,100), type="n")
for( i in 1:chunk_size) with(chunk[i, ], { rect(x-1, y-1, x, y) } )
#for( i in 1:100) with(chunk[i, ], { rect(x-1, y-1, x, y, col="red") } )

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

train <- df_train
test <- df_test

t0 <- proc.time()

xgb_results <- data.frame()
ih <- ncum <- 0

grpx <- chunk %>% count(x)

for (i in 1:nrow(grpx)) {
    xmin <- (chunk[ih+1, ]$x - 1) / 10.
    trnx <- train %>% filter( x >= xmin, x <= xmin + 0.1 )
    tstx <- test %>% filter( x >= xmin, x <= xmin + 0.1 )
    
    ny <- grpx[i, ]$n
    ncum <- ncum + ny
    for(j in 1:ny){
        ih <- ih + 1
        
        tx <- proc.time()
        if(ih %% 10 == 0) cat('.') 
        if(ih %% 100 == 0) cat( sprintf('  elapsed=%f\n', (tx-t0)[3]))

        ymin <- (chunk[ih, ]$y - 1) / 10.
        trn <- trnx %>% filter( y >= ymin, y <= ymin + 0.1 )
        tst <- tstx %>% filter( y >= ymin, y <= ymin + 0.1 )
        
        ## xgb classifier
        xgb_preds <- hp_classify( trn, tst, min_occ = 10)
        xgb_results <- rbind( xgb_results, xgb_preds)
        score_xgb <- calculate_map_score( xgb_preds )
        cat(sprintf("%d: xgb score=%f\n", i, score_xgb))
        
    }
}
    
    
    h_stats <- rbind( h_stats, data.frame(
        score_SC = h_score[i],
        score_xgb = score_xgb,
        n_places_trn = length(unique(trn$place_id)),
        n_places_val = length(unique(val$place_id)),
        n_common = sum( unique(val$place_id) %in% unique(trn$place_id) )
    ))
}

h_stats %>% summarize ( mean_SC = mean(score_SC), sd_SC = sd(score_SC),
                        mean_xgb = mean(score_xgb), sd_xgb = sd(score_xgb))
t1 <- proc.time()
print((t1-t0)[3])
####################### (seed=8)
# h_x h_y     score
# 1  47  72 0.3284765
# 2  21  30 0.1857361
# 3  80  94 0.4002789
# 4  66  77 0.1645068
# 5  33  65 0.2514409
# > print(mean(h_score$score))
# [1] 0.2660879
# > print(sd(h_score$score))
# [1] 0.09860176
##########################
  
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

    predictions <- predictions %>% apply(1, function(x) names( sort( desc(x))[1:3])) %>%
        as.vector() %>% matrix(ncol=3, byrow=TRUE) %>% data.frame() 
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
        bind_cols( data.frame(predictions = apply(top3, 1, paste, collapse=" ")) )
    
    return(preds)
}
    
    
#dev version
# hp_summarize <- hp_summarize_dev
# hp_summarize_dev <- function(trn, hi, hj) {
#     # i,j = hectare id from 1-100 in x and y, respectively
#     stopifnot( hi > 0 & hi <= 100 & hj > 0 & hj <= 100)
#     
#     hdata <- trn %>% filter( x >= (hi-1)/10, x <= hi/10,
#                              y >= (hj-1)/10, y <= hj/10) 
#     
#     hdata$hour_abs <- as.integer(hdata$time / 60)
#     hdata$hour_in_day <- hdata$hour_abs %% 24
#     hdata$day_abs <- as.integer( hdata$time / (60 * 24) )
#     hdata$day_in_week <- hdata$day_abs %% 7
#     hdata$qtr_in_year <- as.integer(hdata$time / (60 * 24 * 91)) %% 4
#     
#     n_places <- length(unique(hdata$place_id))
#     
#     hdata$pid <- as.factor(hdata$place_id)
#     levels_orig <- levels(hdata$pid)
#     new_ids <- expand.grid(LETTERS, letters, 0:9) %>% 
#         mutate(id = paste0(Var1, Var2, Var3)) %>% .[[4]] %>%
#         head( length(levels_orig))
#     levels(hdata$pid) <- new_ids
#     
#     # look for a reasonable threshhold for n_occ ( < 20 place_ids )
#     thresh <- hdata %>% count(place_id, sort=TRUE) %>% 
#         rename(n_occ=n) %>% count(n_occ) %>% 
#         arrange(n_occ) %>%
#         mutate( n_cutoff = sum(n) - cumsum(n)) 
#     thresh
#     thresh %>% ggplot(aes(n_occ, n_cutoff)) + geom_bar(stat="identity")
#     
#     #find places with more than n occurances in grid
#     n_occ <- 10
#     mults <- hdata %>% count(place_id, sort=TRUE) %>% filter(n > n_occ) %>% .[[1]]
# 
#     hlimit <- length(mults)
#     hdata %>% filter(place_id %in% mults[1:hlimit]) %>%
#         ggplot( aes(pid)) + geom_bar() + coord_flip()
#     
#     
#     
#     
#     #for h=98,62:
#     # number of places in validation set = 457 | nrow(val)
#     # number of which are in training set = 435 (95%) | sum(val$place_id %in% hdata$place_id)
#     # number in top10 training = 271 (59%) sum(val$place_id %in% mults[1:10])
#     # number in top20 = 347 (76%), top50 = 410 (88%)
#     
#     # list of place ids that don't occur after day 300 (range = 0, 545)
#     defunct <- hdata %>% group_by(place_id) %>%
#         summarise( last_day = max(day_abs)) %>%
#         filter( last_day <= 300 ) %>%
#         .[[1]]
#     hdata %>% filter(place_id %in% defunct) %>%
#         ggplot( aes(day_abs, pid, col=pid)) + geom_jitter(width=0, height=.1, alpha=.5) + guides(col=FALSE)
#     #number of validates after day 400 that use these  #note validate was prefiltered here (Not passed in) =2
#     val %>% mutate(day_abs = as.integer( time / (60 * 24) )) %>%
#         filter(day_abs > 400, place_id %in% defunct) %>% nrow()  
#     
#     ## Hmmm:  use data after 400 days to predict? More closely models test data?
#     val400 <- val %>% mutate(day_abs = as.integer( time / (60 * 24) )) %>% filter(day_abs > 400)
#     mults400 <- mults[! mults %in% defunct]
#     # number of places in validation set after  = 179 | nrow(val400)
#     # number of which are in training set = 169 (94%) | sum(val400$place_id %in% hdata$place_id)
#     # number in top10 training = 99 (55%) | sum(val400$place_id %in% mults[1:10])
#     # number in top20 = 130 (72%) , top50 = 157 (88%)
#     
#     centroids <- hdata %>% group_by(pid, place_id) %>%
#         summarize( x = mean(x),
#                    y = mean(y),
#                    n = n()) %>%
#         mutate(type = 'centroid') %>%
#         ungroup %>% arrange(desc(n))
#     
#     grp <- hdata %>% filter(place_id %in% mults[1:10]) %>% mutate(n=1, type='event')  %>% select(pid, place_id, x, y, n, type, day_abs) 
#     bind_rows(grp,centroids[1:10,]) %>% ggplot( aes(x, y, group=pid, col=pid)) + 
#         geom_point(aes(size=n, shape=as.factor(type))) 
#    
#     grp %>% ggplot( aes(day_abs, pid, col=pid)) + geom_jitter(width=0, height=.3, alpha=.5)
#     
#     grp <- hdata %>% filter(place_id %in% mults[31:40])
#     grp %>% ggplot( aes(x, y, group=pid, col=pid)) + geom_point()
#     grp %>% ggplot( aes(day_abs, pid, col=pid)) + geom_jitter(width=0, height=.3, alpha=.5)
# 
#     plot(val400$x, val400$y, col=as.factor(val400$place_id), pch=16) 
#     
#     
#     
#     
#     ncut <- min(3, n_places)
#     
#     hprob <- hdata %>%
#         group_by(hour_abs, place_id) %>%
#         summarise(n = n()) %>%
#         arrange(hour_abs)
#     
#         count(place_id, sort=TRUE) %>% 
#         mutate(P = n / sum(n)) %>%
#         head(ncut) %>% 
#         mutate(  rank = 1:ncut) 
#     
#     if(ncut < 3 )  hprob <- hprob %>% bind_rows( hprob[1,] %>% mutate( rank = 3))
#     if(ncut < 2 )  hprob <- hprob %>% bind_rows( hprob[1,] %>% mutate( rank = 2)) %>%
#         mutate(P = n / sum(n))
#     
#     top_places <- hprob %>% 
#         mutate( place_rank = paste0("place", rank)) %>%
#         dplyr::select(place_id, place_rank) %>%
#         tidyr::spread(place_rank, place_id)
#     
#     top_probs <- hprob %>% 
#         mutate( prob_rank = paste0("P", rank)) %>%
#         dplyr::select(P, prob_rank) %>%
#         tidyr::spread(prob_rank, P)
#     
#     data.frame( h_x = hi, h_y = hj, P3cum = sum(hprob$P), n_places = n_places) %>%
#         bind_cols( top_places, top_probs) 
# }
