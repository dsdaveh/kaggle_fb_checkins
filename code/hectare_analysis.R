library(dplyr)
library(tidyr)
library(data.table)
if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')

read_class <- c("integer", "numeric", "numeric", "integer", "integer", "character")
if (! exists("df_train")) df_train <- fread('../input/train.csv', colClasses = read_class)

load("../data/hectare_prob_loc_5fold_indices.RData")

nhect <- 5

set.seed(48)
h_sample <- sample(1:100, nhect * 2, replace=TRUE) %>% matrix(nrow=nhect, ncol=2) %>% as.data.frame()
names(h_sample) <- c('x', 'y')

plot(c(0,100), c(0,100), type="n")
for( i in 1:nhect) with(h_sample[i, ], {
    rect(x-1, y-1, x, y)
    text(x, y, i, pos=4) } )

k_val <- 5  #which validation fold to use
trn <- df_train[ ixfold[ ixfold$kfold != k_val, ]$ix, ]

t0 <- proc.time()
h_score <- data.frame()
hp_loc <- data.frame()
time_filter <- 0
for( i in 1:nhect) {
    locij <- hp_summarize( trn, h_sample[i,1], h_sample[i,2], min_time=0)
    hp_loc <- rbind(hp_loc, locij)
    xmin <- (h_sample[i,1] - 1) / 10.
    ymin <- (h_sample[i,2] - 1) / 10.
    val <- df_train[ ixfold[ ixfold$kfold == k_val, ]$ix, ] %>%
        filter( time >= time_filter,
                x >= xmin, x <= xmin + 0.1,
                y >= ymin, y <= ymin + 0.1) 
    preds <- pred_by_hectare(locij, val) %>%
        rename(predictions = place_id)
    preds$truth <- val$place_id
    
    hij <- data.frame(
        h_x = h_sample[i,1], h_y = h_sample[i,2], 
        score = estimate_map_score(preds, size=nrow(preds), n=1)$MAP) 
    if (i %% 100 == 0) cat(paste( i, '\n'))
    h_score <- rbind(h_score, hij )
    
}
print(h_score)
print(mean(h_score$score))
print(sd(h_score$score))
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
    
#dev version
# hp_summarize <- hp_summarize_dev
hp_summarize_dev <- function(trn, hi, hj) {
    # i,j = hectare id from 1-100 in x and y, respectively
    stopifnot( hi > 0 & hi <= 100 & hj > 0 & hj <= 100)
    
    hdata <- trn %>% filter( x >= (hi-1)/10, x <= hi/10,
                             y >= (hj-1)/10, y <= hj/10) 
    
    hdata$hour_abs <- as.integer(hdata$time / 60)
    hdata$hour_in_day <- hdata$hour_abs %% 24
    hdata$day_abs <- as.integer( hdata$time / (60 * 24) )
    hdata$day_in_week <- hdata$day_abs %% 7
    hdata$qtr_in_year <- as.integer(hdata$time / (60 * 24 * 91)) %% 4
    
    n_places <- length(unique(hdata$place_id))
    
    hdata$pid <- as.factor(hdata$place_id)
    levels_orig <- levels(hdata$pid)
    new_ids <- expand.grid(LETTERS, letters, 0:9) %>% 
        mutate(id = paste0(Var1, Var2, Var3)) %>% .[[4]] %>%
        head( length(levels_orig))
    levels(hdata$pid) <- new_ids
    
    #find places with more than n occurances in grid
    n_occ <- 0
    mults <- hdata %>% count(place_id, sort=TRUE) %>% filter(n > n_occ) %>% .[[1]]

    hdata %>% filter(place_id %in% mults[1:20]) %>%
        ggplot( aes(pid)) + geom_bar() + coord_flip()
    
    #for h=98,62:
    # number of places in validation set = 457 | nrow(val)
    # number of which are in training set = 435 (95%) | sum(val$place_id %in% hdata$place_id)
    # number in top10 training = 271 (59%) sum(val$place_id %in% mults[1:10])
    # number in top20 = 347 (76%), top50 = 410 (88%)
    
    # list of place ids that don't occur after day 300 (range = 0, 545)
    defunct <- hdata %>% group_by(place_id) %>%
        summarise( last_day = max(day_abs)) %>%
        filter( last_day <= 300 ) %>%
        .[[1]]
    hdata %>% filter(place_id %in% defunct) %>%
        ggplot( aes(day_abs, pid, col=pid)) + geom_jitter(width=0, height=.1, alpha=.5) + guides(col=FALSE)
    #number of validates after day 400 that use these  #note validate was prefiltered here (Not passed in) =2
    val %>% mutate(day_abs = as.integer( time / (60 * 24) )) %>%
        filter(day_abs > 400, place_id %in% defunct) %>% nrow()  
    
    ## Hmmm:  use data after 400 days to predict? More closely models test data?
    val400 <- val %>% mutate(day_abs = as.integer( time / (60 * 24) )) %>% filter(day_abs > 400)
    mults400 <- mults[! mults %in% defunct]
    # number of places in validation set after  = 179 | nrow(val400)
    # number of which are in training set = 169 (94%) | sum(val400$place_id %in% hdata$place_id)
    # number in top10 training = 99 (55%) | sum(val400$place_id %in% mults[1:10])
    # number in top20 = 130 (72%) , top50 = 157 (88%)
    
    centroids <- hdata %>% group_by(pid, place_id) %>%
        summarize( x = mean(x),
                   y = mean(y),
                   n = n()) %>%
        mutate(type = 'centroid') %>%
        ungroup %>% arrange(desc(n))
    
    grp <- hdata %>% filter(place_id %in% mults[1:10]) %>% mutate(n=1, type='event')  %>% select(pid, place_id, x, y, n, type, day_abs) 
    bind_rows(grp,centroids[1:10,]) %>% ggplot( aes(x, y, group=pid, col=pid)) + 
        geom_point(aes(size=n, shape=as.factor(type))) 
   
    grp %>% ggplot( aes(day_abs, pid, col=pid)) + geom_jitter(width=0, height=.3, alpha=.5)
    
    grp <- hdata %>% filter(place_id %in% mults[31:40])
    grp %>% ggplot( aes(x, y, group=pid, col=pid)) + geom_point()
    grp %>% ggplot( aes(day_abs, pid, col=pid)) + geom_jitter(width=0, height=.3, alpha=.5)

    plot(val400$x, val400$y, col=as.factor(val400$place_id), pch=16) 
    
    
    
    
    ncut <- min(3, n_places)
    
    hprob <- hdata %>%
        group_by(hour_abs, place_id) %>%
        summarise(n = n()) %>%
        arrange(hour_abs)
    
        count(place_id, sort=TRUE) %>% 
        mutate(P = n / sum(n)) %>%
        head(ncut) %>% 
        mutate(  rank = 1:ncut) 
    
    if(ncut < 3 )  hprob <- hprob %>% bind_rows( hprob[1,] %>% mutate( rank = 3))
    if(ncut < 2 )  hprob <- hprob %>% bind_rows( hprob[1,] %>% mutate( rank = 2)) %>%
        mutate(P = n / sum(n))
    
    top_places <- hprob %>% 
        mutate( place_rank = paste0("place", rank)) %>%
        dplyr::select(place_id, place_rank) %>%
        tidyr::spread(place_rank, place_id)
    
    top_probs <- hprob %>% 
        mutate( prob_rank = paste0("P", rank)) %>%
        dplyr::select(P, prob_rank) %>%
        tidyr::spread(prob_rank, P)
    
    data.frame( h_x = hi, h_y = hj, P3cum = sum(hprob$P), n_places = n_places) %>%
        bind_cols( top_places, top_probs) 
}
