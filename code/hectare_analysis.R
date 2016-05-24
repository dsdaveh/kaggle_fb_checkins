library(dplyr)
library(tidyr)
library(data.table)
if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')

read_class <- c("integer", "numeric", "numeric", "integer", "integer", "character")
if (! exists("df_train")) df_train <- fread('../input/train.csv', colClasses = read_class)

load("../data/hectare_prob_loc_5fold_indices.RData")

nhect <- 10

set.seed(27)
h_sample <- sample(1:100, nhect * 2, replace=TRUE) %>% matrix(nrow=nhect, ncol=2) %>% as.data.frame()
names(h_sample) <- c('x', 'y')

plot(c(0,100), c(0,100), type="n")
for( i in 1:nhect) with(h_sample[i, ], {
    rect(x-1, y-1, x, y)
    text(x, y, i) } )

k_val <- 5  #which validation fold to use
trn <- val <- df_train[ ixfold[ ixfold$kfold != k_val, ]$ix, ]

t0 <- proc.time()
h_score <- data.frame()
hp_loc <- data.frame()
for( i in 1:nhect) {
    locij <- hp_summarize( trn, h_sample[i,1], h_sample[i,2])
    hp_loc <- rbind(hp_loc, locij)
    xmin <- (h_sample[i,1] - 1) / 10.
    ymin <- (h_sample[i,2] - 1) / 10.
    val <- df_train[ ixfold[ ixfold$kfold == k_val, ]$ix, ] %>%
        filter( x >= xmin, x <= xmin + 0.1,
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
#######################
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
    
hp_summarize <- function(trn, i, j) {
    # i,j = hectare id from 1-100 in x and y, respectively
    stopifnot( i > 0 & i <= 100 & j > 0 & j <= 100)
    
    hdata <- trn %>% filter( x >= (i-1)/10, x <= i/10,
                             y >= (j-1)/10, y <= j/10) 
    
    n_places <- length(unique(hdata$place_id))
    ncut <- min(3, n_places)
    
    hprob <- hdata %>%
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
    
    data.frame( h_x = i, h_y = j, P3cum = sum(hprob$P), n_places = n_places) %>%
        bind_cols( top_places, top_probs) 
}
