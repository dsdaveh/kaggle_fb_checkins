library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')

combine_hp <- function(hp1, hp2) {
    hp2 <- hp2 %>% mutate( count4 = round(P1 * n_places),
                           count5 = round(P2 * n_places),
                           count6 = round(P3 * n_places),
                           loc_key = paste(h_x, h_y)) %>%
        mutate (countX = n_places - count4 - count5 - count6) %>%
        rename( place4 = place1, place5 = place2, place6 = place3) %>%
        select( loc_key, starts_with('place'), starts_with('count'))
    hp <- hp1 %>% mutate( count1 = round(P1 * n_places),
                          count2 = round(P2 * n_places),
                          count3 = round(P3 * n_places),
                          loc_key = paste(h_x, h_y)) %>%
        mutate (countX = n_places - count1 - count2 - count3) %>%
        left_join( hp2, by="loc_key") %>%
        gather(key, value, starts_with('place'), starts_with('count'))
    hp_place <- hp %>% filter( grepl('^place', key)) %>%
        mutate(loc_key2 = paste(loc_key, gsub("place", "", key))) %>%
        select(loc_key2, value) %>%
        rename( place_id = value)
    hp_count <- hp %>% filter( grepl('^count', key)) %>%
        mutate(loc_key2 = paste(loc_key, gsub("count", "", key))) %>%
        select(loc_key2, loc_key, h_x, h_y, value) %>%
        mutate( count = as.numeric(value)) %>%
        left_join(hp_place, by="loc_key2") %>%
        group_by( h_x, h_y, loc_key, place_id) %>%
        summarise(count=sum(count)) %>%
        group_by(loc_key) %>% 
        mutate(P = count / sum(count),
               n_places = sum(count)) %>%
        filter( ! is.na(place_id)) %>%
        arrange(desc(P)) %>%
        mutate( rank = 1:n() ) %>%
        filter( rank <= 3 ) %>%
        mutate( P3cum = sum(P))
    
    top_places <- hp_count %>% 
        mutate( place_rank = paste0("place", rank)) %>%
        dplyr::select(place_id, place_rank) %>%
        tidyr::spread(place_rank, place_id)
    
    top_probs <- hp_count %>% 
        mutate( prob_rank = paste0("P", rank)) %>%
        dplyr::select(P, prob_rank) %>%
        tidyr::spread(prob_rank, P)

    hp_comb <- hp_count %>%
        filter( rank == 1) %>%
        select(loc_key, h_x, h_y, P3cum, n_places) %>%
        left_join(top_places, by = "loc_key") %>%
        left_join(top_probs, by = "loc_key")
    
    return(hp_comb %>% ungroup() %>% select( -loc_key ))
}

read_class <- c("integer", "numeric", "numeric", "integer", "integer", "character")
if (! exists("df_train")) df_train <- fread('../input/train.csv', colClasses = read_class)

load(file='../data/hectare_prob_loc_5fold_indices.RData')
k_v <- 5
df_validate <-  df_train[ ixfold[ ixfold$kfold == k_v, ]$ix, ]

score_est <- data.frame()
for (i in 1:4) {
    data_file <- sprintf('../data/hectare_prob_loc_k%d-5.RData', i)
    print(data_file)
    load(file = data_file)
    if (i == 1) {
        hp_trn <- hp_loc
    } else {
        hp_trn <- hp_trn %>% combine_hp( hp_loc )
    }
    preds <- pred_by_hectare(hp_trn, df_validate) %>% 
        rename( predictions = place_id) 
    preds$truth <- df_validate$place_id
    score_est <- rbind(score_est, estimate_map_score(preds, seed=19))
    print(score_est)
}

plot(score_est$MAP)
score_est_bigger_training <- score_est
score_est_bigger_training
# MAP          sd
# 1 0.2692533 0.004420900
# 2 0.2698994 0.004401605
# 3 0.2703161 0.004479075
# 4 0.2714778 0.004136214

## conclusion, MAP trends up with more data (underlying hp_loc), but only about .0001 per 20% of df_train.

# Next question what if we use the different folds for the estimation (holding the training data constant)
load(file = '../data/hectare_prob_loc_k5-5.RData')

score_est <- data.frame()
for(i in 1:4) {
    data_file <- sprintf('../data/hectare_prob_loc_k%d-5.RData', i)
    
    df_validate <-  df_train[ ixfold[ ixfold$kfold == i, ]$ix, ]
    preds <- pred_by_hectare(hp_loc, df_validate) %>% 
        rename( predictions = place_id) 
    preds$truth <- df_validate$place_id
    score_est <- rbind(score_est, estimate_map_score(preds, seed=19))
    print(score_est)
}

score_est_variance <- score_est
score_est_variance
# MAP          sd
# 1 0.2688639 0.003692783
# 2 0.2698606 0.003102349
# 3 0.2694600 0.003891152
# 4 0.2684883 0.004043154