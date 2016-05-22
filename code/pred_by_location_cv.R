library(dplyr)
library(tidyr)

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



load(file='../data/hectare_prob_loc_k1-5.RData')
hp_trn <- hp_loc
load(file='../data/hectare_prob_loc_k2-5.RData')
hp_trn <- hp_trn %>% combine_hp( hp_loc )
#us gen_probability_by_location.R to create hp_loc (or load from disk)
stopifnot( exists("hp_loc"))

if (! exists("df_test")) df_train <- fread('../input/test.csv')

hectare_coord <- function(x) {
    h <- floor(x*10) + 1
    ifelse(h > 100, 100, h)
}
t0 <- proc.time()
preds <- df_test %>% tbl_df %>%
    mutate( hectare = sprintf("%d,%d", hectare_coord(x), hectare_coord(y)) ) %>%
    left_join( hp_loc %>% mutate( hectare = paste0(h_x, ',', h_y), by=hectare)) %>%
    mutate( place_id = paste(place1, place2, place3)) %>%
    dplyr::select(row_id, place_id)
t1 <- proc.time(); (t1-t0)[3]

submit_name <- sprintf("../submissions/pred_by_location_%s.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"))
write.csv(preds, file=submit_name, row.names=FALSE)

