library(dplyr)

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

