# want to see if we can get an estimate for public leaderboard by submitting a fraction of the data
# optimize ...2 

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')

#result_full <- fread("../submissions/hr_Chistyakov_2016_06_05_005238.csv") 
LB_full <- 0.51115 #public LB score on full dataset
#result_ho <- fread("../submissions/hr_Chistyakov_ho_2016_06_05_005238.csv") 
lv_ho   <- 0.4734809 #local validation score on 20% holdout (by time)

read_class <- c("integer", "numeric", "numeric", "integer", "integer", "character")
if (! exists("df_train")) df_train <- fread('../input/train.csv', integer64 = "character")
if (! exists("df_test")) df_test <- fread('../input/test.csv', integer64 = "character")

train <- df_train
setorder(train, time)
train_cut_ix <- as.integer(nrow(train) * .80)
val_ho <- train[(train_cut_ix+1):nrow(train), ]
train_ho <- train[1:train_cut_ix, ]

set.seed(48)
h_backlog <- expand.grid( x=1:100, y=1:100) %>% sample_frac(size=1)
chunk_size = 1000
chunk <- h_backlog[1:chunk_size,]
h_backlog <- h_backlog[-(1:chunk_size),]

plot(c(0,100), c(0,100), type="n")
for( i in 1:chunk_size) with(chunk[i, ], { rect(x-1, y-1, x, y) } )
# for( i in 1:nhect) with(chunk[i, ], {
#     rect(x-1, y-1, x, y)
#     text(x, y, i, pos=4) } )

#evaluate the holdout
t0 <- proc.time()
h_stats <- data.frame()
submit_rows <- integer()
val_rows <- integer()
setkey(result_ho, row_id)
setkey(result_full, row_id)

grpx <- chunk %>% count(x)
chunk <- chunk %>% arrange (x)
ncum <- 0
ih <- 0
for( i in 1:nrow(grpx)) {  
    ix <- chunk[ih+1, ]$x

    xmin <- (ix - 1) / 10.
    tstx <- df_test %>% filter( x >= xmin, x <= xmin + 0.1 )
    valx <- val_ho %>% filter( x >= xmin, x <= xmin + 0.1 )
    
    ny <- grpx[i,]$n
    ncum <- ncum + ny
    cat ( sprintf('processing ix=%d (%d hectares / %d)', i, ny, ncum)) 
    for ( j in 1:ny ) {                 cat('.')
        ih <- ih + 1
        iy <- chunk[ih, ]$y
        ymin <- (iy - 1) / 10.
        tst <- tstx %>% filter( y >= ymin, y <= ymin + 0.1 )
        val <- valx %>% filter( y >= ymin, y <= ymin + 0.1 )
        
        submit_rows <- c(submit_rows, tst$row_id )
        val_rows <- c(val_rows, val$row_id )
    }
    tx <- proc.time(); print((tx-t0)[3])
}
t1 <- proc.time()
print((t1-t0)[3])
#h_stats %>% summarize ( mean_SC = mean(score_sc), sd_SC = sd(score_sc))

val_rows <- unique(val_rows)
submit_rows <- unique(submit_rows)

res_calc_ho <- result_ho[ row_id %in% val_rows]
blanks <- setdiff( result_ho$row_id, val_rows) 
res_calc_ho <- rbind(res_calc_ho, data.frame( row_id=blanks, place_id = "bogus bogus2 bogus3"))
setkey(res_calc_ho, row_id)
res_calc_ho <- res_calc_ho[val_ho, .(row_id, predictions=place_id, truth=i.place_id)]
score_sc <- calculate_map_score( res_calc_ho )

#local validation
lv_rows_frac <- length(val_rows) / nrow(result_ho)
lv_score_proj <- lv_ho * lv_rows_frac
print(c(lv_rows_frac, score_sc, lv_score_proj))

lb_proj <- LB_full * length(submit_rows) / nrow(result_full)
print(lb_proj)

submit_frac <- result_full[ row_id %in% submit_rows]
blanks <- setdiff( result_full$row_id, submit_rows) 
submit_frac <- rbind(submit_frac, data.frame( row_id=blanks, place_id = ""))
submit_name <- sprintf("../submissions/sidebar_hactare_analysis_%s.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"))
write.csv(submit_frac, file=submit_name, row.names=FALSE) # 0.05124 (LB) vs 0.05112 (lb_proj)

t2 <- proc.time()
print((t2-t0)[3])

