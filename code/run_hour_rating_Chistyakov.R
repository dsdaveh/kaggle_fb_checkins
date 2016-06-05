library(dplyr) 
library(readr) 
library(foreach)
library(data.table)
library(ggplot2)

source('fb_checkins_util.R')
source('hour_rating_Chistyakov_func.R')

#################
# local validation
# method 1: split based on time
train <-df_train
set.seed(19)

method <- 1 # 1=time based split 2=random split
if (method == 1) {
    setorder(train, time)
    train_cut_ix <- as.integer(nrow(train) * .80)
    test <- train[(train_cut_ix+1):nrow(train), ]
    train <- train[1:train_cut_ix, ]
} else {
    set.seed(19)
    test_ix <- sample(1:nrow(train), size=round(nrow(train) * 0.8), replace=FALSE)
    test <- train[test_ix, ]
    train <- train[-test_ix, ]
}
truth <- test %>% select(row_id, truth=place_id)

new_dim_x1 <- 290
rat_xy <- 290/725
rat_12 <- 290/145

t1 = proc.time()
result <- pred_by_hour_rating(train, test, new_dim_x1, rat_xy, rat_12) 

na_ix <- which( str_length(result$place_id) == 0)  # no prediction in result
if (length(na_ix) > 0) result <- result[-na_ix]  #just remove these (there usually aren't many)
preds <- result %>%
    select(row_id, predictions = place_id) %>%
    left_join( truth , by="row_id") %>%
    select(predictions, truth)

score <- calculate_map_score(preds)
print(estimate_map_score(preds))

t2 = proc.time()

print((t2-t1)[3])  # 
submit_name <- sprintf("../submissions/hr_Chistyakov_ho_%s.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"))
write.csv(result, file=submit_name, row.names=FALSE)

train <- df_train
test <- fread("../input/test.csv", integer64 = "character")
t3 = proc.time()
result <- pred_by_hour_rating(train, test, new_dim_x1, rat_xy, rat_12) 
t4 = proc.time()
print((t4-t3)[3])  # 


submit_name <- gsub( "_ho_", "_", submit_name)
write.csv(result, file=submit_name, row.names=FALSE)


