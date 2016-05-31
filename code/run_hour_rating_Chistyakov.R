library(dplyr) 
library(readr) 
library(foreach)
library(data.table)

source('fb_checkins_util.R')

run_script <- 'hour_rating_Chistyakov_fork2.R'
submit_name <- sprintf("../submissions/hr_Chistyakov_f2_%s.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"))
if (! exists("WRITE_SUBMISSION") ) WRITE_SUBMISSION = FALSE

#load(file=df_train)
#train <- fread("../input/train.csv", integer64 = "character")
test <- fread("../input/test.csv", integer64 = "character")

t0 = proc.time()
source(run_script)
t1 = proc.time()
print((t1-t0)[3])
if (exists ("WRITE_SUBMISSION")) if (WRITE_SUBMISSION) write.csv(result, file=submit_name, row.names=FALSE)
t2 = proc.time()

#################
# local validation
# method 1: split based on time
train <-df_train
set.seed(19)
frac <- 0.05
if (frac < 1) train <- sample_frac(train, frac)

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

#source(run_script)
t2 = proc.time()
result <- pred_by_hour_rating(train, test, new_dim_x1=290, rat_xy=290/725, rat_12=290/145) 
t3 = proc.time()
print((t3-t2)[3])

na_ix <- which( str_length(result$place_id) == 0)  # no prediction in result
if (length(na_ix) > 0) result <- result[-na_ix]  #just remove these (there usually aren't many)
preds <- result %>%
    select(row_id, predictions = place_id) %>%
    left_join( truth , by="row_id") %>%
    select(predictions, truth)

score <- calculate_map_score(preds)
estimate_map_score(preds)
print(score)




