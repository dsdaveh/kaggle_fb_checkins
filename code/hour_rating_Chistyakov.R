# The idea of data transformation and raiting calculations is from Python script 
# by 'ZFTurbo: https://kaggle.com/zfturbo'

#########################################################################
new_dim_x1  <- 290     # new dimensions for x 
new_dim_y1  <- 725     # new dimensions for y
new_dim_x2  <- 145     # new dimensions for x 
new_dim_y2  <- 362     # new dimensions for y
#########################################################################

library(dplyr) 
library(readr) 
library(foreach)
library(data.table)

train <- fread("../input/train.csv", integer64 = "character")
test <- fread("../input/test.csv", integer64 = "character")

train <- train[,
               .(row_id,
                 x1 = as.integer(floor(x/10 * new_dim_x1)),
                 y1 = as.integer(floor(y/10 * new_dim_y1)),
                 x2 = as.integer(floor(x/10 * new_dim_x2)),
                 y2 = as.integer(floor(y/10 * new_dim_y2)),               
                 hour = as.integer(floor(time/60) %% 24),
                 time,
                 place_id,
                 rating = log10(3+((time + 120.0) / (60 * 24 * 30)))),
               ]

test <- test[,
             .(row_id,
               x1 = as.integer(floor(x/10 * new_dim_x1)),
               y1 = as.integer(floor(y/10 * new_dim_y1)),
               x2 = as.integer(floor(x/10 * new_dim_x2)),
               y2 = as.integer(floor(y/10 * new_dim_y2)),               
               hour = as.integer(floor(time/60) %% 24)),
             ]

# Train group 2
train_group2  <- train[,.(rating=.N, max_time=max(time)), by=.(x2, y2, place_id)] 
setorder(train_group2,x2,y2, -rating, -max_time)
train_group2 <- train_group2[,.(place_id=head(place_id, n = 3)),by=.(x2, y2)]

test_chunks <- split(test, test$hour)
result <- foreach(chunk=1:length(test_chunks), .combine="rbind", .packages = "dplyr") %do% 
{
    print(sprintf("task %d/%d", chunk, length(test_chunks)))
    test_chunk <- test_chunks[[chunk]]
    hour_test <- test_chunk$hour[1] 
    
    #####################################################
    abs_hour_diff_raw <- abs(train$hour - hour_test)
    abs_hour_diff <- ifelse(abs_hour_diff_raw <= 12, abs_hour_diff_raw, 24 - abs_hour_diff_raw)
    
    train[,rating_hour:= cos(abs_hour_diff/4) * rating,]
    
    # Train group 1
    train_group1  <- train[,.(rating=sum(rating_hour), max_time=max(time)), by=.(x1, y1, place_id)]
    setorder(train_group1,x1,y1, -rating, -max_time)
    train_group1 <- train_group1[,.(place_id=head(place_id, n = 3)), by=.(x1, y1)]
    
    # Join 1  
    test_train_join1 <- inner_join(select(test_chunk, row_id, x1, y1),
                                   train_group1,
                                   by = c("x1", "y1")) %>% select(row_id, place_id)
    
    validate_test_train_join1 <- test_train_join1[,.(count=.N),by=(row_id)]
    validate_test_train_join1 <- validate_test_train_join1[count==3]
    test_chunk <- anti_join(test_chunk, validate_test_train_join1, by = "row_id")
    print(sprintf("Join1"))
    
    # Join 2
    test_train_join2 <- left_join(select(test_chunk, row_id, x2, y2),
                                  train_group2,
                                  by = c("x2", "y2")) %>% select(row_id, place_id)
    print(sprintf("Join2"))
    
    # Group all joins
    test_train_join_all <- rbindlist(list(test_train_join1,test_train_join2), use.names=TRUE) %>% 
        unique()
    
    result_new <- test_train_join_all[, .(place_id = paste(head(place_id, 3),collapse=" ")), by = row_id]
    print(sprintf("Group all"))
    
    return(result_new)
}

result$place_id[result$place_id=="NA"] <- ""
#write_csv(result, "../submissions/hour_rating_Chistyakov_result.csv")

#################
# local validation
# method 1: split based on time
train <- df_train
#method <- 1 # 1=time based split 2=random split
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
# now rerun lines 19-87 above
na_ix <- which(result$place_id == 'NA')
result <- result[-na_ix]
preds <- result %>%
    select(row_id, predictions = place_id) %>%
    left_join( truth , by="row_id") %>%
    select(predictions, truth)

#manual score
predictions.list <- str_split(preds$predictions, " ")
not3 <- which(lapply(predictions.list, length) != 3)
preds.3 <- predictions.list[-not3] %>% unlist() %>% matrix(ncol=3, byrow=TRUE) %>% 
    as.data.frame(stringsAsFactors=FALSE) %>% tbl_df()

#JIC
preds.3$V2[(preds.3$V2 == preds.3$V1)] <- "dup"
preds.3$V3[(preds.3$V3 == preds.3$V1)] <- "dup"
preds.3$V3[(preds.3$V3 == preds.3$V2)] <- "dup"

score <- (
    sum(preds.3$V1 == preds$truth[-not3]) +
    sum(preds.3$V2 == preds$truth[-not3]) / 2 +
    sum(preds.3$V3 == preds$truth[-not3]) / 3
) / nrow(preds.3)


