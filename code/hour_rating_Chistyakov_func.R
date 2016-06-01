# Taken from : R - Try to add hour_rating 0.51038
# fork 3 for parameter optimization
# https://www.kaggle.com/chistyakov/facebook-v-predicting-check-ins/r-script-try-to-add-hour-rating/run/248680
# The idea of data transformation and raiting calculations is from Python script 
# by 'ZFTurbo: https://kaggle.com/zfturbo'

library(dplyr) 
library(readr) 
library(foreach)
library(data.table)

# code below is nearly verbatim of original script by Christyakov, just set to a function
pred_by_hour_rating <- function( train, test, new_dim_x1=290, rat_xy=290/725, rat_12=290/145) {
    #########################################################################
    # rat_xy <- 290/725
    # rat_12 <- 290/145
    # new_dim_x1  <- 290     # new dimensions for x 
    new_dim_y1  <- floor( new_dim_x1 / rat_xy )    # new dimensions for y
    new_dim_x2  <- floor( new_dim_x1 / rat_12 )    # new dimensions for x 
    new_dim_y2  <- floor( new_dim_x2 / rat_xy )    # new dimensions for y
    chunk_size  <- 1000000  # for memory usage optimization only
    #########################################################################
    
    
    #train <- fread("../input/train.csv", integer64 = "character")
    #test  <- fread("../input/test.csv", integer64 = "character")
    
    train <- train[,
                   .(row_id,
                     x1 = as.integer(floor(x/10 * new_dim_x1)),
                     y1 = as.integer(floor(y/10 * new_dim_y1)),
                     x2 = as.integer(floor(x/10 * new_dim_x2)),
                     y2 = as.integer(floor(y/10 * new_dim_y2)),               
                     quarter_period_of_day = as.integer(floor((time + 120) / (6*60)) %% 4),
                     hour2 = as.integer(floor(time/120) %% 12),
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
                   quarter_period_of_day = as.integer(floor((time + 120) / (6*60)) %% 4),
                   hour2 = as.integer(floor(time/120) %% 12)),
                 ]
    
    # Train group 2
    train_group2  <- train[,.(rating=.N, max_time=max(time)), by=.(x1, y1, place_id)] 
    setorder(train_group2,x1,y1, -rating, -max_time)
    train_group2 <- train_group2[,.(place_id=head(place_id, n = 3)),by=.(x1, y1)]
    
    # Train group 3
    train_group3  <- train[,.(rating=.N, max_time=max(time)), by=.(x2, y2, place_id)] 
    setorder(train_group3,x2,y2, -rating, -max_time)
    train_group3 <- train_group3[,.(place_id=head(place_id, n = 3)),by=.(x2, y2)]
    
    test_chunks <- split(test, test$hour2)
    result <- foreach(chunk=1:length(test_chunks), .combine="rbind", .packages = "dplyr") %do% 
    {
        print(sprintf("task %d/%d", chunk, length(test_chunks)))
        test_chunk <- test_chunks[[chunk]]
        hour2_test <- test_chunk$hour2[1] 
        
        #####################################################
        train[,rating_hour:= 1 / ((abs(hour2 - hour2_test) + 1)^(1/2) ) * rating,]
        
        # Train group 1
        train_group1  <- train[,.(rating=sum(rating_hour)), by=.(x1, y1, quarter_period_of_day, place_id)] 
        setorder(train_group1,-rating)
        train_group1 <- train_group1[,.(place_id=head(place_id, n = 3)), by=.(x1, y1, quarter_period_of_day)]
        
        # Join 1  
        test_train_join1 <- inner_join(select(test_chunk, row_id, x1, y1, quarter_period_of_day),
                                       train_group1,
                                       by = c("x1", "y1", "quarter_period_of_day")) %>% select(row_id, place_id)
        
        validate_test_train_join1 <- test_train_join1[,.(count=.N),by=(row_id)]
        validate_test_train_join1 <- validate_test_train_join1[count==3]
        test_chunk <- anti_join(test_chunk, validate_test_train_join1, by = "row_id")
        #print(sprintf("Join1"))
        
        # Join 2
        test_train_join2 <- inner_join(select(test_chunk, row_id, x1, y1),
                                       train_group2,
                                       by = c("x1", "y1")) %>% select(row_id, place_id)
        
        validate_test_train_join2 <- test_train_join2[,.(count=.N),by=(row_id)]
        validate_test_train_join2 <- validate_test_train_join2[count==3]
        test_chunk <- anti_join(test_chunk, validate_test_train_join2, by = "row_id")
        #print(sprintf("Join2"))
        
        # Join 3
        test_train_join3 <- left_join(select(test_chunk, row_id, x2, y2),
                                      train_group3,
                                      by = c("x2", "y2")) %>% select(row_id, place_id)
        #print(sprintf("Join3"))
        
        # Group all joins
        test_train_join_all <- rbindlist(list(test_train_join1,test_train_join2,test_train_join3), use.names=TRUE) %>% 
            unique()
        
        result_new <- test_train_join_all[, .(place_id = paste(head(place_id, 3),collapse=" ")), by = row_id]
        print(sprintf("Group all"))
        return(result_new)
    }
    
    result$place_id[result$place_id=="NA"] <- ""
    return(result)
}
#write_csv(result, "result.csv")
#write_csv(result, "../submissions/hour_rating_Chistyakov_fork2_result.csv")

