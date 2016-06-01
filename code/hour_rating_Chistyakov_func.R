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
                     hour = as.integer(floor(time/30) %% 48),
                     time,
                     place_id,
                     rating_history= log10(3+((time + 120.0) / (60 * 24 * 30)))),
                   ]
    
    train <- train[,
                   .(row_id, x1, y1, x2, y2,quarter_period_of_day, hour,time, place_id, rating_history,
                     hour_mean=mean(as.numeric(hour)),
                     hour_sd=sd(as.numeric(hour))),
                   by=(place_id)
                   ]
    
    test <- test[,
                 .(row_id,
                   x1 = as.integer(floor(x/10 * new_dim_x1)),
                   y1 = as.integer(floor(y/10 * new_dim_y1)),
                   x2 = as.integer(floor(x/10 * new_dim_x2)),
                   y2 = as.integer(floor(y/10 * new_dim_y2)),               
                   quarter_period_of_day = as.integer(floor((time + 120) / (6*60)) %% 4),
                   hour = as.integer(floor(time/30) %% 48)),
                 ]
    
    #print("Train group 2")
    train_group2  <- train[,.(rating=.N, max_time=max(time)), by=.(x1, y1, place_id)][order(x1,y1, -rating, -max_time)]
    train_group2 <- train_group2[,.(place_id, pos=seq_len(.N)), by=.(x1, y1)][pos<=3][,pos:=NULL]
    
    #print("Train group 3")
    train_group3  <- train[,.(rating=.N, max_time=max(time)), by=.(x2, y2, place_id)][order(x2,y2, -rating, -max_time)]
    train_group3 <- train_group3[,.(place_id, pos=seq_len(.N)), by=.(x2, y2)][pos<=3][,pos:=NULL]
    
    test_chunks <- split(test, test$hour)
    cat(sprintf("task x/%d:", length(test_chunks)))
    result <- foreach(chunk=1:length(test_chunks), .combine="rbind", .packages = "dplyr") %do% 
     {
        cat(sprintf(" %d", chunk))
        test_chunk <- test_chunks[[chunk]]
        hour_test <- test_chunk$hour[1] 
        
        #####################################################
        train[,rating:= 1 / ((abs(hour - hour_test) + 1)^(1/2) ) * rating_history,]
        #####################################################
        
        #print("Train group 1")
        train_group1  <- train[,.(rating=sum(rating)), by=.(x1, y1, quarter_period_of_day, place_id)][order(-rating)]
        train_group1 <- train_group1[,.(place_id, pos=seq_len(.N)), by=.(x1, y1, quarter_period_of_day)][pos<=3][,pos:=NULL]
        
        #print("Join1")
        setkey(test_chunk,x1, y1, quarter_period_of_day)
        setkey(train_group1,x1, y1, quarter_period_of_day)
        
        test_train_join1 <-
            test_chunk[,.(row_id, x1, y1, quarter_period_of_day)][train_group1, nomatch=0, allow.cartesian=TRUE][,.(row_id, place_id)]
        
        validate_test_train_join <- test_train_join1[,.(count=.N),by=(row_id)][count==3]
        test_chunk <- test_chunk[!validate_test_train_join, on = "row_id"]
        
        #print("Join2")
        setkey(test_chunk,x1, y1)
        setkey(train_group1,x1, y1)
        
        test_train_join2 <-
            test_chunk[,.(row_id, x1, y1)][train_group2, nomatch=0, allow.cartesian=TRUE][,.(row_id, place_id)]
        
        validate_test_train_join <- test_train_join2[,.(count=.N),by=(row_id)][count==3]
        test_chunk <- test_chunk[!validate_test_train_join, on = "row_id"]
        
        #print("Join3")
        setkey(test_chunk,x2, y2)
        setkey(train_group3,x2, y2)
        
        test_train_join3 <-
            train_group3[test_chunk[,.(row_id, x2, y2)], nomatch=NA, allow.cartesian=TRUE][,.(row_id, place_id)]
        
        #validate_test_train_join <- test_train_join3[,.(count=.N),by=(row_id)][count==3]
        #test_chunk <- test_chunk[!validate_test_train_join, on = "row_id"]
        #validate_test_train_join <- NULL
        
        #print("Group all")
        test_train_join_all <- rbindlist(list(test_train_join1,test_train_join2,test_train_join3), use.names=TRUE) %>% unique()
        
        # result_new <- test_train_join_all[, .(place_id = paste(head(place_id, 3),collapse=" ")), by = row_id]
        test_train_join_all <- test_train_join_all[, .(place_id, pos=seq_len(.N)), by = .(row_id)][pos<=3]
        test_train_join_all <- dcast.data.table(test_train_join_all, row_id~pos, value.var="place_id")
        
        #print("Done")
        
        return(test_train_join_all)
    }
    
    #print("Converting to final format")
    setorder(result, row_id)
    library(tidyr)
    result <- unite_(result, "place_id", c("1", "2", "3"), sep = " ")
    return(result)
}
#write_csv(result, "result.csv")
#write_csv(result, "../submissions/hour_rating_Chistyakov_fork2_result.csv")

