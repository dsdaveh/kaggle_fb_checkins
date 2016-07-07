library(data.table)
knn <- fread('~/GitHub/kaggle/Facebook_Checkins/code/KNN_submission.csv')
xgb_p <- fread('~/GitHub/kaggle/Facebook_Checkins/submissions/xgb_vga50x50_probs_chunk1_2016_06_21_061030.csv')
xgb <- fread('~/GitHub/kaggle/Facebook_Checkins/submissions/xgb_vga50x50_chunk1_2016_06_21_061030.csv')
setorder(xgb_p, X1)

nbest <- 100000
best <- xgb_p[ (nrow(xgb_p)-nbest):nrow(xgb_p), .(row_id) ]

setkey(best, row_id)
setkey(xgb, row_id)

replace <- xgb[ best , nomatch=0 ]

knn_out <- knn[ row_id %in% replace$row_id ]

both <- rbind(knn[ ! (row_id %in% replace$row_id) ], replace)
write.csv(both, file='manual_ensemble_100001.csv', row.names = FALSE)

library(dplyr)
setkey(knn_out)
knn1 <- str_split(knn_out$place_id, " ") %>% unlist() %>% matrix(ncol=3, byrow=TRUE) %>% 
    as.data.frame(stringsAsFactors=FALSE) %>% tbl_df()
rep1 <- str_split(replace$place_id, " ") %>% unlist() %>% matrix(ncol=3, byrow=TRUE) %>% 
    as.data.frame(stringsAsFactors=FALSE) %>% tbl_df()
sum(knn1$V1 == rep1$V1)/(nbest+1) # 0.9916201
