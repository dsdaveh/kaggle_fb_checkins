library(class)
library(dplyr)
library(data.table)
if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

read_class <- c("integer", "numeric", "numeric", "integer", "integer", "character")
if (! exists("df_train")) df_train <- fread('../input/train.csv', colClasses = read_class)
if (! exists("df_test")) df_test <- fread('../input/test.csv')

normalize <- function(x) (x - min(x)) / diff(range(x))

trn <- df_train %>% select(-c(row_id,place_id))
tst <- df_test %>% select(-row_id)
trn.truth <- df_train$place_id %>% as.factor()

trn.norm <- trn %>% apply(2, normalize) %>% data.frame() %>% tbl_df()
tst.norm <- tst %>% apply(2, normalize) %>% data.frame() %>% tbl_df()

t0 <- proc.time()
knn1 <- knn( trn.norm, tst.norm, trn.truth, k=1, use.all = TRUE); t1 <- proc.time()
print((t1-t0)[3])

preds <- df_test %>% tbl_df %>%
    bind_cols( data.frame( place_id = knn1 )) %>%
    dplyr::select(row_id, place_id)

submit_name <- sprintf("../submissions/pred_by_knn1.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"))
write.csv(preds, file=submit_name, row.names=FALSE)

