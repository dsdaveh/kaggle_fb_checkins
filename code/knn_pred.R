library(class)
library(dplyr)
library(data.table)
if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

read_class <- c("integer", "numeric", "numeric", "integer", "integer", "character")
if (! exists("df_train")) df_train <- fread('../input/train.csv', colClasses = read_class)
if (! exists("df_test")) df_test <- fread('../input/test.csv')

fsize = .05
set.seed(19)
if (fsize < 1) { 
    trn <- df_train %>% sample_frac(size=fsize)
    tst <- df_test  %>% sample_frac(size=fsize)
} else {
    trn <- df_train
    tst <- df_test
}

normalize <- function(x) (x - min(x)) / diff(range(x))

trn.truth <- trn$place_id %>% as.factor()
trn$place_id <- NULL
trn.norm <- trn %>% apply(2, normalize) %>% data.frame() %>% tbl_df()
tst.norm <- tst %>% apply(2, normalize) %>% data.frame() %>% tbl_df()

t0 <- proc.time()
knn1 <- knn( trn.norm, tst.norm, trn.truth, k=1, prob=TRUE, use.all = TRUE); t1 <- proc.time()
