library(data.table)
library(dplyr)
library(tidyr)

read_class <- c("integer", "numeric", "numeric", "integer", "integer", "character")
if (! exists("df_train")) df_train <- fread('../input/train.csv', colClasses = read_class)

set.seed(19)
nFolds <- 5

df_train.orig <- df_train
n_ix <- nrow(df_train)
ixfold <- data.frame( ix = sample(1:n_ix), kfold = rep(1:5, n_ix)[1:n_ix]) %>% tbl_df
save(ixfold, file = sprintf("../data/hectare_prob_loc_%dfold_indices.RData", nFolds))
for (k in 1:nFolds) {
    df_train <- df_train.orig[ ixfold[ ixfold$kfold == k, 1], ]
    dname <- sprintf("../data/hectare_prob_loc_k%d-%d.RData", k, nFolds)
    print(paste("creating", dname))
    source('gen_probabilty_by_location.R')
}
df_train <- df_train.orig
rm(df_train.orig)

