#library(class)
library(dplyr)
library(data.table)
if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

source('fb_checkins_util.R')

# load(file='../data/df_train.RData'); train <- df_train
if (! exists("train")) train <- fread('../input/train.csv', integer64 = "character")
if (! exists("test")) test <- fread('../input/test.csv', integer64 = "character")

normalize <- function(x) (x - min(x)) / diff(range(x))

###  sampling and validation setup
# train <- df_train
set.seed(19)
frac <- 0.05
if (frac < 1) train <- sample_frac(train, frac)

setorder(train, time)
train_cut_ix <- as.integer(nrow(train) * .80)
val <- train[(train_cut_ix+1):nrow(train), .(
    x.norm = normalize(x),
    y.norm = normalize(y),
    acc.norm = normalize(accuracy),
    hour2.norm = as.integer(floor(time/120) %% 12) / 120
    #quarter_period_of_day.norm = as.integer(floor(time / (6*60)) %% 4) / 4 ,
    #week_day.norm = as.integer(floor(time / (24*60)) %% 7) / 7
    )]
trn <- train[1:train_cut_ix, .(
    x.norm = normalize(x),
    y.norm = normalize(y),
    acc.norm = normalize(accuracy),
    hour2.norm = as.integer(floor(time/120) %% 12) / 120 
    #quarter_period_of_day.norm = as.integer(floor(time / (6*60)) %% 4) / 4 ,
    #week_day.norm = as.integer(floor(time / (24*60)) %% 7) / 7
)]
###

t0 <- proc.time()
knn1 <- knn( trn, val, train$place_id[1:train_cut_ix], k=10)
t1 <- proc.time()
print((t1-t0)[3])

preds <- train[(train_cut_ix+1):nrow(train), .(truth = as.character(place_id))] %>%
    bind_cols( data.frame( predictions = paste(as.character(knn1), "bogus2", "bogus3" ),
                           stringsAsFactors = FALSE)) %>%
    select(predictions, truth)

score <- calculate_map_score(preds)
estimate_map_score(preds)

submit_name <- sprintf("../submissions/pred_by_knn1.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"))
#write.csv(preds, file=submit_name, row.names=FALSE)

