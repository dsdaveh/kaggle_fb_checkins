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
frac <- 1 #0.05
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

new_dim_x1 <- 290
rat_xy <- 290/725
rat_12 <- 290/145

setup_params <- function( params, deltas) {
    grid <- matrix( c(params, 0, 1), ncol = length(params) + 2, byrow=TRUE )
    for( i in 1:length(params)) {
        for ( j in 1:length(deltas)) {
            dparam <- c(params, i, deltas[j])
            dparam[i] <- params[i] * deltas[j]
            grid <- rbind( grid, matrix(dparam, ncol = length(params) + 2, byrow=TRUE))
        }
    }
    return(grid)
}
deltas <- c(.8, .9, 1.1, 1.2)
param_grid <- setup_params( params = c(new_dim_x1, rat_xy, rat_12), deltas = deltas )

#source(run_script)
scores <- numeric(nrow(param_grid))
t1 = proc.time()
for (i in 1:nrow(param_grid)) {
    prow <- param_grid[i, ]
    print(prow)
    t2 = proc.time()
    result <- pred_by_hour_rating(train, test, new_dim_x1=prow[1], rat_xy=prow[2], rat_12=prow[3]) 
    
    na_ix <- which( str_length(result$place_id) == 0)  # no prediction in result
    if (length(na_ix) > 0) result <- result[-na_ix]  #just remove these (there usually aren't many)
    preds <- result %>%
        select(row_id, predictions = place_id) %>%
        left_join( truth , by="row_id") %>%
        select(predictions, truth)
    
    scores[i] <- calculate_map_score(preds)
    print(estimate_map_score(preds))
    
    t3 = proc.time()
    #print((t3-t2)[3])
}
print((t3-t1)[3])  # 5839 secs @ 5%

param_labels <- c("new_dim_x", "rat_xy", "rat_12")
grid_results <- cbind(param_grid, scores) %>% as.data.frame() 
baseline <- grid_results[1, ]
grid_results <- grid_results[-1, ]
baseline <- rbind(baseline, baseline, baseline)
baseline[ ,length(param_labels)+1] <- 1:length(param_labels)
grid_results <- rbind(grid_results, baseline)
names(grid_results) <- c(param_labels, 'param', 'delta', 'scores')
grid_results$param <- as.factor(grid_results$param)
levels(grid_results$param) <- param_labels

grid_results %>% ggplot( aes(x=delta, y=scores, group=param, col=param)) +
    geom_line( size=2 ) +
    ggtitle('parameter sensitivities')

p1max <- grid_results %>% filter(param == param_labels[1]) %>% arrange(desc(scores)) %>% .[[1]] %>% first()
p2max <- grid_results %>% filter(param == param_labels[2]) %>% arrange(desc(scores)) %>% .[[2]] %>% first()
p3max <- grid_results %>% filter(param == param_labels[3]) %>% arrange(desc(scores)) %>% .[[3]] %>% first()

result <- pred_by_hour_rating(train, test, new_dim_x1=p1max, rat_xy=p2max, rat_12=p3max) 

na_ix <- which( str_length(result$place_id) == 0)  # no prediction in result
if (length(na_ix) > 0) result <- result[-na_ix]  #just remove these (there usually aren't many)
preds <- result %>%
    select(row_id, predictions = place_id) %>%
    left_join( truth , by="row_id") %>%
    select(predictions, truth)

score <- calculate_map_score(preds)
grid_results <- grid_results %>% bind_rows( data.frame(
    V1 = p1max,
    V2 = p2max,
    V3 = p3max,
    scores = score,
    param = "opt",
    iter = 0))

submit_name <- sprintf("../submissions/hr_Chistyakov_opt_%s.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"))
if (! exists("WRITE_SUBMISSION") ) WRITE_SUBMISSION = TRUE

#load(file=df_train)
#train <- fread("../input/train.csv", integer64 = "character")
# train <- df_train
# test <- fread("../input/test.csv", integer64 = "character")
#  
# t4 = proc.time()
# result <- pred_by_hour_rating(train, test, new_dim_x1=p1max, rat_xy=p2max, rat_12=p3max) 
# t5 = proc.time()
# print((t5-t4)[3])
# if (exists ("WRITE_SUBMISSION")) if (WRITE_SUBMISSION) write.csv(result, file=submit_name, row.names=FALSE)
# 

