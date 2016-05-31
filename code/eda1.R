library(stats)
library(ggplot2)
library(data.table)
library(dplyr)

df_train <- fread('../input/train.csv', integer64 = 'character')
df_test <- fread('../input/test.csv')
set.seed(19)
trn1 <- sample_frac(df_train, size=.01)
tst1 <- sample_frac(df_test, size=.01)

trn1.place_id <- trn1$place_id
trn1$place_id <- NULL
trn1$src <- "train"
tst1$src <- "test"
d01 <- rbind(trn1, tst1)

d01 %>% ggplot(aes(x=accuracy, group=src, col=src, fill=src)) +
    geom_density(alpha=0.3)
    

d01 %>% 
    mutate( x = round(x * 10), y = round(y * 10)) %>%
    group_by( x, y) %>%
    summarise(avg_accuracy = mean(accuracy)) %>%
    ggplot( aes(x, y, fill=avg_accuracy)) +
    geom_tile()

df_train %>% 
    mutate( x = round(x * 10), y = round(y * 10)) %>%
    group_by( x, y) %>%
    filter(accuracy > 200) %>%
    summarise(avg_accuracy = max(accuracy)) %>%
    ggplot( aes(x, y, fill=avg_accuracy)) +
    geom_tile()

#Time colored by test/train
d01 %>% ggplot(aes(x=time, group=src, col=src, fill=src)) +
    geom_density(alpha=0.3)

# put them together for relative density
d01 %>% ggplot(aes(x=time)) +
    geom_density(alpha=0.3)

# use a relative time
tst_shift <- min(tst1$time)
d01 %>% mutate( time = ifelse( src == "test", time-tst_shift, time)) %>%
    ggplot(aes(x=time, group=src, col=src, fill=src)) +
    geom_density(alpha=0.3)

#use a comparable timespan
d01 %>% mutate( time = ifelse( src == "test",
                               (time-tst_shift)/diff( range( tst1$time)),
                               time/diff( range( trn1$time)))) %>%
    ggplot(aes(x=time, group=src, col=src, fill=src)) +
    geom_density(alpha=0.3)

d01 %>% 
    ggplot(aes(x = x, y = time)) + 
    geom_bin2d(bins = 50)

d01 %>% 
    ggplot(aes(x = x, y = time)) + 
    geom_bin2d(bins = 50)

d01 %>% 
    ggplot(aes(x = y, y = time)) + 
    geom_bin2d(bins = 50)

trn1 %>% bind_cols(data.frame(place_id = as.integer(trn1.place_id))) %>%
    ggplot(aes(place_id)) +
    geom_density()

print(sprintf("Correlation of place_id and x: %s", cor(df_train$place_id, df_train$x)))
print(sprintf("Correlation of place_id and y: %s", cor(df_train$place_id, df_train$y)))

#can't explain why this is sinusoidal
df_train %>% sample_frac(size=.01) %>%
    group_by(place_id) %>%
    summarise(count=n()) %>%
    ggplot(aes(count)) +
    geom_density()

#and this isnt
df_train %>% sample_frac(size=.05) %>%
    group_by(place_id) %>%
    summarise(count=n()) %>%
    ggplot(aes(count)) +
    geom_density()

place_ids <- df_train %>% sample_frac(size=.05) %>%
    group_by(place_id) %>%
    summarise(count=n()) 

library(MASS)
fitted_distr <- fitdistr(place_ids$count, "Poisson")
print(sprintf("Log Liklihood: %s", fitted_distr$loglik))

#pick a random 100m/100m quadrant
cell <- round( runif(2, 0, 10), 1)
newId <- expand.grid(LETTERS, letters) %>% 
    mutate( id = paste0(Var1, Var2)) %>% .[[3]]

trn_cell <- df_train %>% filter( x >= cell[1], x < cell[1] + 0.1,
                              y >= cell[2], y < cell[2] + 0.1) %>% 
    mutate( place_id = as.factor(place_id))

#levels(trn_cell$place_id) <- newId[1:length(levels(trn_cell$place_id))]

length(unique(trn_cell$place_id))
trn_cell %>% 
    ggplot( aes(x=place_id) ) +
    geom_bar()

cell_p <- trn_cell %>%
    count(place_id, sort=TRUE) %>% 
    mutate(P = n / sum(n)) 
cell_p %>%
    top_n(10) %>%
    ggplot(aes(place_id, n)) + geom_bar(stat="identity")
cell_p %>%
    top_n(10) %>%
    ggplot(aes(1:10, P)) + geom_line() + 
    geom_text(aes(label=place_id), angle=45, vjust="inward",hjust="inward") +
    ggtitle('Probabilty of place_id based on location') 
    
##----- starting over with trn1

## Q: what is the penalty for ignoring places of frequency n
trn1 %>% count(place_id, sort=TRUE) %>%
    rename(count=n) %>% count(count) %>%
    mutate( max_p = cumsum(n * count) / sum(n * count)) %>%
    ggplot( aes(x=count, y=max_p )) + geom_line()
plot(-diff(rev(xx$max_p)), type='p')

# A: Okay, I've basically reproduced the distribution (duh) but the answer is that omitting
# places with only 1 count means the best possible score (for the trainging set) is .9127 
# i.e a penalty of 0.087 
# 2 count => max score .786 (additional penalty of 0.126)
# 3 count => max score .6678 (added penalty of 0.118)
# for reference leaderboard #1 is .601