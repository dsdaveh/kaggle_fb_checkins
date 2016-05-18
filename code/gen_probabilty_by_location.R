library(stats)
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)

read_class <- c("integer", "numeric", "numeric", "integer", "integer", "character")
if (! exists("df_train")) df_train <- fread('../input/train.csv', colClasses = read_class)

#full dataset takes approx 6+ hours (225 sec x 100).  Sampled dataset scale down linearly.
fsize = 1
df_samp <- df_train 
if (fsize < 1) df_samp <- df_all %>% sample_frac(size=fsize)

# this allows a name to be passed in or a unique one generated
if (! exists("dname")) dname <- sprintf("../data/hectare_prob_loc_%dpct.RData", round(fsize * 100))

hp_loc <- data.frame()  #hectare probability by location
cutoff <- 3 # number of top probability place_id to keep
t0 <- proc.time()
for (i in 1:100) {
    for (j in 1:100) {
        hdata <- df_samp %>% filter( x >= (i-1)/10, x < i/10,
                                      y >= (j-1)/10, y < j/10) 
        
        n_places <- length(unique(hdata$place_id))
        ncut <- min(cutoff, n_places)
        
        hprob <- hdata %>%
            count(place_id, sort=TRUE) %>% 
            mutate(P = n / sum(n)) %>%
            head(ncut) %>% 
            mutate(  rank = 1:ncut) 
        
        if(ncut < 3 )  hprob <- hprob %>% bind_rows( hprob[1,] %>% mutate( rank = 3))
        if(ncut < 2 )  hprob <- hprob %>% bind_rows( hprob[1,] %>% mutate( rank = 2)) %>%
            mutate(P = n / sum(n))
        
        top_places <- hprob %>% 
            mutate( place_rank = paste0("place", rank)) %>%
            dplyr::select(place_id, place_rank) %>%
            tidyr::spread(place_rank, place_id)
        
        top_probs <- hprob %>% 
            mutate( prob_rank = paste0("P", rank)) %>%
            dplyr::select(P, prob_rank) %>%
            tidyr::spread(prob_rank, P)

        hp_loc <- rbind( hp_loc,
                         data.frame( h_x = i, h_y = j, 
                                     P3cum = sum(hprob$P),
                                     n_places = n_places) %>%
                             bind_cols( top_places, top_probs) )

#         hprob %>%
#             mutate(place_id = as.factor(place_id)) %>%
#             ggplot(aes(1:cutoff, P)) + geom_line() + 
#             geom_text(aes(label=place_id), angle=45, vjust="inward",hjust="inward") +
#             ggtitle(sprintf( 'Probabilty of place_id based on location for hectare(%d,%d)', i, j)) 
    
    }
    t1 <- proc.time()
    print(sprintf("%d hectares processed. Last %d took %f secs", nrow(hp_loc), j, (t1-t0)[3]))
    t0 <- t1
}

save(hp_loc, file=dname)
rm(dname)  #to avoid accidently overwriting
