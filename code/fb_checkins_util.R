library(stringr)
library(dplyr)
library(tidyr)

hp_summarize <- function(trn, i, j, min_time=0) {
    # i,j = hectare id from 1-100 in x and y, respectively
    stopifnot( i > 0 & i <= 100 & j > 0 & j <= 100)
    
    hdata <- trn %>% filter( time >= min_time,
                             x >= (i-1)/10, x <= i/10,
                             y >= (j-1)/10, y <= j/10) 
    
    n_places <- length(unique(hdata$place_id))
    ncut <- min(3, n_places)
    
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
    
    data.frame( h_x = i, h_y = j, P3cum = sum(hprob$P), n_places = n_places) %>%
        bind_cols( top_places, top_probs) 
}

calculate_map_score <- function(preds, check_dups=FALSE) {
    # preds data.frame:
    # predictions = place_id from submissions file
    # truth = correct place_id
    #     predictions      truth
    #     "7866975553 1022620911 6137188517" "7866975553"
    #     "8459113546 9477141147 9353383717" "8459113546"
    #     ...
    
    predictions.list <- str_split(preds$predictions, " ")
    not3 <- which(lapply(predictions.list, length) != 3)
    if(length(not3) > 0) {
        predictions.list <- predictions.list[-not3]
        preds <- preds[-not3]
    }
    preds.3 <- predictions.list %>% unlist() %>% matrix(ncol=3, byrow=TRUE) %>% 
        as.data.frame(stringsAsFactors=FALSE) %>% tbl_df()
    
    if(check_dups) {
        preds.3$V2[(preds.3$V2 == preds.3$V1)] <- "dup"
        preds.3$V3[(preds.3$V3 == preds.3$V1)] <- "dup"
        preds.3$V3[(preds.3$V3 == preds.3$V2)] <- "dup"
    }
    
    score <- (
        sum(preds.3$V1 == preds$truth) +
        sum(preds.3$V2 == preds$truth) / 2 +
        sum(preds.3$V3 == preds$truth) / 3
    ) / nrow(preds.3)
    return(score)
}

estimate_map_score <- function(preds, frac=0.2, n=5, seed=48) {
    # preds data.frame:
    # predictions = place_id from submissions file
    # truth = correct place_id
    #     predictions      truth
    #     "7866975553 1022620911 6137188517" "7866975553"
    #     "8459113546 9477141147 9353383717" "8459113546"
    #     ...
    # frac = sample_frac/size
    # n = number of iterations
    
    scores <- numeric(n)
    for (i in 1:n) {
        scores[i] <- calculate_map_score( sample_frac(preds, frac) )
    }
    return( data.frame( MAP=mean(scores), sd=sd(scores)) )
}
    
estimate_map_score_dep <- function(preds, size=10000, n=30, seed=48) {
    # DEPRECATED: use calculate_map_score instead
    # preds data.frame:
    # predictions = place_id from submissions file
    # truth = correct place_id
    #     predictions      truth
    #     "7866975553 1022620911 6137188517" "7866975553"
    #     "8459113546 9477141147 9353383717" "8459113546"
    #     ...
    
    # #could not get this to go with Xapply  -- takes too long to do full df
 
    #sample scores
    set.seed(seed)
    map_scores <- numeric()
    for (j in 1:n) {
        fsize <- size / nrow(preds)
        samp <- sample_frac(preds, size = fsize )
        apk_scores <- numeric(nrow(samp))
        for (i in 1:nrow(samp)) {
            apk_scores[i] <- apk(3, samp$truth[i], unlist(str_split(samp$predictions[i], " ")) )
        }
        map_scores <- c(map_scores, mean(apk_scores))
    }
    return( data.frame( MAP=mean(map_scores), sd=sd(map_scores)) )
}


hectare_coord <- function(x) {
    h <- floor(x*10) + 1
    ifelse(h > 100, 100, h)
}

# predict place_id's based on location (hectare)
# hp = data frame with top3 place id's for each hectare 
# loc = input data to predict
# returns 
pred_by_hectare <- function(hp, loc) {
    loc %>% tbl_df %>%
        mutate( hectare = sprintf("%d,%d", hectare_coord(x), hectare_coord(y)) ) %>%
        left_join( hp %>% mutate( hectare = paste0(h_x, ',', h_y), by="hectare")) %>%
        mutate( place_id = paste(place1, place2, place3)) %>%
        dplyr::select(row_id, place_id)
}

# https://github.com/benhamner/Metrics/blob/master/R/R/metrics.r#L181
apk <- function(k, actual, predicted)
{
    score <- 0.0
    cnt <- 0.0
    for (i in 1:min(k,length(predicted)))
    {
        if (predicted[i] %in% actual && !(predicted[i] %in% predicted[0:(i-1)]))
        {
            cnt <- cnt + 1
            score <- score + cnt/i 
        }
    }
    score <- score / min(length(actual), k)
    score
}

#' Compute the mean average precision at k
#'
#' This function computes the mean average precision at k
#' of two lists of sequences.
#'
#' @param k max length of predicted sequence
#' @param actual list of ground truth sets (vectors)
#' @param predicted list of predicted sequences (vectors)
#' @export
mapk <- function (k, actual, predicted)
{
    if( length(actual)==0 || length(predicted)==0 ) 
    {
        return(0.0)
    }
    
    scores <- rep(0, length(actual))
    for (i in 1:length(scores))
    {
        scores[i] <- apk(k, actual[[i]], predicted[[i]])
    }
    score <- mean(scores)
    score
}

checkEqualsNumeric <- function( a, b) ifelse(round(a,6) == round(b,6), 'PASS', 'FAIL')
test.apk <- function()
{
    checkEqualsNumeric(apk(2, 1:5, c(6,4,7,1,2)), 1/4)
    checkEqualsNumeric(apk(5, 1:5, c(1,1,1,1,1)), 0.2)
    checkEqualsNumeric(apk(20, 1:100, c(1:20,200:600)), 1)
    checkEqualsNumeric(apk(3, c(1,3), 1:5), 5/6)
    checkEqualsNumeric(apk(3, 1:3, c(1,1,1)), 1/3)
    checkEqualsNumeric(apk(3, 1:3, c(1,2,1)), 2/3)    
}

test.mapk <- function()
{
    checkEqualsNumeric(mapk(10, list(1:5,1:3), list(1:10,c(1:2,4:11,3))), 5/6)
    checkEqualsNumeric(mapk(3, list(1:4), list(1:4)), 1.0)
    checkEqualsNumeric(mapk(3, list(c(1,3,4),c(1,2,4),c(1,3)), list(1:5,1:5,1:5)), 0.685185185185185)
    checkEqualsNumeric(mapk(5, list(1:5,1:5), list(c(6,4,7,1,2),c(1,1,1,1,1))), 0.26)
    checkEqualsNumeric(mapk(3, list(c(1,3),1:3,1:3), list(1:5,c(1,1,1),c(1,2,1))), 11/18)
    
}