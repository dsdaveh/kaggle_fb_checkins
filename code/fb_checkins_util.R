# predict place_id's based on location (hectare)
# hp = data frame with top3 place id's for each hectare 
# loc = input data to predict
# returns 
pred_by_hectare <- function(hp, loc) {
    loc %>% tbl_df %>%
        mutate( hectare = sprintf("%d,%d", hectare_coord(x), hectare_coord(y)) ) %>%
        left_join( hp %>% mutate( hectare = paste0(h_x, ',', h_y), by=hectare)) %>%
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