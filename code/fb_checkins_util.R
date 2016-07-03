library(stringr)
library(dplyr)
library(tidyr)
library(data.table)
library(lubridate)
library(RANN.L1)

EOL = "\n"

normalize <- function(x) (x - min(x)) / diff(range(x))

xgb_importance <- list()
hp_classify_xgb <- function(trn, val, min_occ=2, verbose=0, importance = FALSE) {
    trn <- create_features(trn)
    val <- create_features(val)
    
    places <- trn %>% count(place_id, sort=TRUE) %>% filter(n >= min_occ) %>% .[[1]]
    trn <- trn %>% filter(place_id %in% places)
    trn$place_id <- as.factor(trn$place_id)
    xgb_params$num_class <- length(places)
    
    xx = trn %>% select(-c(row_id, place_id)) %>% as.matrix()
    yy = as.integer( trn$place_id ) - 1
    
    xgb.train <- xgb.DMatrix(xx, label = yy)
    model <- xgboost( xgb.train,
                      nrounds = xgb_nrounds,
                      params = xgb_params, verbose = verbose )
    pred <- predict( model, val %>% select(-c(row_id, place_id)) %>% as.matrix() )
    
    #massively increases run time 
    if (importance) {
        cat ('capturing xgb_importance a (be patient)... ', EOL)
        xgb_importance <<- c(xgb_importance, list(xgb.importance( colnames(xx), model=model )))
    }
    
    top3 <- predict( model, xgb.DMatrix(val %>% select(-c(row_id, place_id)) %>% as.matrix() )) %>%
        top3_preds( levels(trn$place_id) ) 
    preds <- val %>% select( row_id, truth=place_id ) %>% 
        bind_cols( data.frame(predictions = apply(top3[,1:3], 1, paste, collapse=" "),
                              top3[4:6]) )
    
    return(preds)
}

if( ! exists("knn_probs")) knn_probs <- TRUE
if( ! exists("knn_norm")) knn_norm = FALSE
if( ! exists("knn_weights")) knn_weights = 1
if( ! exists("knn_k")) knn_k = 25
hp_classify_knn_RANN <- function(trn, val, min_occ=2, verbose=0, norm=knn_norm, w=knn_weights) {
    # uses RANN.L1::nn (manhattan distance)
    #w is a constant or vector of length ncol(trn2)-2 to multiply features by
    trn2 <- create_features(trn)
    val2 <- create_features(val)
    
    places <- trn2 %>% count(place_id, sort=TRUE) %>% filter(n >= min_occ) %>% .[[1]]
    trn2 <- trn2 %>% filter(place_id %in% places)
    trn2.place_id <- as.factor(trn2$place_id)
    
    if (norm) {
        trn2 <- apply( trn2 %>% select(-c(row_id, place_id)), 2 , normalize)
        val2 <- apply( val2 %>% select(-c(row_id, place_id)), 2 , normalize)
        
        constants <- c( which( is.nan(trn2[1, ])), which( is.nan(val2[1, ])) ) %>% unique
        if (length(constants > 0)) {
            trn2 <- trn2[, -constants]
            val2 <- val2[, -constants]
            w    <- w   [  -constants]
        }
    } else {
        trn2 <- apply( trn2 %>% select(-c(row_id, place_id)), 2 , identity)
        val2 <- apply( val2 %>% select(-c(row_id, place_id)), 2 , identity)
    }
    
    #RANN.L1  (manhattan distances)
    nn <- nn2( t(t( trn2 ) * w),  #train
               t(t( val2 ) * w),  #test
               k = knn_k )
    
    top3_places <- apply(nn$nn.idx, 1, top3_knn, trn2.place_id ) %>% t() %>% as.data.frame %>% tbl_df()
    
    preds <- val %>% select( row_id, truth=place_id)
    preds$predictions <- with(top3_places, paste(V1,V2,V3))
    
    if (knn_probs == TRUE) {
        top3_probs  <- apply(nn$nn.idx, 1, top3_knn, trn2.place_id, prob=TRUE ) %>% t() %>% as.data.frame %>% tbl_df()
        names(top3_probs) <- c('X1', 'X2', 'X3')
        preds <- cbind(preds, top3_probs)
    }
    
    return(preds)
}

top3_knn <- function( nn.index.row, place_ids, prob=FALSE ) {
    top3_places <- data.frame( place_id = place_ids[ nn.index.row ]) %>% 
        count(place_id, sort=TRUE) %>% 
        mutate( prob = n/sum(n)) %>% 
        dplyr::slice(1:3)
    if (prob) {
        out <- top3_places$prob
    } else {
        out <- top3_places$place_id %>% as.character()
    }
    if ( length(out) < 3) {
        fill = ifelse( prob, 0, levels(place_ids)[1])
        out <- c(out, rep(fill, 3 - length(out)))
    }
    return(out)
}


global_features <- function(dt) {
    
    time0 <- ymd_hms('2014-01-01 00:00:00')
    dt$ltime <- dt[, time0 + minutes(time) ]
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    
    dt <- dt[,
             c(.SD, .(
                 abs_hr = as.integer(floor(time/60)), 
                 hour = hour(ltime),
                 weekday = wday(ltime),
                 mday = day(ltime),
                 month = month(ltime),
                 year = (year(ltime) - 2013),
                 quarter_period_of_day = as.integer(hour(ltime) / 6),
                 rating_history= log10(3+month(ltime)) )) ]
    
    n_this_hr <-  dt[, .(g_n_this_hr = .N), abs_hr]
    n_this_hr[, g_hr_chg := c(0, diff( g_n_this_hr)) / g_n_this_hr]
    n_this_hr[, g_n_this_hr := NULL ]
    
    #JOIN n_this_hr to fea
    setkey(dt, abs_hr); setkey(n_this_hr, abs_hr); dt <- dt[n_this_hr]
    
    setkey(dt, time)
    #dt$g_time_diff <- c(0, diff(dt$time))
    
    #remove time absolute time vars (except original)
    dt[, ltime := NULL]
    
    return(dt)
}

create_features_fast <- function(dt) {
    # alters dt in the passing object.  use _safe version to avoid that
    
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    
    n_this_hr <- dt[, .(n_this_hr = .N), abs_hr]
    
    setkey(dt, abs_hr); setkey(n_this_hr, abs_hr); dt <- dt[n_this_hr]
    
    setkey(dt, time)
    dt[, time_diff := c(0, diff(time)) ]
    dt[, time := NULL]
    dt[, abs_hr := NULL]
    
    dt[, rat_hr_chg := n_this_hr * g_hr_chg]
    
    return(dt)
}

create_features_safe <- function(dt) {
    
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    
    n_this_hr <- dt[, .(n_this_hr = .N), abs_hr]
    
    setkey(dt, abs_hr); setkey(n_this_hr, abs_hr); dt <- dt[n_this_hr]
    
    setkey(dt, time)
    dt$time_diff <- c(0, diff(dt$time))
    dt$time <- NULL
    dt$abs_hr <- NULL
    
    dt$rat_hr_chg <- dt$n_this_hr * dt$g_hr_chg
    
    return(dt)
}
create_features <- create_features_safe

top3_preds <- function (pred, place_ids) {
    predictions <- as.data.frame(matrix(pred, ncol=length(place_ids), byrow=TRUE ))
    colnames(predictions) <- place_ids
    
    pred3 <- predictions %>% apply(1, function(x) names( sort( desc(x))[1:3])) %>%
        as.vector() %>% matrix(ncol=3, byrow=TRUE) %>% data.frame() 
    prob3 <- predictions %>% apply(1, function(x)        sort( desc(x))[1:3])  %>%
        as.vector() %>% matrix(ncol=3, byrow=TRUE) %>% data.frame() 
    cbind( pred3, -prob3)
}

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

construct_preds <- function(result, train) {
    # result = results data.table (submissions format)
    # train = train dataset data.table with place_ids corresponding to row_ids in result
    result[,.(row_id, predictions=place_id)][df_train, nomatch=0][,.(row_id, predictions, truth=place_id)]
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

if (! exists("tcheck.print")) tcheck.print = FALSE
if (! exists("tcheck.df")) tcheck.df <- data.frame( stringsAsFactors = FALSE)
tcheck.default_string <- function() sprintf( "t=%d", nrow(tcheck.df))
tcheck.tx <- list( proc.time()) 
tcheck <- function(t=1, desc = tcheck.default_string() ) {
    # t=0 to reset counter, t=1 incremental time output,  t=n time difference from n intervals
    #
    # use:
    # tcheck(0) #reset the counter
    # <computation 1>
    # tcheck()
    # <computation 2>
    # tcheck()
    # tcheck(2)  # for total time
    #
    t <- min( t, length(tcheck.tx))
    pt <- proc.time()
    if (t == 0) { 
        tcheck.tx <<- list( proc.time()) 
        tcheck.df <<- data.frame( elapsed = pt[3], desc = desc,stringsAsFactors = FALSE )
    } else {
        tcheck.tx <<- c( tcheck.tx, list(pt))
        tcheck.df <<- rbind( tcheck.df, data.frame( elapsed = pt[3], desc = desc, stringsAsFactors = FALSE ) )
        tn <- nrow( tcheck.df )
        elapsed_delta <- diff( tcheck.df[ c(tn-t, tn),]$elapsed )
        out_str <- ifelse ( t == 1
                            , sprintf("%f elapsed for %s", elapsed_delta
                                      , tcheck.df[tn, "desc"] )
                            , sprintf("%f elapsed from %s:%s", elapsed_delta
                                      , tcheck.df[tn, "desc"], tcheck.df[tn-t, "desc"]) )
        if (tcheck.print) print( out_str)
        return( out_str )
        #         tn <- length(tcheck.tx)
        #         print ( tcheck.tx[[tn]] - tcheck.tx[[tn-t]]) 
    }
}
get_tcheck <- function() tcheck.df %>% mutate( delta=c( 0, diff(elapsed)) ) %>% select( desc, delta)
