if (! grepl('\\/code$', getwd())) setwd('code')
stopifnot (grepl('\\/code$', getwd()))

library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(xgboost)
library(lubridate)

source('fb_checkins_util.R')

create_features <- function(dt) {
    
    time0 <- ymd_hms('2014-01-01 00:00:00')
    dt[, time:= time0 + minutes(time) ]
    if (! "place_id" %in% names(dt)) dt$place_id <- "TBD"
    
    dt[,
       .(row_id,
         x, y, accuracy,
          hour = hour(time) *4,
          weekday = wday(time) *3,
          mday = day(time) *1./22.,
          month = month(time) *2,
          year = (year(time) - 2013) *10,
          quarter_period_of_day = as.integer(hour(time) / 6),
          rating_history= log10(3+month(time)),
          place_id),
       ]
}



ichunk = 1
submit_name <- sprintf("../submissions/xgb_ha3b_chunk%d_%s.csv", ichunk, format(Sys.time(), "%Y_%m_%d_%H%M%S"))
submit_name_p <- gsub("3b_", "3b_probs_", submit_name)

source('hectare_analysis2b.R')






