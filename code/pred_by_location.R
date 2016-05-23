library(dplyr)
library(data.table)

#us gen_probability_by_location.R to create hp_loc (or load from disk)
stopifnot( exists("hp_loc"))

if (! exists("df_test")) df_test <- fread('../input/test.csv')

t0 <- proc.time()
preds <- pred_by_hectare(hp_loc, df_test)
t1 <- proc.time(); (t1-t0)[3]

submit_name <- sprintf("../submissions/pred_by_location_%s.csv", format(Sys.time(), "%Y_%m_%d_%H%M%S"))
if (exists ("WRITE_SUBMISSION")) if (WRITE_SUBMISSION) write.csv(preds, file=submit_name, row.names=FALSE)


