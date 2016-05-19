read_class <- c("integer", "numeric", "numeric", "integer", "integer", "character")
if (! exists("df_train")) df_train <- fread('../input/train.csv', colClasses = read_class)
if (! exists("df_test")) df_train <- fread('../input/train.csv', colClasses = read_class)

fsize = .01
df_samp <- df_train 
if (fsize < 1) df_samp <- df_all %>% sample_frac(size=fsize)
