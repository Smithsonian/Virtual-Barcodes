library(readxl)
library(dplyr)
library(RSQLite)

database_file <- "data/posters.sqlite3"

posters_df_xls <- data.frame(readxl::read_xlsx(path = "data/Posters.xlsx"))

n_rows <- dim(posters_df_xls)[1]
n_cols <- dim(posters_df_xls)[2]

#Empty df for results
posters_df <- data.frame(matrix(ncol = n_cols, nrow = 0, data = NA))
 
# #Get distinct keys
distinct_mkey <- dplyr::distinct(posters_df_xls, MKEY)
distinct_mkey <- sort(distinct_mkey[, 1])

for (i in 1:length(distinct_mkey)){
  #Get all rows for this key
  datarow <- dplyr::filter(posters_df_xls, MKEY == distinct_mkey[i])
  for (j in 1:n_cols){
    posters_df[i, j] <- paste0(unique(datarow[, j]), collapse = "; ")
  }
}

#Set variable names
names(posters_df) <- names(posters_df_xls)

db <- dbConnect(RSQLite::SQLite(), database_file)

dbWriteTable(db, "posters_new", posters_df)

n <- dbExecute(db, 'DELETE FROM posters WHERE TAKEN = 0;')
n <- dbExecute(db, 'DELETE FROM posters_new WHERE MKEY IN (SELECT MKEY FROM posters);')
n <- dbExecute(db, paste0('INSERT INTO posters (', paste(names(posters_df_xls), collapse = ", "), ', TAKEN) SELECT ', paste(names(posters_df_xls), collapse = ", "), ', 0 as TAKEN FROM posters_new;'))
n <- dbExecute(db, 'VACUUM;')

dbDisconnect(db)
