library(readxl)
library(dplyr)
library(RSQLite)

#source("settings.R")

posters_df_xls <- data.frame(readxl::read_xlsx(path = "data/Posters.xlsx"))

#posters_df_xls <- posters_df_xls %>% select(-one_of(c("ON_TYPE")))

n_rows <- dim(posters_df_xls)[1]
n_cols <- dim(posters_df_xls)[2]

#Delete old version
rm(posters_df)
unlink("data/posters.RData")
unlink("data/posters.sqlite3")
#Empty df for results
posters_df <- data.frame(matrix(ncol = n_cols, nrow = 0, data = NA))

#Get distinct keys
distinct_mkey <- dplyr::distinct(posters_df_xls, MKEY)
distinct_mkey <- sort(distinct_mkey[, 1])




for (i in 1:length(distinct_mkey)){
  #Get all rows for this key
  datarow <- dplyr::filter(posters_df_xls, MKEY == distinct_mkey[i])
  for (j in 1:n_cols){
    posters_df[i, j] <- paste0(unique(datarow[, j]), collapse = ";")
  }
}

#Set variable names
names(posters_df) <- names(posters_df_xls)

#save(x = posters_df, file = "data/posters.RData")

database_file <- "data/posters.sqlite3"

db <- dbConnect(RSQLite::SQLite(), database_file)

dbWriteTable(db, "posters", posters_df)

n <- dbExecute(db, 'CREATE INDEX p_ID_NUMBER ON posters(ID_NUMBER);')
n <- dbExecute(db, 'CREATE INDEX p_OTHER_NUMBER ON posters(OTHER_NUMBER);')
n <- dbExecute(db, 'CREATE INDEX p_TITLE ON posters(TITLE);')
n <- dbExecute(db, 'CREATE INDEX p_TITLE_1 ON posters(TITLE_1);')
n <- dbExecute(db, 'CREATE INDEX p_ITEM_NAME ON posters(ITEM_NAME);')
n <- dbExecute(db, 'CREATE INDEX p_ITEM_NAME_1 ON posters(ITEM_NAME_1);')
n <- dbExecute(db, 'CREATE INDEX p_INSCRIPTION_TEXT ON posters(INSCRIPTION_TEXT);')
n <- dbExecute(db, 'CREATE INDEX p_NOTE ON posters(NOTE);')
n <- dbExecute(db, 'CREATE INDEX p_DESCRIPTION ON posters(DESCRIPTION);')
n <- dbExecute(db, 'CREATE INDEX p_DESCRIPTION_1 ON posters(DESCRIPTION_1);')
n <- dbExecute(db, 'CREATE INDEX p_MKEY ON posters(MKEY);')

dbDisconnect(db)
