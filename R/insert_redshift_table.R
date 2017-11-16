# Purpose of this script is to insert a redshift table into another redshift table
insert_redshift_table <- function(table_name, insert_query) {
  # Install package dependency
  require(RPostgreSQL)

  # Install Redshift Connection
  all_cons <- dbListConnections(PostgreSQL())

  for (con in all_cons) {
    if (length(dbListResults(con)) > 0) {
      dbClearResult(dbListResults(con)[[1]])
    }
    dbDisconnect(con)
  }

  redshift <- create_redshift_con()

  # Upload new email lookup table to S3
  query <- paste("INSERT INTO", table_name2, ";",
                 insert_query)
  dbSendQuery(redshift,query)

}
