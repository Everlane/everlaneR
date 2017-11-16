# Purpose of this script is to drop a table in redshift if table exists
  drop_redshift_table <- function(table_name) {
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
  query <- paste("DROP TABLE if exists", table_name, ";")
  dbSendQuery(redshift,query)

}
