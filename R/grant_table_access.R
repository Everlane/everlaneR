# Purpose of this script is to grant access to a table in redshift if table exists
grant_access <- function(table_name) {
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
  query <- paste("GRANT SELECT ON", table_name, "TO mode_read_only, chartio_read_only;")
  dbSendQuery(redshift,query)

}
