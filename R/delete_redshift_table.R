# Purpose of this script is to drop a table in redshift if table exists
delete_redshift_table <- function(table_name, condition = "") {
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
  query <- ifelse(condition == "", paste("TRUNCATE", table_name, ";"), paste("DELETE FROM", table_name, "WHERE", condition, ";"))
  dbSendQuery(redshift,query)

}
