# Purpose of this script is to drop a table in redshift if table exists
delete_table <- function(connection, table_name, condition = "") {
  # Install package dependency
  require(RPostgreSQL)

  # Upload new email lookup table to S3
  query <- ifelse(condition == "", paste("TRUNCATE", table_name, ";"),
                  paste("DELETE FROM", table_name, "WHERE", condition, ";"))
  dbSendQuery(connection,query)

}
