# Purpose of this script is to drop a table in redshift if table exists
  drop_table <- function(connection, table_name) {
# Install package dependency
  require(RPostgreSQL)

# Upload new email lookup table to S3
  query <- paste("DROP TABLE if exists", table_name, ";")
  dbSendQuery(connection,query)

}
