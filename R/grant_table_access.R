# Purpose of this script is to grant access to a table in redshift if table exists
grant_access <- function(connection, table_name) {
  # Install package dependency
  require(RPostgreSQL)

  # Upload new email lookup table to S3
  query <- paste("GRANT SELECT ON", table_name, "TO mode_read_only, chartio_read_only;")
  dbSendQuery(connection,query)

}
