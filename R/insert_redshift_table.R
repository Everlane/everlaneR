# Purpose of this script is to insert a redshift table into another redshift table
insert_table <- function(connection, destination_table, source_table) {
  # Install package dependency
  require(RPostgreSQL)

  # Upload new email lookup table to S3
  query <- paste("INSERT INTO ", destination_table,
                 "SELECT * FROM ", source_table)
  dbSendQuery(connection,query)

}
