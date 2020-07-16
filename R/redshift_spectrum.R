# Title     : Functions to manage external tables in S3
# Created by: gisela
# Created on: 16/7/20

#' Delete data from an external table
#'
#' To delete data from an external table stored in S3, we need to follow this steps:
#' 1. Grab any partitions that match the condition of the data that needs to be deleted
#' 2. Drop the partitions in the table (ALTER DROP PARTION)
#' 3. Delete the files that correspond to that partition
#'
#' @param redshift redshift connection
#' @param condition sql query to get the partitions to be deleted
#' @param partition_field table's partition column
#' @param table table that the data needs to be deleted
#' @param bucket S3 bucket that the data is being saved to
#' @param s3_key key where the data is being saved to
#'
#' @examples
#' If we want to delete from external_marketing.browse_recommendations_filtered_x_days, partition insert_date=2019-03-13
#' https://s3.console.aws.amazon.com/s3/buckets/everlane-data-athena-marketing/browse_recommendations_filtered_x_days/insert_date%253D2019-03-13/?region=us-east-1&tab=overview
#'
#' delete_partition_external_table(
#'  redshift,
#'  condition = "select distinct insert_date from external_marketing.browse_recommendations_filtered_x_days where date >= '2019-03-13' - 7",
#'  partition_field = "insert_date",
#'  table = "external_marketing.browse_recommendations_filtered_x_days",
#'  bucket = "everlane-data-athena-marketing"
#'  s3_key = "browse_recommendations_filtered_x_days/"
#' )
#'
delete_partition_external_table <- function(redshift, condition, partition_field, table, bucket, s3_key) {
  # Get partitions to delete (Run "condition" sql that returns which partitions to delete)
  partitions_to_delete <- dbGetQuery(redshift, condition)

  for (row in 1:nrow(partitions_to_delete)){
    # Delete partitions from table (Run alter drop partition)
    query =
      sprintf(
        "
            alter table %s
            drop partition (%s = '%s');
            ",
        table,
        partition_field,
        partitions_to_delete[row, partition_field]
      )
    dbGetQuery(redshift, query)
    message("Partition ", partitions_to_delete[row, partition_field], " deleted")

    # Delete corresponding S3 keys from bucket
    # Bucket contents in dataframe
    s3_prefix <- sprintf("%s%s=%s", s3_key, partition_field, partitions_to_delete[row, partition_field])

    bucket <- get_bucket_df(bucket = bucket_name, prefix = s3_prefix)
    # Loop through file list
    for(file in bucket$Key){
      delete_object(file, bucket = bucket_name, prefix = s3_prefix)
      message("File ", file, " deleted from s3")
    }
  }
}