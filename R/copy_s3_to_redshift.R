copy_s3_to_redshift <- function(connection, object_name, object_type = ".csv", table_name, bucket_name = "everlane-data") {

# Purpose of this script is to allow easy creation of
# Install package dependency
  require(aws.s3)
  require(RPostgreSQL)

# Get aws creds
  aws_creds <- get_aws_credentials()   # aws_creds object

# Upload new email lookup table to S3
  query <- paste0("COPY ", table_name, " from 's3://", paste(c(bucket_name, object_name), collapse = "/"), "'
                  credentials 'aws_access_key_id=", aws_creds['aws_access_key_id'],";aws_secret_access_key=",
                  aws_creds['aws_secret_access_key'], "'
                  IGNOREHEADER 1
                  ACCEPTINVCHARS
                  REMOVEQUOTES
                  delimiter \'", ifelse(object_type == ".csv", ",", ifelse(object_type == ".txt", "\\t", ",")), "\'
                  region 'us-east-1'
                  NULL AS 'NA'
                  ;")
  dbSendQuery(connection,query)

  cat(paste("Upload", object_name, "to Redshift: SUCCESS \n"))
}
