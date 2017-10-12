# Purpose: To easily upload files to S3. Objects to call within function are working file name and object name. File and object name are usually the same input but in the event that the user wants them to be different, both inputs must be made in the function.
# Note: You must set your working directory before you run this function. File and object name must have entire pathway name
upload_to_s3 <- function(file_name, object_name, bucket_name = "everlane-data") {

# Install package dependency
  require(aws.s3)

# Read credentials from config
  aws_creds <- get_aws_credentials()

# Copy csv from local drive to S3
  tryCatch({
    put_object(
      file = file_name,
      object = object_name,
      bucket = bucket_name,
      key = aws_creds['aws_access_key_id'],
      secret = aws_creds['aws_secret_access_key']
    )},
    warning = function(x) { return(
      list(put_object(
        file = file_name,
        object = object_name,
        bucket = bucket_name,
        key = aws_creds['aws_access_key_id'],
        secret = aws_creds['aws_secret_access_key']
      ), print(x))
    )},
    error = function(x) { return(
      list(put_object(
        file = file_name,
        object = object_name,
        bucket = bucket_name,
        key = aws_creds['aws_access_key_id'],
        secret = aws_creds['aws_secret_access_key']
      ), print(x))
    )}
  )

  cat(paste("Upload", file_name, "to S3: SUCCESS \n"))
}

