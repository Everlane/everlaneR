# Purpose: To easily upload files to S3. Objects to call within function are working directory path, file name and objectname. File and object name are usually the same input but in the event that the user wants them to be different, both inputs must be made in the function.
upload_to_s3 <- function(directory_path, file_name, object_name) {

# Install package dependency
  require(aws.s3)

# Set up working directory path
  message(paste0("working dir:", getwd()))
  setwd(paste0(getwd(), directory_path))
  message(paste0("working dir:", getwd()))

# Read credentials from config
  aws_creds <- get_aws_credentials()

# Copy csv from local drive to S3
  tryCatch({
    put_object(
      file = file_name,
      object = object_name,
      bucket = "everlane-data",
      key = aws_creds['aws_access_key_id'],
      secret = aws_creds['aws_secret_access_key']
    )},
    warning = function(x) { return(
      list(put_object(
        file = file_name,
        object = object_name,
        bucket = "everlane-data",
        key = aws_creds['aws_access_key_id'],
        secret = aws_creds['aws_secret_access_key']
      ), print(x))
    )},
    error = function(x) { return(
      list(put_object(
        file = file_name,
        object = object_name,
        bucket = "everlane-data",
        key = aws_creds['aws_access_key_id'],
        secret = aws_creds['aws_secret_access_key']
      ), print(x))
    )}
  )

  cat(paste("Upload", file_name, "to S3: SUCCESS \n"))
}

