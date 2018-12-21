#########################################################################################

# Goal: To send or get data from S3 based on path parameter
# Set parameter for these functions are:

# Repo : repository name
# Prefix : results folder that file will be stored in
# Project : name of project
# Method : if stage branches into multiple methodologies (i.e. models), provide method
# Stage : step in the process of project
# Version : most current version of model. format is V + order number i.e. v1, v2, v3, ...

# Note: currently, functions only take .rds files but this can change in the future

#########################################################################################

# Send data to S3 -------------------------
send_output_to_s3 <- function(object,
                              repo = "data-models",
                              prefix = prefix,
                              project = project,
                              stage = stage,
                              method = "",
                              version = version) {
  
  # Create a remote path used to save object in a remote folder 
  remote_path  <- paste0(stage, "_data.rds")
  saveRDS(object, remote_path)
  
  # Create a file path for S3
  s3_path      <- ifelse(method != "", paste(repo, prefix, project, stage, method, version, sep = "/"),
                         paste(repo, prefix, project, stage, version, sep = "/"))
  
  s3_file_name <- paste0(stage, "_data_", gsub(" ", "_", gsub(":", "", Sys.time())), ".rds")
  
  # Upload to S3
  upload_to_s3(file_path = remote_path,
               object_name = paste(s3_path, s3_file_name, sep = "/"))
}



# Download data from S3 -------------------------
get_input_from_s3 <- function(repo = "data-models", 
                              prefix = prefix,
                              project = project,
                              stage = stage, 
                              method = "",
                              version = version) {
  require(everlaneR)
  require(aws.s3)
  
  aws_creds <- get_aws_credentials()
  
  # Grab all items stored in the same S3 folder
  items <- get_bucket(bucket = 'everlane-data',  
                      key = aws_creds[["aws_access_key_id"]],
                      secret = aws_creds[["aws_secret_access_key"]],
                      prefix = ifelse(method != "", paste(repo, prefix, project, stage, method, version, sep = "/"),
                                      paste(repo, prefix, project, stage, version, sep = "/")))
  
  # Arrange by the most recently modified data
  latest_run <- data.frame(items) %>% 
    arrange(desc(LastModified))
  
  message(paste0("Downloading ", stage, "_data from ", str_split(str_split(latest_run$Key[1], pattern = "data_")[[1]][2], pattern = ".rds")[[1]][1]))
  
  # Take the first row
  latest_file <- latest_run[1,"Key"]
  
  # Pull the table from S3
  final_output <-
    s3readRDS(
      object = latest_file,
      bucket = 'everlane-data',
      key = aws_creds[["aws_access_key_id"]],
      secret = aws_creds[["aws_secret_access_key"]]
    )
  
  return(final_output)
  message(paste("Imported", nrow(final_output), "rows of data for ", stage, "phase: SUCCESS \n"))
}