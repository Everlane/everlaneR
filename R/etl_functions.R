#############################################################################################################################
############################################### BUILDING BLOCK FUNCTIONS ####################################################
#############################################################################################################################

#############################################################################################################################

#' Put output of data pipeline step onto s3
#' 
#' Easily upload files to S3. Objects to call within function are working file path and object name.
#' 
#' @param file_path Full path to local filename
#' @param object_name Full path to s3 location to upload to
#' @param bucket_name Name of s3 bucket
#' 
#' @examples 
#' upload_to_s3("local_file.txt","s3_file_path.txt", bucket_name = "everlane-data")
upload_to_s3 <- function(file_path, object_name, bucket_name = "everlane-data") {

# Install package dependency
  require(aws.s3)

# Read credentials from config
  aws_creds <- get_aws_credentials()

# Copy csv from local drive to S3
  tryCatch({
    put_object(
      file = file_path,
      object = object_name,
      bucket = bucket_name,
      key = aws_creds['aws_access_key_id'],
      secret = aws_creds['aws_secret_access_key']
    )},
    warning = function(x) { return(
      list(put_object(
        file = file_path,
        object = object_name,
        bucket = bucket_name,
        key = aws_creds['aws_access_key_id'],
        secret = aws_creds['aws_secret_access_key']
      ), print(x))
    )},
    error = function(x) { return(
      list(put_object(
        file = file_path,
        object = object_name,
        bucket = bucket_name,
        key = aws_creds['aws_access_key_id'],
        secret = aws_creds['aws_secret_access_key']
      ), print(x))
    )}
  )

  cat(paste("Upload", file_path, "to S3: SUCCESS \n"))
}

#############################################################################################################################

#' Copy to Redshift from S3
#' 
#' Easily copy structured text files from S3 to Redshift.
#' 
#' @param connection Database connection object
#' @param object_name Full path to s3 location 
#' @param object_type Possible values of ".csv" and ".txt"
#' @param table_name full name of Redshift table to copy to
#' @param bucket_name name of S3 bucket 
#' 
#' @examples 
#' copy_s3_to_redshift(connection = redshift_conn, object_name = "s3_file_path.txt", object_type = ".txt", table_name = "data.test_table", bucket_name = "everlane-data")
copy_s3_to_redshift <- function(connection, object_name, object_type = ".csv", table_name, bucket_name = "everlane-data") {
  
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

#############################################################################################################################
# GRANT TABLE ACCESS FUNCTION

  # Purpose of this script is to grant access to a table in redshift if table exists
  grant_access <- function(connection, table_name) {
    # Install package dependency
    require(RPostgreSQL)

    # Upload new email lookup table to S3
    query <- paste("GRANT SELECT ON", table_name, "TO mode_read_only, chartio_read_only;")
    dbSendQuery(connection,query)

  }

#############################################################################################################################
# DROP TABLE FUNCTION

  # Purpose of this script is to drop a table in redshift if table exists
  drop_table <- function(connection, table_name) {
    # Install package dependency
    require(RPostgreSQL)

    # Upload new email lookup table to S3
    query <- paste("DROP TABLE if exists", table_name, ";")
    dbSendQuery(connection,query)

  }

#############################################################################################################################
# DELETE REDSHIFT TABLE
  # Purpose of this script is to drop a table in redshift if table exists
  delete_table <- function(connection, table_name, condition = "") {
    # Install package dependency
    require(RPostgreSQL)

    # Upload new email lookup table to S3
    query <- ifelse(condition == "", paste("TRUNCATE", table_name, ";"),
                    paste("DELETE FROM", table_name, "WHERE", condition, ";"))
    dbSendQuery(connection,query)

  }

#############################################################################################################################
# INSERT INTO TABLE FUNCTION

  # Purpose of this script is to insert a redshift table into another redshift table.
  # NOTE: source table must be properly transformed for insert since using SELECT * query.
  insert_table <- function(connection, destination_table, source_table) {
    # Install package dependency
    require(RPostgreSQL)

    # Upload new email lookup table to S3
    query <- paste("INSERT INTO ", destination_table,
                   "SELECT * FROM ", source_table)
    dbSendQuery(connection,query)

  }

#############################################################################################################################
#' Generate create table DDL from a dataframe
#' 
#' Generates SQL to create a new table from R dataframe- useful in conjuction with copying from s3 to Redshift
#' 
#' @param table_name name of table to create on Redshift
#' @param df R dataframe object
#' @return string with create table syntax, can be plugged into dbGetQuery
#' 
#' @examples 
#' redshift_conn <- create_redshift_con()
#' sql_create_statement <- generate_create_table_sql("data.activations",activation_data)
#' dbGetQuery(redshift_conn, sql_create_statement)

generate_create_table_sql <- function(table_name, df) {
  col_classes <- map_chr(df, class) # grab column classes
  max_char <- map_chr(df, function(x) { max(nchar(x), na.rm = TRUE) }) # grab max characters for each column

  # create dataframe with column metadata
  col_specification <- cbind.data.frame(col_classes, max_char)
  col_specification$column_name <- row.names(col_specification)

  col_specification <- col_specification %>%

    # conversion from factors
    mutate(col_classes = as.character(col_classes),
           max_char = as.integer(as.character(max_char))) %>%

    # for character columns, replace with varchar + the max_char + a 100 character buffer
    # for logical, use boolean
    # otherwise paste column name with the column class
    mutate(def = paste0(column_name, " ",
                        ifelse(col_classes == "character",
                               paste0("varchar(",max_char + 100,")"),
                               ifelse(col_classes == "logical",
                                      "boolean",
                                      col_classes)
                        ),
                        " \n")
    )

  # delimit with commas
  create_table_sql <- paste0("create table ",table_name , "(", paste0(col_specification$def, collapse = ","), ")")

  return(create_table_sql)
}


