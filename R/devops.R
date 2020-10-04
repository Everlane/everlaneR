# credential and connection management for everlane data
#
# basic tasks:
# - store and manage aws credentials
# - store and manage redshift credentials
# - connect to redshift db
#

#' Retrieve AWS credentials
#'
#' Read AWS credentials from local ~/.aws/ directory and store as list. Requires a properly formatted credentials file.
#' @return credentials list containing access key and secret key
#'
#' @examples
#' creds <- get_aws_credentials()
#' get_bucket(
#'   bucket = 'everlane-data-deploy',
#'   key = aws_creds[["aws_access_key_id"]],
#'   secret = aws_creds[["aws_secret_access_key"]],
#'   prefix = paste(model_name, component, version, "",sep = "/"),
#'   max = Inf
#' )
get_aws_credentials <- function() {
  # reads keys from credentials file in the ~/.aws/ folder (requires user to have aws cli installed)
  aws_creds_file = 
    tryCatch({
      file(paste0(Sys.getenv("HOME"),'/.aws/','credentials'), "r")
    }, warning = function(w) {
      file(paste0("./Data/",'credentials'), "r")
    }, error = function(e) {
      file(paste0("./Data/",'credentials'), "r")
    })
  # initialize empty list to store aws credentials
  aws_creds <- list()

  i = 0
  # read in list of credentials
  while (TRUE) {
    # loop through lines in credentials files
    line = readLines(aws_creds_file, n = 1, warn = FALSE)
    if ( (length(line) == 0 ) | (i==3)) {
      break # break loop if line is empty
    }
    line <- gsub("\n", "", gsub(" ", "", line)) # basic scrubbing of newline character and empty spaces

    # create key-value element in list for credential in current line
    # key is character before '=', value is character after '='
    aws_creds[[substr(line, 1, regexpr('=', line)[1]- 1)]] <- substr(line, regexpr('=', line)[1] + 1, nchar(line))
  }
  close(aws_creds_file)
  return(aws_creds)
}

#' Retrieve Redshift credentials
#'
#' Read Redshift credentials from local ~/.aws/ directory and store as list. Requires a properly formatted redshift file.
#' @return credentials list containing host, dbname, user, password, and port
#'
#' @examples
#' drv <- dbDriver("PostgreSQL")
#' connection_details <- get_redshift_credentials()
#' dbConnect(drv,
#'           host = connection_details['host'],
#'           dbname = connection_details['dbname'],
#'           user = connection_details['user'],
#'           password = connection_details['password'],
#'           port = connection_details['port'])
get_redshift_credentials <- function() {
  # reads keys from redshift file in the ~/.aws/ folder (requires user to have aws cli installed)
  redshift_creds_file = 
    tryCatch({
      file(paste0(Sys.getenv("HOME"),'/.aws/','redshift'), "r")
    }, warning = function(w) {
      file(paste0("./Data/",'redshift'), "r")
    }, error = function(e) {
      file(paste0("./Data/",'redshift'), "r")
    })  
  redshift_creds <- list()

  # loop through lines in credentials files
  while (TRUE) {
    line = readLines(redshift_creds_file, n = 1, warn = FALSE)
    if ( length(line) == 0 ) {
      break # break loop if line is empty
    }
    line <- gsub("\n", "", gsub(" ", "", line)) # basic scrubbing of newline character and empty spaces

    # create key-value element in list for credential in current line
    # key is character before '=', value is character after '='
    redshift_creds[[substr(line, 1, regexpr('=', line)[1]- 1)]] <- substr(line, regexpr('=', line)[1] + 1, nchar(line))
  }
  close(redshift_creds_file)
  return(redshift_creds)
}

#' Create Redshift connection object
#' Read credentials from redshift file, and return valid connection object.
#' @return database connection object
#'
#' @examples
#' redshift_conn <- create_redshift_con()
create_redshift_con <- function() {
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  connection_details <- get_redshift_credentials() # create list containing redshift credentials

  # connect to redshift using the dbConnect function from the RPostgreSQL package
  return(
    dbConnect(drv, host = connection_details['host'],
              dbname = connection_details['dbname'],
              user = connection_details['user'], password = connection_details['password'],
              port = connection_details['port'])
  )

}

#' Disconnect all connections to Redshift
dbDisconnectAll <- function() {
  all_cons <- dbListConnections(PostgreSQL())

  for (con in all_cons) {
    if (length(dbListResults(con)) > 0) {
      dbClearResult(dbListResults(con)[[1]])
    }
    dbDisconnect(con)
  }

  message("All connections disconnected.")
}

#' Read generic credentials list from file
#' construct function that grabs credentials from any api -- api creds must always have api name in front of credentials
#' @param api string representing api name- must match prefix of relevant elements in env file
#'
#' @examples
#' sailthru_creds <- get_credentials('sailthru') # credentials in env file need to be formatted like sailthru_access_key
get_credentials <- function(api) {
  # reads keys from credentials file in the ~/.aws/ folder (requires user to have aws cli installed)
  creds_file = file(paste0(Sys.getenv("HOME"),'/.aws/','env'), "r")

  # initialize empty list to store aws credentials
  creds <- list()

  # read in list of credentials
  while (TRUE) {
    # loop through lines in credentials files
    line = readLines(creds_file, n = 1, warn = FALSE)
    if ( length(line) == 0 ) {
      break # break loop if line is empty
    }
    line <- gsub("\n", "", gsub(" ", "", line)) # basic scrubbing of newline character and empty spaces

    if (!grepl(api, line)) {
      next
    }

    # create key-value element in list for credential in current line
    # key is character before '=', value is character after '='
    creds[[substr(line, 1, regexpr('=', line)[1]- 1)]] <- substr(line, regexpr('=', line)[1] + 1, nchar(line))
  }
  close(creds_file)
  return(creds)
}



