# credential and connection management for everlane data
#
# basic tasks:
# - store and manage aws credentials
# - store and manage redshift credentials
# - connect to redshift db
#

# store aws credentials- necessary for interacting with S3 and data pipelines
#############################################################################
  get_aws_credentials <- function() {
    # reads keys from credentials file in the ~/.aws/ folder (requires user to have aws cli installed)
    aws_creds_file = file(paste0(Sys.getenv("HOME"),'/.aws/','credentials'), "r")

    # initialize empty list to store aws credentials
    aws_creds <- list()

    # read in list of credentials
    while (TRUE) {
      # loop through lines in credentials files
      line = readLines(aws_creds_file, n = 1, warn = FALSE)
      if ( length(line) == 0 ) {
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

# store redshift credentials- necessary for querying redshift datastore
#############################################################################
  get_redshift_credentials <- function() {
    # reads keys from redshift file in the ~/.aws/ folder (requires user to have aws cli installed)
    redshift_creds_file = file(paste0(Sys.getenv("HOME"),'/.aws/','redshift'), "r")
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
    close(redshift_creds)
    return(redshift_creds)
  }

# construct redshift connection string and connect to redshift db
#############################################################################
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
