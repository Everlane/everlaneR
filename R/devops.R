get_aws_credentials <- function() {
  aws_creds_file = file(paste0(Sys.getenv("HOME"),'/.aws/','credentials'), "r")
  aws_creds <- list()
  while (TRUE) {
    line = readLines(aws_creds_file, n = 1, warn = FALSE)
    if ( length(line) == 0 ) {
      break
    }
    line <- gsub("\n", "", gsub(" ", "", line))
    aws_creds[[substr(line, 1, regexpr('=', line)[1]- 1)]] <- substr(line, regexpr('=', line)[1] + 1, nchar(line))
  }
  close(aws_creds_file)
  return(aws_creds)
}

get_redshift_credentials <- function() {
  aws_creds_file = file(paste0(Sys.getenv("HOME"),'/.aws/','redshift'), "r")
  aws_creds <- list()
  while (TRUE) {
    line = readLines(aws_creds_file, n = 1, warn = FALSE)
    if ( length(line) == 0 ) {
      break
    }
    line <- gsub("\n", "", gsub(" ", "", line))
    aws_creds[[substr(line, 1, regexpr('=', line)[1]- 1)]] <- substr(line, regexpr('=', line)[1] + 1, nchar(line))
  }
  close(aws_creds_file)
  return(aws_creds)
}

create_redshift_con <- function() {
  require(RPostgreSQL)
  drv <- dbDriver("PostgreSQL")
  connection_details <- get_redshift_credentials()
  return(
    dbConnect(drv, host = connection_details['host'],
              dbname = connection_details['dbname'],
              user = connection_details['user'], password = connection_details['password'],
              port = connection_details['port'])
  )

}
