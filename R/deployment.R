#' Read inputs in a data pipeline.
#' 
#' Get the correct version and run of a previous step (called input) in an Everlane data pipeline.
#' 
#' @param model_name Name of the model/pipeline.
#' @param version Version of input.
#' @param input_name File name of input.
#' @param run_time Created timestamp of input.  Defaults to latest run.  If date over timestamp is provided, will look for latest run on that day.
#' 
#' @examples 
#' get_pipeline_input("ops_forecasts_returned_units", 2, "train_dataprep.rds")
get_pipeline_input <- function(model_name, version, input_name, run_time=NULL) {
  require(aws.s3)
  require(lubridate)
  aws_creds <- get_aws_credentials()
  
  items <- 
    get_bucket(
      bucket = 'everlane-data-deploy',  
      key = aws_creds[["aws_access_key_id"]],
      secret = aws_creds[["aws_secret_access_key"]],
      prefix = paste0(model_name, "/", version, "/"),
      max = Inf
    )
  
  if (is.null(run_time)) {
    run <-
      data.frame(items) %>% 
      arrange(desc(LastModified)) %>%
      filter(grepl(input_name, Key))
    
    file_name <- latest_run[1, "Key"]
  }
  else if (is.Date(run_time)) {
    run <-
      data.frame(items) %>% 
      filter(grepl(input_name, Key) & grepl(runtime, Key)) %>%
      arrange(desc(LastModified))
    
    file_name <- latest_run[1, "Key"]
  }
  else {
    time_split <- strsplit(as.character(run_time), " ")
    
    if (length(time_split) > 1) {
      stop("Argument run_time has more than one element.")
    }
    
    time_split <- time_split[[1]]
    if (length(time_split) < 3) {
      tz <- "0700"
    }
    else if (time_split[3] == "UTC") {
      tz <- "0000"
    }
    else if (time_split[3] == "PST") {
      tz <- "0700"
    }
    else if (time_spit[3] == "PDT") {
      tz <- "0800"
    }
    
    run_time_string <- 
      paste0(
        time_split[1],
        "_",
        gsub(":", "-", time_split[2]),
        "_",
        tz
      )
    
    run <-
      data.frame(items) %>% 
      filter(grepl(input_name, Key)) %>%
      filter(
        map(strsplit(key, "/"), function(x) {return(x[3])}) == run_time_string
      ) %>%
      arrange(desc(LastModified))
    
    file_name <- latest_run[1, "Key"]
  }
  
  input_data <- 
    s3readRDS(
      object = file_name,
      bucket = 'everlane-data-deploy',
      key = aws_creds[["aws_access_key_id"]],
      secret = aws_creds[["aws_secret_access_key"]]
    )
  
  return(input_data)
}

#' Get data dependencies in pipeline from data-deploy script.
#' 
#' Parse command line arguments from data-deploy script and return back an input dataset(s) and a fullpath string.
#' 
#' @param model_name Name of the model/pipeline.
#' @param file_name Optional manual override to read in input dataset.
#' 
#' @examples 
#' feed("ops_forecasts_returned_units", "train_dataprep.rds")
feed <- function(model_name, file_name) {
  args = commandArgs(trailingOnly=TRUE)
  
  if (length(args) == 0) {
    stop("Fullpath to this file is required as an argument!")
  } else if (length(args) > 1) {
    fullpath <- args[1]
    version <- args[2]
    input_name <- args[3]
    
    if (length(args) == 4) {
      run_time <- args[4]
    }
    else {
      run_time <- NULL
    }
    
    input_data <- get_pipeline_input(model_name, version, input_name)
  } else {
    fullpath <- args[1]  
    input_data <- readRDS(paste0(fullpath, "/", file_name))
  }
  
  feed_output <- list(input_data = input_data, fullpath = fullpath)
  return(feed_output)
}