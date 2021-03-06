% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/etl_functions.R
\name{copy_s3_to_redshift}
\alias{copy_s3_to_redshift}
\title{Copy to Redshift from S3}
\usage{
copy_s3_to_redshift(connection, object_name, object_type = ".csv",
  table_name, bucket_name = "everlane-data")
}
\arguments{
\item{connection}{Database connection object}

\item{object_name}{Full path to s3 location}

\item{object_type}{Possible values of ".csv" and ".txt"}

\item{table_name}{full name of Redshift table to copy to}

\item{bucket_name}{name of S3 bucket}
}
\description{
Easily copy structured text files from S3 to Redshift.
}
\examples{
copy_s3_to_redshift(connection = redshift_conn, object_name = "s3_file_path.txt", object_type = ".txt", table_name = "data.test_table", bucket_name = "everlane-data")
}
