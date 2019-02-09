#' Produce x/y coordinates of each hex in a hexbin plot
#'
#' Given a tidy dataset, return a tidy dataset containing x/y coordinates for hexes
#'
#' @param df R object containing a tidy dataset
#' @param x_column_name Name of column that will represent X axis of graph
#' @param y_column_name Name of column that will represent Y axis of graph
#' @return R dataframe with two extra columns representing coordinate of centers for each hex in a hexbin plot
#'
#' @examples
#' create_pipeline_output(training_data_final,"activation_curves", "train_dataprep", 2, "rds")
tidy_hexbin <- function(df, x_column_name, y_column_name) {
  require(hexbin)
  require(dplyr)

  hexbin_obj <- hexbin(x = df[[x_column_name]], y = df[[y_column_name]], IDs = TRUE)

  hex_id <- hexbin_obj@cID
  hex_xy <- cbind.data.frame(id = hexbin_obj@cell, hcell2xy(hexbin_obj))
  colnames(hex_xy) <- c("hex_id", "x_axis_coord", "y_axis_coord")

  hexbin_data <-
    cbind.data.frame(df, hex_id) %>%
    left_join(hex_xy, by = c("hex_id" = "hex_id"))

  return(hexbin_data)
}
