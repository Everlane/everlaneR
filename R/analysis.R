
### GIVEN A TIDY DATASET, RETURN A TIDY DATASET CONTAINING X/Y COORDINATES FOR HEXES
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