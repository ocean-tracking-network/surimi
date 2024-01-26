# Switch between OTN CSVs and OTN parquets. If you get one, return the other.

otn_convert <- function(file) {
  # Needs libraries: tools, sfarrow, arrow, parquetize

  # Get the file extension from the argument.
  extension <- file_ext(file)

  # If it's a parquet...
  if (extension == "parquet") {
    # Craft and return an OTN CSV.
    # Read the file into a dataframe.
    otn_data <- arrow::read_parquet(file)
  } else if (extension == "csv") {
    # Craft and return an OTN Parquet file.
    # Read the file into a dataframe
    otn_data <- read.csv(file)
  }
}
