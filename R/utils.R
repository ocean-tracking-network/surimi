load_file <- function(input_file) {
  if (!is.data.frame(input_file)) {
    # Grab the extension.
    extension <- tools::file_ext(input_file)
    # If it's a parquet, read it in as one...
    if (extension == "parquet") {
      output_frame <- read_parquet(input_file)
    } else if (extension == "xlsx" || extension == "xls") {
      output_frame <- read_excel(input_file)
    } else {
      # Otherwise bring it in as a CSV.
      output_frame <- read.csv(input_file, na = c("", "null", "NA"))
    }
    return(output_frame)
  } else {
    return(input_file)
  }
}
