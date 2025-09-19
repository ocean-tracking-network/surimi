ato_dep_from_otn <- function(otn_file, type="meta") {
  
  if (!is.data.frame(otn_file)) {
    extension <- tools::file_ext(otn_detections)
    if (extension == "parquet") {
      otn_file <- read_parquet(otn_detections)
    } else {
      otn_file <- read.csv(otn_detections, na = c("", "null", "NA"))
    }
  }
  
  if(type == "meta") {
    
  }
  
  else if(type == "extract") {
    
  }
  
  else {
    message("Invalid type specified. Use either 'meta' (if loading from a metadata file) or 'extract' (if deriving deployment metadata from a detection extract).")
  }
}