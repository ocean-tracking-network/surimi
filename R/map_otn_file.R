map_otn_file <- function(filename, derive = TRUE, coll_code = NULL) {
  extension <- tools::file_ext(filename)
  if(extension == "parquet") {
    return(otn_imos_parquet_column_map(filename, derive = derive, coll_code = coll_code, tagname_column = "tagName"))
  }
  else {
    return(otn_imos_column_map(filename, derive = derive, coll_code = coll_code, tagname_column = "tagname"))
  }
}