#' @title Determine whether an input file is CSV or Parquet and pipe it into the correct mapping function.
#'
#' @param filename The path to the file to be processed.
#' @param derive Passed through to the mapping functions; determines whether or not receiver and tag metadata will be derived from the detection extract or not.
#' @param coll_code Passed through to the mapping functions; allows user to supply collectionCode if they are passing their own rcvr/tag metadata, which won't contain the collectionCode.
#'
#' @returns The output of the appropriate mapping function.
#' @export
#'
map_otn_file <- function(filename, derive = TRUE, coll_code = NULL) {
  extension <- tools::file_ext(filename)
  if (extension == "parquet") {
    return(otn_imos_parquet_column_map(filename, derive = derive, coll_code = coll_code, tagname_column = "tagName"))
  } else {
    return(otn_imos_column_map(filename, derive = derive, coll_code = coll_code, tagname_column = "tagname"))
  }
}
