#' @title Determine whether an input file is CSV or Parquet and pipe it into the correct mapping function. Hopefully this is all a stopgap until the typical format of OTN -> ATO and then ATO -> IMOS gets done, at which point
#' those pipeline pieces will connect together. But for now, this will keep dependent software like Remora running.
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
    #If the file is a parquet file, then just call the appropriate mapping function.
    return(otn_imos_new_style_column_map(filename, derive = derive, coll_code = coll_code, tagname_column = "tagName"))
  } else if(extension == "csv") {
    #Since 'new_style' can take both parquets and new CSVs, here we need to check which is getting passed. We'll just get the first couple of columns to check.
    col_check <- read.csv(filename, nrows=3)
    cols <- names(col_check)
    #On a whim we'll use 'datecollected' to check if it's an old-style file. 
    if ("datecollected" %in% cols) {
      return(otn_imos_column_map(filename, derive = derive, coll_code = coll_code, tagname_column = "tagname"))
    }
    else {
      return(otn_imos_new_style_column_map(filename, derive = derive, coll_code = coll_code, tagname_column = "tagName", format="csv"))
    }
  }
  else {
    message("map_otn_file can only take parquet and CSV files. Please double check that you're using the right file type and try again.")
  }
}
