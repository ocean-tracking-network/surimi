#' @title Join output from Remora back onto its OTN detection extract.
#'
#' @description Take two parameters- an OTN detection extract and the output created by Remora on parsing that detection extract- and merge them back together such that the Remora QC columns are appended to the OTN extract,
#' preserving appropriate ordering and getting back all the OTN data. This function exists because, to get OTN data into Remora, we have to cut it up until it looks like IMOS data (this problem was the genesis of Surimi, in fact).
#' But that means the output from Remora has all IMOS-formatted columns and is missing some information, because we either had to discard it to get into IMOS format or because we can't re-synthesize it from what's in the IMOS
#' files. However, we do have enough information to join the two tables, thereby obviating the data loss problem by taking us all the way back to the original data, with a little something extra attached.
#'
#' @param detection_extract Path to an OTN detection extract corresponding to the remora output in the second parameter.
#' @param remora_output Path to Remora's QC output corresponding to the OTN detection extract in the first parameter.
#' @param style Whether this is a 'new' file (parquet or CSV with updated columns) or 'old' (CSV from before we renamed all our columns)
#'
#' @return The OTN detection extract, but with the remora QC attached as appropriate.
#'
#' @importFrom dplyr '%>%' mutate rename left_join
#' @importFrom tidyr unite separate
#' @importFrom lubridate ymd_hms
#' @importFrom nanoparquet write_parquet
#' @export
#'
#

rollup <- function(detection_extract, remora_output, style="new", write=FALSE, path=NULL) {
  # Read in the two dataframes.
  otn_dets <- load_file(detection_extract)
  remora_out <- read.csv(remora_output)
  
  # Select the appropriate columns from remora_output.
  remora_to_merge <- remora_out %>%
    dplyr::select(
      transmitter_id,
      tag_id,
      detection_datetime,
      receiver_id,
      ends_with("_QC")
    )
  
  # Get the dates into the same format for comparison.
  if(style == "old"){
    otn_dets <- otn_dets %>%
      mutate(
        datecollected = ymd_hms(datecollected),
        receiver = as.character(receiver)
      )
  }
  
  else {
    otn_dets <- otn_dets %>%
      mutate(
        dateCollectedUTC = ymd_hms(dateCollectedUTC),
        receiver = as.character(receiver)
      )
  }
  
  remora_to_merge <- remora_to_merge %>%
    mutate(
      detection_datetime = ymd_hms(detection_datetime),
      receiver_id = as.character(receiver_id)
    )
  
  # Join them to otn_dets
  if(style == "old") {
    otn_det_output <- left_join(
      otn_dets,
      remora_to_merge,
      by = join_by(
        tagname == transmitter_id,
        catalognumber == tag_id,
        datecollected == detection_datetime,
        receiver == receiver_id
      )
    )
  }
  
  else {
    otn_det_output <- left_join(
      otn_dets,
      remora_to_merge,
      by = join_by(
        tagName == transmitter_id,
        catalogNumber == tag_id,
        dateCollectedUTC == detection_datetime,
        receiver == receiver_id
      )
    )
  }
  
  #If the write argument is true, then we will want to write the output to a file.
  if(write == TRUE){
    #If path is NULL, we need to supply something of our own.
    if(is.null(path)) {
      #We'll use parquet as our default.
      path = paste0("./rolled_output.parquet")
    }
    #We'll now grab the extension from path...
    extension <- tools::file_ext(path)
    #... and use that to determine which function to invoke for writing.
    if(extension == "parquet") {
      write_parquet(otn_det_output, path, compresion=c("snappy"))
    }
    else if (extension == "csv"){
      write.csv(otn_det_output, path)
    }
    else {
      message(paste0("File extension ", extension, " is invalid. Please supply a path with either a .parquet or .csv extension if you want rollup to automatically write the output."))
    }
  }
  
  # Return the merged columns either way, so that the user has them as a dataframe to work with.
  return(otn_det_output)
}