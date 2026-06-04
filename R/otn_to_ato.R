##' @title Convert OTN detection data to an ATO object.
##'
##' @description Takes an OTN detection extract and optionally receiver/tag metadata and returns an ATO object.
##'
##' @param otn_detections The dataframe containing detection information.
##' @param otn_receivers The dataframe containing receiver information.
##' @param otn_tags The dataframe containing tag information.
##'
##' @importFrom dplyr select '%>%' mutate rename group_by arrange distinct filter left_join
##' @importFrom tidyr unite
##' @importFrom lubridate ymd as_date
##' @importFrom nanoparquet read_parquet
##'
##' @return Returns an ATO object.
##' @export
##'

otn_to_ato <- function(otn_detections, otn_receivers = "", otn_tags = "") {
  # Determine whether or not we're dealing with a file or a dataframe.
  if (!is.data.frame(otn_detections)) {
    # If it's not a dataframe, get the extension.
    extension <- tools::file_ext(otn_detections)
    if (extension == "parquet") {
      # If it's a parquet file, read it in with read_parquet.
      otn_detections <- read_parquet(otn_detections)
    } else {
      # Otherwise, read it in with read.csv.
      otn_detections <- read.csv(otn_detections, na = c("", "null", "NA"))
    }
  }


  # Make the "detections" object.
  
  #Filter out releases.
  otn_detections_filtered <- dplyr::filter(otn_detections, otn_detections$receiver != "release")
  #View(otn_detections_filtered)
  
  det <- make_det(
    datetime = as.POSIXct(otn_detections_filtered$dateCollectedUTC),
    frac_second = NA_real_,
    receiver_serial = as.character(otn_detections_filtered$receiver),
    transmitter = otn_detections_filtered$tagName,
    sensor_value = as.numeric(otn_detections_filtered$sensorValue),
    tz = "UTC"
  )

  # In both cases, if a file is supplied, we'll make the metadata objects using the information therein;
  # otherwise, we'll attempt to make approximately correct receiver/tag metadata from only what's contained in
  # the extract.
  if (otn_receivers != "") {
    dep <- ato_dep_from_otn(otn_receivers, type = "meta")
  } else {
    dep <- ato_dep_from_otn(otn_detections, type = "extract")
  }

  if (otn_tags != "") {
    tag <- ato_tag_from_otn(otn_tags, type = "meta")
    # If we have a tag metadata file, we can derive an animal object; if we don't, there's hardly enough information to bother,
    # and if people are so inclined they can add it manually through the ATO's default functions and ATools.
    ani <- ato_ani_from_otn(otn_tags)
  } else {
    tag <- ato_tag_from_otn(otn_detections, type = "extract")
    ani <- ato_ani_from_otn(otn_detections, type = "extract")
  }

  OTN_ATO <- init_ato(
    det = det,
    dep = dep,
    ani = ani,
    tag = tag
  )

  return(OTN_ATO)
}
