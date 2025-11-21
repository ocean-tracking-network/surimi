##' @title Convert OTN detection data to an ATO object.
##'
##' @description Takes an OTN detection extract and optionally receiver/tag metadata and returns an ATO object.
##'
##' @param otn_detections The dataframe containing detection information.
##' @param otn_receivers The dataframe containing receiver information.
##' @param otn_tags The dataframe containing tag information.
##' @param derive An optional flag that allows the user to pass in fewer than all three files. If given, the code will use the detection
##' extract dataframe to generate dataframes for either or both of the receiver and tag dataframes, if they are not passed in. Although
##' this will result in missing information, it does let the user supply only a detection extract file, which is a situation some may
##' find themselves in.
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

  # Now we have a dataframe we can start loading into an ATO object. Let's make an instance of the object.
  OTN_ATO <- new("ATO")

  # Make the "detections" object,
  det <- make_det(
    datetime = otn_detections$dateCollectedUTC,
    # frac_second = NA_real_,
    receiver_serial = as.integer(otn_detections$receiver),
    transmitter = otn_detections$tagName,
    sensor_value = otn_detections$sensorValue,
    tz = "UTC"
  )

  OTN_ATO <- add(OTN_ATO, det)

  # I used to have a 'derive' argument as in some of the original OTN-to-IMOS functions but then I realised it was safer to just
  # automatically try to derive receiver/tag metadata from the extract if no file is supplied.
  dep <- ""
  tag <- ""

  # In both cases, if a file is supplied, we'll make the metadata objects using the information therein;
  # otherwise, we'll attempt to make approximately correct receiver/tag metadata from only what's contained in
  # the extract.
  if (otn_receivers != "") {
    dep <- ato_dep_from_otn(otn_receivers, type = "meta")
  } else {
    dep <- ato_dep_from_otn(otn_file, type = "extract")
  }

  OTN_ATO <- add(OTN_ATO, dep)

  if (otn_tags != "") {
    tag <- ato_tag_from_otn(otn_tags, type = "meta")
  } else {
    tag <- ato_tag_from_otn(otn_file, type = "extract")
  }

  OTN_ATO <- add(OTN_ATO, tag)

  return(OTN_ATO)
}
