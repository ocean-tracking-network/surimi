##' @title Convert GLATOS detection data to an ATO object.
##'
##' @description Takes a GLATOS detection sheet and optionally receiver/tag metadata and returns an ATO object.
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

glatos_to_ato <- function(glatos_detections, glatos_receivers = "", glatos_tags = "") {
  glatos_detections <- load_file(glatos_detections)

  # Now we have a dataframe we can start loading into an ATO object. Let's make an instance of the object.
  GLATOS_ATO <- new("ATO")

  # Make the "detections" object,
  det <- make_det(
    datetime = as.POSIXct(glatos_detections$detection_timestamp_utc),
    frac_second = NA_real_,
    receiver_serial = as.integer(glatos_detections$receiver_sn),
    transmitter = paste(glatos_detections$transmitter_codespace, "-", glatos_detections$transmitter_id, sep = ""), # Might have to synthesize this from other fields
    sensor_value = as.numeric(glatos_detections$sensor_value),
    tz = "UTC"
  )

  GLATOS_ATO <- add(GLATOS_ATO, det)

  # I used to have a 'derive' argument as in some of the original OTN-to-IMOS functions but then I realised it was safer to just
  # automatically try to derive receiver/tag metadata from the extract if no file is supplied.
  dep <- ""
  tag <- ""

  # In both cases, if a file is supplied, we'll make the metadata objects using the information therein;
  # otherwise, we'll attempt to make approximately correct receiver/tag metadata from only what's contained in
  # the extract.
  if (glatos_receivers != "") {
    dep <- ato_dep_from_glatos(glatos_receivers, type = "meta")
  } else {
    dep <- ato_dep_from_glatos(glatos_detections, type = "extract")
  }

  GLATOS_ATO <- add(GLATOS_ATO, dep)

  # In the detection extract zip I had for reference there didn't seem to be a bespoke Tag metadata file. I'll leave this structure here in case we need to build it out further but I think it's only ever going to flop into the else.
  # if (glatos_tags != "") {
  #  tag <- ato_tag_from_glatos(glatos_tags, type = "meta")

  # } else {
  #  tag <- ato_tag_from_glatos(glatos_detections, type = "extract")
  # }

  # GLATOS_ATO <- add(GLATOS_ATO, tag)

  return(GLATOS_ATO)
}
