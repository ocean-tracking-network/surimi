##' @title Convert GLATOS detection data to an ATO object.
##'
##' @description Takes a GLATOS detection sheet and optionally receiver metadata and returns an ATO object.
##'
##' @param glatos_detections The dataframe containing detection information.
##' @param glatos_receivers The dataframe containing receiver information.
##' 
##' @importFrom dplyr select '%>%' mutate rename group_by arrange distinct filter left_join
##' @importFrom tidyr unite
##' @importFrom lubridate ymd as_date
##' @importFrom nanoparquet read_parquet
##'
##' @return Returns an ATO object.
##' @export
##'

glatos_to_ato <- function(glatos_detections, glatos_receivers = "") {
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
  # automatically try to derive receiver metadata from the extract if no file is supplied.
  dep <- ""

  # In both cases, if a file is supplied, we'll make the metadata objects using the information therein;
  # otherwise, we'll attempt to make approximately correct receiver/tag metadata from only what's contained in
  # the extract.
  if (glatos_receivers != "") {
    dep <- ato_dep_from_glatos(glatos_receivers, type = "meta")
  } else {
    dep <- ato_dep_from_glatos(glatos_detections, type = "extract")
  }

  GLATOS_ATO <- add(GLATOS_ATO, dep)

  # Tag information is not part of what's distributed in glatos publications so we can neither load it nor meaningfully derive it.

  ani <- ato_ani_from_glatos(glatos_detections)

  GLATOS_ATO <- add(GLATOS_ATO, ani)

  return(GLATOS_ATO)
}
