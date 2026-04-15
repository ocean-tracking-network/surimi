##' @title Convert GLATOS workbook to an ATO object.
##'
##' @description Takes a GLATOS workbook and returns an ATO object.
##'
##' @param glatos_workbook Path to the glatos workbook.
##' 
##' @importFrom dplyr select '%>%' mutate rename group_by arrange distinct filter left_join
##' @importFrom tidyr unite
##' @importFrom lubridate ymd as_date
##' @importFrom nanoparquet read_parquet
##' @importFrom glatos read_glatos_workbook
##'
##' @return Returns an ATO object.
##' @export
##'
##'

glatos_workbook_to_ato <- function(glatos_workbook) {
  #First we'll read in the workbook using the glatos package. One day this functionality may live inside surimi, but not yet.
  glatos_list <- read_glatos_workbook(glatos_workbook)
  
  #Now we have a list containing 'metadata', 'animals', and 'receivers.' We really only need animals and receivers, so we'll split it up and get those.
  glatos_animals <- glatos_list$animals
  
  glatos_receivers <- glatos_list$receivers
  
  #Now that we have those, we can start casting info the same as we would with the glatos_to_ato function but with more tag info. 
  GLATOS_ATO <- new("ATO")
  
  #Start with det, as always.
  det <- make_det(
    datetime = as.POSIXct(glatos_detections$detection_timestamp_utc),
    frac_second = NA_real_,
    receiver_serial = as.integer(glatos_detections$receiver_sn),
    transmitter = paste(glatos_detections$transmitter_codespace, "-", glatos_detections$transmitter_id, sep = ""), # Might have to synthesize this from other fields
    sensor_value = as.numeric(glatos_detections$sensor_value),
    tz = "UTC"
  )
  
  GLATOS_ATO <- add(GLATOS_ATO, det)
  
  #There's only one file to supply, so we don't need to have the optional part here like in the other functions. 
  dep <- ato_dep_from_glatos_workbook(glatos_receivers)
  GLATOS_ATO <- add(GLATOS_ATO, dep)
  
  tag <- ato_tag_from_glatos_workbook(glatos_animals)
  GLATOS_ATO <- add(GLATOS_ATO, tag)
  
  ani <- ato_ani_from_glatos_workbook(glatos_animals)
  GLATOS_ATO <- add(GLATOS_ATO, ani)
  
  return(GLATOS_ATO)
}