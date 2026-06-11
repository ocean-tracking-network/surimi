##' @title Convert GLATOS detection data to an ATO object.
##'
##' @description Takes a GLATOS detection sheet and optionally receiver metadata and a glatos workbook and returns an ATO object. The original plan was to have the workbook be loadable as a separate object but I ran
##' into problems loading it on its own without any detection data so I'm adding it here. The standalone functions may need to exist either way but I figure if we can build this function out such that it loads as
##' much as possible, I'm OK with that. 
##'
##' @param glatos_detections The dataframe containing detection information.
##' @param glatos_receivers The dataframe containing receiver information.
##' @param glatos_workbook The path to the XLSX file containing the glatos workbook associated with the detections, if available. 
##'
##' @importFrom dplyr select '%>%' mutate rename group_by arrange distinct filter left_join
##' @importFrom tidyr unite drop_na any_of
##' @importFrom lubridate ymd as_date
##' @importFrom nanoparquet read_parquet
##'
##' @return Returns an ATO object.
##' @export
##'

glatos_to_ato <- function(glatos_detections, glatos_receivers = "", glatos_workbook = "") {
  glatos_detections <- load_file(glatos_detections)
  glatos_deployments <- NULL
  glatos_animals <- NULL
  
  #If we have the workbook, load that into something we can use with the glatos reading functions.
  # First we'll read in the workbook using the glatos package. One day this functionality may live inside surimi, but not yet.
  if(glatos_workbook != "") {
    glatos_list <- read_glatos_workbook(glatos_workbook)
    
    # Now we have a list containing 'metadata', 'animals', and 'receivers.' We really only need animals and receivers, so we'll split it up and get those.
    glatos_animals <- glatos_list$animals
    
    #I know this rename might be a little confusing but since someone might optionally hand in a receiver metadata sheet, I have to use different wording so that they don't get mixed up. 
    glatos_deployments <- glatos_list$receivers
    
    #temporarily dealing with some NaN values while I test. 
    glatos_deployments <- drop_na(glatos_deployments, any_of(c("recover_date_time", "deploy_date_time")))
  }
  
  # Make the "detections" object,
  det <- make_det(
    datetime = as.POSIXct(glatos_detections$detection_timestamp_utc),
    frac_second = NA_real_,
    receiver_serial = as.character(glatos_detections$receiver_sn),
    transmitter = paste(glatos_detections$transmitter_codespace, "-", glatos_detections$transmitter_id, sep = ""), 
    sensor_value = as.numeric(glatos_detections$sensor_value),
    tz = "UTC"
  )

  # I used to have a 'derive' argument as in some of the original OTN-to-IMOS functions but then I realised it was safer to just
  # automatically try to derive receiver metadata from the extract if no file is supplied.
  dep <- ""

  #In the case of receivers, we'll prefer the workbook information, then the receiver metadata, then the detection extract itself from which we will attempt to derive the data. 
  if(!is.null(glatos_deployments)) {
    dep <- ato_dep_from_glatos_workbook(glatos_deployments)
  }
  else if (glatos_receivers != "") {
    dep <- ato_dep_from_glatos(glatos_receivers, type = "meta")
  } 
  else {
    dep <- ato_dep_from_glatos(glatos_detections, type = "extract")
  }

  # Tag information is not part of what's distributed in glatos publications so we can't derive it, but if someone has their workbook handy, we can use that to get tag data.
  if(!is.null(glatos_animals)) {
    tag <- ato_tag_from_glatos_workbook(glatos_animals)
    
    #We also need to create observations, which we can do from the glatos_animals information since it records whether or not the tag has been captured and recovered. 
    obs <- ato_obs_from_glatos_workbook(glatos_animals)
    
    #We can also get animal information from the glatos workbook, so if we have that, then let's do it.
    ani <- ato_ani_from_glatos_workbook(glatos_animals)
    
    #If we can do all of those, then we can make and return an appropriately-filled ATO with all of our data.
    GLATOS_ATO <- init_ato(
      det = det,
      dep = dep,
      ani = ani,
      tag = tag,
      obs = obs
    )
  }
  
  #If we don't have the workbook then we will just have to make what animal data we can from the extract and call it a day. Tag and Obs
  #Won't get created and passed in. 
  else {
    ani <- ato_ani_from_glatos(glatos_detections)
    GLATOS_ATO <- init_ato(
      det = det,
      dep = dep,
      ani = ani
    )
  }

  return(GLATOS_ATO)
}
