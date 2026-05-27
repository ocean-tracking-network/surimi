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
  # First we'll read in the workbook using the glatos package. One day this functionality may live inside surimi, but not yet.
  glatos_list <- read_glatos_workbook(glatos_workbook)

  # Now we have a list containing 'metadata', 'animals', and 'receivers.' We really only need animals and receivers, so we'll split it up and get those.
  glatos_animals <- glatos_list$animals

  glatos_receivers <- glatos_list$receivers

  # Now that we have those, we can start casting info the same as we would with the glatos_to_ato function but with more tag info.
  GLATOS_ATO <- init_ato()

  # Start with det, as always- although GLATOS workbooks don't have detection data, it will help us down the road to populate and assign this anyway.
  det <- make_det(
    datetime = as.POSIXct(Sys.Date()),
    frac_second = NA_real_,
    receiver_serial = "workbook-generated",
    transmitter = "workbook-generated",
    sensor_value = as.numeric(NA_integer_),
    tz = "UTC"
  )

  GLATOS_ATO <- set_det(GLATOS_ATO, det)

  # There's only one file to supply, so we don't need to have the optional part here like in the other functions.
  dep <- ato_dep_from_glatos_workbook(glatos_receivers)
  GLATOS_ATO <- set_dep(GLATOS_ATO, dep)

  tag <- ato_tag_from_glatos_workbook(glatos_animals)
  GLATOS_ATO <- set_tag(GLATOS_ATO, tag)

  ani <- ato_ani_from_glatos_workbook(glatos_animals)
  GLATOS_ATO <- set_ani(GLATOS_ATO, ani)
  
  obs <- ato_obs_from_glatos_workbook(glatos_animals)
  GLATOS_ATO <- set_obs(GLATOS_ATO, obs)

  return(GLATOS_ATO)
}
