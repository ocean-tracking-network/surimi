otn_to_ato <- function(otn_detections, otn_receivers = "", otn_tags = "") {
  # Determine whether or not we're dealing with a file or a dataframe.
  if (!is.data.frame(otn_detections)) {
    extension <- tools::file_ext(otn_detections)
    if (extension == "parquet") {
      otn_detections <- read_parquet(otn_detections)
    } else {
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

  dep <- ato_dep_from_otn()

  OTN_ATO <- add(OTN_ATO, dep)

  make_tag <- function(manufacturer = NA_character_,
                       model = NA_character_,
                       power_level = NA_real_,
                       ping_rate = NA_real_,
                       ping_variation = NA_real_,
                       serial = NA_integer_,
                       transmitter = NA_character_,
                       activation_datetime = as.POSIXct(NA_real_),
                       battery_life = NA_real_,
                       sensor_type = NA_character_,
                       sensor_unit = NA_character_,
                       animal = NA_character_,
                       tz = "UTC") {
    OTN_ATO <- add(OTN_ATO, tag)
  }

  return(OTN_ATO)
}
