otn_to_ato <- function(otn_detections) {
  #Determine whether or not we're dealing with a file or a dataframe. 
  if (!is.data.frame(otn_detections)) {
    extension <- tools::file_ext(otn_detections)
    if (extension == "parquet") {
      otn_detections <- read_parquet(otn_detections)
    } else {
      otn_detections <- read.csv(otn_detections, na = c("", "null", "NA"))
    }
  }
  
  #Now we have a dataframe we can start loading into an ATO object. Let's make an instance of the object.
  OTN_ATO <- new("ATO")
  
  #Make the "detections" object, 
  det <- make_det(
    datetime = otn_detections$dateCollectedUTC,
    #frac_second = NA_real_,
    receiver_serial = as.integer(otn_detections$receiver),
    transmitter = otn_detections$tagName,
    sensor_value = otn_detections$sensorValue,
    tz = "UTC"
  )
  
  OTN_ATO <- add(OTN_ATO, det)
  
  #Got to double check this against the IMOS stuff, see how we're ginning that up. 
  dep <- make_dep(receiver_model = NA_character_,
                  receiver_serial = otn_detections$receiver,
                  receiver_codeset = otn_detections$codeSpace,
                  deploy_location = otn_detections$station
                  deploy_datetime = as.POSIXct(NA_real_),
                  deploy_lat = otn_detections$decimalLatitude,
                  deploy_lon = otn_detections$decimalLongitude,
                  recover_datetime = actel:::example.deployments$Stop,
                  recover_lat = actel:::example.spatial$Latitude[-18],
                  recover_lon = actel:::example.spatial$Longitude[-18],
                  transmitter = NA_character_,
                  transmitter_model = NA_character_,
                  transmitter_serial = NA_integer_,
                  tz = "UTC")
    
  )
  
  return(OTN_ATO)
 }