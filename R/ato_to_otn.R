ato_to_otn <- function(ato_object, dets=TRUE, rcvr=FALSE, tag=FALSE, output_folder = ".", collectioncode = "") {
  #We need to perform the creation operations in reverse- take what data we can out of the ATO and put it into dataframes, which we then write to spreadsheets. That means that we're going to need to rejoin a lot
  #of the ATO data into a single dataframe so that the data will be properly aligned across the rows. 
  
  #We'll start by extracting the most relevant data structures in the ATO. 
  ato_dets <- det(ato_object)
  ato_deps <- dep(ato_object)
  ato_tags <- tag(ato_object)
  ato_anis <- ani(ato_object)
  
  #We'll join the above as appropriate when constructing each of the potential pieces below. 
  #We'll do each one individually so as to cover off instances where one but not the other are needed. 
  
  if(dets == TRUE) {
    #We're going to export data from the ATO so as to create an OTN detection extract (or equivalent approximation). 
    #Start by instantiating a dataframe with the appropriate column names. 
    det_colnames <- c(
      "collectionCode", #Supplied by parameter
      "catalogNumber", 
      "organismID", #'animal' from ato_anis (joined)
      "scientificName",
      "commonName",
      "dateLastModified", 
      "detectedBy", 
      "station", 
      "receiver", #Comes in from the det object.
      "bottomDepth", 
      "receiverDepth", #Comes in from dep object (joined)
      "tagName", #comes in from the det object.
      "codeSpace", #Comes in via tag, splitting the tagName into tagID and codeSpace. 
      "sensorName", 
      "sensorRaw",
      "sensorType", 
      "sensorValue", #comes in from the det object.
      "sensorUnit",
      "dateCollectedUTC", #Comes in from the det object. 
      "uncorrectedDateCollectedUTC", 
      "decimalLongitude", #Comes in from the receiver object; if we know the appropriate receiver we can get the lat/lon
      "decimalLatitude", #Ditto
      "geodeticDatum", 
      "geometry", 
      "localArea", 
      "citation", 
      "unqDetecID", 
      "contactPOC", 
      "contactPI"
    )
    
    #Since we remove the releases in the process of making the detection object, we need to re-add them when we export. 
    #Everything in animals is made from a release anyway, so we have a 1:1 animals-to-releases relationship, we just need to create a
    #release record for each animal. Following code written up by Hugo Flavio. 
    releases_det <- det(ato_object)
    releases_tag <- ato_tags[!is.na(ato_tags$animal), ]
    
    releases_det[1:nrow(releases_tag), ] <- NA
    releases_det <- releases_det[1:nrow(releases_tag), ]
    
    releases_det$datetime <- releases_tag$release_datetime
    releases_det$receiver_serial <- "release"
    releases_det$transmitter <- releases_tag$transmitter
    releases_det$animal <- releases_tag$animal
    
    ato_det_joined <- rbind(ato_dets, releases_det)
    
    #We're going to use the internal matching columns to link everything together on the way back in.
    ato_det_joined <- cbind(ato_det_joined, ato_tags[ato_det_joined$tag_match, !colnames(ato_tags) %in% c("transmitter", "valid", "ani_match", "animal")])
    ato_det_joined <- separate_wider_delim(ato_det_joined, cols=transmitter, delim="-", names = c("codespace_1", "codespace_2", "tagID"))
    
    ato_det_joined <- cbind(ato_det_joined, ato_deps[ato_det_joined$dep_match, ])
    
    ato_det_joined <- cbind(ato_det_joined, ato_anis[ato_det_joined$ani_match, ])
    
    #View(ato_det_joined)
    
    #We're going to instantiate our dataframe here since we'll end up needing to key off the number of rows after this join. 
    det_df <- data.frame(matrix(ncol=length(det_colnames), nrow=nrow(ato_det_joined)))
    colnames(det_df) <- det_colnames
    
    det_df$collectionCode <- collectioncode
    det_df$organismID <- ato_det_joined$animal
    det_df$dateCollectedUTC <- ato_det_joined$datetime
    det_df$receiver <- ato_det_joined$receiver_serial
    det_df$receiverDepth <- ato_det_joined$deploy_z
    det_df$tagName <- ato_det_joined$transmitter
    det_df$codeSpace <- paste(ato_det_joined$codespace_1, "-", ato_det_joined$codespace_2, sep="")
    det_df$decimalLongitude <- ato_det_joined$deploy_lon
    det_df$decimalLatitude <- ato_det_joined$deploy_lat
    det_df$sensorValue <- ato_det_joined$sensor_value
    
    write.csv(det_df, paste(output_folder, "/surimi_output_det.csv", sep=""))
  }
  
  if(rcvr == TRUE) {
    rcvr_colnames <- c(
      "OTN_ARRAY",
      "STATION_NO", #Comes in from the deps object.
      "DEPLOY_DATE_TIME",#Comes in from the deps object.
      "DEPLOY_LAT", #Comes in from the deps object.
      "DEPLOY_LONG", #Comes in from the deps object.
      "BOTTOM_DEPTH", #Comes in from the deps object.
      "RISER_LENGTH",
      "INSTRUMENT_DEPTH", 
      "INS_MODEL_NO", #Comes in from the deps object.
      "INS_SERIAL_NO", #Comes in from the deps object.
      "CODE_SET", #Comes in from the deps object.
      "TRANSMITTER", #Comes in from the deps object.
      "TRANSMIT_MODEL", #Comes in from the deps object.
      "AR_MODEL_NO",
      "AR_SERIAL_NO", 
      "DEPLOYED_BY",
      "RECOVERED",
      "RECOVER_DATE_TIME", #Comes in from the deps object.
      "RECOVER_LAT", #Comes in from the deps object.
      "RECOVER_LONG", #Comes in from the deps object.
      "DATA_DOWNLOADED",
      "DOWNLOAD_DATE_TIME",
      "FILENAME", 
      "COMMENTS"
    )
    
    rcvr_df <- data.frame(matrix(ncol=length(rcvr_colnames), nrow=nrow(ato_deps)))
    colnames(rcvr_df) <- rcvr_colnames
    
    rcvr_df$INS_MODEL_NO <- ato_deps$receiver_model
    rcvr_df$INS_SERIAL_NO <- ato_deps$receiver_serial
    rcvr_df$CODE_SET <- ato_deps$receiver_codeset
    rcvr_df$STATION_NO <- ato_deps$deploy_location
    rcvr_df$DEPLOY_DATE_TIME <- ato_deps$deploy_datetime
    rcvr_df$DEPLOY_LAT <- ato_deps$deploy_lat
    rcvr_df$DEPLOY_LONG <- ato_deps$deploy_lon
    rcvr_df$BOTTOM_DEPTH <- ato_deps$deploy_z
    rcvr_df$RECOVER_DATE_TIME <- ato_deps$recover_datetime
    rcvr_df$RECOVER_LAT <- ato_deps$recover_lat
    rcvr_df$RECOVER_LONG <- ato_deps$recover_lon
    rcvr_df$TRANSMITTER <- ato_deps$transmitter
    rcvr_df$TRANSMIT_MODEL <- ato_deps$transmitter_model
    
    write.csv(rcvr_df, paste(output_folder, "/surimi_output_rcvr.csv", sep=""))
  }
  
  if(tag == TRUE) {
    
    tag_colnames <- c(
      "ANIMAL_ID",
      "TAG_TYPE",
      "TAG_MANUFACTURER", #Comes in from the tag object
      "TAG_MODEL", #Comes in from the tag object
      "TAG_SERIAL_NUMBER", #Comes in from the tag object
      "TAG_ID_CODE", #Comes in from the tag object, split out of transmitter
      "TAG_CODE_SPACE", #Comes in from the tag object, split out of transmitter
      "TAG_IMPLANT_TYPE",
      "TAG_IMPLANT_METHOD", 
      "TAG_ACTIVATION_DATE",
      "EST_TAG_LIFE", #Comes in from the tag object as battery_life
      "TAGGER", 
      "TAG_OWNER_PI",
      "TAG_OWNER_ORGANIZATION",
      "COMMON_NAME_E", 
      "SCIENTIFIC_NAME", 
      "CAPTURE_LOCATION", 
      "CAPTURE_LATITUDE", 
      "CAPTURE_LONGITUDE", 
      "WILD_OR_HATCHERY",
      "STOCK", 
      "LENGTH (m)",
      "WEIGHT (kg)",
      "LENGTH_TYPE", 
      "LENGTH2(m)",
      "LENGTH2_TYPE", 
      "LIFE_STAGE",
      "AGE", 
      "AGE_UNITS", 
      "SEX", 
      "DNA_SAMPLE_TAKEN", 
      "TREATMENT_TYPE", 
      "RELEASE_GROUP", 
      "RELEASE_LOCATION", 
      "RELEASE_LATITUDE", 
      "RELEASE_LONGITUDE",
      "UTC_RELEASE_DATE_TIME",
      "HARVEST_DATE", 
      "CAPTURE_DEPTH(m)",
      "TEMPERATURE_CHANGE (degrees C)",
      "HOLDING_TEMPERATURE (degrees C)",
      "PREOP_HOLD_PERIOD",
      "POSTOP_HOLD_PERIOD", 
      "SURGERY_LOCATION", 
      "DATE_OF_SURGERY",
      "SURGERY_LATITUDE", 
      "SURGERY_LONGITUDE", 
      "SEDATIVE", 
      "SEDATIVE_CONCENTRATION (ppm)",
      "ANAESTHETIC", 
      "BUFFER", 
      "ANAESTHETIC_CONCENTRATION (ppm)",
      "BUFFER_CONCENTRATION_IN_ANAESTHETIC (ppm)",
      "ANAESTHETIC_CONCENTRATION_IN_RECIRCULATION (ppm)",
      "BUFFER_CONCENTRATION_IN_RECIRCULATION (ppm)", 
      "DISSOLVED_OXYGEN (ppm)",
      "COMMENTS"
    )
    
    tag_df <- data.frame(matrix(ncol=length(tag_colnames), nrow=nrow(ato_tags)))
    colnames(tag_df) <- tag_colnames
    
    #We can get additional data by splitting up the transmitter into the code_space and tag ID code, essentially doing the reverse operation as at ingestion.
    ato_tags <- separate_wider_delim(ato_tags, cols=transmitter, delim="-", names = c("codespace_1", "codespace_2", "tagID"))
    
    #We'll start by pulling over what data we can from the tag object, though we may have to fold in data from @ani, which will take some matching and merging. But one thing at a time. 
    tag_df$TAG_MANUFACTURER <- ato_tags$manufacturer
    tag_df$TAG_MODEL <- ato_tags$model
    tag_df$TAG_SERIAL_NUMBER <- ato_tags$serial
    tag_df$TAG_ID_CODE <- ato_tags$tagID
    tag_df$TAG_CODE_SPACE <- paste(ato_tags$codespace_1, "-", ato_tags$codespace_2, sep="")
    tag_df$TAG_ACTIVATION_DATE <- ato_tags$activation_datetime
    tag_df$EST_TAG_LIFE <- ato_tags$battery_life
    tag_df$RELEASE_LOCATION <- ato_tags$release_location
    tag_df$UTC_RELEASE_DATE_TIME <- ato_tags$release_datetime
    tag_df$RELEASE_LATITUDE <- ato_tags$release_latitude
    tag_df$RELEASE_LONGITUDE <- ato_tags$release_longitude
    
    write.csv(tag_df, paste(output_folder, "/surimi_output_tag.csv", sep=""))
  }
  
  return(TRUE)
}