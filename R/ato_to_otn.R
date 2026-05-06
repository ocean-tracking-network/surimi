ato_to_otn <- function(ato_object, dets=TRUE, rcvr=FALSE, tag=FALSE, output_folder = ".") {
  #We need to perform the creation operations in reverse- take what data we can out of the ATO and put it into dataframes, which we then write to spreadsheets. That means that we're going to need to rejoin a lot
  #of the ATO data into a single dataframe so that the data will be properly aligned across the rows. 
  
  #We'll start by extracting the most relevant data structures in the ATO. 
  ato_dets <- ato_object@det
  ato_deps <- ato_object@dep
  ato_tags <- ato_object@tag
  ato_anis <- ato_object@ani
  
  #We'll join the above as appropriate when constructing each of the potential pieces below. 
  #We'll do each one individually so as to cover off instances where one but not the other are needed. 
  
  #(Note to self- I am starting with just the receiver one in the hopes that it'll be a simpler build; it will also get to the quick of something Dan was wondering about.)
  
  if(dets == TRUE) {
    
    
    
    #We're going to export data from the ATO so as to create an OTN detection extract (or equivalent approximation). 
    #Start by instantiating a dataframe with the appropriate column names. 
    det_df <- data.frame(matrix(ncol=29, nrow=0))
    
    colnames(det_df) <- c(
      "collectionCode",
      "catalogNumber", 
      "organismID",
      "scientificName",
      "commonName",
      "dateLastModified", 
      "detectedBy", 
      "station", 
      "receiver", #Comes in from the det object.
      "bottomDepth",
      "receiverDepth", 
      "tagName", #comes in from the det object.
      "codeSpace",
      "sensorName", 
      "sensorRaw",
      "sensorType", 
      "sensorValue", #comes in from the det object.
      "sensorUnit",
      "dateCollectedUTC", #Comes in from the det object. 
      "uncorrectedDateCollectedUTC", 
      "decimalLongitude", 
      "decimalLatitude", 
      "geodeticDatum", 
      "geometry", 
      "localArea", 
      "citation", 
      "unqDetecID", 
      "contactPOC", 
      "contactPI",
    )
    
    #Now we need to get the data that we can get out of the ATO. We can start with the data in @det, which will be our point of reference for everything else. 
    ato_dets <- ato_object@det
    
    det_df$dateCollectedUTC <- ato_dets$datetime
    det_df$receiver <- ato_dets$receiver_serial
    det_df$tagName <- ato_dets$transmitter
    det_df$sensorValue <- ato_dets$sensor_value
  }
  
  if(rcvr == TRUE) {
    rcvr_df <- data.frame(matrix(ncol=24, nrow=nrow(ato_deps)))
    
    colnames(rcvr_df) <- c(
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
    
    write.csv(rcvr_df, paste(output_folder, "/ato_csv_output.csv", sep=""))
  }
  
  if(tag == TRUE) {
    
    
  }
  
  return(TRUE)
}