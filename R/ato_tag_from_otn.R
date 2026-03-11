ato_tag_from_otn <- function(otn_file, type = "meta") {
  # Read in the file we've been given if we haven't been handed a dataframe.
  if (!is.data.frame(otn_file)) {
    # Grab the extension.
    extension <- tools::file_ext(otn_detections)
    # If it's a parquet, read it in as one...
    if (extension == "parquet") {
      otn_file <- read_parquet(otn_detections)
    } 
    else if (extension == "xlsx" || extension == "xls") {
      otn_file <- read_excel(otn_detections) 
    }
    else {
      # Otherwise bring it in as a CSV.
      otn_file <- read.csv(otn_detections, na = c("", "null", "NA"))
    }
  }

  # If we've been given a metadata file, we read it in as one.
  if (type == "meta") {
    # This we can pull directly from the metadata.
    tag <- make_tag(
      manufacturer = otn_file$TAG_MANUFACTURER,
      model = otn_file$TAG_MODEL,
      power_level = NA_real_, # ???
      ping_rate = NA_real_, # ???
      ping_variation = NA_real_, # ???
      serial = otn_file$TAG_SERIAL_NUMBER,
      transmitter = NA_character_, # ???
      activation_datetime = as.POSIXct(otn_file$TAG_ACTIVATION_DATE),
      battery_life = otn_file$EST_TAG_LIFE,
      sensor_type = NA_character_, # ???
      sensor_unit = NA_character_, # ???
      animal = otn_file$ANIMAL_ID,
      tz = "UTC"
    )
    return(tag)
  } else if (type == "extract") {
    # Otherwise, we can do a similar step to derive tag info from a detection extract.
    # Group by tagname. We may need to add the option to use alternative columns in the future, but that's doable, I think.
    # tagname_column <- as.name(tagname_column)
    distinctTag <- otn_file %>%
      group_by(across("tagName")) %>%
      distinct(across("tagName"), .keep_all = TRUE)

    # To get the correct transmitter lat/lon, we need to get the releases.
    releases <- otn_file %>%
      filter(receiver == "release") %>%
      group_by(catalogNumber) %>%
      distinct(catalogNumber, .keep_all = TRUE)

    message("Number of releases:")
    message(nrow(releases))

    tag <- distinctTag %>%
      select(
        collectionCode,
        tagName,
        commonName,
        scientificName,
        decimalLongitude,
        decimalLatitude,
        catalogNumber
      )
    # Now we can join the releases to get the appropriate transmitter_deployment_lat/lon
    tag <- left_join(tag,
      releases %>% dplyr::select(
        catalogNumber,
        decimalLatitude,
        decimalLongitude,
        dateCollectedUTC,
        station
      ),
      by = "catalogNumber"
    )

    tag <- as.data.frame(tag)

    tag_return <- make_tag(
      manufacturer = NA_character_,
      model = NA_character_,
      power_level = NA_real_, # ???
      ping_rate = NA_real_, # ???
      ping_variation = NA_real_, # ???
      serial = as.integer(tag$tagName),
      transmitter = NA_character_, # ???
      activation_datetime = as.POSIXct(NA_real_),
      battery_life = NA_real_,
      sensor_type = NA_character_, # ???
      sensor_unit = NA_character_, # ???
      animal = NA_character_,
      tz = "UTC"
    )


    return(tag_return)
  } else {
    message("Invalid type specified. Use either 'meta' (if loading from a metadata file) or 'extract' (if deriving deployment metadata from a detection extract).")
  }
}
