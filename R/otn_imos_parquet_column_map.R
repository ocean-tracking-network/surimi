##' @title Map OTN-formatted data from our new Parquet detection extracts  to IMOS-format
##'
##' @description Takes three dataframes in the OTN format- one for a detection extract, one for receiver deployment metadata, and one for tag metadata-
##' and rearranges, renames, and creates columns until they can pass for IMOS-format dataframes. This allows us to pass the data directly
##' into Remora without making substantial changes to how that code runs or what it looks for. This is functionally identical to otn_imos_column_map() except that the
##' column names on the OTN side reflect that the detection dataframe came from our new parquet format rather than our old CSV format.
##'
##' @param det_dataframe The dataframe containing detection information.
##' @param rcvr_dataframe The dataframe containing receiver information.
##' @param tag_dataframe The dataframe containing tag information.
##' @param derive An optional flag that allows the user to pass in fewer than all three files. If given, the code will use the detection
##' extract dataframe to generate dataframes for either or both of the receiver and tag dataframes, if they are not passed in. Although
##' this will result in missing information, it does let the user supply only a detection extract file, which is a situation some may
##' find themselves in.
##' @param coll_code The user-supplied collectioncode, which we'll use to populate the receiver_project_name and tagging_project_name
##' columns in the receiver and tag metadata files respectively. We don't have a good way to associate the relevant info from the det
##' extract to the appropriate columns in the rcvr/tag metadata, but those datasets are restricted to one collectioncode each, so
##' we can just take it from the user at the time they run the code.
##' @param tagname_column The name of the column that's equivalent to 'tagname', if the tagname column isn't present. Should only be
##' necessary if deriving.
##'
##' @importFrom dplyr select '%>%' mutate rename group_by arrange distinct filter left_join
##' @importFrom tidyr unite
##' @importFrom lubridate ymd as_date
##' @importFrom nanoparquet read_parquet
##'
##' @return Returns a list containing three approximately IMOS-formatted dataframes.
##' @export
##'
otn_imos_parquet_column_map <- function(det_dataframe, rcvr_dataframe = NULL, tag_dataframe = NULL, derive = TRUE, coll_code = NULL, tagname_column = "tagName") {
  # We need to ultimately produce the following:
  # - A detections dataframe with columns appropriate to the IMOS spec.
  # - A receiver dataframe with appropriate columns, if necessary with data derived from the detections dataframe.
  # - A tag dataframe, same constraints as above.
  # - An animal measurements dataframe with data derived from the tag dataframe.

  # At the end of all this we return three dataframes- det_data, rec_data, tag_data- that can be passed through to the merge at the bottom of the get_data function.
  # Animal measurements is a bit weird so I'm going to ignore it for now.

  # This probably won't have the full range of columns that the equivalent IMOS data would.

  # This way, if we don't end up having any way to change these throughout- i.e, no rcvr/tag sheets have been passed-
  # we just return whatever we got, unaltered. Probably null.
  tag_return <- tag_dataframe
  rcvr_return <- rcvr_dataframe

  # Quit instantly if there is no detections dataframe. This is unlikely since this check already happens in the function above, but for completeness'
  # sake we'll include it.
  if (is.null(det_dataframe)) stop("\033[31;1mCan not run otn -> imos conversion without a detections file!\033[0m\n")

  if (!is.data.frame(det_dataframe)) {
    det_dataframe <- read_parquet(det_dataframe)
  }

  # If we don't get passed a receiver or tag dataframe, we derive them from det. This will give us hopefully enough info that we can create the final
  # detection dataframe to be returned, which will be valid for Remora. Ideally.
  if (is.null(rcvr_dataframe) && derive) {
    message("Deriving receiver dataframe...")
    rcvr_return <- derive_rcvr_from_det(det_dataframe)
  }

  if (is.null(tag_dataframe) && derive) {
    message("Deriving tag dataframe...")
    tag_return <- derive_tag_from_det(det_dataframe, tagname_column)
  }

  # Construct a little lookup table for the aphiaIDs. This keeps us from having to query the WORMS database over and over again (for example, the data I tested on had 300 entries for 'blue shark')- lot of redundant querying there.
  View(det_dataframe$scientificName)

  lookup <- get_unique_aphiaids(det_dataframe$scientificName)

  View(lookup)

  # Start by mapping the Detections dataframe.
  det_return <- det_dataframe %>%
    select(
      dateCollectedUTC,
      catalogNumber,
      tagName,
      collectionCode,
      commonName,
      scientificName,
      detectedBy,
      station,
      receiver,
      sensorType,
      sensorValue,
      sensorUnit,
      decimalLongitude,
      decimalLatitude
    ) %>%
    mutate(
      cleandate = ymd(as_date(dateCollectedUTC)),
      CAAB_species_id = NA,
      WORMS_species_aphia_id = sapply(det_dataframe$scientificName, USE.NAMES = FALSE, FUN = get_aphiaid_from_lookup, lookup = lookup),
      animal_sex = NA,
      receiver_name = receiver,
      receiver_project_name = detectedBy,
      transmitter_serial_number = NA,
      transmitter_type = NA,
      transmitter_sensor_type = NA,
      transmitter_sensor_slope = NA,
      transmitter_sensor_intercept = NA,
      transmitter_sensor_unit = NA,
      transmitter_status = NA,
      transmitter_estimated_battery_life = NA,
      transmitter_sensor_value = NA,
      embargo_date = NA,
      transmitter_deployment_latitude = NA,
      transmitter_deployment_longitude = NA,
      transmitter_deployment_datetime = NA,
    ) %>%
    # We need to make these transmitter and receiver deployment ID columns so that we have something to join on. These do not necessarily correspond to our
    # own catalognumber fields.
    unite(
      receiver_deployment_id, c("collectionCode", "station"),
      sep = "-", remove = FALSE
    ) %>%
    rename(
      transmitter_id = tagName,
      tag_id = catalogNumber,
      species_common_name = commonName,
      species_scientific_name = scientificName,
      detection_datetime = dateCollectedUTC,
      installation_name = collectionCode,
      station_name = station,
      receiver_id = receiver,
      receiver_deployment_longitude = decimalLongitude,
      receiver_deployment_latitude = decimalLatitude # counterintuitively, we rename these here even though they get renamed BACK to their originals
      # back outside this function. The code outside still has to work for IMOS formatted data so we can't change it too much, so when we massage
      # the column names like so, we have to introduce a step that maybe we'd rather skip.
    )
  det_return$transmitter_deployment_id <- det_return$tag_id
  det_return$tagging_project_name <- det_return$installation_name

  # If we have receiver_meta, convert that to an IMOS friendly version.
  if (!is.null(rcvr_dataframe) && !derive) {
    rcvr_return <- rcvr_dataframe %>%
      dplyr::select(
        OTN_ARRAY,
        STATION_NO,
        DEPLOY_DATE_TIME,
        DEPLOY_LAT,
        DEPLOY_LONG,
        BOTTOM_DEPTH,
        RISER_LENGTH,
        INSTRUMENT_DEPTH,
        INS_MODEL_NUMBER,
        INS_SERIAL_NUMBER,
        CODE_SET,
        TRANSMITTER,
        TRANSMIT_MODEL,
        AR_MODEL_NUMBER,
        AR_SERIAL_NUMBER,
        DEPLOYED_BY,
        RECOVERED,
        RECOVER_DATE_TIME,
        RECOVER_LAT,
        RECOVER_LONG,
        DATA_DOWNLOADED,
        DOWNLOAD_DATE_TIME,
        FILENAME
      ) %>%
      # We're going to merge INS_MODEL_NUMBER and INS_SERIAL_NUMBER to make receiver_name
      unite(
        receiver_name, c("INS_MODEL_NUMBER", "INS_SERIAL_NUMBER"),
        sep = "-", remove = FALSE
      ) %>%
      unite(
        receiver_deployment_id, c("OTN_ARRAY", "STATION", "INS_SERIAL_NUMBER"),
        sep = "-", remove = FALSE
      ) %>%
      rename(
        receiver_status = RECOVERED,
        receiver_deployment_datetime = DEPLOY_DATE_TIME,
        installation_name = OTN_ARRAY,
        station_name = STATION_NO,
        receiver_deployment_latitude = DEPLOY_LAT,
        receiver_deployment_longitude = DEPLOY_LONG,
        depth_below_surface = INSTRUMENT_DEPTH,
        receiever_recovery_datetime = RECOVER_DATE_TIME,
        receiver_recovery_latitude = RECOVER_LAT,
        receiver_recovery_longitude = RECOVER_LONG,
      ) %>%
      mutate(
        purchasing_organization = NA,
        receiver_project_name = coll_code,
      )
  }

  # And if we have tag metadata, convert that too.
  if (!is.null(tag_dataframe) && !derive) {
    tag_return <- tag_dataframe %>%
      dplyr::select(
        `ANIMAL_ID   (floy tag ID, pit tag code, etc.)`,
        TAG_TYPE,
        TAG_MANUFACTURER,
        TAG_MODEL,
        TAG_SERIAL_NUMBER,
        TAG_ID_CODE,
        TAG_CODE_SPACE,
        TAG_IMPLANT_TYPE,
        TAG_IMPLANT_METHOD,
        TAG_ACTIVATION_DATE,
        EST_TAG_LIFE,
        TAGGER,
        TAG_OWNER_PI,
        TAG_OWNER_ORGANIZATION,
        COMMON_NAME_E,
        SCIENTIFIC_NAME,
        CAPTURE_LOCATION,
        CAPTURE_LATITUDE,
        CAPTURE_LONGITUDE,
        WILD_OR_HATCHERY,
        STOCK,
        # `LENGTH (m)`,
        # `WEIGHT (kg)`,
        LENGTH_TYPE,
        # `LENGTH2 (m)`,
        # LENGTH2_TYPE,
        LIFE_STAGE,
        AGE,
        AGE_UNITS,
        SEX,
        DNA_SAMPLE_TAKEN,
        TREATMENT_TYPE,
        RELEASE_GROUP,
        UTC_RELEASE_DATE_TIME,
        RELEASE_LOCATION,
        RELEASE_LATITUDE,
        RELEASE_LONGITUDE,
        COMMENTS,
        HARVEST_DATE
      ) %>%
      mutate(
        cleandate = ymd(as_date(UTC_RELEASE_DATE_TIME)),
        transmitter_sensor_slope = NA,
        transmitter_sensor_intercept = NA,
        transmitter_sensor_unit = NA,
        transmitter_status = NA,
        embargo_date = NA,
        transmitter_recovery_latitude = NA,
        transmitter_recovery_longitude = NA,
        tagging_project_name = coll_code,
      ) %>%
      unite(
        transmitter_id, c("TAG_CODE_SPACE", "TAG_ID_CODE"),
        sep = "-", remove = FALSE
      ) %>%
      unite(
        transmitter_deployment_id, c("TAG_CODE_SPACE", "TAG_ID_CODE", "cleandate", "RELEASE_LATITUDE", "RELEASE_LONGITUDE"),
        sep = "-", remove = FALSE
      ) %>%
      rename(
        transmitter_serial_number = TAG_SERIAL_NUMBER,
        transmitter_type = TAG_MODEL,
        transmitter_sensor_type = TAG_TYPE,
        transmitter_estimated_battery_life = EST_TAG_LIFE,
        species_common_name = COMMON_NAME_E,
        species_scientific_name = SCIENTIFIC_NAME,
        animal_sex = SEX,
        placement = TAG_IMPLANT_TYPE,
        transmitter_deployment_locality = RELEASE_LOCATION,
        transmitter_deployment_latitude = RELEASE_LATITUDE,
        transmitter_deployment_longitude = RELEASE_LONGITUDE,
        transmitter_deployment_datetime = UTC_RELEASE_DATE_TIME,
        transmitter_deployment_comments = COMMENTS,
        transmitter_recovery_datetime = HARVEST_DATE,
      )
  }
  return(list("detections" = det_return, "receivers" = rcvr_return, "tags" = tag_return))
}


##' @title Derive a receiver metadata data.frame from OTN-formatted data to IMOS-format
##'
##' @description Hack together a piecemeal receiver metadata dataframe for instances
##' where we get detection data, no receiver/tag metadata, but still want to act
##' as though we DID get receiver/tag metadata.
##'
##' @param det_dataframe The dataframe containing detection information.
##' Most likely a detection extract.
##'
##' @importFrom dplyr select '%>%' mutate rename group_by arrange lead
##' @importFrom tidyr unite
##'
##' @keywords internal
##'
derive_rcvr_from_det <- function(det_dataframe) {
  # To start, we will filter the releases out of our detections dataframe.
  no_releases <- det_dataframe %>% filter(receiver != "release")

  # The first thing we need to do is gin up some inferred min and max deployment dates.
  # We'll use the following code to do so.
  rcvr_grouped <- NULL

  # Start by grouping the detections by station, and ordering them by date.
  rcvr_grouped_list <- no_releases %>%
    group_by(station) %>%
    arrange(dateCollectedUTC, .by_group = TRUE)

  # Set min date and max date to null.
  minDate <- NULL
  maxDate <- NULL

  # Create a 'lead' dataframe for us to compare our current dataframe against.
  rcvr_grouped_list_next <- lead(rcvr_grouped_list)

  # For each row in the list
  for (i in 1:nrow(rcvr_grouped_list)) {
    row <- rcvr_grouped_list[i, ]

    # If minDate is null, set it to the currently available date. minDate being null implies that
    # we're just starting with this station (see where it's set to Null, below)
    if (is.null(minDate)) {
      minDate <- row$dateCollectedUTC
    }

    # Get the next row from our "lead" frame.
    nextStation <- rcvr_grouped_list_next[i, ]

    # If our next station is Null (i.e, we're at the end of the frame), or the next station is different from
    # the current one (i.e, we've reached the end of this time chunk)...
    if (is.na(nextStation$receiver) || nextStation$receiver != row$receiver) {
      # Set Maxdate to our current date.
      maxDate <- row$dateCollectedUTC

      # Add the min and max dates as entries in the row.
      row <- row %>% mutate(
        minDetectionDate = minDate,
        maxDetectionDate = maxDate
      )

      # if rcvr_group hasn't been instantiated yet, use row to create it.
      if (is.null(rcvr_grouped)) {
        rcvr_grouped <- row
      }
      # Otherwise, just add the row to the group of receivers.
      else {
        rcvr_grouped <- rbind(rcvr_grouped, row)
      }

      # reset our min and max date to null so the next group will be handled properly.
      minDate <- NULL
      maxDate <- NULL
    }
  }

  # Now we have rcvr_grouped, which contains the receiver metadata with the inferred min and max dates.
  # We can now rename the columns and do the remainder of the manipulation work as normal.

  rcvr <- rcvr_grouped %>%
    select(
      # Select the columns from the detection extract that we have access to.
      collectionCode,
      detectedBy,
      station,
      receiver,
      receiverDepth,
      decimalLongitude,
      decimalLatitude,
      minDetectionDate,
      maxDetectionDate
    ) %>%
    unite( # ??
      receiver_deployment_id, c("collectionCode", "station"),
      sep = "-", remove = FALSE
    ) %>%
    rename(
      # Rename those columns to fit the imos spec
      installation_name = detectedBy,
      station_name = station,
      receiver_name = receiver,
      receiver_project_name = collectionCode,
      depth_below_surface = receiverDepth,
      receiver_deployment_longitude = decimalLongitude,
      receiver_deployment_latitude = decimalLatitude
    ) %>%
    mutate(
      # Add NA-filled columns for anything that can't be derived.
      receiver_id = receiver_name,
      purchasing_organisation = NA,
      receiver_status = NA,
      receiver_deployment_datetime = NA,
      receiver_recovery_datetime = NA,
      receiver_recovery_longitude = NA,
      receiver_recovery_latitude = NA
    )

  return(as.data.frame(rcvr))
}


##' @title Derive a tag metadata data.frame from OTN-formatted data to IMOS-format
##'
##' @description Hack together a piecemeal tag metadata data.frame for instances
##' where we get detection data, no receiver/tag metadata, but still want to act
##' as though we DID get receiver/tag metadata.
##'
##' @param det_dataframe The dataframe containing detection information.
##' Most likely a detection extract.
##'
##' @param tagname_column The name of the column equivalent to 'tagname', if tagname isn't present. This
##' is getting passed in from the above function, but it's here too as a fallback in case the function gets used on its own.
##'
##' @importFrom dplyr select '%>%' mutate rename group_by arrange distinct filter
##' @importFrom dplyr left_join
##' @importFrom tidyr unite
##'
##' @keywords internal
##'
derive_tag_from_det <- function(det_dataframe, tagname_column = "tagName") {
  # Group by tagname.
  distinctTag <- det_dataframe %>%
    group_by(across(tagname_column)) %>%
    distinct(across(tagname_column), .keep_all = TRUE)

  # To get the correct transmitter lat/lon, we need to get the releases.
  releases <- det_dataframe %>%
    filter(receiver == "release") %>%
    group_by(catalogNumber) %>%
    distinct(catalogNumber, .keep_all = TRUE) %>%
    rename(
      transmitter_deployment_id = catalogNumber,
      transmitter_deployment_latitude = decimalLatitude,
      transmitter_deployment_longitude = decimalLongitude,
      transmitter_deployment_datetime = dateCollectedUTC,
      transmitter_deployment_locality = station
    )
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
    ) %>%
    rename(
      transmitter_id = tagName,
      species_common_name = commonName,
      species_scientific_name = scientificName,
      transmitter_deployment_id = catalogNumber,
      tagging_project_name = collectionCode,
      latitude = decimalLatitude,
      longitude = decimalLongitude
    ) %>%
    mutate(
      transmitter_serial_number = NA,
      transmitter_type = NA,
      transmitter_sensor_type = NA,
      transmitter_sensor_slope = NA,
      transmitter_sensor_intercept = NA,
      transmitter_sensor_unit = NA,
      transmitter_estimated_battery_life = NA,
      transmitter_status = NA,
      animal_sex = NA,
      placement = NA,
      transmitter_deployment_comments = NA,
      embargo_date = NA,
      transmitter_recovery_datetime = NA,
      transmitter_recovery_latitude = NA,
      transmitter_recovery_longitude = NA,
    )


  # Now we can join the releases to get the appropriate transmitter_deployment_lat/lon
  tag <- left_join(tag,
    releases %>% dplyr::select(
      transmitter_deployment_id,
      transmitter_deployment_latitude,
      transmitter_deployment_longitude,
      transmitter_deployment_datetime,
      transmitter_deployment_locality
    ),
    by = "transmitter_deployment_id"
  )

  return(as.data.frame(tag))
}
