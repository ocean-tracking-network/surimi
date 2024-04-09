#' @title Map IMOS detection data to an OTN-like format
#' @description In the same way that otn_imos_column_map takes OTN data and massages it into an IMOS-like format for REMORA,
#' this function and its ilk take IMOS data (in this case, animal detections) and massages it into an OTN-like format, for the
#' purposes of reporting and more general applicability within the OTN suite of programs.
#'
#' @param rcvr_dataframe A dataframe containing IMOS receiver metadata.
#'
#' @return A dataframe containing the above data in an OTN-like format.
#'
#' @importFrom dplyr '%>%' mutate rename
#' @importFrom tidyr separate
#' @export
#'

imos_to_otn_detections <- function(detection_dataframe, coll_code = NULL) {
  det_return <- detection_dataframe %>%
    dplyr::select(
      detection_datetime,
      receiver_name,
      transmitter_id,
      transmitter_sensor_value,
      transmitter_sensor_unit,
      transmitter_sensor_type,
      station_name,
      receiver_project_name,
      receiver_deployment_latitude,
      receiver_deployment_longitude,
      species_scientific_name,
      species_common_name,
      ends_with("_QC"),
    ) %>%
    mutate(
      sensorname = transmitter_id,
      detectedby = receiver_project_name,
      collectioncode = coll_code,
    ) %>%
    rename(
      date_and_time = detection_datetime,
      receiver = receiver_name,
      tagname = transmitter_id,
      sensorvalue = transmitter_sensor_value,
      sensorunit = transmitter_sensor_unit,
      latitude = receiver_deployment_latitude,
      longitude = receiver_deployment_longitude,
      scientificname = species_scientific_name,
      commonname = species_common_name,
      sensortype = transmitter_sensor_type,
      station = station_name,
    )

  return(det_return)
}
