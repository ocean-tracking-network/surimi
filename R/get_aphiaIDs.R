##' @title Get AphiaIDs for scientific names
##'
##' @description Takes a column of scientific names and creates a lookup table (read: named list) of the unique scientific names
##' against their aphia IDs. We can use worrms to query the WORMS REST service for the aphiaIDs, but doing it for every row is
##' time intensive in a way we don't want. This way, we can create the lookup client-side and then do all the querying only as
##' we need to.
##'
##' @param scinames A vector (dataframe column) containing the list of scientific names from a detection extract dataframe in
##' Surimi.
##'
##' @importFrom worrms wm_name2id
##'
##' @return Returns a named list with the scientific name as the key and the aphiaID as the value.
##'

# get a table of unique scientific names and aphiaIDs.
get_unique_aphiaids <- function(scinames) {
  # Get the unique names.
  unique_names <- unique(scinames)

  # Create an empty list to hold our name/value pairs.
  aphia_ids <- list()

  # Build the dict
  for (name in unique_names) {
    # wm_name2id is a worrms function for associating a scientific name with its Aphia ID.
    aphia_ids[[name]] <- worrms::wm_name2id(name)
  }

  # return the table.
  return(aphia_ids)
}


##' @title Consult a lookup table for the aphiaID.
##'
##' @description This is the helper function that we use in the sapply when mutating the WORMS_species_aphia_id into existence.
##'
##' @param sciname A Scientific name as a string.
##' @param lookup The named list containing key-value pairs of scientific names and aphiaIDs.
##'
##' @return Returns the appropriate aphiaID corresponding to the sciname.
##'

get_aphiaid_from_lookup <- function(sciname, lookup) {
  # Get the aphiaID from the lookup table.
  aphiaid <- lookup[[sciname]]
  # Strip off the 'named' part (this is what isn't working)
  # aphiaid <- as.character(unname(aphiaid))
  return(aphiaid)
}
