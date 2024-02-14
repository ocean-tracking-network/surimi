# We can use the worrms library to get aphiaIDs, but since we could be dealing with very large datasets, we don't want to query the worms DB as part of the mutate/column mapping process, since we'd be doing a query for
# every row and therefore doing a lot of duplicate work and slowing down the code (I tried this- setting the 'WORMS_species_aphia_id' column equal to an lapply of wm_name2id on the scientific name column- and even on a dataset
# of only a few hundred rows, it bloated the time quite badly.) What we're going to do instead is collect and pre-query aphiaIDs for only the unique scientific names, for which we'll create a lookup table that we can then
# use to build out the column a little more quickly.

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

get_aphiaid_from_lookup <- function(sciname, lookup) {
  # Get the aphiaID from the lookup table.
  aphiaid <- lookup[[sciname]]
  # Strip off the 'named' part (this is what isn't working)
  # aphiaid <- as.character(unname(aphiaid))
  return(aphiaid)
}
