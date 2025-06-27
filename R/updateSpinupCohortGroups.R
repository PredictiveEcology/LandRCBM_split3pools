updateSpinupCohortGroups <- function(spinupOut){
  spinupOutputs <- spinupOut$output
  # Step 1: Combine pools and state columns into a single data.table
  # Round pools to mg/g
  combinedOutputs <- cbind(
    spinupOutputs$pools,
    spinupOutputs$state[, c("spatial_unit_id", "age", "species")]
  ) |> as.data.table()
  
  # Step 2: Create a unique cohort group ID for identical rows
  combinedOutputs[, cohortGroupID := .GRP, by = names(combinedOutputs)]
  
  # Step 3: Update the cohortGroupID in the key table
  spinupOut$key$cohortGroupID <- combinedOutputs$cohortGroupID
  
  # Step 4: Update spinup outputs
  spinupOut$output <- lapply(spinupOutputs, function(tbl){
    tbl <- as.data.table(tbl)
    tbl[, row_idx := combinedOutputs$cohortGroupID]
    tbl <- unique(tbl)
    tbl[, row_idx := NULL]
    as.data.frame(tbl)
  })
  
  return(spinupOut)
}