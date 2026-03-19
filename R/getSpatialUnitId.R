getSpatialUnitId <- function(row_idx, cbm_key, standDT){
  # get the spatial unit look-up table
  cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), libcbmr::get_cbm_defaults_path())
  spuMeta <- data.table::as.data.table(RSQLite::dbReadTable(cbmDBcon, "spatial_unit")) |>
    merge(data.table::as.data.table(RSQLite::dbReadTable(cbmDBcon, "admin_boundary_tr"))[
      locale_id == 1, .(admin_boundary_id, admin_name = name)],
      by = "admin_boundary_id")
  data.table::setnames(spuMeta, "id", "spatial_unit_id")
  RSQLite::dbDisconnect(cbmDBcon)
  
  # get the admin_id and eco_id of the new cohorts
  newCohortMeta <- merge(data.table(row_idx = row_idx), cbm_key[,.(row_idx, pixelIndex)], by = "row_idx")
  newCohortMeta <- merge(newCohortMeta, standDT, by = "pixelIndex")
  
  # extract the spatial_unit_id for each cohort Groups
  newCohortMeta <- merge(newCohortMeta[,.(row_idx, admin_name, eco_id)], spuMeta[,.(spatial_unit_id, admin_name, eco_id = eco_boundary_id)], by = c("admin_name", "eco_id"))
  newCohortMeta <- unique(newCohortMeta[,.(row_idx, spatial_unit_id)])
  
  # There should be only 1 spatial unit id per cohortgroup
  if(any(duplicated(newCohortMeta$row_idx)) | length(row_idx) != nrow(newCohortMeta)){
    stop("Error in assigning spatial unit to cohort group.")
  }
  
  return(newCohortMeta$spatial_unit_id)
}