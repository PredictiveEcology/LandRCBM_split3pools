# Same result as `unique(dt, by = by)` for a `dt` keyed by `id`, which must be one
# of the `by` columns, but only compares rows that share an `id` value:
# rows with different `id` values cannot be duplicates of each other.
# This avoids a multi-column comparison of every row when `id` is mostly
# unique, as for cohort IDs.
uniqueByRepeatedId <- function(dt, id, by = names(dt)){
  stopifnot(id %in% by)
  idValues <- dt[[id]]
  n <- length(idValues)
  if (!identical(key(dt)[1L], id) || anyNA(idValues)) return(unique(dt, by = by))
  if (n < 2L) return(dt)
  
  # Rows are sorted by `id`: rows sharing an `id` value with a neighbour
  sameAsNext <- idValues[-n] == idValues[-1L]
  if (!any(sameAsNext)) return(dt)
  shared <- which(c(sameAsNext, FALSE) | c(FALSE, sameAsNext))
  keep <- rep(TRUE, n)
  keep[shared] <- !duplicated(dt[shared], by = by)
  dt[keep]
}
