mergeMaps <- function(map1, map2, out = "map", indexName = "newIndex") {
  dt <- data.table(index1 = as.integet(map1[]),
                   index2 = as.integer(map2[])) |>
    setorder(col1, na.last = TRUE)
  dt[, newIndex := .GRP, by = .(index1, index2)]
  
  if(out == "map") {
    mapOut <- map1
    mapOut[order(map1[])] <- dt$newIndex
    mapOut <- mask(mapOut, map1)
    names(mapOut) <- indexName
    return(mapOut)
  } else if (out == "table") {
    dt <- na.omit(dt) |> unique()
    colnames(dt) <- c(names(map1), names(map2), indexName)
    return(dt)
  } else if (out == "both") {
    mapOut <- map1
    mapOut[order(map1[])] <- dt$newIndex
    mapOut <- mask(mapOut, map1)
    names(mapOut) <- indexName
    dt <- na.omit(dt) |> unique()
    colnames(dt) <- c(names(map1), names(map2), indexName)
    return(list(map = mapOut,
                dt = dt))
  } else {
    stop("the argument ", out, " should be `map`, `table`, or `both`.")
  }
  
}