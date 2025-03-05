test_that("functions to mergeMaps works", {

  #normal behaviour
  map1 <- terra::rast(
    matrix(data = c(rep(1,3), rep(2, 2), rep(3, 4)), nrow = 3, ncol = 3)
  )
  map2 <- terra::rast(
    matrix(data = c(rep(1,2), rep(2, 3), rep(3, 1), rep(4,3)), nrow = 3, ncol = 3)
  )
  expected_map <- terra::rast(
    matrix(data = c(rep(1,2), rep(2, 1), rep(3, 2), rep(5,1), rep(4,3)), nrow = 3, ncol = 3)
  )
  
  out1 <- mergeMaps(map1, 
                    map2,
                    out = "both")
  expect_named(out1, c("map", "dt"))
  expect_is(out1$map, "SpatRaster")
  expect_is(out1$dt, "data.table")
  expect_equivalent(out1$map[], expected_map[])
  
  out2 <- mergeMaps(map1, 
                    map2,
                    out = "map")
  expect_is(out2, "SpatRaster")
  expect_equivalent(out2[], expected_map[])
  
  
  out3 <- mergeMaps(map1, 
                    map2,
                    out = "table")
  expect_is(out3, "data.table")
})


