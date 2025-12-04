test_that("split_list partitions inputs", {
  x <- as.list(letters[1:5])
  parts <- split_list(x, 2)
  expect_length(parts, 2)
  expect_equal(unlist(parts, use.names = FALSE), x)
})

test_that("load_preprocess_data reads and splits biomes", {
  skip_if_not_installed("sf")
  tmp_dir <- withr::local_tempdir()
  biomes <- sf::st_sf(
    biome = c("a", "a", "b"),
    agreement = c(0.5, 0.6, 0.7),
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
      sf::st_polygon(list(rbind(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1)))),
      sf::st_polygon(list(rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2))))
    ),
    crs = 4326
  )
  shp_path <- file.path(tmp_dir, "biomes.shp")
  suppressWarnings(sf::st_write(biomes, shp_path, quiet = TRUE))

  res <- load_preprocess_data(shp_path)
  expect_true(is.list(res))
  expect_named(res, c("a", "b"))
  expect_equal(nrow(res$a), 2)
  expect_equal(nrow(res$b), 1)
})
