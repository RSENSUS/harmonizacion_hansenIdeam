#' Load and preprocess input data
#'
#' @param path_biomes Path to the input shapefile containing the biomes
#' @return A list of sf objects, each representing a polygon
#' @export
load_preprocess_data <- function(path_biomes) {
  masked <- sf::st_read(path_biomes, quiet = TRUE)
  # Remove biomes for which the threshold attribute is empty (NA)
  masked <- masked[!is.na(masked$agreement), ]
  split(masked, masked$biome)
}
