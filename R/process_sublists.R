#' Process each sublist of sf objects to download and save rasters
#'
#' @param biomat A list of sublists of sf objects
#' @param output_dir The directory where the output rasters will be saved
#' @param download_path The path where downloaded rasters will be stored
#' @param change_vals A sequence of years for which data will be downloaded
#' @param bin_output Logical. If TRUE, download binary forest/no-forest maps; otherwise keep canopy percentages.
#' @param n_cores Number of cores for parallel processing
#' @importFrom ecochange echanges
#' @importFrom terra rast
#' @importFrom terra writeRaster
#' @return None
#' @export
#' @author Jeronimo Rodriguez-Escobar <jeronimo.rescobar@gmail.com>
process_sublists <- function(biomat, output_dir, download_path, change_vals = seq(22, 23, 1), bin_output = FALSE, n_cores = 2) {
  # Function to process each sf object and download rasters
  process_raster <- function(sf_obj, output_file) {
    # Download the raster data
    d <- echanges(
      sf_obj,
      lyrs = c("treecover2000", "lossyear"),
      path = download_path,
      eco_range = c(sf_obj$threshold, 100),
      change_vals = change_vals,
      binary_output = bin_output,
      mc.cores = n_cores
    )

    # Convert each RasterLayer to SpatRaster
    d <- lapply(d, function(x) {
      if (inherits(x, "RasterLayer")) {
        return(terra::rast(x))
      } else {
        stop("Expected a RasterLayer")
      }
    })

    # Stack the bands
    r <- rast(d)

    # Save the stacked raster
    writeRaster(r, paste0(output_file, '.tif'), overwrite = TRUE)
  }

  # Apply the function to each sublist
  for (i in seq_along(biomat)) {
    biomat_r <- biomat[[i]]
    message(paste("Processing sublist", i, "of", length(biomat)))

    # Create output directory for the sublist
    sublist_output_dir <- file.path(output_dir, paste0("sublist_", i))
    if (!dir.exists(sublist_output_dir)) {
      dir.create(sublist_output_dir, recursive = TRUE)
    }

    # Apply the process to each sf object in the sublist
    lapply(seq_along(biomat_r), function(idx) {
      sf_obj <- biomat_r[[idx]]
      output_file <- file.path(sublist_output_dir, paste0("raster_", idx))
      process_raster(sf_obj, output_file)
      invisible(NULL)
    })
  }
}
