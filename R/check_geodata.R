#' Check and Download Geospatial Data from GitHub
#'
#' Checks if the required geospatial data files are present in the package's `inst/extdata` directory. If the files are missing, they are downloaded from a specified GitHub repository. The user can specify which files to download, or download all available files by default.
#'
#' This function uses the `curl` package to download files, and supports displaying a progress bar during the download process. It is particularly useful for large files or when the default download method (`download.file`) is slow or unreliable.
#'
#' @param files Character vector. The names of the data files to download. If `NULL`, all available files will be downloaded.
#' @param overwrite Logical. Whether to overwrite existing files in `inst/extdata/`. Default is `FALSE`.
#' @param quiet Logical. If `TRUE`, suppresses message outputs when files already exist. Default is `FALSE`.
#' @param max_retries Integer. The maximum number of retries if a download fails. Default is `3`.
#' @return A character vector of file paths to the downloaded or existing files.
#' @examples
#' \donttest{
#' # Check for and download all geospatial data from GitHub
#' file_paths <- check_geodata()
#'
#' # Check and download specific files
#' file_paths <- check_geodata(files = c("boundary.rda"))
#' }
#' @importFrom curl curl_download
#' @importFrom utils packageDescription
#' @export
check_geodata <- function(files = NULL, overwrite = FALSE, quiet = FALSE, max_retries = 3) {

  # Define the fixed GitHub URL (do not expose to the user)
  github_url <- "https://raw.githubusercontent.com/Rimagination/ggmapcn-data/main/data/"

  # Get the package path where the data should be stored
  package_path <- system.file(package = "ggmapcn")

  # Define the directory to save the downloaded files
  extdata_path <- file.path(package_path, "extdata")

  # First, check if the file exists in the package's inst/extdata directory
  file_list <- c("China_sheng.rda",
                 "China_shi.rda",
                 "China_xian.rda",
                 "boundary.rda",
                 "buffer_line.rda",
                 "China_mask.gpkg",
                 "world.rda",
                 "gebco_2024_China.tif",
                 "vege_1km_projected.tif")

  # If no specific files are requested, download all files
  if (is.null(files)) {
    files <- file_list
  } else {
    # Check for invalid file names
    invalid_files <- setdiff(files, file_list)
    if (length(invalid_files) > 0) {
      stop("Invalid file names requested: ", paste(invalid_files, collapse = ", "))
    }
  }

  # Create a vector to store the paths of the downloaded files
  downloaded_files <- vector("character", length(files))

  # Loop through the requested files and check if they already exist
  for (i in seq_along(files)) {
    file_name <- files[i]

    # First, try to get the file from the extdata path
    file_path <- file.path(extdata_path, file_name)

    if (!file.exists(file_path)) {
      # If the file doesn't exist in extdata, try tempdir()
      temp_path <- file.path(tempdir(), file_name)
      if (!file.exists(temp_path)) {
        # If not in tempdir() either, proceed with downloading
        file_path <- temp_path
      } else {
        # If found in tempdir(), use it and skip download
        downloaded_files[i] <- temp_path
        if (!quiet) {
          message(file_name, " already exists in tempdir(). Skipping download.")
        }
        next
      }
    } else {
      # If file exists in extdata, use it
      downloaded_files[i] <- file_path
      if (!quiet) {
        message(file_name, " already exists in extdata. Skipping download.")
      }
      next
    }

    # Construct the raw URL for the file from GitHub
    file_url <- paste0(github_url, file_name)

    # Download the file if it doesn't exist locally or if overwrite is TRUE
    if (!quiet) {
      message("Downloading ", file_name, " from GitHub...")
    }

    # Attempt to download with retries
    attempt <- 1
    success <- FALSE
    while (attempt <= max_retries && !success) {
      tryCatch({
        # Use curl_download from the curl package to download the file
        curl_download(
          url = file_url,
          destfile = file_path,
          handle = curl::new_handle(progressfunction = function(down, up) {
            if (!quiet) {
              percent <- (down / up) * 100
              cat(sprintf("\rDownloading %s: %.2f%%", file_name, percent))
            }
          })
        )

        # Mark as successful if download completes
        success <- TRUE
        downloaded_files[i] <- file_path
        if (!quiet) {
          cat("\nDownload complete!\n")
        }
      }, error = function(e) {
        # Handle specific errors like timeout or SSL errors
        if (attempt == max_retries) {
          warning(paste("Failed to download", file_name, "after", max_retries, "attempts."))
          downloaded_files[i] <- NA
        } else {
          warning(paste("Attempt", attempt, "failed to download", file_name, "due to error:", e$message))
          message("Retrying in 5 seconds...")
          Sys.sleep(5)  # Wait for 5 seconds before retrying
        }
      })

      # Increment retry attempt counter
      attempt <- attempt + 1
    }
  }

  # Return the paths of downloaded or existing files
  return(downloaded_files)
}
