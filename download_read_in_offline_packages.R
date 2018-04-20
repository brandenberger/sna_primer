#' --- Get package dependencies
#'
#' @param packs A string vector of package names
#'
#' @return A string vector with packs plus the names of any dependencies
getDependencies <- function(packs){
  dependencyNames <- unlist(
    tools::package_dependencies(packages = packs, db = available.packages(), 
                                which = c("Depends", "Imports"),
                                recursive = TRUE))
  packageNames <- union(packs, dependencyNames)
  packageNames
}
# Calculate dependencies
packages <- getDependencies(c("ggplot2","GGally","statnet"))
# We can then download the right package type for the environment we’re going to be training. Often our customers are on Windows so we would download the “win.binary” type. We’re also going to save the package file names too so that we can install them by filename later.

# Download the packages to the working directory.
# Package names and filenames are returned in a matrix.
# setwd("D:/my_usb/packages/")
pkgInfo <- download.packages(pkgs = packages, destdir = "offline_packages", type = "win.binary")
# Save just the package file names (basename() strips off the full paths leaving just the filename)
write.csv(file = "offline_packages\\pkgFilenames.csv", basename(pkgInfo[, 2]), row.names = FALSE)

# --- read offline

# Set working directory to the location of the package files
setwd("D:/my_usb/packages/")

# Read the package filenames and install
pkgFilenames <- read.csv("pkgFilenames.csv", stringsAsFactors = FALSE)[, 1]
install.packages(pkgFilenames, repos = NULL, type = "win.binary")