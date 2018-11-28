LoadPackages = function(packageNameList, gitUser = NULL) {
  # Load installed package or install it from CRAN (with argument gitUser
  # missing or NULL) or a GitHub user's repo if the package is missing and then
  # load it. A utility function `install_github` from package `remotes` will be
  # installed if not found from that or another package.

  for (iPackages in packageNameList) {

    if (!require(iPackages, character.only = TRUE)) {
      # Attempts to load package, if it does not exist the above returns TRUE

      if (!is.null(gitUser)) {

        if (!exists("install_github")) {
          LoadPackages("remotes")
        }
        install_github(paste(gitUser, iPackages, sep = "/"))

      } else {
        install.packages(iPackages, dependencies = TRUE)
      }

      library(iPackages, character.only = TRUE)
    }

  }

}