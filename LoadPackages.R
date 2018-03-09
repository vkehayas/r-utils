LoadPackages = function(packageName, gitUser = NULL) {
  #' Load installed package or install it from CRAN (with argument gitUser 
  #' missing or NULL) or a GitHub user's repo if the package is missing and then 
  #' load it. A utility function `install_github` from package `ghit` will be 
  #' installed if not found from that or another package.
  

    if (!require(packageName, character.only = TRUE)) {
      # Attempts to load package. If it does not exist, the above returns TRUE
      
      if (!is.null(gitUser)) {
        
        if (!exists("install_github")) {
          LoadPackages("ghit")
        }
        install_github(paste(gitUser, packageName, sep = "/"))
        
      } else {
        install.packages(packageName, dependencies = TRUE)
      }
      
      library(packageName, character.only = TRUE)
    }
    
  
}