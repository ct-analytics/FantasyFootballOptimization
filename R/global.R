# Used packages
packages <- c("shiny", 
              "shinydashboard", 
              "shinythemes", 
              "ffsimulator", 
              "ggplot2", 
              "ggthemes",
              "lubridate", 
              "ffscrapr", 
              "DT", 
              "dplyr", 
              'shinyjs', 
              "patchwork",
              "bslib",
              "thematic")

# Run the following command to verify that the required packages are installed. If some package
# is missing, it will be installed automatically
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
  }
})


