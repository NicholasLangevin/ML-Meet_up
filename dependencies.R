## App dependencies
library(rstudioapi)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## Validation que certains packages sont installés



## Packages nécessaires pour l'application
require(shiny)
require(shinydashboard)
require(shinyWidgets)
require(MASS)
require(rstudioapi)
require(car)
require(ggplot2)
require(tidyr)
require(dplyr)
require(alr3)
require(DT)
require(heuristica)



# source : 
# https://stats.stackexchange.com/questions/145790/anova-table-for-model-in-r
simpleAnova <- function(object, ...) {
  # Fonction trouvée sur les internet : 
  # https://stats.stackexchange.com/questions/145790/anova-table-for-model-in-r
  # Compute anova table
  tab <- anova(object, ...)
  
  # Obtain number of predictors
  p <- nrow(tab) - 1
  
  # Add predictors row
  predictorsRow <- colSums(tab[1:p, 1:2])
  predictorsRow <- c(predictorsRow, predictorsRow[2] / predictorsRow[1])
  
  # F-quantities
  Fval <- predictorsRow[3] / tab[p + 1, 3]
  pval <- pf(Fval, df1 = p, df2 = tab$Df[p + 1], lower.tail = FALSE)
  predictorsRow <- c(predictorsRow, Fval, pval)
  
  # Simplified table
  tab <- rbind(predictorsRow, tab[p + 1, ])
  row.names(tab)[1] <- "Predictors"
  return(tab)
  
}