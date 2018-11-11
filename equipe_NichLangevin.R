## Hackaton Meet-up machine learning - 10 novembre 2018
## Gabriel Crépeault-Cauchon
## Alexandre Gagnon
## Nicholas Langevin


## Packages nécessaires ####
library(heuristica)
library(rstudioapi)

## Place le work directory pour n'importe quel ordinateur 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## Importation des données
raw.data <- read.csv("data/train-features.csv", header = T)
toitvert <- read.csv("data/data-id-train.csv")
dat <- cbind(toitvert, raw.data[,-1])

#  On retire l'identifiant et l'indicateur des toits vers pour calculer les PCAs
pca.model <- prcomp(dat[,-c(1,2)])
pca.x <- pca.model$x
pca.summary <- summary(pca.model)

## Visualisation de la variance cumulative des variables explicatives
plot(pca.summary$importance[3,])

## Séparation des données d'entrainement et de validation
k <- sample(1:nrow(pca.x),0.7 * nrow(pca.x), replace = F)
f1_score <- matrix(NA, nrow = length(importance), ncol = length(treshold))

## Fonction qui calcul les f1 score pour des vecteurs d'importance et de treshold
calcul_f1 <- function(importance, treshold)
{
  colnames(f1_score) <- treshold
  rownames(f1_score) <- importance
  for (i in seq_along(importance))
  {
    for (j in seq_along(treshold))
    {
      dat.pca <- data.frame(Ind = toitvert$green_roof_ind,
                            pca.x[, which(pca.summary$importance[3,] <= importance[i])])
      model.pca <- glm(Ind~., data = dat.pca, family = binomial(link = 'logit'))
      # summary(model.pca) ; anova(model.pca)
      train <- dat.pca[k,]
      valid <- dat.pca[-k,]
      
      
      train.mod <- glm(Ind~., data = train, family = binomial(link = 'logit'))
      
      ## Calcul des prédictions et de la conversion
      valid$Pred <- predict(train.mod, newdata = valid, type = 'response')
      valid$validbin <- ifelse(valid$Pred >= treshold[j], 1, 0)
      
      ## Générer la matrice des confusions et calculer les statistiques pertinentes
      confusion_mat <- confusionMatrixFor_Neg1_0_1(valid$Ind, valid$validbin)[-1,-1]
      precision <- confusion_mat[2,2] / sum(confusion_mat[,2])
      recall <- confusion_mat[2,2] / sum(confusion_mat[2,])
      
      f1_score[i,j] <- 2 * (precision * recall) / (precision + recall)
    }
    
  }
  f1_score
}

## Tests pour différentes valeur d'importance et de treshold
importance <- c(seq(0.5, 0.9, 0.1), 0.95)
treshold <- seq(0.15, 0.4, 0.05)
calcul_f1(importance, treshold)

## On réduit les intervalles tester
importance <- c(seq(0.5, 0.9, 0.1), 0.95)
treshold <- seq(0.15, 0.2, 0.01)
calcul_f1(importance, treshold)

## modèle avec meilleur F1_score selon la dernière matrice produite: 
treshold_final <- 0.17
importance_final <- 0.8
f1_final <- 0.387931

dat.pca_final <- data.frame(Ind = toitvert$green_roof_ind,
                      pca.x[, which(pca.summary$importance[3,] <= importance_final)])
mod_final <- glm(Ind~., data = dat.pca_final, family = binomial(link = 'logit'))

predictions_final <- ifelse(mod_final$fitted.values>= treshold_final, 1, 0)
conf_mat_final <- confusionMatrixFor_Neg1_0_1(dat.pca_final$Ind,
                                              predictions_final)





conf_mat_final[-1,-1] / length(mod_final$fitted.values)

## Sauvgarde de l'environement de travail pour le shiny app
save(list = ls(all.names = TRUE), file = "environement")



