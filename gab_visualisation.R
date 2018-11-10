# pour PCA on installe ggbiplot
# install.packages("ggbiplot")

library(heuristica)
library(dplyr)
setwd("Desktop/data/")
raw.data <- read.csv("train-features.csv", header = T)
toitvert <- read.csv("data-id-train.csv")
dat <- cbind(toitvert, raw.data[,-1])

# Exploration des données

  

glm.models <- function(str)
{
    mod <- list()
    glm.formula <- as.formula(paste0('green_roof_ind~', str))
    mod$logit <- glm(glm.formula, data = data, family = binomial('logit'))
    mod$cauchit <- glm(glm.formula, data = data, family = binomial('cauchit'))
    mod$probit <- glm(glm.formula, data = data, family = binomial('probit'))
    mod$log <- glm(glm.formula, data = data, family = binomial('log'))
    mod$inverse <- glm(glm.formula, data = data, family = binomial('inverse'))
    
    # résultat
    sapply(mod, function(x) summary(x))
}

## Cross-validation


#  PCA MODEL
pca.model <- prcomp(dat[,-c(1,2)])
pca.x <- pca.model$x
pca.summary <- summary(pca.model)


plot(pca.import$importance[3,])


## Définition des seuils





## Séparation des données d'entrainement
k <- sample(1:nrow(dat.pca),0.7 * nrow(dat.pca), replace = F)

f1_score <- matrix(NA, nrow = length(importance), ncol = length(treshold))


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


importance <- c(seq(0.5, 0.9, 0.1), 0.95)
treshold <- seq(0.15, 0.4, 0.05)
calcul_f1(importance, treshold)

## 
importance <- c(seq(0.5, 0.9, 0.1), 0.95)
treshold <- seq(0.15, 0.2, 0.01)
calcul_f1(importance, treshold)

##
importance <- c(seq(0.5, 0.9, 0.1), 0.95)
treshold <- seq(0.15, 0.2, 0.01)

## modèle avec meilleur F1_score : 
treshold_final <- 0.17
importance_final <- 0.8
f1_final <- 0.387931




## 
dat.pca_final <- data.frame(Ind = toitvert$green_roof_ind,
                      pca.x[, which(pca.summary$importance[3,] <= 0.8)])
mod_final <- glm(Ind~., data = dat.pca_final, family = binomial(link = 'logit'))

plot(mod_final$fitted.values, toitvert$green_roof_ind)




