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
seuil <- c(seq(0.1, 0.9, 0.1), seq(0.91, 0.99, 0.01))
treshold <- seq(0.15, 0.4, 0.01)



## Séparation des données d'entrainement
k <- sample(1:nrow(dat.pca),0.7 * nrow(dat.pca), replace = F)
train <- dat.pca[k,]
valid <- dat.pca[-k,]

for (i in length(seuil))
{
  dat.pca <- data.frame(Ind = toitvert$green_roof_ind,
                        pca.x[, which(pca.summary$importance[3,] <= seuil[i])])
  model.str <- 'Ind~.'
  model.pca <- glm(as.formula(mod.str), data = dat.pca, family = binomial(link = 'logit'))
  summary(model.pca) ; anova(model.pca)
  
  train.mod <- glm(as.formula(mod.str), data = train, family = binomial(link = 'logit'))
  
  
}









valid$Pred <- predict(train.mod, newdata = valid, type = 'response')
valid$validbin <- ifelse(valid$Pred >= treshold, 1, 0)



summary(valid$Pred)

(mat <- confusionMatrixFor_Neg1_0_1(valid$Ind, valid$validbin)[-1,-1])

precision <- mat[2,2] / sum(mat[,2])
recall <- mat[2,2] / sum(mat[2,])
f1_score <- 2 * (precision * recall) / (precision + recall)
data.frame(f1_score, precision, recall)


mod.final <- glm(as.formula(mod.str), data = dat.pca, family = binomial(link = 'logit'))




