## Méthode de prévision 
# set.seed(9742)
# n = nrow(data)
# k = 4
# sample = sample(1:n,size = n, replace = FALSE)
# test <- list()
# train <- list()
# for(k in 1:4){
#     lower = 1
#     upper= 334
#     echantillion = sample[lower:upper]
#     test[[k]] <- data %>% slice(echantillion)
#     train[[k]] <- data %>% slice(-echantillion)
#     lower = lower + 334
#     upper = upper + 334
# }
data <- dat.pca
n <- nrow(data)
K <- 4
taille <- n %/% K
set.seed(5)
alea <- runif(n)
rang <- rank(alea)
bloc <- (rang-1)%/%taille + 1
bloc <- as.factor(bloc)
print(summary(bloc))

sapply(1:4, function(x) sum(data[bloc == x,]$Ind) / 450)





plot(unlist(MSEP), pch = 18)
mean(unlist(MSEP))
