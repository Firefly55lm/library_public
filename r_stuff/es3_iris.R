library(psych)
library(ggplot2)

data(iris)
?iris
View(iris)
summary(iris)

#### GRAFICI ####

ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
  geom_boxplot()

ggplot(iris, aes(Species, Sepal.Width, fill = Species)) +
  geom_boxplot()

ggplot(iris, aes(Species, Petal.Length, fill = Species)) +
  geom_boxplot()

ggplot(iris, aes(Species, Petal.Width, fill = Species)) +
  geom_boxplot()

ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
  geom_boxplot() +
  geom_point(aes(size = Sepal.Width, colour = Petal.Width))

ggplot(iris, aes(Species, Sepal.Length, fill = Species)) +
  geom_boxplot() +
  geom_point(aes(size = Petal.Length, colour = Petal.Width))

# Verifica relazione tra Sepal.Length e Sepal.Width
ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(size = 2, colour = "darkblue") +
  geom_smooth(colour = "darkorange")
# Nessun tipo di relazione significative

#### MODELLI ####
library(MASS)

lda1 = lda(Species ~ Sepal.Length + Sepal.Width, data = iris, CV = T)
lda1

res1 = table(Predicted = lda1$class, Actual = iris$Species)
res1

#                        Actual
#Predicted    setosa versicolor virginica
#setosa         49          0         0
#versicolor      1         35        15
#virginica       0         15        35

# Risultati buoni
mr1 = 1 - sum(diag(res1))/sum(res1)
mr1


lda2 = lda(Species ~ Petal.Length + Petal.Width, data = iris, CV = T)
lda2

res2 = table(Predicted = lda2$class, Actual = iris$Species)
res2

mr2 = 1 - sum(diag(res2))/sum(res2)
mr2
# Risultati eccellenti


lda3 = lda(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, CV = T)
lda3

res3 = table(Predicted = lda3$class, Actual = iris$Species)
res3

mr3 = 1 - sum(diag(res3))/sum(res3)
mr3

# Risultati eccellenti, migliori del 2% (non una miglioria così importante considerando il numero di predittori raddoppiato)

ggplot(iris, aes(Species, Petal.Length, fill = Species, palette = "set2")) +
  geom_boxplot() +
  geom_point(aes(size = Sepal.Width, colour = Sepal.Length))

ggplot(iris, aes(Species, Sepal.Width, fill = Species, palette = "set2")) +
  geom_boxplot()

# La variabile Sepal.Width aiuta ma non apporta, almeno graficamente, delle differenze così marcate tra le 3 specie

# Si prova una lda scartando tale variabile, per un numero di 3 predittori 

lda4 = lda(Species ~ Petal.Length + Petal.Width + Sepal.Length, data = iris, CV = T)
lda4

res4 = table(Predicted = lda4$class, Actual = iris$Species)
res4

mr4 = 1 - sum(diag(res4))/sum(res4)
mr4

# Apporto appena apprezzabile, modello scartato


# PER UN MODELLO DI LDA EFFICACE E MOLTO PARSIMONIOSO SI SCEGLIE LDA2, PER UN MODELLO PIÙ EFFICACE MA CON IL DOPPIO DEI
# PREDITTORI RISPETTO AL PRECEDENTE SI SCEGLIE LDA3
print("Bontà di classificazione lda2:")
1 - mr2
print("Bontà di classificazione lda3:")
1 - mr3

# ALBERO
library(rpart)
root1 = rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, method = "class")
plot(root1, uniform = T, main = "Classification tree for flowers' species")
text(root1, use.n = T, all = T, cex = 0.8)

printcp(root1)
plotcp(root1)
summary(root1)


# KNN
library(class)
knn10 = knn.cv(train = iris[,-5], cl = iris$Species, k = 10, prob = T)
knn10

mrk10 = 1 - sum(iris$Species == knn10)/NROW(iris)  
mrk10

# Performance eccellente ma praticamente uguale alla lda4, con lo svantaggio della scarsa interpretabilità.
# LA SCELTA FINALE RICADE SULLA LDA2, PARSIMONIOSA (2 PREDITTORI) E PERFORMANCE (M.E. = 0,04)


#### RISULTATI ####

# LDA2, 2 PREDITTORI:
mr2

# LDA3, 4 PREDITTORI:
mr3

# KNN10:
mrk10


