library(MASS)
data(Boston)
?Boston
View(Boston)
summary(Boston)

bos = Boston
View(bos)

bos$chas = as.factor(bos$chas)

summary(bos)

#### ANALISI ESPLORATIVA ####

library(ggplot2)
library(corrplot)

corrplot(cor(bos[-4]), method = "square")

ggplot(bos, aes(chas, nox, fill = chas, palette = "set2")) +
  geom_boxplot()
# Non considerata significativa tramite l'analisi grafica

ggplot(bos, aes(indus, nox)) +
  geom_point(colour = "darkgreen") +
  geom_smooth(colour = "red")
# 1

ggplot(bos, aes(age, nox)) +
  geom_point(colour = "darkgreen") +
  geom_smooth(colour = "red")
# 1 / 2

ggplot(bos, aes(dis, nox)) +
  geom_point(colour = "darkgreen") +
  geom_smooth(colour = "red")
# 2

ggplot(bos, aes(rad, nox)) +
  geom_point(colour = "darkgreen") +
  geom_smooth(colour = "red")
# scartata

ggplot(bos, aes(tax, nox)) +
  geom_point(colour = "darkgreen") +
  geom_smooth(colour = "red")
# scartata

ggplot(bos, aes(lstat, nox)) +
  geom_point(colour = "darkgreen") +
  geom_smooth(colour = "red")
# 3

#### CREAZIONE MODELLO ####
library(caret)

train.control = trainControl(method = "LOOCV")

lm1 = train(nox ~ indus + age + poly(dis, 2) + poly(lstat, 3), data = bos, method = "lm",
               trControl = train.control)
lm1
summary(lm1)
# Ottimo modello basandosi su R quadro e RMSE
# La variabile lstat in forma polinomiale cubica non risulta performante. Si decide di semplificarla

lm2 = train(nox ~ indus + age + poly(dis, 2) + lstat, data = bos, method = "lm",
               trControl = train.control)
lm2
summary(lm2)

# Variabili adesso tutte significative, con una riduzione della bontà minima a fronte di una complessità molto inferiore rispetto a lm1


#### VERIFICA ASSUNZIONI ####
res = lm2$resid

qqnorm(scale(res))
abline(0,1)

shapiro_test = shapiro.test(res)
shapiro_test

      