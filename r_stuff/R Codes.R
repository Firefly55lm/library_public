####### DATASET #######

#### SELEZIONE DATASET ####
#install.packages("MASS") # installazione libreria
library(MASS) # caricamento libreria
data(Boston) # caricamento dataset
?Boston # visualizzazione help del dataset
summary(Boston) # visualizzazione sommario del dataset

#install.packages("alr4")
library(alr4)
data(BigMac2003)
?BigMac2003
BM = BigMac2003 # assegnazione del dataset alla variabile (nome più comodo da richiamare)
summary(BM)
View(BM) # apre il database in una nuova scheda

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#### RAPPRESENTAZIONE VARIABILI ####
#install.packages("corrplot")
library(corrplot)
cr = cor(BM)
corrplot(cr, method = "circle")

#install.packages("ggplot2")
library(ggplot2)

plot(BM$BigMac, # variabile nell'asse x
     BM$Bread, # variabile nell'asse y
     col = 'blue', # olore dei punti
     pch = 20, # riempimento
     cex = 1, # scala dimensionale dei punti
     main = "Relazione tra il prezzo del BigMac e il prezzo del pane", # titolo del grafico 
     xlad = "Prezzo del BigMac [$]", # etichetta asse x
     ylab = "Prezzo pane [$]") # etichetta asse y

plot(BigMac~., data = BM) # plotta una variabile nell'asse y in funzione di tutte le variabili nel dataset (una per volta)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#### MANIPOLAZIONE DATASET ####
names(BM)[-c(1, 4, 9)] = c("Bread", "Rice", "Bus", "Apt", "TeachGI", "TeachNI", "TeachHours") # rinomina le colonne ad eccezione di quelle indicate nel combine
BM = BM[BM$TaxRate >= 0,] # elimina le righe per le quali taxrate < 0 (valori anomali nel taxrate)
BM = BM[,colnames(BM) != "TeachGI"] # elimina la colonna

# Trasformazione variabie quantitativa in qualitativa
media = mean(BM$BigMac)
#quantile(BigMac2003$BigMac)
BM_factor = factor(
  ifelse( BM$BigMac > media,
          "Alto",
          "Basso")
)
BM_factor # visualizza il factor
BM$BM_factor = BM_factor # aggiunge la colonna BM_factor al database BM

summary(BM)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#### SUDDIVISIONE DATASET ####
set.seed(1)
train = sample(1:nrow(BigMac2003), nrow(BigMac2003)*0.6) # genera un vettore contente un sample casuale di numeri di riga, per numeri da 1 al numero di righe del dataset (60% del dataset)
train # visualizza il vettore generato
test = -train # genera un vettore contenente i numeri di riga non presenti nel vettore train

trainingset = BM[train, ] # crea il trainingset prendendo le righe dal database in base al vettore train
testset = BM[test, ]

View(trainingset)
View(testset)

#===========================================================================================================================================================================================================================================#


####### MODELS #######

#### REGRESSIONE LINEARE ####
?lm

#DEFINZIONE
lm1 = lm(BigMac ~ Bread + Rice + Bus, data = BM) # imposta Y~X, data = dataset di riferimento
summary(lm1) 
plot(lm1)

formula(lm1)
coefficients(lm1) # genera un vettore contenente i coefficienti del modello. Sintassi alternativa: coef(lm1)
deviance(lm1)
residuals(lm1)
model.matrix(lm1) # matrice X
vcov(lm1) # matrice varianze-covarianze

plot(BigMac ~ Bread, data = BM) # plotta la variabile BigMac in funzione della variabile Bread
abline(lm1) # Aggiunge una linea di regressione (Warning: si utilizzano solo i primi due dei 4 coefficienti di regressione)

# RISULTATI (lm1)
# Call:
# lm(formula = BigMac ~ Bread + Rice + Bus, data = BM) # ribadisce la formula del modello

# Residuals: # valore minimo, valore massimo, quartili e mediana dei residui
#   Min      1Q    Median    3Q     Max 
# -55.796 -10.369  -2.482   8.304  82.005 

# Coefficients: # valore dei coefficienti per predittore

#             Estimate Std. Error   t value  Pr(>|t|) # p-value    
#  (Intercept)  14.6750     8.1159   1.808   0.0753 .  
#  Bread         0.3624     0.1696   2.137   0.0364 *  
#  Rice          1.0637     0.1917   5.549 5.88e-07 *** # maggiormente significativa (basso p-value)
#  Bus          -7.6519     3.7840  -2.022   0.0473 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 20.59 on 64 degrees of freedom
# Multiple R-squared:  0.5851,	Adjusted R-squared:  0.5657 
# F-statistic: 30.09 on 3 and 64 DF,  p-value: 2.966e-12

# Aggiunta di un effetto interazione tra le avariabili Apt:TeachNI
lm2 = lm(BigMac ~ Apt*TeachNI, data = BM, subset = train) # il parametro subset assegna il sottocampione da considerare
summary(lm2)

lm2 = lm(BigMac ~ Apt + TeachNI + Apt:TeachNI, data = trainingset) # in alternativa a subset si specifica direttamente il database trainingset
summary(lm2)

plot(BigMac ~ Bus, data = BM) # si osserva una relazione tendenzialmente non lineare
lines(smooth.spline(BM$Bus, BM$BigMac), col = 'blue', lwd = 3) # aggiunge una linea curva allo scatter
# Aggiunta di una componente polinomiale quadratica con poly(x, exp)
lm3 = lm(BigMac ~ poly(Bus, 2) + Rice + Apt:TeachNI, data = BM)
summary(lm3)

# PREDIZIONE
estY1 = predict(lm1, testset) # predizione dei valori sul testset (in alternativa indicare BM[test, ])
MSE1_test = mean( (testset$BigMac - estY1)^2 ) # calcolo test MSE per mezzo della media dei quadrati delle differenze tra valori Y del testset (reali) e valori predetti
estY1

estY2 = predict(lm2, testset)
MSE2_test = mean( (testset$BigMac - estY2)^2 )
estY2

estY3 = predict(lm3, testset)
MSE3_test = mean( (testset$BigMac - estY3)^2 )
estY3

MSE1_test
MSE2_test
MSE3_test

sqrt(MSE1_test) # RMSE, radice quadrata MSE
sqrt(MSE2_test)
sqrt(MSE3_test)

bestMSE = min(MSE1_test, MSE2_test, MSE3_test)
if (bestMSE == MSE1_test) {
  cat("Best model: lm1, with MSE =", MSE1_test, "and RMSE =", sqrt(MSE3_test) ) 
} else if (bestMSE == MSE2_test) {
  cat("Best model: lm2, with MSE =", MSE2_test, "and RMSE =", sqrt(MSE3_test) ) 
} else if (bestMSE == MSE3_test) {
  cat("Best model: lm3, with MSE =", MSE3_test, "and RMSE =", sqrt(MSE3_test) ) 
}


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

#### REGRESSIONE LOGISTICA ####
?glm

logit1 = glm(BM_factor ~ poly(Bus, 2) + Rice, 
             data = BM, subset = train, family = "binomial") # il parametro family specifica la famiglia di glm: in questo caso binomial per la logit
summary(logit1)
table(BM_factor[train])

boxplot(Bus ~ BM_factor, data = BM)
logit2 = glm(BM_factor ~ poly(Bus, 2), 
             data = BM, subset = train, family = "binomial") 
summary(logit2)
logit2$fitted.values # visualizza le probabilità della classificazione

plot(trainingset$Bus, trainingset$BM_factor, xlab = "Bus ticket price", ylab = "BigMac price (Low/High)", main = "Logit 2")
lines(sort(trainingset$Bus), sort(logit2$fitted.values),col = "red",lwd = 2)

logit3 = glm(BM_factor ~ Bread, 
             data = trainingset, family = "binomial") 
summary(logit3)
plot(trainingset$Bread, trainingset$BM_factor, xlab = "Bread price", ylab = "BigMac price (Low/High)", main = "Logit 3")
lines(sort(trainingset$Bread), sort(logit3$fitted.values),col = "red",lwd = 2)


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#


#### DATASET QUALITATIVO - IRIS ####

?lda

library(psych)

data(iris)
?iris
View(iris)
summary(iris)

pairs.panels(iris[1:4],
             gap = 0,
             bg = c("red", "green", "blue")[iris$Species],
             pch = 21)

set.seed(1)
train_iris = sample(1:nrow(iris), nrow(iris)*0.6)
test_iris = -train 

tr_iris = iris[train_iris, ] 
ts_iris = iris[test_iris, ]

View(tr_iris)
View(ts_iris)


#### LINEAR DISCRIMINANT ANALYSIS ####
library(MASS)

# DEFINIZIONE
lda1 = lda(tr_iris$Species ~ ., tr_iris)
lda1

# PREDIZIONE
pred.lda1 = predict(lda1, ts_iris)
pred.lda1

ldahist(data = pred.lda1$x[,1], g = ts_iris$Species) # istogramma del modello
ldahist(data = pred.lda1$x[,2], g = ts_iris$Species)

plda1 = pred.lda1$class # restituisce le classi predette per mezzo del modello utilizzato
cm = table(Predicted = plda1, Actual = ts_iris$Species) # confusion matrix dei risultati

# CROSS VALIDATION
lda1.cv <- lda(iris$Species ~ ., data = iris, CV=TRUE)
lda1.cv

pred_lda1.cv = lda1.cv$class # restituisce le classi predette per mezzo del modello utilizzato
cmCV = table(Predicted = pred_lda1.cv, Actual = iris$Species) 
cmCV

meCV = 1-((cmCV[1] + cmCV[5] + cmCV[9])/sum(cmCV)) # calcolo automatizzato dell'ME (per tabella 3x3)
meCV


#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#


#### QUADRATIC DISCRIMINANT ANALYSIS ####

# DEFINIZIONE
qda1 = qda(tr_iris$Species ~., tr_iris)
qda1

names(qda1) # mostra i parametri estraibili dal modello qda

pred.qda1 = predict(qda1, ts_iris)
pred.qda1

pqda1 = pred.qda1$class
table(Predicted = pqda1, Actual = ts_iris$Species)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#


#### NAYVE BAYES ####

#install.packages("caTools")
library("caTools")

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#


#### KNN ####

#install.packages("class")
library(class)
?knn

# DEFINIZIONE
knn10 = knn(train = tr_iris[-5], # indica il trainingset escludendo l'ultima colonna (Species)
            test = ts_iris[-5], # indica il testset escludendo l'ultima colonna (Species)
            cl = tr_iris$Species, # factor contenente le vere classificazioni del trainingset
            k = 10, prob = T) # determinazione di k
knn10

# RISULTATI
# > knn10
# Classificazione
# [1] setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa     setosa    
# [12] setosa     setosa     setosa     setosa     setosa     setosa     setosa     versicolor versicolor versicolor versicolor
# [23] versicolor versicolor versicolor versicolor versicolor versicolor versicolor virginica  versicolor versicolor versicolor
# [34] versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor versicolor virginica  virginica 
# [45] versicolor virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica  virginica 
# [56] virginica  virginica  virginica  virginica  virginica 
# 
# Probabilità legata alla classificazione (prob = T)
# attr(,"prob")
# [1] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000
# [13] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 0.9166667 1.0000000 0.8000000 1.0000000 0.7000000 1.0000000
# [25] 1.0000000 1.0000000 1.0000000 1.0000000 0.6000000 0.5000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 0.9000000
# [37] 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 0.8000000 1.0000000 1.0000000 0.8000000
# [49] 1.0000000 0.9000000 0.6000000 1.0000000 0.6000000 1.0000000 1.0000000 0.9000000 0.6000000 1.0000000 1.0000000 0.8000000

# ALTRI MODELLI
knn20 = knn(train = tr_iris[-5],
            test = ts_iris[-5],
            cl = tr_iris$Species,
            k = 20, prob = T)
knn20


table(knn10, knn20)
table(knn10, ts_iris$Species)  
table(knn20, ts_iris$Species)

library(class)
knn10 = knn.cv(train = iris[,-5], cl = iris$Species, k = 10, prob = T)
knn10

mre10 = 1 - sum(iris$Species == knn10)/NROW(iris)  
mre10

#===========================================================================================================================================================================================================================================#


####### RESAMPLING METHODS #######

#install.packages("ISLR")
library(ISLR)
data("Auto")
View(Auto)
?Auto

library(boot) # contiene le funzioni di CV e Bootstrap


#### CROSS VALIDATION ####
?cv.glm

# K-FOLD
glm.fit = glm(mpg ~ horsepower, data=Auto)
summary(glm.fit)
cv.err = cv.glm(Auto, glm.fit, K = 10) # se K non è specificato, esegue una LOOCV
summary(cv.err)

cv.err$delta
# > cv.err$delta
# [1] 24.23151 24.23114  # il primo valore è standard per la K fold, il secondo valore è bias-corrrected per avvicinarsi alla LOOCV

# LOOCV
glm.fit = glm(mpg ~ horsepower, data=Auto)
summary(glm.fit)
cv.err = cv.glm(Auto, glm.fit)
summary(cv.err)

cv.err$delta


#### BOOTSTRAP ####

boot.fn <- function(data, index) { #funzione customizzata per bootstrap
  return(coef(lm(mpg ~ horsepower, data=Auto, subset = index)))
}
boot.fn(Auto, 1:392)




####### TEST #######

#install.packages("ggplot2")
library(ggplot2)

library(alr4)
data(BigMac2003)
BM = BigMac2003

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

##### TEST DI NORMALITÀ DEGLI ERRORI #####
lm = lm(BigMac ~ Bread + Rice, data = BM)
summary(lm)

#### QQ PLOT ####
residuals = lm$residuals # estrae i residui del modello lm
qqnorm(scale(residuals)) # plotta i residui
abline(0,1) # aggiunge una bisettrice 

#### T-TEST SULLA MEDIA ####
?t.test
t.test(residuals)

#### TEST SHAPIRO - WILK ####
shapiro_test = shapiro.test(residuals)
shapiro_test

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

##### TEST DI AUTOCORRELAZIONE DEGLI ERRORI #####
library(ISLR)
data = Auto

#### TEST DI DURBIN - WATSON ####
#install.packages("lmtest")
library(lmtest)
?dwtest
dwtest(lm, data=Auto) 
# DW = 1.9632, p-value = 0.4279 # valore DW prossimo a 2, dunque scarsa autocorrelazione

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

##### TEST DI OMOSCHEDASTICITÀ / ETEROSCHEDASTICITÀ #####

# GRAFICAMENTE
plot(lm$fitted.values, abs(residuals),
     ylab="Residui", xlab="Valori stimati", 
     main="Residui in valore assoluto vs valori stimati")

# TEST DI BREUSCH - PAGAN
?bptest
bptest(lm) # accettata H0: omoschedasticità

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

##### STIMA ROBUSTA DEGLI STANDARD ERROR #####

#install.packages("sandwich")
library(sandwich)

vcov(lm) # matrice di varianza-covarianza
vcovHC(lm) # matrice di varianza-covarianza depurata dell'effetto eteroschedasticità
vcovHAC(lm) # matrice di varianza-covarianza depurata dell'effetto eteroschedasticità e autocorrelazione
coeftest(lm, vcov = vcov) # test t sui coefficienti
coeftest(lm, vcov = vcovHC) # test t sui coefficienti utilizzando lo stimatore HC
coeftest(lm, vcov = vcovHAC) # test t sui coefficienti utilizzando lo stimatore HAC

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

##### OUTLIERS / LEVERAGE POINTS #####

#### OUTLIERS ####
rstand = rstandard(lm) # residui standardizzati

plot(rstand, main="Residui standardizzati")
abline(h=2, col=2)
abline(h=-2, col=2)

rstand[abs(rstand)>2] # sospetti valori anomali

# RESIDUI JACKNIFE
?rstudent
rjack = rstudent(lm) # residui studentizzati
rjack

plot(rjack, main="Residui jacknife")
abline(h=3, col=2)
abline(h=-3, col=2)
id<-names(rjack)
identify(1:length(rjack),rjack, id)

# SOLUZONE ALTERNATIVA
#install.packages("car")
library(car) 
outlierTest(lm)
crPlots(lm)

#### LEVERAGE POINTS ####
leveragePlots(lm)

#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#

##### PUNTI INFLUENTI #####

#### DISTANZA DI COOK ####
cook<-cooks.distance(lm)
cook

par(mfrow=c(1,1))
plot(cook, main="Distanza di Cook")
identify(1:length(cook), cook, id)





