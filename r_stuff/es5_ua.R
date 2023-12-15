data(USArrests)
ua = USArrests
summary(ua)
View(ua)
?USArrests

library(ggplot2)
library(corrplot)

#### CHARTS ####

corrplot(cor(ua))

ggplot(ua, aes(Murder, Assault)) +
  geom_point(colour = "lightblue") +
  geom_smooth(colour = "red")

ggplot(ua, aes(Rape, Assault)) +
  geom_point(colour = "lightblue") +
  geom_smooth(colour = "red")

# Relazione quasi lineare ad eccezione della coda destra, poche osservazioni (50)






#### MODELS ####

# CROSS VALIDATION LOOCV
library(caret)

train.control = trainControl(method = "LOOCV")
model1 = train(Assault ~ Murder + Rape, data = ua, method = "lm",
               trControl = train.control)
print(model1)

# TEST SET
set.seed(55)
train = sample(1:nrow(ua), nrow(ua)*0.6)
test = -train

tr_set_ua = ua[train, ]
ts_set_ua = ua[test, ]

lm1 = lm(Assault ~ Murder + Rape, data = ua)
summary(lm1)

pred_lm1 = predict(lm1, ts_set_ua)
MSE1 = mean((ts_set_ua$Assault - pred_lm1)^2)
MSE1

# L'approccio si conferma poco performante rispetto alla CV in presenza di poche sossrvaizoni


eq1 = function(x){coef(lm1)[2]*x+coef(lm1)[1]}

ggplot(ua)+
  geom_point(aes(Murder, Assault), colour = "blue") +
  geom_point(aes(Rape, Assault), colour = "orange") +
  stat_function(fun = eq1, geom = "line", color = "red") +
  labs(title = "Model: lm1", x = "Murder / Rape")
  
ggplot(ua)+
  geom_point(aes(Murder, Assault, colour = "Murder")) +
  geom_point(aes(Rape, Assault, colour = "Rape")) +
  stat_function(fun = eq1, geom = "line", color = "red") +
  labs(title = "Model: lm1", x = "") +
  scale_color_manual(name = 'X values',
                     breaks = c('Murder', 'Rape'),
                     values = c('Murder'='blue', 'Rape'='orange'))


#### VALUTAZIONE ASSUNZIONI ####

# NORMALITÀ
residuals = lm1$residuals

qqnorm(scale(residuals))
abline(0,1)

shapiro_test = shapiro.test(residuals)
shapiro_test
# Il test di shapiro è prossimo a 1, pertanto media degli errori è considerabile pari a 0 e, dunque, questi seguono una distribuzione normale


# AUTOCORRELAZIONE
library(lmtest)
dwtest(lm1, data=ua) 
# Leggera autocorrelazione positiva

# OMOSCHEDASTICITÀ

plot(lm1$fitted.values, abs(lm1$residuals),
     ylab="Residui", xlab="Valori stimati", 
     main="Residui in valore assoluto vs valori stimati")

bptest(lm1)

# Omoschedasticità: varianza degli errori costante
