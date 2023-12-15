library(ggplot2)
data("diamonds")
?diamonds
summary(diamonds)
View(diamonds)

library(corrplot)
corrplot(cor(diamonds[,c(1,5:9)]))

#### GRAFICI ####

ggplot(diamonds, aes(clarity, price, fill = clarity)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Diamonds price per clarity") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
# Niente di particolarmente significativo

ggplot(diamonds, aes(cut, price, fill = cut)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Diamonds price per cut quality") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
# Niente di particolarmente significativo

ggplot(diamonds, aes(color, price, fill = color)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Diamonds price per color") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")
# Niente di particolarmente significativo



ggplot(diamonds, aes(carat, price)) +
  geom_point(size = 0.5, color = "skyblue") +
  geom_smooth(color = "red")
# Ottima relazione quasi lineare, entro un valore di carat di circa 2,5. Oltre, nella coda destra, si perde notevolmente precisione

ggplot(diamonds, aes(x, price)) +
  geom_point(size = 0.5, color = "skyblue") +
  geom_smooth(color = "red")
# Ottima relazione quasi lineare o al massimo quadratica, lp da eliminare con grandezza x = 0

ggplot(diamonds, aes(y, price)) +
  geom_point(size = 0.5, color = "skyblue") +
  geom_smooth(color = "red")
# Ottima relazione quasi lineare o al massimo quadratica, lp da eliminare con grandezza y = 60 e y = 0

# PULIZIA VALORI ANOMALI
dm = diamonds
dm = dm[dm$x > 0,]
dm = dm[dm$y > 0,]
dm = dm[dm$y < 30,]

summary(dm)

# REPLOT
ggplot(dm, aes(x, price)) +
  geom_point(size = 0.5, color = "skyblue") +
  geom_smooth(color = "red")

ggplot(dm, aes(y, price)) +
  geom_point(size = 0.5, color = "skyblue") +
  geom_smooth(color = "red")


#### MODELLI ####
lm1 = lm(price ~ carat + poly(x, 2) + poly(y, 2), data = dm)
lm1
summary(lm1)

library(caret)

train.control = trainControl(method = "cv", number = 10)

model1 = train(price ~ carat + poly(x, 2) + poly(y, 2), data = dm, method = "lm",
               trControl = train.control)
print(model1)

model2 = train(price ~ carat + x + y, data = dm, method = "lm",
               trControl = train.control)
print(model2)

# Si preferisce il secondo modello (più semplice)

