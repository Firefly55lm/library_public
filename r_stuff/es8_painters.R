library(MASS)
data("painters")

?painters
summary(painters)
View(painters)


library(corrplot)
corrplot(cor(painters[-5]))


library(ggplot2)

ggplot(painters, aes(School,  Composition, fill = School)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2")

ggplot(painters, aes(School,  Expression, fill = School)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2")

ggplot(painters, aes(School,  Drawing, fill = School)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2")

ggplot(painters, aes(School,  Colour, fill = School)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2")


library(class)

knn10 = knn.cv(train = painters[,-5], cl = painters$School, k = 10, prob = T)
knn10

mek10 = 1 - sum(painters$School == knn10)/NROW(painters)  
mek10



library(rpart)

fit = rpart(School ~ Colour + Drawing + Expression + Composition, method="class", data = painters)
printcp(fit)
plot(fit, uniform=TRUE)
text(fit, use.n=TRUE, all=TRUE, cex=1)







