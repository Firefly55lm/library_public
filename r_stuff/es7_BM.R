library(alr4)
data = (BigMac2003)
bm = BigMac2003
?BigMac2003
summary(bm)

bm = bm[bm$TaxRate >= 0,]
bm = bm[,colnames(bm) != "TeachGI"]

colnames(bm)[which(names(bm) == "TeachHours")] = "TH"
colnames(bm)[which(colnames(bm) == "TH")] = "TeachHours"

summary(bm)

library(ggplot2)
library(corrplot)

corrplot(cor(bm[,-10]), method = "square")


bm$BM = ifelse(bm$BigMac >= median(bm$BigMac), "Alto", "Basso")
View(bm)
bm$BM = as.factor(bm$BM)
summary(bm)

ggplot(bm, aes(BM, Rice, fill = BM)) +
  geom_boxplot() +
# Ok

ggplot(bm, aes(BM, Bread, fill = BM)) +
  geom_boxplot()
# Ok

ggplot(bm, aes(BM, FoodIndex, fill = BM)) +
  geom_boxplot()
# Ok

ggplot(bm, aes(BM, TeachNI, fill = BM)) +
  geom_boxplot()
# Ok

ggplot(bm, aes(BM, Apt, fill = BM)) +
  geom_boxplot()
# Ok

ggplot(bm, aes(BM, Bus, fill = BM)) +
  geom_boxplot()
# Ok
levels(bm$BM)

library(boot)
logit1 = glm(BM ~ Rice + Bread + FoodIndex + TeachNI + Apt + Bus, data = bm, family = "binomial")
summary(logit1)

set.seed(55)
l1_CV = cv.glm(bm, logit1, K = 10)
l1_CV$delta

l1_LOOCV = cv.glm(bm, logit1)
l1_LOOCV$delta

