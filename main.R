#### IMPORT DATASET ####
fifa = read.csv("FIFA22.csv")

# Visualization
View(fifa)
summary(fifa)

# Drop useless columns
to_drop = c("Photo", "Flag", "Club.Logo", "Special", "Work.Rate", "Body.Type", "Real.Face", "Loaned.From", "Marking",
            "Position", "Best.Overall.Rating", "Wage", "Preferred.Foot", "Weak.Foot", "Jersey.Number", "Joined", "Contract.Valid.Until")
fifa = fifa[, -which(colnames(fifa) %in% to_drop)]

# Fixing Value
fifa$Value = ifelse(grepl("M$", fifa$Value), as.numeric(gsub("[^0-9.]", "", fifa$Value))*1000000,as.numeric(gsub("[^0-9.]", "", fifa$Value))*1000)

# Fixing Release.Clause
fifa$Release.Clause = ifelse(grepl("M$", fifa$Release.Clause), as.numeric(gsub("[^0-9.]", "", fifa$Release.Clause))*1000000,as.numeric(gsub("[^0-9.]", "", fifa$Release.Clause))*1000)

View(fifa)
summary(fifa)


#### GRAPHS ####

#install.packages("ggplot2")
library(ggplot2)

ggplot(fifa, aes(Best.Position, Value, fill = Best.Position)) +
  geom_boxplot() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90))

ggplot(fifa, aes(Overall, Value)) +
  geom_point(color = "darkred")

ggplot(fifa, aes(Age, Value)) +
  geom_point(color = "darkorange")


# Conversion of Value in character variable
fifa$Value = fifa$Value/1000000
fifa$Value = ifelse(fifa$Value >= 50, "Alto", ifelse(fifa$Value <= 10, "Basso", "Medio"))
fifa$Value = factor(fifa$Value, levels = c("Basso", "Medio", "Alto")) # Riordina le classi

ggplot(fifa, aes(Value, Overall, fill = Value)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2")


#### ANALYSIS ####
#install.packages("rpart")
library(rpart)

# variables selection
fifa = fifa[, which(colnames(fifa) %in% c("Value", "Age", "Overall", "Potential", "International.Reputation", "Skill.Moves"))]
summary(fifa)

# Dataset split (train and test)
set.seed(5)
train = sample(1:nrow(fifa), nrow(fifa)*0.65)
test_sample = fifa[-train, ]
train_sample = fifa[train, ]


# FITTING MODELS

set.seed(5)
# Maximum extension tree
tree_1 = rpart(Value ~ ., data = train_sample, method = "class", cp = 0, xval = 10)
summary(tree_1)
plot(tree_1)
text(tree_1, pretty = 0)

# Variables importance
var_importance = data.frame(tree_1$variable.importance)
var_importance

ggplot(var_importance, aes(x = row.names(var_importance), y = tree_1.variable.importance, fill = row.names(var_importance))) +
  geom_bar(stat = 'identity') +
  scale_fill_brewer(palette = "Set2") +
  labs(x = 'Variabili', y = 'Importance') +
  ggtitle("IMPORTANZA DELLE VARIABILI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = 'none')

# Pruning the tree
tree_1_pruned = rpart(Value ~ ., data = train_sample, method = "class", xval = 10)
summary(tree_1_pruned)
plot(tree_1_pruned)
text(tree_1_pruned, pretty = 0)

# Plot CV results
plotcp(tree_1_pruned)

# Prediction and accuracy
tree1_pred = predict(tree_1_pruned, test_sample, type = "class")
conf_matrix_t1 = table(tree1_pred, test_sample$Value)
conf_matrix_t1

true_classifications_t1 = conf_matrix_t1[1,1] + conf_matrix_t1[2,2] + conf_matrix_t1[3,3]
acc_tree1 = true_classifications_t1 / sum(conf_matrix_t1)
me_tree1 = 1-acc_tree1
cat("\nModello: tree_1; Accuracy:", acc_tree1,"; ME:", me_tree1)

# CV accuracy
library(caret)
set.seed(5)
control = trainControl(method="cv", number=10) # Parametri della cv
tree_cv = train(y = fifa$Value, x = fifa[,-4], method = "rpart", trControl = control)
tree_cv


# Tree with only Overall as predictor
set.seed(5)

tree_2 = rpart(Value ~ Overall, data = train_sample, method = "class", xval = 10)
summary(tree_2)
plot(tree_2)
text(tree_2, pretty = 0)

tree2_pred = predict(tree_2, test_sample, type = "class")
conf_matrix_t2 = table(tree2_pred, test_sample$Value)
conf_matrix_t2

true_classifications_t2 = conf_matrix_t2[1,1] + conf_matrix_t2[2,2] + conf_matrix_t2[3,3]
acc_tree2 = true_classifications_t2 / sum(conf_matrix_t2)
me_tree2 = 1-acc_tree2
cat("\nModello: tree_2; Accuracy:", acc_tree2,"; ME:", me_tree2)


# RANDOM FOREST
library(randomForest)
set.seed(5)

y_train = train_sample$Value
x_train = subset(train_sample, select = -Value)

y_test = test_sample$Value
x_test = subset(test_sample, select = -Value)

# Fit
rf = randomForest(x_train, y_train, proximity=TRUE, xtest=x_test, ytest=y_test, importance=TRUE, ntree=100)
rf

# CV
set.seed(5)
rf_cv = rfcv(x_train, y_train, cv.fold = 10)
rf_cv

with(rf_cv, plot(n.var, error.cv)) # Plot dei risultati
