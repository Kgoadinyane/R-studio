bank<-read.csv("bank(1).csv")
head(bank)
str(bank)
glimpse(bank)
sum(is.na(bank))
view(bank)
missing_values <- colSums(is.na(bank))
print(missing_values)

set.seed(123)
split <- sample.split(bank,SplitRatio = 0.8)
train <- subset(bank,split =="TRUE")
test <- subset(bank, split =="FALSE")

logistic_model <- glm( y ~ ., data = train, family = "binomial",)
summary(logistic_model)

plot(bank$age,bank$y,xlab="CLIENT AGE",ylab="RESULTS OF SUBSCRIBE OR NOT")
predictions <- predict(logistic_model, newdata = test, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "yes", "no")

confusion_matrix <- table(predicted_classes, test$y)
print(confusion_matrix)

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
# Precision
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
# Recall (Sensitivity)
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
# F1-Score
f1_score <- 2 * (precision * recall) / (precision + recall)
print(paste("Accuracy:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-Score:", f1_score))
