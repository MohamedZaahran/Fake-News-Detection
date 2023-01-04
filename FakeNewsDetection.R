# Installing the packages we have used
install.packages("tidyverse")
install.packages("gmodels")
install.packages("tm")
install.packages("naniar")
install.packages("caTools")
install.packages("caret")
install.packages("randomForest")

# Loading the libraries we have used
library(tidyverse) 
library(gmodels)
library(naniar)
library(tm)
library(caTools)
library(e1071)
library(caret)
library(randomForest)

#------------------------------------------------------------------------------------------------------------------------------

setwd("F:/ASU/5TH SEMSTER/Statistical Inference/Project")
# Reading the data 
data <- read.csv("news.csv")
summary(data)

# Removing ID column from the dataset
data <- data[,-1]
# Removing title column from the dataset
data <- data[,-1]

# First, let's check if the data contains any missing values
anyNA(data)
# Let's check if there are any duplicates using duplicated()
sum(duplicated(data$X))

#------------------------------------------------------------------------------------------------------------------------------

# Counting how many fake and real news
data %>% group_by(label) %>% summarise(count=n())

# Converting label column to factor 
data$label <- factor(data$label)

# Printing the number of fake and real news
real <- as.numeric(table(data$label)[1])
fake <- as.numeric(table(data$label)[2])
total <- fake + real

# Creating data for the pie chart
x <- c(fake / total, real / total)
labels <- c("Fake", "Real")
piepercent <- round(100 * x / sum(x), 1)

# Plotting the pie chart
pie(x, piepercent, main = "Real & Fake percentages", col = c("steelblue3", "tan2"), radius = 1, cex = 1.6, cex.main = 2)
legend("topright", c("Fake", "Real"), cex = 1, fill = c("steelblue3", "tan2"))

#------------------------------------------------------------------------------------------------------------------------------

# Applying a function for finding exclamation marks
data$exc <- sapply(data$text, function(x) length(unlist(strsplit(as.character(x), "\\!+"))))
# Counting exclamation marks in fake and real news
data %>% group_by(label) %>% summarise(Exclamation = sum(exc))
# Printing the standard deviation of the exclamation marks count
cat("Standard deviation of exclamation marks =", sd(data$exc))

# Getting the number of exclamation marks in each fake and real news
fake_exc <- sum(subset(data, label == "FAKE")$exc)
real_exc <- sum(subset(data, label == "REAL")$exc)

# Creating data for the bar chart
value <- c(fake_exc, real_exc)
Labels <- c("Fake", "Real")
df <- data.frame(Fake_Real = Labels, Exclamation_Mark = value)

# Plotting the bar chart
theme_update(plot.title = element_text(hjust = 0.5))
p <- ggplot(data = df, aes(x = labels, y = value)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_text(aes(label = value), vjust = 1.6, color = "black", size = 7) +
  coord_cartesian(ylim = c(0, 6000)) +
  ggtitle("Exclamation Mark per Fake & Real") +
  xlab("Labels") + ylab("Exclamation Marks") +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 20,face = "bold"), plot.title = element_text(size = 20, face = "bold"))
p

#------------------------------------------------------------------------------------------------------------------------------

# Applying a function for finding question marks
data$ques <- sapply(data$text, function(x) length(unlist(strsplit(as.character(x), "\\?+"))))
# Counting question marks in fake and real news
data %>% group_by(label) %>% summarise(Question_Marks = sum(ques))
# Printing the standard deviation of the question marks count
cat("Standard deviation of question marks =", sd(data$ques))

# Getting the number of question marks in each fake and real news
fake_ques <- sum(subset(data, label == "FAKE")$ques)
real_ques <- sum(subset(data, label == "REAL")$ques)

# Creating data for the bar chart
value <- c(fake_ques, real_ques)
Labels <- c("Fake", "Real")
df <- data.frame(Fake_Real = Labels, Question_Mark = value)

# Plotting the bar chart 
theme_update(plot.title = element_text(hjust = 0.5))
p <- ggplot(data = df, aes(x = labels, y = value)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_text(aes(label = value), vjust = 1.6, color = "black", size = 7) +
  coord_cartesian(ylim = c(0, 9000)) +
  ggtitle("Question Mark per Fake & Real") +
  xlab("Labels") + ylab("Question Marks") +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 20, face = "bold"), plot.title = element_text(size = 20, face = "bold"))
p

#------------------------------------------------------------------------------------------------------------------------------

# Cleaning text column
# Removing mentions, hashtags, numbers, urls, punctuations, emojis, newlines and extra spaces.
data$text <- tolower(data$text)
data$text <- gsub('[^[:alnum:] ]', '', data$text)
data$text <- gsub("@\\w+", "", data$text)
data$text <- gsub("#\\w+", "", data$text)
data$text <- removeNumbers(data$text)
data$text <- gsub("https://.+", "", data$text)
data$text <- removePunctuation(data$text)
data$text <- gsub('\\p{So}|\\p{Cn}', '', data$text, perl = TRUE)
data$text <- gsub("\n", "", data$text)
data$text <- trimws(gsub("\\s+", " ", data$text))

#------------------------------------------------------------------------------------------------------------------------------

# Applying tokenization method 
dtm <- DocumentTermMatrix(data$text)

# Removing SparseTerms
# Here we keep the '0.999' of columns  and remove the columns that have the least amount of ones '0.001' 
dtm <- removeSparseTerms(dtm, 0.999)
dataset <- as.data.frame(as.matrix(dtm))

# Encoding the target feature as factor
dataset$label <- data$label
head(dataset)

# Making the label column zeros and ones only
dataset$label <- gsub("FAKE", 0, dataset$label)
dataset$label <- gsub("REAL", 1, dataset$label)

dataset$label <- factor(dataset$label, levels = c(0, 1))
which(colnames(dataset) == "label")

#------------------------------------------------------------------------------------------------------------------------------

# Splitting the dataset into the training and test sets
training_set <- dataset[1:5068,]
test_set <- dataset[5069:6335,]

# Fitting Random Forest Classification to the training set
set.seed(123)
# Here we except the column number 2231 "label column"  
classifier <- randomForest(x = training_set[-2231],
                           y = training_set$label,
                           ntree = 20)

# Predicting the Test set results
head(test_set)
y_pred <- predict(classifier, newdata = test_set[-2231])

# Making the Confusion Matrix
cm <- table(test_set[,2231], y_pred)
confusionMatrix(cm)

#------------------------------------------------------------------------------------------------------------------------------

# Applying tokenization method 
data_doc <- DocumentTermMatrix(data$text)

# Splitting the dataset into the training and test sets
TrainingDataset <- data_doc[1:5068,]
TestDataset <- data_doc[5069:6335,]
Train_Labels <- data[1:5068,]$label
Test_Labels <- data[5069:6335,]$label

# Printing proportions
prop.table(table(Train_Labels))
prop.table(table(Test_Labels))

#------------------------------------------------------------------------------------------------------------------------------

# Finding the frequent words 
Frequent_Words <- findFreqTerms(TrainingDataset, 10)

# Putting the frequent words in TrainingDataset and TestDataset
Freq_Train <- TrainingDataset[ ,Frequent_Words]
Freq_Test <- TestDataset[ ,Frequent_Words]

# Checking if a word is present in line 
count <- function(x)
{
  x <- ifelse(x > 0, "Yes", "No")
}

Train <- apply(Freq_Train, MARGIN = 2, count)
head(Train)
Test <- apply(Freq_Test, MARGIN = 2, count)

#------------------------------------------------------------------------------------------------------------------------------

# Applying Naive Bayes Algorithm
# Fitting Naive Bayes Model to training dataset
classifier_cl <- naiveBayes(Train, Train_Labels)

# Predicting on test data
y_pred <- predict(classifier_cl, Test)

# Confusion Matrix
CM <- table(Test_Labels, y_pred)

# Model Evaluation
confusionMatrix(CM)

#------------------------------------------------------------------------------------------------------------------------------

# Creating data for the bar chart
acc <- c(round((cm[[1,1]] + cm[[2,2]]) / sum(cm), digits = 2), round((CM[[1,1]] + CM[[2,2]]) / sum(CM), digits = 2))
algorithms <- c("Random Forest", "Naive Bayes")
df <- data.frame(Algorithm = algorithms, Accuracy = acc)

# Plotting the bar chart
theme_update(plot.title = element_text(hjust = 0.5))
p <- ggplot(data = df, aes(x = Algorithm, y = Accuracy)) +
  geom_bar(stat = "identity", width = 0.5, fill = "steelblue") +
  geom_text(aes(label = Accuracy), vjust = 1.6, color = "black", size = 7) +
  coord_cartesian(ylim = c(0, 1)) + ggtitle("Accuracy per Algorithm") +
  theme(axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15), axis.title = element_text(size = 20, face = "bold"), plot.title = element_text(size = 20, face = "bold"))
p

#------------------------------------------------------------------------------------------------------------------------------

#write.csv(data, file = 'data.csv', row.names = FALSE)