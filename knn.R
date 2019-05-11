# Decision Tree Classification

# Importing the dataset
dataset = read.csv('cleandata.csv')
dataset = dataset[5:26]


for(i in 1:142193)
{
        if(dataset$RainTomorrow[i]==1)
        {
                dataset$RainTomorrow[i] = 0
        }
}


for(i in 1:142193)
{
        if(dataset$RainTomorrow[i]==2)
        {
                dataset$RainTomorrow[i] = 1
        }
}


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$RainTomorrow, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-22] = scale(training_set[-22])
test_set[-22] = scale(test_set[-22])

# Fitting K-NN to the Training set and Predicting the Test set results
library(class)
y_pred = knn(train = training_set[, -22],
             test = test_set[, -22],
             cl = training_set[, 22],
             k = 5,
             prob = TRUE)

# Making the Confusion Matrix
cm = table(test_set[, 22], y_pred)

cm

# y_pred
#     0     1
# 0 21358   705
# 1  2819  3556

