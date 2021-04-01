library(tidyverse)
library(class)

knn_data=read_csv(file="/Users/sahillsharma/Desktop/BUSA8000/Week 4/wk4_data.csv")

head(knn_data)

str(knn_data)
summary(knn_data)

#plot the data
ggplot(knn_data) + 
  geom_point(aes(x=Factor_1,y=Factor_2,color=Classification)) +
  xlab("Factor 1") +
  ylab("Factor 2") + 
  ggtitle("Plot of knn data") + 
  labs(color="Classification")

# Training data = the data without the missing NA values
knn_train_data = na.omit(knn_data) %>%
  select(Factor_1,Factor_2)

head(knn_train_data)

# Labels = the labels/answers without the missing NA values
knn_labels = na.omit(knn_data) %>%
  pull(Classification)
head(knn_labels)

# Test data = the missing values that need to be predicted using the KNN algorithm
knn_test_data = knn_data %>%
  filter(is.na(Classification)) %>%
  select(Factor_1,Factor_2)

head(knn_test_data)

# The KNN algorithm
knn_output = knn(knn_train_data,knn_test_data,knn_labels,k=5)

head(knn_output)

# Plot of KNN data
ggplot() + 
  geom_point(aes(x=knn_test_data$Factor_1,y=knn_test_data$Factor_2,color=knn_output)) + 
  geom_point(aes(x=knn_train_data$Factor_1,y=knn_train_data$Factor_2,color=knn_labels),alpha=0.1) + 
  xlab("Factor 1") + 
  ylab("Factor 2") + 
  ggtitle("Plot of knn data") + 
  labs(color="Classification")

