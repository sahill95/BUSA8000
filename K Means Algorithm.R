# Import the dataset

k_means_data=read_csv(file="/Users/sahillsharma/Desktop/BUSA8000/Week 4/wk4_data.csv")

# plot the data - get a feel of what you're working with
ggplot(k_means_data) + 
  geom_point(aes(x=Factor_1,y=Factor_2,color=Classification)) +
  xlab("Factor 1") +
  ylab("Factor 2") + 
  ggtitle("Plot of knn data") + 
  labs(color="Classification")

# data for k means algorithm, removal of categorical variables
k_means_data = k_means_data %>%
  select(-ID,-Classification)

# k means cluster function. k = 2
clustering = kmeans(k_means_data,2)

class(clustering)
clustering$cluster
clustering$size
clustering$centers

clustered_data = k_means_data %>%
  mutate(Cluster = as.character(clustering$cluster))

head(clustered_data)

ggplot() + 
  geom_point(data=clustered_data,aes(x=Factor_1,y=Factor_2,color=Cluster),alpha=0.1) +
  geom_point(data=as_tibble(clustering$centers),aes(x=Factor_1,y=Factor_2),shape=4)
