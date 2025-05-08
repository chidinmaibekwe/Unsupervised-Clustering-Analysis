#load  necessary libraries
library(dplyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(NbClust)

#load the data
country_data <- readr::read_csv(".\\umldata\\Country-data.csv")

#store country names for later use
country_names <- country_data$country
country_data <- country_data %>% select(-country)# remove country column

print("\summary statistics of the data:")
print(summary(country_data))

#scaling
#data preprocessing: scaling
scaled_data <- scale(country_data)

#convert the scaled data back to a dataframe for easier handling
scaled_data <- as.data.frame(scaled_data)

print("\nsummary statistics of the scaled data:")
print(summary (scaled_data))

























silhouette_ scores<- c()
for (i in 2:10) {
  kmeans_result <-  kmeans(scaled_data, center = i, nstart = 10)
  silhouette_avg <- silhouette(kmeans_result$cluster, dist(scaled_data)) %>%
    summary() %>%
    .$avg.width
  silhouette_score <- c(silhouette_scores, silhouette_avg)}

silhouette_plot <- ggplot2(data.frame(clusters = 2:10, Silhouette= silhouette = silhouette_scores), aes(x = clusters, y = silhouette))+
  geom_line()+
  geom_point()+
  geom_vline()xintercept = 3, linetype = "dashed", color = "red") + #indicating k=3
  labs(title = "Silhouette Analysis for Optimal k",
       x = "Number of Clusters (k)",
       y = "Average Silhouette Width")+
  theme_minimal()


}