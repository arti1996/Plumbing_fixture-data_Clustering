#Put root variable here:
rootDirectory = "D:/Pasta Personalizada/Minha Biblioteca/00 Arti/03_Projetos/Plumbing fixture data"
setwd(rootDirectory)

        #Imports:
if(!require(readxl)){
  install.packages("readxl")
  library(readxl)
}
if(!require("ggpubr")){
  install.packages("ggpubr")
  library(ggpubr)
}
if(!require("tidyverse")){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require("cluster")){
  install.packages("cluster")
  library(cluster)
}
if(!require("factoextra")){
  install.packages("factoextra")
  library(factoextra)
}
if(!require("gridExtra")){
  install.packages("gridExtra")
  library(gridExtra)
}
if(!require("pvclust")){
  install.packages("pvclust")
  library(pvclust)
}
if(!require("xlsx")){
  install.packages("xlsx")
  library(xlsx)
}
    #Data Importation
Data_to_work <- read_excel("Data_to_work.xlsx", sheet = "Data")
#View(Data_to_work) #view on RStudio is disabled
#str(Data_to_work) #original data format
data = data.frame(Data_to_work) #we will work in data.frame format
#str(data)
#head(data)

        #Explore Analysis
print("Summary of time:")
summary(data$t)
print("Summary of y:")
summary(data$y)

    #dot plot
#plot data, understand correlation between values and time
plot(data$y ~ data$t, col = "black", lwd = 0.5, pch = "*",
     main = "dot plot: Values VS Time", xlab = "Time", ylab = "Values")

    #Correlations
cor(data$t, data$y)

    #Boxplot of values
boxplot(data$y, main = "dot plot: Values VS Time", ylab = "Values")

#Normality
#qqnorm(data$y, pch = "*", frame = FALSE); qqline(data$y, col = "steelblue", lwd = 2)
#Or:
ggqqplot(data$y)
hist(data$y, probability=T, main = "Histogram of Values", xlab = "Values Grupped"); lines(density(data$y),col=2)

    #Density plot
ggdensity(data$y, main = "Density plot of Values", xlab = "Values")
#Density plot suggest 3 modal distribuction, probably we have 3 clusters here.

#Data is not normal, by 1st impression. Will testing [used Shapiro-Wilk Test]:
    #Null hypothesis: The data is normally distributed. If p> 0.05, normality can be assumed
print("Null hypothesis: The data is normally distributed. If p> 0.05, normality can be assumed.")
shapiro.test(data$y)
print("Reject null hypothesis.") #Reject Normality

    #All data is depending on time?
mod = lm(data$y ~ data$t)
summary(mod)
anova(mod)
#Time is irrelevant to analyse. We need to do clustering

        #Clustering Algorithm
#Data to classify
data_cl = data.frame(data$t, data$y)

    # K-means Not Hierarchical Clustering
#Used: https://data-flair.training/blogs/clustering-in-r-tutorial/
kmeans3 <- kmeans(data$y, centers = 3) #nstart = Null
data$Kmeans3_cluster = kmeans3$cl
fviz_cluster(kmeans3, data = data_cl) #data$y
#head(data)

    # Ward Hierarchical Clustering
#Used: https://www.statmethods.net/advstats/cluster.html
d <- dist(data$y, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram
  #5 clusters
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red")
data$ward_5cluster = groups
  #4 clusters
groups <- cutree(fit, k=4) # cut tree into 3 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=4, border="blue")
data$ward_4cluster = groups
  #3 clusters
groups <- cutree(fit, k=3) # cut tree into 3 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=3, border="darkgreen")
data$ward_3cluster = groups

        #Export to Excel
write.xlsx(data, file="Data_to_export.xlsx", sheetName = "Exported_data")

        #Clusters Plots
    #Kmeans Plot
plot(data$y ~ data$t, col = "black", lwd = 0.5, pch = "*",
     main = "Clusters Kmeans (n=3)", xlab = "Time", ylab = "Values")
Kmeans_cl1 = subset(x = data, subset = (data$Kmeans3_cluster == 1), select = c(t, y), drop = FALSE)
Kmeans_cl2 = subset(x = data, subset = (data$Kmeans3_cluster == 2), select = c(t, y), drop = FALSE)
Kmeans_cl3 = subset(x = data, subset = (data$Kmeans3_cluster == 3), select = c(t, y), drop = FALSE)
points(Kmeans_cl1, col="red", lwd = 0.5, pch = "*")
points(Kmeans_cl2, col="blue", lwd = 0.5, pch = "*")
points(Kmeans_cl3, col="violet", lwd = 0.5, pch = "*")

#Test Normality by Clusters
print("Null hypothesis: The data is normally distributed. If p> 0.05, normality can be assumed.")
shapiro.test(Kmeans_cl1$y)
shapiro.test(Kmeans_cl2$y)
shapiro.test(Kmeans_cl3$y)
print("Reject null hypothesis.") #Reject Normality


    #Ward Hierarchical Clustering Plot: 3 clusters
plot(data$y ~ data$t, col = "black", lwd = 0.5, pch = "*",
     main = "Clusters Ward Hierarchical Clustering (n=3)", xlab = "Time", ylab = "Values")
ward_3cl1 = subset(x = data, subset = (data$ward_3cluster == 1), select = c(t, y), drop = FALSE)
ward_3cl2 = subset(x = data, subset = (data$ward_3cluster == 2), select = c(t, y), drop = FALSE)
ward_3cl3 = subset(x = data, subset = (data$ward_3cluster == 3), select = c(t, y), drop = FALSE)
points(ward_3cl1, col="red", lwd = 0.5, pch = "*")
points(ward_3cl2, col="violet", lwd = 0.5, pch = "*")
points(ward_3cl3, col="blue", lwd = 0.5, pch = "*")

#Test Normality by Clusters
print("Null hypothesis: The data is normally distributed. If p> 0.05, normality can be assumed.")
shapiro.test(ward_3cl1$y)
shapiro.test(ward_3cl2$y)
shapiro.test(ward_3cl3$y)
print("Reject null hypothesis.") #Reject Normality


#Ward Hierarchical Clustering Plot: 4 clusters
plot(data$y ~ data$t, col = "black", lwd = 0.5, pch = "*",
     main = "Clusters Ward Hierarchical Clustering (n=4)", xlab = "Time", ylab = "Values")
ward_4cl1 = subset(x = data, subset = (data$ward_4cluster == 1), select = c(t, y), drop = FALSE)
ward_4cl2 = subset(x = data, subset = (data$ward_4cluster == 2), select = c(t, y), drop = FALSE)
ward_4cl3 = subset(x = data, subset = (data$ward_4cluster == 3), select = c(t, y), drop = FALSE)
ward_4cl4 = subset(x = data, subset = (data$ward_4cluster == 4), select = c(t, y), drop = FALSE)
points(ward_4cl1, col="red", lwd = 0.5, pch = "*")
points(ward_4cl2, col="violet", lwd = 0.5, pch = "*")
points(ward_4cl3, col="blue", lwd = 0.5, pch = "*")
points(ward_4cl4, col="darkorange", lwd = 0.5, pch = "*")

#Test Normality by Clusters
print("Null hypothesis: The data is normally distributed. If p> 0.05, normality can be assumed.")
shapiro.test(ward_4cl1$y)
shapiro.test(ward_4cl2$y)
shapiro.test(ward_4cl3$y)
shapiro.test(ward_4cl4$y)
print("Reject null hypothesis.") #Reject Normality


#Ward Hierarchical Clustering Plot: 5 clusters
plot(data$y ~ data$t, col = "black", lwd = 0.5, pch = "*",
     main = "Clusters Ward Hierarchical Clustering (n=5)", xlab = "Time", ylab = "Values")
ward_5cl1 = subset(x = data, subset = (data$ward_5cluster == 1), select = c(t, y), drop = FALSE)
ward_5cl2 = subset(x = data, subset = (data$ward_5cluster == 2), select = c(t, y), drop = FALSE)
ward_5cl3 = subset(x = data, subset = (data$ward_5cluster == 3), select = c(t, y), drop = FALSE)
ward_5cl4 = subset(x = data, subset = (data$ward_5cluster == 4), select = c(t, y), drop = FALSE)
ward_5cl5 = subset(x = data, subset = (data$ward_5cluster == 5), select = c(t, y), drop = FALSE)
points(ward_5cl1, col="red", lwd = 0.5, pch = "*")
points(ward_5cl2, col="violet", lwd = 0.5, pch = "*")
points(ward_5cl3, col="blue", lwd = 0.5, pch = "*")
points(ward_5cl4, col="darkorange", lwd = 0.5, pch = "*")
points(ward_5cl5, col="lightblue", lwd = 0.5, pch = "*")

#Test Normality by Clusters
print("Null hypothesis: The data is normally distributed. If p> 0.05, normality can be assumed.")
shapiro.test(ward_5cl1$y)
shapiro.test(ward_5cl2$y)
shapiro.test(ward_5cl3$y)
shapiro.test(ward_5cl4$y)
shapiro.test(ward_5cl5$y)
print("Reject null hypothesis.") #Reject Normality