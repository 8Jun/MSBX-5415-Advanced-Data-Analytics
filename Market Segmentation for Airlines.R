airlines = file.choose()
airlines = read.csv(airlines)

z = summary(airlines)
kable(z)

install.packages("caret")
library(caret)

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)

summary(airlinesNorm)

airlinesNormDist = dist(airlinesNorm, method="euclidean")
airlinesNormHierClust = hclust(airlinesNormDist, method="ward.D")
plot(airlinesNormHierClust)

plot(airlinesNormHierClust)
rect.hclust(airlinesNormHierClust, k = 5, border = "red")

clusterGroups = cutree(airlinesNormHierClust, k = 5)
# Subset the clusters into 5 different groups
HierCluster1 = subset(airlinesNorm, clusterGroups == 1)
HierCluster2 = subset(airlinesNorm, clusterGroups == 2)
HierCluster3 = subset(airlinesNorm, clusterGroups == 3)
HierCluster4 = subset(airlinesNorm, clusterGroups == 4)
HierCluster5 = subset(airlinesNorm, clusterGroups == 5)

nrow(HierCluster1)

tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)

# k-means algorithm
set.seed(88)
kmc = kmeans(airlinesNorm, centers=5, iter.max = 1000)
# Subset into 5 different cluster datasets
KmeansCluster1 = subset(airlinesNorm, kmc$cluster == 1)

KmeansCluster2 = subset(airlinesNorm, kmc$cluster == 2)

KmeansCluster3 = subset(airlinesNorm, kmc$cluster == 3)

KmeansCluster4 = subset(airlinesNorm, kmc$cluster == 4)

KmeansCluster5 = subset(airlinesNorm, kmc$cluster == 5)

nrow(KmeansCluster1)
nrow(KmeansCluster2)
nrow(KmeansCluster3)
nrow(KmeansCluster4)
nrow(KmeansCluster5)

tapply(airlines$Balance, kmc$cluster, mean)
tapply(airlines$QualMiles, kmc$cluster, mean)
tapply(airlines$BonusTrans, kmc$cluster, mean)
tapply(airlines$FlightMiles, kmc$cluster, mean)
tapply(airlines$FlightTrans, kmc$cluster, mean)
tapply(airlines$DaysSinceEnroll, kmc$cluster, mean)
tapply(airlines$BonusMiles, kmc$cluster, mean)