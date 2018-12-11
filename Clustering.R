library("cluster", lib.loc="~/R/win-library/3.3")
library("factoextra", lib.loc="~/R/win-library/3.3")
library("NbClust", lib.loc="~/R/win-library/3.3")
library("fpc", lib.loc="~/R/win-library/3.3")

# Import Data
Diabetes <- read.csv("C:/Users/Sandhya/OneDrive/Full Time 2016/Projects/Clustering/Diabetics.csv")

# Exploratory data analysis
names(Diabetes)
summary(Diabetes)

# Race - (Missing indicator is "?" - Assign it to Other)
# Diabetes$race <- ifelse(Diabetes$race=="?","Other",as.character(Diabetes$race))
Diabetes$race <- ifelse(Diabetes$race=="?",6,as.factor(Diabetes$race))

# 3 obs have unknow or invalid gender - delete those obs
Diabetes <- subset(Diabetes, gender!="Unknown/Invalid")

# 95% of weight data is missing - hence weight variable can be deleted
# ID variiables can be deleted
Diabetes$weight <- NULL
Diabetes <- Diabetes[-c(1,6:8)]

# Low variance variables can be deleted
low_var <- names(Diabetes) %in%  c("acetohexamide","troglitazone","examide","citoglipton",
             "glipizide.metformin","glimepiride.pioglitazone","metformin.rosiglitazone",
             "metformin.pioglitazone")
Diabetes <- Diabetes[!low_var]

# Box plots 
boxplot(Diabetes$num_lab_procedures)
boxplot(Diabetes$number_outpatient)
table(Diabetes$number_outpatient)

# Number of Continuous variables are 8
# Number of Categorical variables are 25

####### Consider only Numeric Variables
NumVars <- c("time_in_hospital","num_lab_procedures","num_procedures","num_medications",
             "number_outpatient","number_emergency","number_inpatient","number_diagnoses")
Diabetes_Num <- Diabetes[NumVars]
Diabetes_Num.scaled <- scale(Diabetes_Num)

#Diabetes_Num.scaled <- Diabetes_Num.scaled[sample(nrow(Diabetes_Num.scaled), 1000), ]
#Diabetes_Num <- Diabetes_Num[sample(nrow(Diabetes_Num), 3000), ]

# Optimal number of clusters in the data - Elbow Method
factoextra::fviz_nbclust(Diabetes_Num.scaled, kmeans, method = "wss") +
  ggplot2::geom_vline(xintercept = 3, linetype = 2)

# wss uses with sum of squares criterion; silhouette uses 
require(cluster) 
factoextra::fviz_nbclust(Diabetes_Num.scaled, kmeans, method = "silhouette")

# Compute the number of clusters - find optimal clusters
library(NbClust)
nb <- NbClust(Diabetes_Num.scaled, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")
# Visualize the result
library(factoextra)
fviz_nbclust(nb) + theme_minimal()


# K-Means 
KMeans_Output <- stats::kmeans(Diabetes_Num,3)

# Another way of doing k-means clustering
km.res <- eclust(Diabetes_Num.scaled, "kmeans", k = 3,
                 nstart = 25, graph = FALSE)
fviz_cluster(km.res, geom = "point", frame.type = "norm")

# silhouette plot - Internal Cluster Validation
dissE <- daisy(Diabetes_Num)
sil <- cluster::silhouette(KMeans_Output$cluster, dissE)
plot(sil)
# more elegant data visualization
library(factoextra)
fviz_silhouette(sil)
# from the plot average silhouette width is 0.4 implies a weak structure

# Dunn Index - Another Internal cluster validation measure
# dunn - minimum separation / maximum diameter. Dunn index, see Halkidi et al. (2002).
# dunn2 - minimum average dissimilarity between two cluster / maximum average within cluster dissimilarity, another version of the family of Dunn indexes
Diabetes_Num_Dist <- dist(Diabetes_Num, method = "euclidean")
stats<- fpc::cluster.stats(Diabetes_Num_Dist, KMeans_Output$cluster)
stats$dunn
stats$dunn2

Diabetes_Num$Cluster <- KMeans_Output$cluster
Diabetes_Num$readmitted <- Diabetes$readmitted

#AUC - Area  under the curve
library(pROC)
auc(Diabetes_Num$readmitted, Diabetes_Num$Cluster)

# 0.502 auc implies very weak clustering

# Confusion Matrix
confusion_Matrix <- table(Diabetes_Num$readmitted, Diabetes_Num$Cluster)

# Partitioning (clustering) of the data into k clusters "around medoids", a more robust version of K-means.

# PAM clustering (Partitioning around medoids)
pam_Output <- cluster::pam(Diabetes_Num, 2)

# PAM clustering
pam.res <- eclust(Diabetes_Num.scaled, "pam", k = 3, graph = FALSE)
# Visualize pam clusters
fviz_cluster(pam.res, geom = "point", frame.type = "norm")

# silhouette plot
dissE <- daisy(Diabetes_Num)
sil <- cluster::silhouette(pam_Output$clustering, dissE)
plot(sil)
#sil is similar to sil1
sil1 <- cluster::silhouette(pam_Output$clustering, dist(Diabetes_Num))
plot(sil1)
# more elegant data visualization
library(factoextra)
fviz_silhouette(sil)
# PAM performs better than k-means but is still weak

# Dunn Index - Another Internal cluster validation measure
Diabetes_Num_Dist <- dist(Diabetes_Num, method = "euclidean")
stats<- fpc::cluster.stats(Diabetes_Num_Dist, pam_Output$clustering)
stats$dunn
stats$dunn2


# Enhanced hierarchical clustering
res.hc <- eclust(Diabetes_Num.scaled, "hclust", k = 3,
                 method = "complete", graph = FALSE) 
# silhouette plot
dissE <- daisy(Diabetes_Num.scaled)
sil <- cluster::silhouette(res.hc$cluster, dissE)
plot(sil)
sil1 <- cluster::silhouette(res.hc$cluster, dist(Diabetes_Num.scaled))
plot(sil1)
# Average Silhouette width is too small
# more elegant data visualization
library(factoextra)
fviz_silhouette(sil)
# Hierarchical clustering doesn't perform well - (Avg silhouette width is -0.04, negative coefficient implies that the points are not in the right cluster)


# comparing 2 cluster solutions - External cluster validation (known output vs model clusters)
library(fpc)
# Dist matrix
Diabetes_Num_Dist <- dist(Diabetes_Num, method = "euclidean")
stats <- fpc::cluster.stats(Diabetes_Num_Dist, Diabetes_Num$readmitted, KMeans_Output$cluster)
#The corrected Rand index provides a measure for assessing the similarity between
# two partitions, adjusted for chance. Its range is -1 (no agreement) to 1 
# (perfect agreement)
stats$corrected.rand
stats$vi


#### Categorical data
CatVars <- c("race","gender","age","num_medications","payer_code","medical_specialty",
             "diag_1","diag_2","diag_3","max_glu_serum","max_glu_serum","A1Cresult","metformin" ,         
              "repaglinide", "nateglinide", "chlorpropamide","glimepiride" ,       
              "glipizide" ,"glyburide" , "tolbutamide", "pioglitazone" ,      
              "rosiglitazone","acarbose", "miglitol","tolazamide" ,        
              "insulin","glyburide.metformin","change","diabetesMed")
Diabetes_Cat <- Diabetes[CatVars]
# Data must be converted to numeric before using KModes algorithm - Algorithm is based on similarity
Diabetes_Cat <- data.matrix(Diabetes_Cat, rownames.force = NA)

#Diabetes_Cat <- Diabetes_Cat[sample(nrow(Diabetes_Cat), 3000), ]

# K-Modes
kmodes_Output <- klaR::kmodes(Diabetes_Cat,3, iter.max=5)

plot(Diabetes_Cat,col=kmodes_Output$cluster) 

plot(jitter(Diabetes_Cat), col = kmodes_Output$cluster)
points(kmodes_Output$modes, col = 1:5, pch = 8)


#### All Variables
Mixed <- c("time_in_hospital","num_lab_procedures","num_procedures","num_medications",
  "number_outpatient","number_emergency","number_inpatient","number_diagnoses",
  "race","gender","age","num_medications","payer_code","medical_specialty",
  "diag_1","diag_2","diag_3","max_glu_serum","max_glu_serum","A1Cresult","metformin" ,         
  "repaglinide", "nateglinide", "chlorpropamide","glimepiride" ,       
  "glipizide" ,"glyburide" , "tolbutamide", "pioglitazone" ,      
  "rosiglitazone","acarbose", "miglitol","tolazamide" ,        
  "insulin","glyburide.metformin","change","diabetesMed")

Diabetes_mixed <- Diabetes[Mixed]

# Diabetes_mixed <- Diabetes_mixed[sample(nrow(Diabetes_mixed), 3000), ]
library("clustMixType", lib.loc="~/R/win-library/3.3")
kprototype_Output <- clustMixType:: kproto(Diabetes_mixed,3)

# For numerical variables boxplots and for factor variables barplots of each cluster are generated
clprofiles(kprototype_Output,Diabetes_mixed)

# Predicted clusters
Diabetes_mixed$Cluster <- kprototype_Output$cluster
Diabetes_mixed$readmitted <- Diabetes$readmitted

# Cluster Evaluation
#AUC - Area  under the curve
library(pROC)
auc(Diabetes_mixed$readmitted, Diabetes_mixed$Cluster)

# Confusion Matrix
confusion_Matrix <- table(Diabetes_mixed$readmitted, Diabetes_mixed$Cluster)

# Reference:
# http://www.sthda.com/english/wiki/clustering-validation-statistics-4-vital-things-everyone-should-know-unsupervised-machine-learning
