library(scales)
# reading the file DataSet1.csv
df=read.csv("Data.csv")

ec=df
# Encoding categorical data
ec$Gender = factor(ec$Gender,
                   levels = c('Female', 'Male'),
                   labels = c(1, 2))

set.seed(30)
#Creating random 5 numbers 
p=5
RValue<- sample(1:100, p, replace=F)
print(RValue)

#Creating Missing value function  for hieght

MisingValueGeneration=function(p)
{
  for (i in 1:p) {
    M=RValue[i]
    ec$Height[M]=""
  }
  write.csv(ec,"dataMissingHieght.csv")
}

MisingValueGeneration(p)

df1=read.csv("dataMissingHieght.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Height), ]
df3 <- df1[is.na(df1$Height), ]
#View(df2)
#View(df3)

#Knn functionS for height

KNNforHieghtEuclidean=function(K,j)
{
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[, 4] - df3[j, 4])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Height)
  return(a)
}


KNNforHieghtManhattan=function(K,j)
{
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[, 4] - df3[j, 4])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Height)
  return(a)
}

WeightedKnnHeightEuclidean=function(k,j){
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[, 4] - df3[j, 4])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Height)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}

WeightedKnnHeightManhattan=function(k,j){
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[, 4] - df3[j, 4])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Height)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}
# Accuracy function for Height
Accuracy=function(P)
{
  for(k in 1:P){
    M1=RValue[k]
    a1=df[M1,2]
    a2=df3[df3$X==M1,"Heigh1"]
    Er=(abs(a1-a2)/a1)*100
    df3$ImputedAccuracy[df3$X==M1]=100-Er
  }
  x1=mean(df3$ImputedAccuracy)
  return(x1)
}

# 1NN for 5% missing value of hieght Euclidean distance
for(j1 in 1:p)
{
  b=KNNforHieghtEuclidean(1,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Euclidean distance
x3=Accuracy(p)
print(x3)

Results=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
write.csv(Results,"results.csv")

###################################
#KNN where K=10 for height 5% mssing value Eculidean 

for(j1 in 1:p)
{
  b=KNNforHieghtEuclidean(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

###################
#weighted Knn wher K=10  for hieght 5% missing value euclidean

for(j1 in 1:p)
{
  e=WeightedKnnHeightEuclidean(10,j1)
  df3$Heigh1[j1]=e
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


###############################
# 1NN for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforHieghtManhattan(1,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Manhattan distance
x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# KNN where K=10  for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforHieghtManhattan(10,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# weighted KNN where K=10 for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=WeightedKnnHeightManhattan(10,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for weighted Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



#######################################10%###########################



#Creating random  numbers 
p=10
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration(10)

df1=read.csv("dataMissingHieght.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Height), ]
df3 <- df1[is.na(df1$Height), ]
#View(df2)
#View(df3)

#1NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclidean(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclidean(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightEuclidean(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattan(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattan(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightManhattan(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#######################################20%###########################



#Creating random  numbers 
p=20
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration(p)

df1=read.csv("dataMissingHieght.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Height), ]
df3 <- df1[is.na(df1$Height), ]
#View(df2)
#View(df3)

#1NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclidean(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclidean(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightEuclidean(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattan(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattan(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightManhattan(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




#######################Weight###################


set.seed(30)
#Creating random 5 numbers 
p=5
RValue<- sample(1:100, p, replace=F)
print(RValue)

# Missing value  function for Weight 

MisingValueGeneration1=function(p)
{
  for (i in 1:p) {
    M=RValue[i]
    ec$Weight[M]=""
  }
  write.csv(ec,"dataMissingWeight.csv")
}

MisingValueGeneration1(5)

df1=read.csv("dataMissingWeight.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Weight), ]
df3 <- df1[is.na(df1$Weight), ]
#View(df2)
#View(df3)

#Knn functionS

KNNforWeightEuclidean2=function(K,j)
{
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[, 3] - df3[j, 3])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Weight)
  return(a)
}


KNNforWeightManhattan2=function(K,j)
{
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[, 3] - df3[j, 3])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Weight)
  return(a)
}

WeightedKnnWeightEuclidean2=function(k,j){
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[, 3] - df3[j, 3])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Weight)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}

WeightedKnnWeightManhattan2=function(k,j){
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[, 3] - df3[j, 3])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Weight)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}

#Accuracy function for Weight
Accuracy1=function(P)
{
  for(k in 1:P){
    M1=RValue[k]
    a1=df[M1,3]
    a2=df3[df3$X==M1,"Weight1"]
    Er=(abs(a1-a2)/a1)*100
    df3$ImputedAccuracy1[df3$X==M1]=100-Er
  }
  x1=mean(df3$ImputedAccuracy)
  return(x1)
}

# 1NN for 5% missing value of hieght Euclidean distance
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2(1,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Euclidean distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

###################################
#KNN where K=10 for height 5% mssing value Eculidean 

for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

###################
#weighted Knn wher K=10  for hieght 5% missing value euclidean

for(j1 in 1:p)
{
  e=WeightedKnnWeightEuclidean2(10,j1)
  df3$Weight1[j1]=e
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


###############################
# 1NN for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2(1,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Manhattan distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# KNN where K=10  for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2(10,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# weighted KNN where K=10 for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=WeightedKnnWeightManhattan2(10,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for weighted Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



#######################################10%###########################



#Creating random  numbers 
p=10
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration1(10)

df1=read.csv("dataMissingWeight.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Weight), ]
df3 <- df1[is.na(df1$Weight), ]
#View(df2)
#View(df3)

#1NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightEuclidean2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightManhattan2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#######################################20%###########################



#Creating random  numbers 
p=20
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration1(p)

df1=read.csv("dataMissingWeight.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Weight), ]
df3 <- df1[is.na(df1$Weight), ]
#View(df2)
#View(df3)

#1NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightEuclidean2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightManhattan2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



#######################Gender##############################################

p=5
RValue<- sample(1:100, p, replace=F)
ec1=df
# Encoding categorical data
ec1$Gender = factor(ec1$Gender,
                    levels = c('Female', 'Male'),
                    labels = c(1, 2))

print(RValue)
# missing Value function for Gender
MisingValueGeneration2=function(p)
{
  for (i in 1:p) {
    M=RValue[i]
    ec$Gender[M]=""
  }
  write.csv(ec,"dataMissingGender.csv")
}

MisingValueGeneration2(p)

#View(ec)



df1=read.csv("dataMissingGender.csv")

df2 <- df1[!is.na(df1$Gender), ]
df3 <- df1[is.na(df1$Gender), ]


#Knn Function for Gender

KNNforGenderEuclidean=function(K,j){
  df2$distance=sqrt((df2[,3] - df3[j,3])^2 + (df2[, 4] - df3[j, 4])^2)
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  #View(df5)
  #getting the mode
  ux1=unique(df5$Gender)
  a=ux1[which.max(tabulate(match(ux1,df5$Gender)))]
  print(a)
  return(a)
}
KNNforGenderManhattan=function(K,j){
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[, 4] - df3[j, 4])
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  #View(df5)
  #getting the mode
  ux1=unique(df5$Gender)
  a=ux1[which.max(tabulate(match(ux1,df5$Gender)))]
  print(a)
  return(a)
}


WeightedKNNforGenderEuclidean=function(K,j){
  df2$distance=sqrt((df2[,3] - df3[j,3])^2 + (df2[, 4] - df3[j, 4])^2)
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight4=W2
  W3=df5$Gender
  df5$finalWeight=W2*W3
  df6=df5[which(df5$Gender=='2'), ]
  df7=df5[which(df5$Gender=='1'), ]
  b1=sum(df6$finalWeight)
  a1=sum(df7$finalWeight)
  c=ifelse(a1>b1,1,2)
  return(c)
}


WeightedKNNforGenderManhattan=function(K,j){
  df2$distance=abs(df2[,3] - df3[j,3]) + abs(df2[, 4] - df3[j, 4])
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight4=W2
  W3=df5$Gender
  df5$finalWeight=W2*W3
  df6=df5[which(df5$Gender=='2'), ]
  df7=df5[which(df5$Gender=='1'), ]
  b1=sum(df6$finalWeight)
  a1=sum(df7$finalWeight)
  c=ifelse(a1>b1,1,2)
  return(c)
}

#Accuracy Function for Gender

Accuracy2=function(P)
{
  for(k in 1:P){
    M1=RValue[k]
    print(M1)
    a1=ec1[M1,1]
    print(a1)
    a2=df3[df3$X==M1,"Gender1"]
    print(a2)
    #Er=(abs(a1-a2)/a1)*100
    #print(Er)
    Ac=ifelse(a1==a2,100,0)
    print(Ac)
    df3$ImputedAccuracy[df3$X==M1]=Ac
  }
  x1=mean(df3$ImputedAccuracy)
  print(x1)
}

#######################################

# 1NN eculidean 5% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclidean(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

# 1NN MAnhattan 5% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderManhattan(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


##################Knn K=10 eculidean 5% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclidean(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

############### Weighted Knn K=10, 5% missing euclidean

for(j1 in 1:p)
{
  b=WeightedKNNforGenderEuclidean(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# KNN K=10, 5% missing Manhattan

for(j1 in 1:p)
{
  b=KNNforGenderManhattan(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# Weighted KNN K=10, 5% missing Manhattan

for(j1 in 1:p)
{
  b=WeightedKNNforGenderManhattan(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


################## 10 % missing

p=10
RValue<- sample(1:100, p, replace=F)
MisingValueGeneration2(p)
df1=read.csv("dataMissingGender.csv")

df2 <- df1[!is.na(df1$Gender), ]
df3 <- df1[is.na(df1$Gender), ]

# 1NN eculidean 10% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclidean(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

# 1NN MAnhattan 10% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderManhattan(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


##################Knn K=10 eculidean 10% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclidean(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

############### Weighted Knn K=10, 10% missing euclidean

for(j1 in 1:p)
{
  b=WeightedKNNforGenderEuclidean(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# KNN K=10, 10% missing Manhattan

for(j1 in 1:p)
{
  b=KNNforGenderManhattan(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# Weighted KNN K=10, 10% missing Manhattan

for(j1 in 1:p)
{
  b=WeightedKNNforGenderManhattan(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



##############
p=20
RValue<- sample(1:100, p, replace=F)
MisingValueGeneration2(p)
df1=read.csv("dataMissingGender.csv")

df2 <- df1[!is.na(df1$Gender), ]
df3 <- df1[is.na(df1$Gender), ]

# 1NN eculidean 20% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclidean(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

# 1NN MAnhattan 20% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderManhattan(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


##################Knn K=10 eculidean 20% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclidean(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

############### Weighted Knn K=10, 20% missing euclidean

for(j1 in 1:p)
{
  b=WeightedKNNforGenderEuclidean(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# KNN K=10, 20% missing Manhattan

for(j1 in 1:p)
{
  b=KNNforGenderManhattan(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# Weighted KNN K=10, 20% missing Manhattan

for(j1 in 1:p)
{
  b=WeightedKNNforGenderManhattan(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="No","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



#########feature scaling#############
ec3=df
# Encoding categorical data
ec3$Gender = factor(ec3$Gender,
                    levels = c('Female', 'Male'),
                    labels = c(1, 2))


# Min max scaling for Height and Weight
ec3$Height.rescaled <- rescale(ec3$Height) 
#View(ec3)

ec3$Weight.rescaled <- rescale(ec3$Weight) 
#View(ec3)

set.seed(30)
#Creating random 5 numbers 
p=5
RValue<- sample(1:100, p, replace=F)
print(RValue)

# Missing value for hieght feature Scaling

MisingValueGenerationF1=function(P)
{
  for (i in 1:P) {
    M=RValue[i]
    ec3$Height[M]=""
  }
  write.csv(ec3,"dataMissingHieght1.csv")
}

MisingValueGenerationF1(5)

df1=read.csv("dataMissingHieght1.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Height), ]
df3 <- df1[is.na(df1$Height), ]
#View(df2)
#View(df3)

#Knn functionS

KNNforHieghtEuclideanF1=function(K,j)
{
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[, 6] - df3[j, 6])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Height)
  return(a)
}


KNNforHieghtManhattanF1=function(K,j)
{
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[, 6] - df3[j, 6])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Height)
  return(a)
}

WeightedKnnHeightEuclideanF1=function(k,j){
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[, 6] - df3[j, 6])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Height)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}

WeightedKnnHeightManhattanF1=function(k,j){
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[, 6] - df3[j, 6])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Height)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}

#Accuracy for Feature Scaling height
Accuracy=function(P)
{
  for(k in 1:P){
    M1=RValue[k]
    a1=df[M1,2]
    a2=df3[df3$X==M1,"Heigh1"]
    Er=(abs(a1-a2)/a1)*100
    df3$ImputedAccuracy[df3$X==M1]=100-Er
  }
  x1=mean(df3$ImputedAccuracy)
  return(x1)
}

# 1NN for 5% missing value of hieght Euclidean distance
for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF1(1,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Euclidean distance
x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

###################################5%
#KNN where K=10 for height 5% mssing value Eculidean 

for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF1(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

###################
#weighted Knn wher K=10  for hieght 5% missing value euclidean

for(j1 in 1:p)
{
  e=WeightedKnnHeightEuclideanF1(10,j1)
  df3$Heigh1[j1]=e
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


###############################
# 1NN for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF1(1,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Manhattan distance
x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# KNN where K=10  for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF1(10,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# weighted KNN where K=10 for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=WeightedKnnHeightManhattanF1(10,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for weighted Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



#######################################10%###########################

p=10
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGenerationF1(p)
df1=read.csv("dataMissingHieght1.csv")
#View(df1)
df2 <- df1[!is.na(df1$Height), ]
df3 <- df1[is.na(df1$Height), ]
#View(df2)
#View(df3)

#1NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF1(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF1(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightEuclideanF1(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF1(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF1(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightManhattanF1(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#######################################20%###########################

p=10
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGenerationF1(p)
df1=read.csv("dataMissingHieght1.csv")
#View(df1)
df2 <- df1[!is.na(df1$Height), ]
df3 <- df1[is.na(df1$Height), ]
#View(df2)
#View(df3)

#1NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF1(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF1(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightEuclideanF1(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF1(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF1(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightManhattanF1(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




#######################Weight###################


set.seed(30)
#Creating random 5 numbers 
p=5
RValue<- sample(1:100, p, replace=F)
print(RValue)

#Creating  Missing value for Wieght

MisingValueGeneration1F1=function(P)
{
  for (i in 1:P) {
    M=RValue[i]
    ec3$Weight[M]=""
  }
  write.csv(ec3,"dataMissingWeight.csv")
}

MisingValueGeneration1F1(p)

df1=read.csv("dataMissingWeight.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Weight), ]
df3 <- df1[is.na(df1$Weight), ]
#View(df2)
#View(df3)

#Knn functionS weight feature scaling

KNNforWeightEuclidean2F1=function(K,j)
{
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[,5] - df3[j, 5])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Weight)
  return(a)
}


KNNforWeightManhattan2F1=function(K,j)
{
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[,5] - df3[j, 5])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Weight)
  return(a)
}

WeightedKnnWeightEuclidean2F1=function(k,j){
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[, 5] - df3[j, 5])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Weight)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}

WeightedKnnWeightManhattan2F1=function(k,j){
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[, 5] - df3[j, 5])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Weight)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}
# Accuracy function for weight feature scaling
Accuracy1=function(P)
{
  for(k in 1:P){
    M1=RValue[k]
    a1=df[M1,3]
    a2=df3[df3$X==M1,"Weight1"]
    Er=(abs(a1-a2)/a1)*100
    df3$ImputedAccuracy1[df3$X==M1]=100-Er
  }
  x1=mean(df3$ImputedAccuracy)
  return(x1)
}

# 1NN for 5% missing value of hieght Euclidean distance
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F1(1,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Euclidean distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

###################################
#KNN where K=10 for height 5% mssing value Eculidean 

for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F1(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

###################
#weighted Knn wher K=10  for hieght 5% missing value euclidean

for(j1 in 1:p)
{
  e=WeightedKnnWeightEuclidean2F1(10,j1)
  df3$Weight1[j1]=e
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


###############################
# 1NN for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F1(1,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Manhattan distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# KNN where K=10  for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F1(10,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# weighted KNN where K=10 for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=WeightedKnnWeightManhattan2F1(10,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for weighted Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



#######################################10%###########################



#Creating random  numbers 
p=10
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration1F1(10)

df1=read.csv("dataMissingWeight.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Weight), ]
df3 <- df1[is.na(df1$Weight), ]
#View(df2)
#View(df3)

#1NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F1(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F1(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightEuclidean2F1(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F1(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F1(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightManhattan2F1(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#######################################20%###########################



#Creating random  numbers 
p=20
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration1F1(p)

df1=read.csv("dataMissingWeight.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Weight), ]
df3 <- df1[is.na(df1$Weight), ]
#View(df2)
#View(df3)

#1NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F1(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F1(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightEuclidean2F1(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F1(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F1(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightManhattan2F1(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



#######################Gender##############################################

p=5
RValue<- sample(1:100, p, replace=F)
print(RValue)
ecf1=df
# Encoding categorical data
ecf1$Gender = factor(ecf1$Gender,
                     levels = c('Female', 'Male'),
                     labels = c(1, 2))

MisingValueGeneration2F1=function(P)
{
  for (i in 1:P) {
    M=RValue[i]
    ec3$Gender[M]=""
  }
  write.csv(ec3,"dataMissingGender.csv")
}

MisingValueGeneration2F1(p)

#View(ec)



df1=read.csv("dataMissingGender.csv")

df2 <- df1[!is.na(df1$Gender), ]
df3 <- df1[is.na(df1$Gender), ]


#Knn for Weight Feature scaling

KNNforGenderEuclideanF1=function(K,j){
  df2$distance=sqrt((df2[,5] - df3[j,5])^2 + (df2[, 6] - df3[j, 6])^2)
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  #View(df5)
  #getting the mode
  ux1=unique(df5$Gender)
  a=ux1[which.max(tabulate(match(ux1,df5$Gender)))]
  print(a)
  return(a)
}
KNNforGenderManhattanF1=function(K,j){
  df2$distance=abs(df2[,5] - df3[j,5]) + abs(df2[, 6] - df3[j, 6])
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  #View(df5)
  #getting the mode
  ux1=unique(df5$Gender)
  a=ux1[which.max(tabulate(match(ux1,df5$Gender)))]
  print(a)
  return(a)
}


WeightedKNNforGenderEuclideanF1=function(K,j){
  df2$distance=sqrt((df2[,5] - df3[j,5])^2 + (df2[, 6] - df3[j, 6])^2)
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight4=W2
  W3=df5$Gender
  df5$finalWeight=W2*W3
  df6=df5[which(df5$Gender=='2'), ]
  df7=df5[which(df5$Gender=='1'), ]
  b1=sum(df6$finalWeight)
  a1=sum(df7$finalWeight)
  c=ifelse(a1>b1,1,2)
  return(c)
}


WeightedKNNforGenderManhattanF1=function(K,j){
  df2$distance=abs(df2[,5] - df3[j,5]) + abs(df2[, 6] - df3[j, 6])
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight4=W2
  W3=df5$Gender
  df5$finalWeight=W2*W3
  df6=df5[which(df5$Gender=='2'), ]
  df7=df5[which(df5$Gender=='1'), ]
  b1=sum(df6$finalWeight)
  a1=sum(df7$finalWeight)
  c=ifelse(a1>b1,1,2)
  return(c)
}
#Accuracy for gender

Accuracy2=function(P)
{
  for(k in 1:P){
    M1=RValue[k]
    print(M1)
    a1=ecf1[M1,1]
    print(a1)
    a2=df3[df3$X==M1,"Gender1"]
    print(a2)
    #Er=(abs(a1-a2)/a1)*100
    #print(Er)
    Ac=ifelse(a1==a2,100,0)
    print(Ac)
    df3$ImputedAccuracy[df3$X==M1]=Ac
  }
  x1=mean(df3$ImputedAccuracy)
  print(x1)
}

#######################################5% Gender###########

# 1NN eculidean 5% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF1(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

# 1NN MAnhattan 5% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF1(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


##################Knn K=10 eculidean 5% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

############### Weighted Knn K=10, 5% missing euclidean

for(j1 in 1:p)
{
  b=WeightedKNNforGenderEuclideanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# KNN K=10, 5% missing Manhattan

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# Weighted KNN K=10, 5% missing Manhattan

for(j1 in 1:p)
{
  b=WeightedKNNforGenderManhattanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


################## 10 % Gender##########################

p=10
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration2F1(p)
df1=read.csv("dataMissingGender.csv")

df2 <- df1[!is.na(df1$Gender), ]
df3 <- df1[is.na(df1$Gender), ]

# 1NN eculidean 10% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF1(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

# 1NN MAnhattan 10% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF1(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


##################Knn K=10 eculidean 10% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

############### Weighted Knn K=10, 10% missing euclidean

for(j1 in 1:p)
{
  b=WeightedKNNforGenderEuclideanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# KNN K=10, 10% missing Manhattan

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# Weighted KNN K=10, 10% missing Manhattan

for(j1 in 1:p)
{
  b=WeightedKNNforGenderManhattanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



##############20%Gender########
p=20
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration2F1(p)
df1=read.csv("dataMissingGender.csv")

df2 <- df1[!is.na(df1$Gender), ]
df3 <- df1[is.na(df1$Gender), ]

# 1NN eculidean 20% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF1(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

# 1NN MAnhattan 20% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF1(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


##################Knn K=10 eculidean 20% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

############### Weighted Knn K=10, 20% missing euclidean

for(j1 in 1:p)
{
  b=WeightedKNNforGenderEuclideanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# KNN K=10, 20% missing Manhattan

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# Weighted KNN K=10, 20% missing Manhattan

for(j1 in 1:p)
{
  b=WeightedKNNforGenderManhattanF1(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Min-Max","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)



######################featureScaling2#######

ec4=df
# Encoding categorical data
ec4$Gender = factor(ec4$Gender,
                    levels = c('Female', 'Male'),
                    labels = c(1, 2))


# Zscore feature scaling
ec4$Height.rescaled <-scale(ec4$Height, center = TRUE, scale = TRUE)
#View(ec4)

ec4$Weight.rescaled <-scale(ec4$Weight, center = TRUE, scale = TRUE)
#View(ec4)

set.seed(30)
#Creating random 5 numbers 
p=5
RValue<- sample(1:100, p, replace=F)
print(RValue)

#Creating 5% Missing value for hieght

MisingValueGenerationF2=function(P)
{
  for (i in 1:P) {
    M=RValue[i]
    ec4$Height[M]=""
  }
  write.csv(ec4,"dataMissingHieght1.csv")
}

MisingValueGenerationF2(5)

df1=read.csv("dataMissingHieght1.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Height), ]
df3 <- df1[is.na(df1$Height), ]
#View(df2)
#View(df3)

#Knn functionS

KNNforHieghtEuclideanF2=function(K,j)
{
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[, 6] - df3[j, 6])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Height)
  return(a)
}


KNNforHieghtManhattanF2=function(K,j)
{
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[, 6] - df3[j, 6])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Height)
  return(a)
}

WeightedKnnHeightEuclideanF2=function(k,j){
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[, 6] - df3[j, 6])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Height)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}

WeightedKnnHeightManhattanF2=function(k,j){
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[, 6] - df3[j, 6])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Height)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}

Accuracy=function(P)
{
  for(k in 1:P){
    M1=RValue[k]
    a1=df[M1,2]
    a2=df3[df3$X==M1,"Heigh1"]
    Er=(abs(a1-a2)/a1)*100
    df3$ImputedAccuracy[df3$X==M1]=100-Er
  }
  x1=mean(df3$ImputedAccuracy)
  return(x1)
}

# 1NN for 5% missing value of hieght Euclidean distance
for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF2(1,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Euclidean distance
x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

###################################5%
#KNN where K=10 for height 5% mssing value Eculidean 

for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF2(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

###################
#weighted Knn wher K=10  for hieght 5% missing value euclidean

for(j1 in 1:p)
{
  e=WeightedKnnHeightEuclideanF2(10,j1)
  df3$Heigh1[j1]=e
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


###############################
# 1NN for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF2(1,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Manhattan distance
x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# KNN where K=10  for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF2(10,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# weighted KNN where K=10 for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=WeightedKnnHeightManhattanF2(10,j1)
  df3$Heigh1[j1]=b
}

#calculation accuracy for weighted Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



#######################################10%###########################

p=10
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGenerationF2(p)
df1=read.csv("dataMissingHieght1.csv")
#View(df1)
df2 <- df1[!is.na(df1$Height), ]
df3 <- df1[is.na(df1$Height), ]
#View(df2)
#View(df3)

#1NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF2(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF2(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightEuclideanF2(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF2(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF2(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightManhattanF2(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#######################################20%###########################

p=10
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGenerationF2(p)
df1=read.csv("dataMissingHieght1.csv")
#View(df1)
df2 <- df1[!is.na(df1$Height), ]
df3 <- df1[is.na(df1$Height), ]
#View(df2)
#View(df3)

#1NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF2(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforHieghtEuclideanF2(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightEuclideanF2(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF2(1,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforHieghtManhattanF2(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnHeightManhattanF2(10,j1)
  df3$Heigh1[j1]=b
}

x3=Accuracy(p)
print(x3)

newrow1=data.frame("feature"="Height","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




#######################Weight###################


set.seed(30)
#Creating random 5 numbers 
p=5
RValue<- sample(1:100, p, replace=F)
print(RValue)

#Creating 5% Missing value for hieght

MisingValueGeneration1F2=function(P)
{
  for (i in 1:P) {
    M=RValue[i]
    ec3$Weight[M]=""
  }
  write.csv(ec3,"dataMissingWeight.csv")
}

MisingValueGeneration1F2(p)

df1=read.csv("dataMissingWeight.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Weight), ]
df3 <- df1[is.na(df1$Weight), ]
#View(df2)
#View(df3)

#Knn functionS

KNNforWeightEuclidean2F2=function(K,j)
{
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[,5] - df3[j, 5])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Weight)
  return(a)
}


KNNforWeightManhattan2F2=function(K,j)
{
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[,5] - df3[j, 5])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=K)
  #View(df5)
  a=mean(df5$Weight)
  return(a)
}

WeightedKnnWeightEuclidean2F2=function(k,j){
  df2$distance=sqrt((df2[,2] - df3[j,2])^2 + (df2[, 5] - df3[j, 5])^2)
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Weight)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}

WeightedKnnWeightManhattan2F2=function(k,j){
  df2$distance=abs(df2[,2] - df3[j,2]) + abs(df2[, 5] - df3[j, 5])
  df4=df2[order(df2$distance),]
  df5=head(df4, n=k)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight2=W2
  b=sum(df5$weight2*df5$Weight)
  a=sum(df5$weight2)
  c=b/a
  return(c)
}

Accuracy1=function(P)
{
  for(k in 1:P){
    M1=RValue[k]
    a1=df[M1,3]
    a2=df3[df3$X==M1,"Weight1"]
    Er=(abs(a1-a2)/a1)*100
    df3$ImputedAccuracy1[df3$X==M1]=100-Er
  }
  x1=mean(df3$ImputedAccuracy)
  return(x1)
}

# 1NN for 5% missing value of hieght Euclidean distance
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F2(1,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Euclidean distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

###################################
#KNN where K=10 for height 5% mssing value Eculidean 

for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

###################
#weighted Knn wher K=10  for hieght 5% missing value euclidean

for(j1 in 1:p)
{
  e=WeightedKnnWeightEuclidean2F2(10,j1)
  df3$Weight1[j1]=e
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


###############################
# 1NN for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F2(1,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for 1nn with 5% missing value Manhattan distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# KNN where K=10  for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F2(10,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



# weighted KNN where K=10 for 5% missing value of hieght Manhattan distance
for(j1 in 1:p)
{
  b=WeightedKnnWeightManhattan2F2(10,j1)
  df3$Weight1[j1]=b
}

#calculation accuracy for weighted Knn where K=10 with 5% missing value Manhattan distance
x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



#######################################10%###########################



#Creating random  numbers 
p=10
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration1F2(10)

df1=read.csv("dataMissingWeight.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Weight), ]
df3 <- df1[is.na(df1$Weight), ]
#View(df2)
#View(df3)

#1NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F2(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightEuclidean2F2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F2(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,10% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightManhattan2F2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#######################################20%###########################



#Creating random  numbers 
p=20
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration1F2(p)

df1=read.csv("dataMissingWeight.csv")
#View(df1)

##separting data frame with and without 'NA'
df2 <- df1[!is.na(df1$Weight), ]
df3 <- df1[is.na(df1$Weight), ]
#View(df2)
#View(df3)

#1NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F2(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING EUCLIDEAN
for(j1 in 1:p)
{
  b=KNNforWeightEuclidean2F2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightEuclidean2F2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")




##########Manhattan

#1NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F2(1,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#10NN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=KNNforWeightManhattan2F2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


#WEIGHTED KNN,20% MSSING Manhattan
for(j1 in 1:p)
{
  b=WeightedKnnWeightManhattan2F2(10,j1)
  df3$Weight1[j1]=b
}

x3=Accuracy1(p)
print(x3)

newrow1=data.frame("feature"="Weight","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



#######################Gender##############################################

p=5
RValue<- sample(1:100, p, replace=F)
print(RValue)
ecf2=df
# Encoding categorical data
ecf2$Gender = factor(ecf2$Gender,
                     levels = c('Female', 'Male'),
                     labels = c(1, 2))

MisingValueGeneration2F2=function(P)
{
  for (i in 1:P) {
    M=RValue[i]
    ec3$Gender[M]=""
  }
  write.csv(ec3,"dataMissingGender.csv")
}

MisingValueGeneration2F2(p)

#View(ec)



df1=read.csv("dataMissingGender.csv")

df2 <- df1[!is.na(df1$Gender), ]
df3 <- df1[is.na(df1$Gender), ]




KNNforGenderEuclideanF2=function(K,j){
  df2$distance=sqrt((df2[,5] - df3[j,5])^2 + (df2[, 6] - df3[j, 6])^2)
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  #View(df5)
  #getting the mode
  ux1=unique(df5$Gender)
  a=ux1[which.max(tabulate(match(ux1,df5$Gender)))]
  print(a)
  return(a)
}
KNNforGenderManhattanF2=function(K,j){
  df2$distance=abs(df2[,5] - df3[j,5]) + abs(df2[, 6] - df3[j, 6])
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  #View(df5)
  #getting the mode
  ux1=unique(df5$Gender)
  a=ux1[which.max(tabulate(match(ux1,df5$Gender)))]
  print(a)
  return(a)
}


WeightedKNNforGenderEuclideanF2=function(K,j){
  df2$distance=sqrt((df2[,5] - df3[j,5])^2 + (df2[, 6] - df3[j, 6])^2)
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight4=W2
  W3=df5$Gender
  df5$finalWeight=W2*W3
  df6=df5[which(df5$Gender=='2'), ]
  df7=df5[which(df5$Gender=='1'), ]
  b1=sum(df6$finalWeight)
  a1=sum(df7$finalWeight)
  c=ifelse(a1>b1,1,2)
  return(c)
}


WeightedKNNforGenderManhattanF2=function(K,j){
  df2$distance=abs(df2[,5] - df3[j,5]) + abs(df2[, 6] - df3[j, 6])
  df4=df2[order(df2$distance),]
  #View(df4)
  df5=head(df4, n=K)
  m=df5$distance
  W2=ifelse(m==0.000000,1,1/m^2)
  df5$weight4=W2
  W3=df5$Gender
  df5$finalWeight=W2*W3
  df6=df5[which(df5$Gender=='2'), ]
  df7=df5[which(df5$Gender=='1'), ]
  b1=sum(df6$finalWeight)
  a1=sum(df7$finalWeight)
  c=ifelse(a1>b1,1,2)
  return(c)
}


Accuracy2=function(P)
{
  for(k in 1:P){
    M1=RValue[k]
    print(M1)
    a1=ecf2[M1,1]
    print(a1)
    a2=df3[df3$X==M1,"Gender1"]
    print(a2)
    #Er=(abs(a1-a2)/a1)*100
    #print(Er)
    Ac=ifelse(a1==a2,100,0)
    print(Ac)
    df3$ImputedAccuracy[df3$X==M1]=Ac
  }
  x1=mean(df3$ImputedAccuracy)
  print(x1)
}

#######################################

# 1NN eculidean 5% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF2(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

# 1NN MAnhattan 5% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF2(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


##################Knn K=10 eculidean 5% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

############### Weighted Knn K=10, 5% missing euclidean

for(j1 in 1:p)
{
  b=WeightedKNNforGenderEuclideanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# KNN K=10, 5% missing Manhattan

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# Weighted KNN K=10, 5% missing Manhattan

for(j1 in 1:p)
{
  b=WeightedKNNforGenderManhattanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="5%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


################## 10 % missing Gender###########

p=10
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration2F2(p)
df1=read.csv("dataMissingGender.csv")

df2 <- df1[!is.na(df1$Gender), ]
df3 <- df1[is.na(df1$Gender), ]

# 1NN eculidean 10% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF2(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

# 1NN MAnhattan 10% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF2(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


##################Knn K=10 eculidean 10% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

############### Weighted Knn K=10, 10% missing euclidean

for(j1 in 1:p)
{
  b=WeightedKNNforGenderEuclideanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# KNN K=10, 10% missing Manhattan

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# Weighted KNN K=10, 10% missing Manhattan

for(j1 in 1:p)
{
  b=WeightedKNNforGenderManhattanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="10%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")



##############20% Gender#################
p=20
RValue<- sample(1:100, p, replace=F)
print(RValue)
MisingValueGeneration2F2(p)
df1=read.csv("dataMissingGender.csv")

df2 <- df1[!is.na(df1$Gender), ]
df3 <- df1[is.na(df1$Gender), ]

# 1NN eculidean 20% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF2(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

# 1NN MAnhattan 20% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF2(1,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)
newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "1NN",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


##################Knn K=10 eculidean 20% missing Gender

for(j1 in 1:p)
{
  b=KNNforGenderEuclideanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")

############### Weighted Knn K=10, 20% missing euclidean

for(j1 in 1:p)
{
  b=WeightedKNNforGenderEuclideanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Euclidean", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# KNN K=10, 20% missing Manhattan

for(j1 in 1:p)
{
  b=KNNforGenderManhattanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")


############# Weighted KNN K=10, 20% missing Manhattan

for(j1 in 1:p)
{
  b=WeightedKNNforGenderManhattanF2(10,j1)
  df3$Gender1[j1]=b
}

x3=Accuracy2(p)
print(x3)

newrow1=data.frame("feature"="Gender","DistanceMethod" = "Manhattan", "ImputationMethod" = "Weighted K-NN ,K=10",
                   "FeatureScaling"="Zscore","Missing Value"="20%","Accuracy" = x3)
Results=rbind(Results,newrow1)
write.csv(Results,"results.csv")
write.csv(Results,"results.csv")