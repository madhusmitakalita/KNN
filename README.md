# KNN_fromScratch


In the data set random missing values have been created one feature at time for 5%, 10%    and 20% for each feature. Below are different imputation methods, distance methods, feature scaling methods and imputation accuracy measures used in the program.
Imputation Method:
 1NN: In this imputation method for each of the missing value, the distance has been calculated between all other instances. For calculating the distance, the other two features are considered. The minimum distance instance’s corresponding value is considered as imputed value. 
KNN: K have been kept 10(square root of 100). In this method 10 neighbours were found for each missing value by calculating the distance. For continuous values, the average of these 10 instances corresponding value is considered as imputed value. And for categorical, the most frequent value of the 10 instances are considered as imputed value.
Weighted KNN: In this method, first found the first 10 neighbours. Then each of the neighbour’s weight is calculated using W=1/d2(where d is the distance). If the distance is zero, I considered W=1.  Then for continuous feature, with the below formula the imputed value has been calculated.In my case K=10.
For categorical feature missing value imputation, after calculating the weight, the category which total weight is more is considered as imputed value.

Feature Scaling method used: First the whole program was run without feature scaling.
Then the below two methods are used for feature scaling:
Min-Max: In R programming language directly “rescale” function can be used to Min-Max normalization.
Z-Score normalization: In R programming language directly “scale” function can be used to Min-Max normalization.

Imputation Accuracy: For categorical feature and for continuous feature different method are used to calculate accuracy.  
For continuous first for each missing instance accuracy is the calculated using the below formula:
100-((a-b)/b) *100 where a= original value, b= imputed value
Then the mean of each instance accuracy is calculated as that feature’s imputed accuracy.
For categorical as there are only two values, and if the imputed value is not the original one, it is calculated as 0% otherwise it is considered as 100% for each missing instance. Then the mean of each instance accuracy is calculated as that feature’s imputed accuracy


