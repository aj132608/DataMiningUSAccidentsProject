#PROJECT 2
# setwd("/Users/zaynabsaeed/Documents/CS4331_001_hsaeed_v1")
data <- read.csv("diabetic_data.csv", sep = ",", header = TRUE)
id <- read.csv("IDs_mapping.csv", sep = ",", header = TRUE)

set.seed(5) #generates random numbers

#observing size of dataset
n <- dim(data)[1]

#partitioning dataset
train_ind <- runif(n) < 0.80 #since its a large dataset, 80% of the original data must be maintained on training set

data_train <- data[train_ind,] #training set
data_test <- data[!train_ind,] #testing set

#two-fold cross-validation was used to provide less bias


t.test(data_train$number_diagnoses,data_test$number_diagnoses)
#test is valid since p-value is greater than 0.05

#two-sample z-test
p1<-sum(data_train$metformin=="Steady")/dim(data_train)[1]
p2<-sum(data_test$metformin=="Steady")/dim(data_test)[1]
p_pooled<-(sum(data_train$metformin=="Steady")+sum(data_test$metformin=="Steady"))/
  (dim(data_train)[1]+dim(data_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) *
                  (1/dim(data_train)[1]+1/dim(data_test)[1]))

#no evidence that the "Steady" response differs from both data sets due to p-value > 0.05
#thus the partition is valid

#READMITTED analysis
table(data_train$readmitted)


#balancing the <30 value to 36%
x<-(.36*81478-9196)/.64
x 
to.resample <- which(data_train$readmitted == "<30")
our.resample <- sample(x = to.resample, size =  31462, replace = TRUE)
our.resample <- data_train[our.resample,]


#balancing the >30 value to 47%
x<-(.47*81478-28415)/.53
x 
to.resample1 <- which(data_train$readmitted == ">30")
our.resample1 <- sample(x = to.resample1, size =  18640, replace = TRUE)
our.resample1 <- data_train[our.resample1,]


#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1)
t.v1 <- table(train_data_rebal$readmitted)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "NO" outcome.




#METFORMIN analysis
#checking if metforming data is unbalanced
table(data_train$metformin)
#it is unblanced

#balancing the Steady value to 45%
x<-(.45*81478-14717)/.55
x #39905
to.resample <- which(data_train$metformin == "Steady")
our.resample <- sample(x = to.resample, size = 39905, replace = TRUE)
our.resample <- data_train[our.resample,]

#balancing the Up value to 40%
to.resample1 <- which(data_train$metformin == "Up")
x<-(.40*81478-870)/.6
x #52868
our.resample1 <- sample(x = to.resample1, size = 52868, replace = TRUE)
our.resample1 <- data_train[our.resample1,]

#balancing the Down value to 40%
to.resample2 <- which(data_train$metformin == "Down")
x<-(.40*81478-449)/.6
x #53570
our.resample2 <- sample(x = to.resample2, size = 53570, replace = TRUE)
our.resample2 <- data_train[our.resample2,]

#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1, our.resample2)
t.v1 <- table(train_data_rebal$metformin)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.


#REPAGLINIDE analysis
table(data_train$repaglinide)
#data is imbalanced

#balancing the Down value to 47%
x<-(.47*81478-41)/.53
x
to.resample <- which(data_train$repaglinide == "Down")
our.resample <- sample(x = to.resample, size = 72176, replace = TRUE)
our.resample <- data_train[our.resample,]

#balancing the Up value to 47%
x<-(.47*81478-87)/.53
x
to.resample1 <- which(data_train$repaglinide == "Up")
our.resample1 <- sample(x = to.resample1, size = 72089, replace = TRUE)
our.resample1 <- data_train[our.resample1,]

#balancing the Steady value to 47%
x<-(.47*81478-1088)/.53
x
to.resample2 <- which(data_train$repaglinide == "Steady")
our.resample2 <- sample(x = to.resample2, size = 72089, replace = TRUE)
our.resample2 <- data_train[our.resample2,]

#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample,our.resample1, our.resample2)

t.v1 <- table(train_data_rebal$repaglinide)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.


#NATEGLINIDE analysis
table(data_train$nateglinide)
#data is imbalanced

#balancing the Down value to 47%
x<-(.47*81478-9)/.53
x
to.resample <- which(data_train$nateglinide == "Down")
our.resample <- sample(x = to.resample, size = 72237, replace = TRUE)
our.resample <- data_train[our.resample,]

#balancing the Up value to 47%
x<-(.47*81478-21)/.53
x
to.resample1 <- which(data_train$nateglinide == "Up")
our.resample1 <- sample(x = to.resample1, size = 72214, replace = TRUE)
our.resample1 <- data_train[our.resample1,]

#balancing the Steady value to 47%
x<-(.47*81478-532)/.53
x
to.resample2 <- which(data_train$nateglinide == "Steady")
our.resample2 <- sample(x = to.resample2, size = 71250, replace = TRUE)
our.resample2 <- data_train[our.resample2,]

#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1, our.resample2)

t.v1 <- table(train_data_rebal$nateglinide)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.



#CHLORPROPAMIDE analysis
table(data_train$chlorpropamide)
#data is imbalanced

#Down could not be rebalanced as there were errors occuring where the No value would
#drastically increase and Down would not be affected.


#balancing the Steady value to 47%
x<-(.47*81478-60)/.53
x
to.resample <- which(data_train$chlorpropamide == "Steady")
our.resample <- sample(x = to.resample, size = 72140, replace = TRUE)
our.resample <- data_train[our.resample,]


#balancing the Up value to 47%
x<-(.47*81478-5)/.53
x
to.resample1 <- which(data_train$chlorpropamide == "Up")
our.resample1 <- sample(x = to.resample1, size = 72244, replace = TRUE)
our.resample1 <- data_train[our.resample1,]


#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1)

t.v1 <- table(train_data_rebal$chlorpropamide)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.


#GLIMEPIRIDE analysis
table(data_train$glimepiride)

#balancing the Down value to 47%
x<-(.47*81478-152)/.53
x
to.resample <- which(data_train$glimepiride == "Down")
our.resample <- sample(x = to.resample, size = 71967, replace = TRUE)
our.resample <- data_train[our.resample,]

#balancing the Steady value to 47%
x<-(.47*81478-3739)/.53
x
to.resample1 <- which(data_train$glimepiride == "Steady")
our.resample1 <- sample(x = to.resample1, size =  65199, replace = TRUE)
our.resample1 <- data_train[our.resample1,]

#balancing the Up value to 47%
x<-(.47*81478-256)/.53
x
to.resample2 <- which(data_train$glimepiride == "Up")
our.resample2 <- sample(x = to.resample2, size =  71771, replace = TRUE)
our.resample2 <- data_train[our.resample2,]


#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1,our.resample2)

t.v1 <- table(train_data_rebal$glimepiride)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.


#ACETOHEXAMIDE Analysis
table(data_train$acetohexamide)
#balancing the Steady value is not needed as there is only 1 result.

#Establishing a baseline performance
#This model would be classified as a Binary Classification where the "No" outcome will 
#result in a all negative model whereas the "Steady" outcome will result in a all positive model.


#GLIPIZIDE Analysis
table(data_train$glipizide)

#balancing the Down value to 45%
x<-(.45*81478-449)/.55
x
to.resample <- which(data_train$glipizide == "Down")
our.resample <- sample(x = to.resample, size = 65847, replace = TRUE)
our.resample <- data_train[our.resample,]

#balancing the Steady value to 45%
x<-(.45*81478-9014)/.45
x
to.resample1 <- which(data_train$glipizide == "Steady")
our.resample1 <- sample(x = to.resample1, size =  61446, replace = TRUE)
our.resample1 <- data_train[our.resample1,]

#balancing the Up value to 45%
x<-(.45*81478-628)/.55
x
to.resample2 <- which(data_train$glipizide == "Up")
our.resample2 <- sample(x = to.resample2, size =  65522, replace = TRUE)
our.resample2 <- data_train[our.resample2,]

#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1, our.resample2)

t.v1 <- table(train_data_rebal$glipizide)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.



#GLYBURIDE analysis
table(data_train$glyburide)

#balancing the Down value to 40%
x<-(.40*81478-439)/.6
x
to.resample <- which(data_train$glyburide == "Down")
our.resample <- sample(x = to.resample, size = 65865, replace = TRUE)
our.resample <- data_train[our.resample,]

#balancing the Steady value to 45%
x<-(.47*81478-7441)/.53
x
to.resample2 <- which(data_train$glyburide == "Steady")
our.resample2 <- sample(x = to.resample2, size =  58214, replace = TRUE)
our.resample2 <- data_train[our.resample2,]


#balancing the Up value to 47%
x<-(.47*81478-662)/.53
x
to.resample1 <- which(data_train$glyburide == "Up")
our.resample1 <- sample(x = to.resample1, size =  71005, replace = TRUE)
our.resample1 <- data_train[our.resample1,]


#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1, our.resample2)

t.v1 <- table(train_data_rebal$glyburide)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.



#TOLBUTAMIDE analysis
table(data_train$tolbutamide)

#balancing the Steady value to 50%
x<-(.50*81478-18)/.5
x
to.resample <- which(data_train$tolbutamide == "Steady")
our.resample <- sample(x = to.resample, size = 81442, replace = TRUE)
our.resample <- data_train[our.resample,]

#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample)

t.v1 <- table(train_data_rebal$tolbutamide)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing a baseline performance
#This model would be classified as a Binary Classification where the "No" outcome will 
#result in a all negative model whereas the "Steady" outcome will result in a all positive model.



#PIOGLITAZONE analyis
table(data_train$pioglitazone)

#balancing the Down value to 55%
x<-(.45*81478-89)/.55
x
to.resample <- which(data_train$pioglitazone == "Down")
our.resample <- sample(x = to.resample, size = 66502, replace = TRUE)
our.resample <- data_train[our.resample,]


#balancing the Steady value to 55%
x<-(.46*81478-5569)/.54
x
to.resample1 <- which(data_train$pioglitazone == "Steady")
our.resample1 <- sample(x = to.resample1, size =   59094, replace = TRUE)
our.resample1 <- data_train[our.resample1,]


#balancing the Up value to 55%
x<-(.45*81478-192)/.55
x
to.resample2 <- which(data_train$pioglitazone == "Up")
our.resample2 <- sample(x = to.resample2, size =  66314, replace = TRUE)
our.resample2 <- data_train[our.resample2,]


#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1, our.resample2)

t.v1 <- table(train_data_rebal$pioglitazone)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.



#ROSIGLITAZONE analysis
table(data_train$rosiglitazone)

#balancing the Down value to 55%
x<-(.45*81478-72)/.55
x
to.resample <- which(data_train$rosiglitazone == "Down")
our.resample <- sample(x = to.resample, size = 66532, replace = TRUE)
our.resample <- data_train[our.resample,]


#balancing the Steady value to 55%
x<-(.45*81478-4853)/.55
x
to.resample1 <- which(data_train$rosiglitazone == "Steady")
our.resample1 <- sample(x = to.resample1, size = 57840, replace = TRUE)
our.resample1 <- data_train[our.resample1,]

#balancing the Up value to 55%
x<-(.45*81478-130)/.55
x
to.resample2 <- which(data_train$rosiglitazone == "Up")
our.resample2 <- sample(x = to.resample2, size =  66427, replace = TRUE)
our.resample2 <- data_train[our.resample2,]


#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1, our.resample2)

t.v1 <- table(train_data_rebal$rosiglitazone)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.



#ACARBOSE
table(data_train$acarbose)

#balancing the Down value to 50%
x<-(.50*81478-3)/.50
x
to.resample <- which(data_train$acarbose == "Down")
our.resample <- sample(x = to.resample, size = 81472, replace = TRUE)
our.resample <- data_train[our.resample,]

#balancing the Steady value to 50%
x<-(.50*81478-239)/.50
x
to.resample1 <- which(data_train$acarbose == "Steady")
our.resample1 <- sample(x = to.resample1, size = 81000, replace = TRUE)
our.resample1 <- data_train[our.resample1,]

#balancing the Up value to 50%
x<-(.50*81478-8)/.50
x
to.resample2 <- which(data_train$acarbose == "Up")
our.resample2 <- sample(x = to.resample2, size = 81462, replace = TRUE)
our.resample2 <- data_train[our.resample2,]


#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1, our.resample2)

t.v1 <- table(train_data_rebal$acarbose)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.



#MIGLITOL
table(data_train$miglitol)

#balancing the Down value to 50%
x<-(.50*81478-3)/.50
x
to.resample <- which(data_train$miglitol == "Down")
our.resample <- sample(x = to.resample, size = 81472, replace = TRUE)
our.resample <- data_train[our.resample,]

#balancing the Steady value to 50%
x<-(.50*81478-26)/.50
x
to.resample1 <- which(data_train$miglitol == "Steady")
our.resample1 <- sample(x = to.resample1, size = 81426, replace = TRUE)
our.resample1 <- data_train[our.resample1,]

#balancing the Up value to 50%
x<-(.50*81478-2)/.50
x
to.resample2 <- which(data_train$miglitol == "Up")
our.resample2 <- sample(x = to.resample2, size = 81474, replace = TRUE)
our.resample2 <- data_train[our.resample2,]


#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1, our.resample2)

t.v1 <- table(train_data_rebal$miglitol)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.



#TROGLITAZONE analysis
table(data_train$troglitazone)
#no need to balance as there is only one result of Steady value


#TOLAZMIDE  
table(data_train$tolazamide)

#balancing the Steady value to 50%
x<-(.50*81478-29)/.50
x
to.resample <- which(data_train$tolazamide == "Steady")
our.resample <- sample(x = to.resample, size = 81420, replace = TRUE)
our.resample <- data_train[our.resample,]

#balancing the Up value not necessary as there is only one value

#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample)

t.v1 <- table(train_data_rebal$tolazamide)
t.v1


t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing a baseline performance
#This model would be classified as a Binary Classification where the "No" outcome will 
#result in a all negative model whereas the "Steady" outcome will result in a all positive model.




#EXAMIDE analysis
table(data_train$examide)
#No balancning required as there is only one value displayed.
#No need to establish Baseline Performance

#CITOGLIPTON analysis
table(data_train$examide)
#No balancning required as there is only one value displayed.
#No need to establish Baseline Performance


#INSULIN analysis
table(data_train$insulin)


#balancing the Down value to 30%
x<-(.30*81478-9771)/.70
x
to.resample <- which(data_train$insulin == "Down")
our.resample <- sample(x = to.resample, size = 20960, replace = TRUE)
our.resample <- data_train[our.resample,]

#balancing the Steady value to 20%
x<-(.20*81478-9771)/.80
x
to.resample1 <- which(data_train$insulin == "Steady")
our.resample1 <- sample(x = to.resample1, size = 8155, replace = TRUE)
our.resample1 <- data_train[our.resample1,]


#balancing the Up value to 30%
x<-(.30*81478-9771)/.70
x
to.resample2 <- which(data_train$insulin == "Up")
our.resample2 <- sample(x = to.resample2, size = 20960, replace = TRUE)
our.resample2 <- data_train[our.resample2,]


#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1, our.resample2)

t.v1 <- table(train_data_rebal$insulin)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.



#GLYBURIDE.METFORMIN analysis
table(data_train$glyburide.metformin)

#balancing the Down value to 40%
x<-(.49*81478-5)/.51
x
to.resample <- which(data_train$glyburide.metformin == "Down")
our.resample <- sample(x = to.resample, size =  78272, replace = TRUE)
our.resample <- data_train[our.resample,]

#balancing the Steady value to 50%
x<-(.50*81478-544)/.50
x
to.resample1 <- which(data_train$glyburide.metformin == "Steady")
our.resample1 <- sample(x = to.resample1, size =  80390, replace = TRUE)
our.resample1 <- data_train[our.resample1,]


#balancing the Up value to 30%
x<-(.49*81478-7)/.51
x
to.resample2 <- which(data_train$glyburide.metformin == "Up")
our.resample2 <- sample(x = to.resample2, size = 78269, replace = TRUE)
our.resample2 <- data_train[our.resample2,]


#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample, our.resample1, our.resample2)

t.v1 <- table(train_data_rebal$glyburide.metformin)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing Baseline Performance
#This model would be classified as a K-nary Classification where all the predictions
#will belong to the "No" outcome.



#GLIPIZIDE.METFORMIN analysis
table(data_train$glipizide.metformin)

#balancing the Steady value to 20%
x<-(.49*81478-9)/.51
x
to.resample1 <- which(data_train$glipizide.metformin == "Steady")
our.resample1 <- sample(x = to.resample1, size =  78265, replace = TRUE)
our.resample1 <- data_train[our.resample1,]

#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample1)

t.v1 <- table(train_data_rebal$glipizide.metformin)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing a baseline performance
#This model would be classified as a Binary Classification where the "No" outcome will 
#result in a all negative model whereas the "Steady" outcome will result in a all positive model.



#GLIMEPIRIDE.PIOGLITAZONE analysis
table(data_train$glimepiride.pioglitazone)
#balancing not necessary as there is only one value in the "Steady" outcome.


#METFORMIN.ROSIGLITAZONE analysis
table(data_train$metformin.rosiglitazone)

#balancing the Steady value to 20%
x<-(.49*81478-2)/.51
x
to.resample1 <- which(data_train$metformin.rosiglitazone == "Steady")
our.resample1 <- sample(x = to.resample1, size =  78278, replace = TRUE)
our.resample1 <- data_train[our.resample1,]

#displaying the balanced results
train_data_rebal <- rbind(data_train, our.resample1)

t.v1 <- table(train_data_rebal$metformin.rosiglitazone)
t.v1

t.v2<-rbind(t.v1,round(prop.table(t.v1),4))
colnames(t.v2) <- c("Glimepiride=Down","Glimepiride=No", "Glimepiride=Steady", "Glimepiride=Up")
rownames(t.v2) <- c("count","proportion")
t.v2

#Establishing a baseline performance
#This model would be classified as a Binary Classification where the "No" outcome will 
#result in a all negative model whereas the "Steady" outcome will result in a all positive model.



#METFORMIN.PIOGLITAZONE analysis
table(data_train$metformin.pioglitazone)
#balancing not necessary as there is only one value in the "Steady" outcome.

#Establishing a baseline performance
#This model would be classified as a Binary Classification where the "No" outcome will 
#result in a all negative model whereas the "Steady" outcome will result in a all positive model.
