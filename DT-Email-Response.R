# Mail-Response-Prediction-DT
Using Decision Tree to predict who replies to direct Emails

# Springleaf : Whether to send an Email or Not!

# Read the train data set: it has almost 145000 rows (clients)

#train <- read.csv(".../train.csv",skip=30000,stringsAsFactors=FALSE)

train <- read.csv(".../train.csv",skip=140000,stringsAsFactors=FALSE)


# since the data set is large, I preferred using a faster function "fread"
# Also since fread is still not final and it has some bugs, I should
# require bit64 package to prevent fread to change some inputs into some
# strange looking symbols
#library("data.table")
#require("bit64")
#train <- fread('.../train.csv', header = T, sep = ',')


##########################  #####################################
################################  ###############################
######################################  #########################
###########################################  ####################
# Let's try to find some correlations between different columns (variables)
# note that some of the columns are NOT numeric
# So we should handle them differently

num_train <- train[, sapply(train,is.numeric)]
# Let's delete those columns that have zero variance. Since these don't
# variate throughout the samples, so they are not affecting the output
# require(matrixStats)
col_vars <- apply(num_train,2,var,use="pairwise.complete.obs")
num_train <-num_train[,col_vars!=0]


#cor(num_train[,c("X9998.32","X0.772")],use="pairwise.complete.obs")
#cors <- cor(num_train[,c(1871:1879)],use="pairwise.complete.obs")
# Let's scale the data:
scaled_train<-scale(num_train[,1:(ncol(num_train)-1)])
scaled_train <- cbind(scaled_train,num_train[,ncol(num_train)])

# Let's find the correlation submatrix, and its inverse
# This code use="pairwise.complete.obs" deletes the un-observed (NA) data
covs <- cov(scaled_train,use="pairwise.complete.obs")
selection <- c(1:ncol(covs))
precision <- solve(covs[selection,selection])
covs[,ncol(covs)]
precision[,ncol(precision)]

# By observing the covariance between inputs and the output
# one can see none of the inputs covariances with the output
# is larger than 0.1 (excepts the variances of output with itself)
which(abs(covs[,ncol(covs)])>0.1)

###################
#############################
#Lets try to find the covariances for non-numeric (nominal) data

nom_train <- train[, !sapply(train,is.numeric)]
nom_train <- nom_train[,-ncol(nom_train)]

# lets find those columns that are not variating and eliminate them

j <- 1
flag <- vector()
for (i in 1:ncol(nom_train)) {
  levels <- levels(factor(nom_train[,i]))
  variations <- levels[levels!=""] # deleting the missing values that are not NA
  if(length(variations)==1) {
    flag[j] <- i
    j <- j+1
  }
}

nom_train <- nom_train[,-flag]

# Also delete the NA rows
nom_train <-nom_train[,!colSums(is.na(nom_train))==nrow(nom_train)]

# Now we only have 41 categorical columns
# Let's add the output column to this database

nom_train <-cbind(nom_train,train[,ncol(train)])


# we use chi-squared test to find correlation between inputs and the output
# we deleted those columns with p.values larger than 0.01
library(MASS)

j <- 1
flag <- vector()
for (i in 1:(ncol(nom_train)-1)) {
  table <- table(nom_train[,i],nom_train[,ncol(nom_train)])
  corr <- chisq.test(table)

  if(corr$p.value>=0.01) {
    flag[j] <- i
    j <- j+1
  }
}

# deleteing those columns with p>=0.01 from the nom_train

nom_train <- nom_train[,-flag]






##########################  ##############################################
#################################  #######################################
#####################################  ###################################
######################################## #################################
############################################  ############################
#################################################  #######################
####################################################  ####################


######################## TESTING ###############
################################################
#################################################
# First let's only find those column names that we need
columns <- colnames(train)
matches <- which(columns %in% colnames(input)) #the indices of the matching columns
col_choices <- c(1:(ncol(train)-1))
col_choices[matches] <- NA
col_choices[-matches] <- "NULL"
col_choices[1]<-NA

#Lets read only those 8 columns from the test data
test <- read.csv(".../test.csv",colClasses=col_choices,stringsAsFactors=FALSE)
ID <- test[,1]
test<-test[,-1]
colnames(test) <-colnames(input)

# we should replace those "" values in each column with NA
test[test==""] <- NA

# factoring all the columns
for (i in 1:ncol(test)) {
  test[,i] <- factor(test[,i])
}


# Learning Part: DT -> C5.0
###########################################################
###############################################
####################################

# shuffle the rows of nom_train
nom_train <- nom_train[sample(nrow(nom_train)),]
input <- nom_train[,-ncol(nom_train)]
output <- nom_train[,ncol(nom_train)]

# Some of the inputs are only dates, let's ignore them for a moment
dates <-c(3:15)
input <-input[,-dates]

# we should replace those "" values in each column with NA
input[input==""] <- NA

# lets make all the columns as factors
for (i in 1:ncol(input)) {
  input[,i] <- factor(input[,i])
  levels(input[,i]) <- union(levels(test[,i]),levels(input[,i]))
}
output <- factor(output)

# DT Code:
library(party)
decision_tree <- ctree(output~.,data=input)
decision_tree
plot(decision_tree)
table(predict(decision_tree),output)

# predict 0 or 1
prediction <- predict(decision_tree,newdata=test)
levels(prediction)

# probabilities: with what probability we choose "1"?
output_prob <- treeresponse(decision_tree,newdata=test)
out_prob_matrix <- matrix(unlist(output_prob), ncol = 2, byrow = TRUE)
options("scipen"=10) # replace 1e+05 with 100000
final_data <-cbind(as.integer(ID),out_prob_matrix[,2])
#colnames(final_data) <- c("ID","target")
write.table(final_data,file="D:/LSU/Fall 2015/SpringLeaf/submission.csv"
            ,sep=",",col.names=c("ID","target"),row.names=FALSE)


