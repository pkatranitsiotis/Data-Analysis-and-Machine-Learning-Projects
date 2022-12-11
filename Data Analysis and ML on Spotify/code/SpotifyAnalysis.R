#Import of the dataset
library(readr)
df <- read_csv("dataset/SpotifyData.csv")
View(df)

#Observe and categorize correctly the variables 
str(df)
df$explicit<-as.factor(df$explicit)
df$key<-as.factor(df$key)
df$mode<-as.factor(df$mode)

#### split the data into training and test set ####
set.seed(1)
train=sample(nrow(df),floor(nrow(df)*0.7)) #the 70% of the data
train.set=df[train,]
test.set=df[-train,] #the remaing 30% of the dara

#### Multiple Linear Regression ####

#observe the linearity
cormatrix<-cor(df[,c(1,2,3,5,6,7,10,12,13,16,18,19)])
cormatrix

#pearson correlation only for popularity
cormatrix[,10]

#fit the multiple linear model
lm.fit<-lm(popularity~valence+year+acousticness+danceability+duration_ms+energy+
             instrumentalness+liveness+loudness+speechiness,data=train.set)
summary(lm.fit)

#diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)

#using test set as the prediction data
lm.pred=predict(lm.fit,test.set)

#calculation of the test error
mean((lm.pred-test.set$popularity)^2) #MSE


#### Logistic Regression ####

#fiting of the logistic regression on the trainig set
fit.glm=glm(explicit~key+mode+danceability+liveness+energy+valence+acousticness+loudness,
            data=train.set,family = binomial)
summary(fit.glm)

#use the test data as the new data
glm.pred=predict(fit.glm,test.set,type="response")

temp1=rep(0,nrow(test.set))
temp1[glm.pred>=0.5]=1


#calculation of the accouracy of logistic regression:

##confusion matrix
t<-table(temp1,test.set$explicit)
t

##accuracy
log.accuracy=(t[1,1]+t[2,2])/(t[1,1]+t[1,2]+t[2,1]+t[2,2])
log.accuracy


#### Trees ####

library(tree)

#fitting of the regression tree on the training set
t=tree(valence~.,data=train.set)
summary(t)

#plot of the tree
plot(t)
text(t,pretty=0,cex=0.6)

#use test set a the evaluation data
pred.tree=predict(t,test.set)

#calculation of the test error
tree.error=mean((test.set$valence-pred.tree)^2)
tree.error

#cross-validation
cv.valence=cv.tree(t,FUN=prune.tree)
cv.valence

par(mfrow=c(1,2))
plot(cv.valence$size,cv.valence$dev,type = "b",xlab = "size",ylab="dev")
plot(cv.valence$k,type = "b",ylab="k")
#WE observe that the best tree is the full tree with size=10


#### unsuperivsed learning - Hierachical Clustering ####

#restructure of the data
new.df<-df[,c(15,3,5,7,10,12)]
new.df$acousticness<-as.integer(ifelse(new.df$acousticness>=0.5,1,0))
new.df$danceability<-as.integer(ifelse(new.df$danceability>=0.5,1,0))
new.df$energy<-as.integer(ifelse(new.df$energy>=0.5,1,0))
new.df$instrumentalness<-as.integer(ifelse(new.df$instrumentalness>=0.5,1,0))
new.df$liveness<-as.integer(ifelse(new.df$liveness>=0.5,1,0))
new.df<-as.data.frame(new.df)

head(new.df)
str(new.df)

#creating clusters

#we take a random sample of 20000 observations to ensure the independancy and for low RAM reasons
temp=sample(1:20000)
clusters.df=new.df[temp,]

distances=dist(clusters.df,method = "euclidean")
cluster.songs=hclust(distances,method = "complete")
plot(cluster.songs)

#deciding the best height
cluster.groups=cutree(cluster.songs, k=4)

##example
#finding the row of the song perfect on the dataset
subset(new.df,name=="Perfect") #There are several rows but only the 19024 is the one sung by Ed Sheeran
cluster.groups[19024] #Find in which cluster was put

#Five recommendation songs for perfect
cluster2=subset(new.df,cluster.groups==2)
cluster2$name[1:5]
