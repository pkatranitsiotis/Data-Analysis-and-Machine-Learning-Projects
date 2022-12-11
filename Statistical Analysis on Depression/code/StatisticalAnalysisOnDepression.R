#### Important Libraries ####
library(readr)
library(car) #for Levene's test
library(ggpubr)
library(readxl)


#### enter dataset in R ####
df<-read_csv("b_depressed.csv")
df<-as.data.frame(df)
df<-na.omit(df) #delete the na values

head(df)

#### categorize correctly each variable ####
str(df)
df$sex<-as.factor(df$sex)
df$Married<-as.factor(df$Married)
df$incoming_salary<-as.factor(df$incoming_salary)
df$incoming_own_farm<-as.factor(df$incoming_own_farm)
df$incoming_business<-as.factor(df$incoming_business)
df$incoming_no_business<-as.factor(df$incoming_no_business)
df$labor_primary<-as.factor(df$labor_primary)
df$depressed<-as.factor(df$depressed)

#### Descriptive Statistics ####
d0<-subset(df,depressed==0)
d1<-subset(df,depressed==1)

#for not depressed
t10<-summary(d0$Age)
t20<-summary(d0$Number_children)
t30<-summary(d0$living_expenses)
t40<-summary(d0$incoming_agricultural)
t50<-summary(d0$lasting_investment)
t60<-summary(d0$no_lasting_investmen)
t0<-cbind(t10,t20,t30,t40,t50,t60)
colnames(t0)<-c("Age","Children","Living Expenses","Incoming Agricultural","Lasting Investment","No Lasting Investment")
t0

#for depressed
t11<-summary(d1$Age)
t21<-summary(d1$Number_children)
t31<-summary(d1$living_expenses)
t41<-summary(d1$incoming_agricultural)
t51<-summary(d1$lasting_investment)
t61<-summary(d1$no_lasting_investmen)
t1<-cbind(t11,t21,t31,t41,t51,t61)
colnames(t1)<-c("Age","Children","Living Expenses","Incoming Agricultural","Lasting Investment","No Lasting Investment")
t1


#histograms
par(mfrow=c(3,2))
hist(df$Age,prob=TRUE,col = "#3399CC",main="Histogram of Age",xlab = "Age")
x <- seq(min(df$Age), max(df$Age), length = 40)
f <- dnorm(x, mean = mean(df$Age), sd = sd(df$Age))
lines(x, f, col = "red", lwd = 2)

hist(df$Number_children,prob=TRUE,col = "#3399CC",main="Histogram of Number of children",xlab = "Number of children")
x <- seq(min(df$Number_children), max(df$Number_children), length = 40)
f <- dnorm(x, mean = mean(df$Number_children), sd = sd(df$Number_children))
lines(x, f, col = "red", lwd = 2)

hist(df$living_expenses,prob=TRUE,col = "#3399CC",main="Histogram of living expenses",xlab="Living Expenses")
x <- seq(min(df$living_expenses), max(df$living_expenses), length = 40)
f <- dnorm(x, mean = mean(df$living_expenses), sd = sd(df$living_expenses))
lines(x, f, col = "red", lwd = 2)

hist(df$incoming_agricultural,prob=TRUE,col = "#3399CC",main="Histogram of incoming agricultural",xlab = "Incoming Agricultural")
x <- seq(min(df$incoming_agricultural), max(df$incoming_agricultural), length = 40)
f <- dnorm(x, mean = mean(df$incoming_agricultural), sd = sd(df$incoming_agricultural))
lines(x, f, col = "red", lwd = 2)

hist(df$lasting_investment,prob=TRUE,col = "#3399CC",main="Histogram of lasting investment",xlab = "Lasting Investment")
x <- seq(min(df$lasting_investment), max(df$lasting_investment), length = 40)
f <- dnorm(x, mean = mean(df$lasting_investment), sd = sd(df$lasting_investment))
lines(x, f, col = "red", lwd = 2)

hist(df$no_lasting_investmen,prob=TRUE,col = "#3399CC",main="Histogram of non-lasting investment",xlab = "Non-lasting Investment")
x <- seq(min(df$no_lasting_investmen), max(df$no_lasting_investmen), length = 40)
f <- dnorm(x, mean = mean(df$no_lasting_investmen), sd = sd(df$no_lasting_investmen))
lines(x, f, col = "red", lwd = 2)

#### X-squared ####

#import of the questionairie
unemployed <- read_excel("Cleaned Data.xlsx")
head(unemployed)
str(unemployed)
unemployed$`I am currently employed at least part-time`<-as.factor(unemployed$`I am currently employed at least part-time`)
unemployed$Depression<-as.factor(unemployed$Depression)

levels(unemployed$`I am currently employed at least part-time`)<-c("Unemployed","Employed")
levels(unemployed$Depression)<-c("Non-Depressed","Depressed")

#Chis-square test
t<-table(unemployed$`I am currently employed at least part-time`,unemployed$Depression)
t
chisq.test(t)

#### t-test: number of depressed for Gender ####

#pre-processed steps
df.gender <- read_csv("prevalence-of-depression-males-vs-females (2).csv")
View(df.gender)

df17<-subset(df.gender,Year==2017)

df17.1<-df17[,c(1,3,4)]
df17.1$Gender<-"M"
colnames(df17.1)<-c("Entity","Year","Number_of_Depressed","Gender")

df17.2<-df17[,c(1,3,5)]
df17.2$Gender<-"F"
colnames(df17.2)<-c("Entity","Year","Number_of_Depressed","Gender")

df17.new<-rbind(df17.1,df17.2)
df17.new$Gender<-as.factor(df17.new$Gender)
head(df17.new)


#descriptive statistics - Number of Depressed people per gender
male.depressed<-subset(df17.new,Gender="M")
female.depressed<-subset(df17.new,Gender="F")
t1<-subset(df17.new,Gender="M")
t2<-subset(df17.new,Gender="F")
t1<-summary(subset(df17.new,Gender=="M")$Number_of_Depressed)
t2<-summary(subset(df17.new,Gender=="F")$Number_of_Depressed)
t<-rbind(t1,t2)
row.names(t)<-c("Number of Depressed Males","Number of Depressed Females")
t

#boxplot for depression between gender
p <- ggplot(df17.new, aes(x=Gender, y=Number_of_Depressed)) + 
  geom_boxplot(fill=c("#E69F00", "#56B4E9"))+
  labs(x="Gender",y="Number of Depressed")
p #boxplot of number of depressed people among gender

#normality test
ggqqplot(df17.new$Number_of_Depressed)
shapiro.test(df17.new$Number_of_Depressed)

#homoscedacity test
leveneTest(Number_of_Depressed~Gender,data=df17.new)

#The assumptions of t-test are not occured and thus non-parametric test will be used

#non-parametric test (Mann-Whitney U-test)
wilcox.test(Number_of_Depressed~Gender,data=df17.new)

wilcox.test(Number_of_Depressed~Gender,data=df17.new,alternative="less") #for deeper research


#### Anova ####
df1 <- read_excel("Mental health Depression disorder Data.xlsx")
View(df1)

#pre-processed steprs
df2<-subset(df1,Year==2017)

df3<-df2[,c(1,3,4)]
colnames(df3)<-c("Entity","Year","Percentage_of_population")
df3$Mental_illness<-"Schizophrenia"

df4<-df2[,c(1,3,5)]
colnames(df4)<-c("Entity","Year","Percentage_of_population")
df4$Mental_illness<-"Bipolar"


df5<-df2[,c(1,3,7)]
colnames(df5)<-c("Entity","Year","Percentage_of_population")
df5$Mental_illness<-"Anxiety"

df6<-df2[,c(1,3,9)]
colnames(df6)<-c("Entity","Year","Percentage_of_population")
df6$Mental_illness<-"Depression"

anova.df<-rbind(df3,df4,df5,df6)
anova.df$Mental_illness<-as.factor(anova.df$Mental_illness)

#normality test
ggqqplot(anova.df$Percentage_of_population)
shapiro.test(anova.df$Percentage_of_population)

#homoscedacity
leveneTest(anova.df$Percentage_of_population~anova.df$Mental_illness)

#first insight (with boxplot)
boxplot(Percentage_of_population~Mental_illness,data=anova.df,col = c("#3399CC","#66CC99","#CC9966","#CC0033"),
        xlab="Mental Illness",ylab = "Percentage of Population")

#parametric test
test.aov<-aov(Percentage_of_population~Mental_illness,data=anova.df)
summary(test.aov) #we reject H0

TukeyHSD(test.aov) #post-hoc test

#non-parametric test
kruskal.test(Percentage_of_population~Mental_illness,data=anova.df)


#### correlation ####

#linear correlation test
cor.set<-d1[,c(4,6,8,9,10,11,12,13,18,19,21,22)] #for depressed people
plot(cor.set) #non linear relations


plot(df$durable_asset,df$lasting_investment)
cor.test(df$durable_asset,df$lasting_investment,method = "kendall",exact=FALSE)

#Spearman Correlation
cor(cor.set, method = "spearman")
cor(d0[,c(10,21)])
cor.test(d1$durable_asset,d1$lasting_investment,method = "spearman",exact=FALSE)


#### linear regression #### 

cor(cor.set, method = "pearson")

#scatterplot
ggplot(df, aes(x=durable_asset, y=lasting_investment)) + 
  geom_point()+
  labs(x="Durable Asset",y="Lasting Investment")+
  geom_smooth(method=lm, se=FALSE)

#fit of the linear regression model
lm.fit<-lm(lasting_investment~durable_asset,data=d1)
summary(lm.fit)
lm.fit
par(mfrow=c(2,2))
plot(lm.fit)


#### logistic regression ####

#split the data into training and test data
set.seed(2)
train<-sample(nrow(df),floor(nrow(df)*0.7))
train.set<-df[train,]
test.set<-df[-train,]


#fit a logistic regression model on train set
fit.glm<-glm(depressed~sex+Married+incoming_salary+incoming_own_farm+incoming_business+labor_primary,data=df,subset=train,family=binomial)
summary(fit.glm)

new.glm<-glm(depressed~Married,data=train.set,family=binomial)
new.glm

pred<-predict(new.glm,test.set,type="response")
glm.pred.fit=rep(0,nrow(test.set))
glm.pred.fit[pred>0.5]=1

#confusion matrix
table(glm.pred.fit,test.set$depressed)
