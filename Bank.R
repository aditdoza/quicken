
#First Install Packages Needed (Skip this step if the packages are already installed)
install.packages("tidyverse")
install.packages("gridExtra")
install.packages("rpart.plot")
intall.packages("rpart")
install.packages("car")
install.packages("caret")
install.packages("caTools")

#Once the following packages installed load the following library
library(car)
library(ggplot2)
library(knitr)
library(data.table)
library(rpart)
library(rpart.plot)
library(dplyr)
library(tidyverse)
library(gridExtra)
library(caTools)
library(caret)

#Reading our data, location can be specified accordingly based on where the data is saved
bank<- read.csv("C:/Users/Adit Doza/Downloads/DSA Data Set.csv", header=T,sep=",")


# Now we start Data Validation Process
#Checking Data Summary
summary(bank)

#Check for Duplicated Rows in our Data,12 should be found
sum(duplicated(bank))

#Check for Missing Data in Rows, no missing data found
sum(!complete.cases(bank))

#Check for Missing Values of Rows in all columns, none found
all.empty = rowSums(is.na(bank))==ncol(bank)
sum(all.empty)

#Check Missing Values in Variable list, none found
sapply(bank, function(x) sum(is.na(x)))

# Now Process Data Cleaning 
# Since no missing values we saved a lot time

# Remove all the repeated rows
bank.clean = bank %>% distinct

#Checking number of Rows, It reduced from 41188 to 41176 so successfully deleted repeated rows 
nrow(bank.clean)

#Check the Summary of the cleaned data
summary(bank.clean)


#Generate a new data frame with the clean data
banks=bank.clean

#Recode String Categorical Variable of y into numeric
banks$y = ifelse(banks$y=='yes',1,0)

#Check the proportion of subscriptions and graph plots
prop.table(table(banks$y))

#Checking subscriptions on barplot
barplot(prop.table(table(banks$y)))

ggplot(banks, aes(x=y)) + 
geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::label_percent(accuracy=0.1L)(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.2) +
  labs(y = "Percent", fill="Term Deposit Subscription") +
  scale_y_continuous(labels = scales::label_percent(accuracy=0.1L))



#Check Structure of dataset again to see if successfully converted
str(banks)

#Get Unique Values for education
as.vector(unique(banks[,4]))

# Recoding Variables of education in shorter form, useful for ggplots later
#NOTE: This recode maynot work if there is conflict with Dplyr 
#sometimes library(car) doesn't work with specific R versions 
#as Rtools need to be installed

banks$education = recode(banks$education, "'basic.4y'='4yrs';
                                    'high.school'='HS';
                                     'basic.6y'='6yrs';
                                     'basic.9y'='9yrs';
                                     'professional.course'='PC';
                                     'unknown'='Unknown';
                                     'university.degree'='Uni';
                                     'illiterate'='IL'")

#check Unique Values for education recoded
as.vector(unique(banks[,4]))

#Frequency table for education
table(banks$education)

#Get Unique Values for Job
as.vector(unique(banks[,2]))

# Recoding Variables of Job in shorter form, useful for ggplots later
# This maynot work if library(car) has conflict with dplyr for some R versions

banks$job = recode(banks$job, "'housemaid'='hs-maid';
                                    'services'='services';
                                     'admin.'='admin';
                                     'blue-collar'='b-collar';
                                     'technician'='tech';
                                     'retired'='retired';
                                     'management'='mgt';
                                     'unemployed'='unemp';
                                      'self-employed'='self-emp';
                                     'unknown'='Unknown';
                                     'entrepreneur'='Ent';
                                     'student'='student'")

#check Unique Values for job recoded
as.vector(unique(banks[,2]))

#Frequency table for job
table(banks$job)

#Save the clean data in our directory
write.csv(banks, file = "C:/Users/Adit Doza/Downloads/banks.csv")

#Check the head and summary of the dataset and nrow and ncol
head(banks)
summary(banks)
nrow(banks)
ncol(banks)     



#Check the Age Distribution of clients
summary(banks$age)

#Histogram of Age Distribution of Clients
ggplot(banks) + geom_histogram(aes(x=age),color="black", fill="burlywood1", binwidth = 5) +
  ggtitle('Age Distribution of Clients') +
  ylab('Number of Clinets') +
  xlab('Age (in years)') +
  geom_vline(aes(xintercept = mean(age), color = "red")) +
  scale_x_continuous(breaks = seq(0,100,5)) +
  theme(legend.position = "none")



#Plotting Age Distribution by Subscription Status 
#(note: I used summarise function as I have Hmisc package installed, otherwise summarize should be used)


#Plotting subscription according to age distribution
  ggplot (bank.clean, aes(x=age)) + 
  geom_bar(color = "burlywood", fill = "burlywood") +
  facet_grid(cols=vars(y)) + 
  ggtitle('Age Distribution by Subscription') + ylab('Count') + xlab('Age(in years)') 

#Plotting subscription according to marital status (Numbers)
    ggplot (bank.clean, aes(x=marital)) + 
    geom_bar(color = "burlywood", fill = "burlywood") +
    facet_grid(cols=vars(y)) + 
    ggtitle('Marital Status by Subscription') + ylab('Count') + xlab('Marital Status') 
    
    # Plotting of Marital Status (Percentages)
    marital<-ggplot(bank.clean, aes(x= marital,  group=y)) + 
      geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
      geom_text(aes( label = scales::label_percent(accuracy=0.1L)(..prop..),
                     y= ..prop.. ), stat= "count", vjust = -.1) +
      labs(y = "Percent", fill="marital status") +
      facet_grid(~y) +
      scale_y_continuous(labels = scales::percent)
##Plotting MArital Status with Vertical Label
    marital + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))    

    #Frequency table for marital
    table(banks$marital)
    #proportion of marital
    prop.table(table(banks$marital))
    
#Plotting according to Loan status (Numbers)
ggplot (bank.clean, aes(x=loan)) + 
  geom_bar(color = "blue", fill = "dodgerblue") +
  facet_grid(cols=vars(y)) + 
  ggtitle('Loan vs Subscription Status') + ylab('Count') + xlab('Loan')


# Plotting of Loan Status (Percentages)
ggplot(bank.clean, aes(x= loan,  group=y)) + 
geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.3) +
  labs(y = "Percent", fill="loan") +
  facet_grid(~y) +
  scale_y_continuous(labels = scales::percent)

#Plotting according to Housing status (Numbers)
ggplot (bank.clean, aes(x=housing)) + 
  geom_bar(color = "blue", fill = "dodgerblue") +
  facet_grid(cols=vars(y)) + 
  ggtitle('Housing Bar Plot') + ylab('Count') + xlab('Housing') 

# Plotting of Housing Status (Percentages)
ggplot(bank.clean, aes(x= housing,  group=y)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.1) +
  labs(y = "Percent", fill="house ownership") +
  facet_grid(~y) +
  scale_y_continuous(labels = scales::percent) +
  ggtitle('House Ownership vs Subscription Status')

#Plotting according to Default status (Numbers)
ggplot (bank.clean, aes(x=default)) + 
  geom_bar(color = "blue", fill = "dodgerblue") +
  facet_grid(cols=vars(y)) + 
  ggtitle('Default Bar Plot') + ylab('Count') + xlab('Default Status') 

# Plotting of Default Status (Percentages)
ggplot(bank.clean, aes(x= default,  group=y)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.2) +
  labs(y = "Percent", fill="default status") +
  facet_grid(~y) +
  scale_y_continuous(labels = scales::percent)

table(banks$default)


# Subscription based on education level(numbers)
ggplot(data = bank.clean, aes(x=education)) +
  geom_bar() +
  facet_grid(cols=vars(y)) +
  ggtitle("Term Deposit Subscription based on Education Level") +
  xlab(" Education Level") +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))

#Education level percentages based on subscription i.e. in tabular form
#(note: I used summarise function as I have Hmisc package installed, otherwise summarize should be used)

banks %>% 
  group_by(education) %>% 
  summarise(pct.yes = mean(y=="1")*100) %>% 
  arrange(desc(pct.yes))

# Subscription based on education level (percentages)
education <-ggplot(bank.clean, aes(x= education, group=y)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::label_percent(accuracy = 1L)(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.1) +
  labs(y = "Percent", fill="education") +
  facet_grid(~y) +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L))

education + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Subscription based on job level(numbers)
ggplot(data = banks, aes(x=job)) +
  geom_bar() +
  facet_grid(cols=vars(y)) +
  ggtitle("Term Deposit Subscription based on Job Level") +
  xlab(" Job Level") +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))

#Job level percentages based on subscription i.e. in tabular form
#(note: I used summarise function as I have Hmisc package installed, otherwise summarize should be used)

banks %>% 
  group_by(job) %>% 
  summarise(pct.yes = mean(y=="1")*100) %>% 
  arrange(desc(pct.yes))

# Subscription based on job level (percentages)
job<-ggplot(bank.clean, aes(x= job, group=y)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::label_percent(accuracy = 1L)(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.1) +
  labs(y = "Percent", fill="job") +
  facet_grid(~y) +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L))

job + theme(axis.text.x = element_text(angle = 90, vjust = 0.1, hjust=1))

#Frequency table for month
table(banks$month)
#proportion of monthly
prop.table(table(banks$month))

# Subscription based on month level(numbers)
ggplot(data = bank.clean, aes(x=month)) +
  geom_bar() +
  facet_grid(cols=vars(y)) +
  ggtitle("Term Deposit Subscription based on Month Level") +
  xlab(" Month Level") +
  guides(fill=guide_legend(title="Subscription of Term Deposit"))

# Subscription based on month level (percentages)
month<-ggplot(bank.clean, aes(x= month, group=y)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::label_percent(accuracy = 1L)(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.3) +
  labs(y = "Percent", fill="month") +
  facet_grid(~y) +
  scale_y_continuous(labels = scales::label_percent(accuracy=1L))

month + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#Subscription based on number of campaign contacts
ggplot(data=bank.clean, aes(x=campaign))+
  geom_histogram(color = "blue", fill = "dodgerblue", binwidth = 5)+
    facet_grid(cols=vars(y)) +
  ggtitle("Subscription based on Number of Contact during the Campaign")+
  xlab("Number of Contact during the Campaign")+
  xlim(c(min=1,max=30)) 

#checking percentages of campaigns
#(note: I used summarise function as I have Hmisc package installed, otherwise summarize should be used)
banks %>% 
  group_by(campaign) %>% 
  summarise(pct.yes = mean(y=="1")*100) %>%
  arrange(desc(pct.yes))


## Finding Range and Summary of Duration
range(banks$duration)
## Our call duration ranges from 0 to 4918 seconds
summary(banks$duration)
## We get 0.0(min) 102.0(1stQ) 180.0(Median) 258.3(Mean) 319.0(3rdQ) 4918.0(max)

#Mean Duration
meandur <- banks %>% group_by(y) %>% summarise(grp2.mean=mean(duration))

#plotting duration according to subscription status without Vline
ggplot(banks, aes(x=duration, fill = y)) +
  geom_histogram(binwidth = 2) +
  facet_grid(cols = vars(y)) +
  coord_cartesian(xlim = c(0,5000), ylim = c(0,400))

#plotting duration according to subscription status with meandur Vline
ggplot(banks, aes(x=duration, fill = y)) +
  geom_histogram(binwidth = 2) +
  facet_grid(cols = vars(y)) +
  coord_cartesian(xlim = c(0,5000), ylim = c(0,400))+
geom_vline(data = meandur, aes(xintercept = grp2.mean), color = "red", linetype = "dashed")

## Evaluate Our Existing Model Performance
## We need to evaluate Sensitivity and Specificity Test

#Checking the Mean Model Prediction First
mean(banks$ModelPrediction)

#Checking the Specificity and Sensitivity of our Existing Model Prediction
tapply(banks$ModelPrediction, INDEX=banks$y, FUN=mean)


## Setting Logistic Model##
# Train/Test Data Split. Using the caTools package to create train/test splits

set.seed(123)
split = sample.split(banks$y, SplitRatio = .7)
train = subset(banks,split == TRUE)
test = subset(banks,split == FALSE)


# scale the numeric variables
train[,c(1,11:14,16:21)] = scale(train[,c(1,11:14,16:21)])
test[,c(1,11:14,16:21)] = scale(test[,c(1,11:14,16:21)])

#Create the logistic regression model

model1 = glm(y ~ ., data = train, family = binomial)
summary(model1)


#Create confusion matrix
threshold=0.50
predicted_values<-ifelse(predict(model1,type="response",test)>threshold,1,0)
actual_values<-test$y
conf_matrix<-table(predicted_values,actual_values)
conf_matrix

#Sensitivity and specificity. Using caret package
sensitivity(conf_matrix)

specificity(conf_matrix)


#Create the decision tree model 

tree <- rpart(y~., method='class' , data= train)


#Create confusion matrix
threshold=0.50
tree_predicted_values<-ifelse(predict(tree,type="prob",test)>threshold,1,0)
tree_actual_values<-test$y
tree_conf_matrix<-table(tree_predicted_values[,2],tree_actual_values)
tree_conf_matrix

#Sensitivity and specificty
sensitivity(tree_conf_matrix)

specificity(tree_conf_matrix)

