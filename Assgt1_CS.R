# File:    Assn1_CS.R
# Course:  IDS572 - Data Mining for Bussiness
# Subject: Assingment 1
# Author:  Christopher Solis, uic.edu, @csolisoc
# Date:    2020-09-13

Dir = dirname(rstudioapi::getSourceEditorContext()$path) # find working directory (where this R script is!)
setwd(Dir) # set your current directory to where your R script is
getwd() # confirm that the new directory is rith

####################################################################################################
# 1. Load packages #################################################################################
# If you didn't download tidyverse, type the follwing command in the console:
# install.packages(c("tidyverse"))
library(ggplot2) # Notice that ggplot2 was downloaded from tidyverse
library(tidyverse)
library(lubridate)

#####################################################################################################
# 2. Read data ######################################################################################
lcdf <- read_csv('lcData5m.csv')
head(lcdf)


#####################################################################################################
# 3. Explore data ###################################################################################
#How does loan status vary by loan grade
lcdf %>% group_by(loan_status, grade) %>% tally()
#or, using table
table(lcdf$loan_status, lcdf$grade)
#   Do you have loans with status other than "Fully Paid" or "Charged Off"?  
#    If so, you should filter these out. For example, if there are some loans with status of "current", 
#       you can filter these out by lcdf <- lcdf %>%  filter(loan_status !="Current")


#How does number of loans, loan amount, interest rate vary by grade
lcdf %>% group_by(grade) %>% tally()
lcdf %>% group_by(grade) %>% summarise(sum(loan_amnt))   #and/or what is the mean loan_amnt by grade?
lcdf %>% group_by(grade) %>% summarise(mean(int_rate))

#Or plot these..
ggplot(lcdf, aes( x = int_rate)) + geom_histogram()
ggplot(lcdf, aes( x = loan_amnt)) + geom_histogram(aes(fill=grade))
ggplot(lcdf, aes( x = loan_amnt)) + geom_histogram() + facet_wrap(~loan_status)

#.....


#Some summarized info on loans by grade
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt))

#calculate the annualized percentage return
lcdf$annRet <- ((lcdf$total_pymnt -lcdf$funded_amnt)/lcdf$funded_amnt)*(12/36)*100

#summarize by grade
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), avgInterest= mean(int_rate), stdInterest=sd(int_rate), avgLoanAMt=mean(loan_amnt), avgPmnt=mean(total_pymnt), avgRet=mean(annRet), stdRet=sd(annRet), minRet=min(annRet), maxRet=max(annRet))


#Some loans are paid back early - find out the actual loan term in months
#  Since last_pymnt_d is a chr variable, we need to covert it to a date var
lcdf$last_pymnt_d<-paste(lcdf$last_pymnt_d, "-01", sep = "")
lcdf$last_pymnt_d<-parse_date_time(lcdf$last_pymnt_d,  "myd")

lcdf$actualTerm <- ifelse(lcdf$loan_status=="Fully Paid", as.duration(lcdf$issue_d  %--% lcdf$last_pymnt_d)/dyears(1), 3)

#Then, considering this actual term, we can calculate the actual annual return 
#lcdf$actualReturn <- 


#For cost-based performance, we want to see the average interest rate, and the average of proportion of loan amount paid back, grouped by loan_status
lcdf%>% group_by(loan_status) %>% summarise(  intRate=mean(int_rate), totRet=mean((total_pymnt-funded_amnt)/funded_amnt)  )
# Notice that the totRet on Charged Off loans as -0.366, so, for every dollar invested, there is a loss of .366 cents.   For Fully Paid loans, the totRet seems less than what may be  expected from intRate -- how do you explain this?


#you may like to look at some of these variables
lcdf %>% select(loan_status, loan_amnt, funded_amnt, total_pymnt, int_rate, actualTerm, actualReturn ) %>% view()

#some more summaries
lcdf %>% group_by(grade) %>% summarise(nLoans=n(), defaults=sum(loan_status=="Charged Off"), defaultRate=defaults/nLoans, ....)



#convert emp_length to factor -- can order the factors in  a meaningful way
lcdf$emp_length <- factor(lcdf$emp_length, levels=c("n/a", "< 1 year","1 year","2 years", "3 years" ,  "4 years",   "5 years",   "6 years",   "7 years" ,  "8 years", "9 years", "10+ years" ))

#Look at loan purpose
lcdf %>% group_by(purpose) %>% tally()
# do you want to recode some categories with very few cases to "other"
lcdf$purpose <- fct_recode(lcdf$purpose, other="wedding", other="educational", other="renewable_energy")


#Note - character variables can cause a problem with some model packages, so better to convert all of these to factors
lcdf= lcdf %>% mutate_if(is.character, as.factor)



#####################################################################################################
# 4. Some derived attributes ########################################################################
#Derived attribute: proportion of satisfactory bankcard accounts 
lcdf$propSatisBankcardAccts <- ifelse(lcdf$num_bc_tl>0, lcdf$num_bc_sats/lcdf$num_bc_tl, 0)

#Another one - lets calculate the length of borrower's history with LC
#  i.e time between earliest_cr_line and issue_d
lcdf$earliest_cr_line<-paste(lcdf$earliest_cr_line, "-01", sep = "")
lcdf$earliest_cr_line<-parse_date_time(lcdf$earliest_cr_line, "myd")
lcdf$issue_d<-parse_date_time(lcdf$issue_d, "myd")

lcdf$borrHistory <- as.numeric(lcdf$issue_d-lcdf$earliest_cr_line)/365
#or we can use the lubridate functions to precidely handle date-times durations
#  lcdf$borrHistory <- as.duration(lcdf$earliest_cr_line %--% lcdf$issue_d  ) / dyears(1)


#Another new attribute: ratio of openAccounts to totalAccounts
#....
#.....
#####################################################################################################

# 5. Drop some variables for potential leakage, others ##############################################
#Drop some other columns which are not useful and those which will cause 'leakage'
lcdf <- lcdf %>% select(-c(fico_range_low, fico_range_high, last_fico_range_low, funded_amnt_inv, term, emp_title, pymnt_plan, ...., hardship_flag, ))

#some additional vars to drop
varsToRemove=c("issue_d", "earliest_cr_line", "application_type")
lcdf <- lcdf %>% select(-varsToRemove)

#.....what are other variables you should drop....



#####################################################################################################
# 5. Missing values ################################################################
#Drop vars with all empty values
lcdf <- lcdf %>% select_if(function(x){!all(is.na(x))})

#missing value proportions in each column
colMeans(is.na(lcdf))
# or, get only those columns where there are missing values
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]

#remove variables which have more than, for example, 60% missing values
nm<-names(lcdf)[colMeans(is.na(lcdf))>0.6]
lcdf <- lcdf %>% select(-nm)


#Impute missing values - first get the columns with missing values
colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]
#summary of data in these columns
nm<- names(lcdf)[colSums(is.na(lcdf))>0]
summary(lcdf[, nm])

#mths_since_last_delinq: has 48% missings, these pertain to no delinquincy, so replace by max value (176) or a value higher than the max (500) -- we will try this out on a temporary dataset lcx with the attributes that have misisng values
lcx<-lcdf[, c(nm)]
colMeans(is.na(lcx))[colMeans(is.na(lcx))>0]
lcx<- lcx %>% replace_na(list(mths_since_last_delinq = 500))
#For revol_util, suppose we want to replace the misisng values by the median
lcx<- lcx %>% replace_na(list(revol_util=median(lcx$revol_util, na.rm=TRUE)))

#Similarly for the other variables
#If we are sure this is working and what we want, can replace the missing values on the lcdf dataset
lcdf<- lcdf %>% replace_na(list(mths_since_last_delinq=500, revol_util=median(lcdf$revol_util, na.rm=TRUE), ...... ))


#####################################################################################################
# 6. Build Models ###################################################################################
library(rpart)

#It can be useful to convert the target variable, loan_status to  a factor variable
lcdf$loan_status <- factor(lcdf$loan_status, levels=c("Fully Paid", "Charged Off"))

#split the data into trn, tst subsets
nr<-nrow(lcdf)
trnIndex<- sample(1:nr, size = round(0.5*nr), replace=FALSE)
lcdfTrn <- lcdf[trnIndex, ]
lcdfTst <- lcdf[-trnIndex, ]


lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "information"), control = rpart.control(minsplit = 30))

lcDT1 <- rpart(loan_status ~., data=lcdfTrn, method="class", parms = list(split = "information"), control = rpart.control(cp=0.0001, minsplit = 50))

#Do we want to prune the tree -- check for performance with dfferent cp levels
printcp(lcDT1)
lcDT1p<- prune.rpart(lcDT1, cp=0.0003)
#####################################################################################################

# 7. Evaluate performance ###########################################################################
predTrn=predict(lcDT1,lcdfTrn, type='class')
table(pred = predTrn, true=lcdfTrn$loan_status)
mean(predTrn == lcdfTrn$loan_status)
table(pred = predict(lcDT1,lcdfTst, type='class'), true=lcdfTst$loan_status)
mean(predict(lcDT1,lcdfTst, type='class') ==lcdfTst$loan_status)

#With a different classsification threshold
CTHRESH=0.3
predProbTrn=predict(lcDT1,lcdfTrn, type='prob')
predTrnCT = ifelse(predProbTrn[, 'Charged Off'] > CTHRESH, 'Charged Off', 'Fully Paid')
table(predTrnCT , true=lcdfTrn$loan_status)
# Or, to set the predTrnCT values as factors, and then get the confusion matrix
table(predictions=factor(predTrnCT, levels=c("Fully Paid", "Charged Off")), actuals=lcdfTrn$loan_status)


#Or you can use the confusionMatrix fuction from the caret package
library(caret)
confusionMatrix(predTrn, lcdfTrn$loan_status)
#if you get an error saying that the 'e1071' package is required, 
# you should install and load that too
#Notice that the output says 
#   'Positive' class: Fully Paid
#So,the confusionMatrix based performance measures are based 
#  on the "Fully Paid" class as the class of interest.
# If you want to get performance measure for "Charged Off", use 
#    the positive- paremeter
confusionMatrix(predTrn, lcdfTrn$loan_status, positive="Charged Off")


#ROC plot
library(ROCR)

score=predict(lcDT1,lcdfTst, type="prob")[,"Charged Off"]
pred=prediction(score, lcdfTst$loan_status, label.ordering = c("Fully Paid", "Charged Off"))
#label.ordering here specifies the 'negative', 'positive' class labels   

#ROC curve
aucPerf <-performance(pred, "tpr", "fpr")
plot(aucPerf)
abline(a=0, b= 1)

#AUC value
aucPerf=performance(pred, "auc")
aucPerf@y.values

#Lift curve
liftPerf <-performance(pred, "lift", "rpp")
plot(liftPerf)



#####################################################################################################
# 7. Performance with profit.loss ###################################################################
#Incorporating profits & costs
PROFITVAL <- 10 #profit (on $100) from accurately identifying Fully_paid loans
COSTVAL <- -50  # loss (on $100) from incorrectly predicting a Charged_Off loan as Full_paid
scoreTst <- predict(lcDT1,lcdfTst, type="prob")[,"Fully Paid"]   
#Note- we want to identify those loans wth high prob for being FullyPaid
prPerf <- data.frame(scoreTst)
prPerf <- cbind(prPerf, status=lcdfTst$loan_status)
prPerf <- prPerf[order(-scoreTst) ,]  #sort in desc order of  prob(fully_paid)
prPerf$profit <- ifelse(prPerf$status == 'Fully Paid', PROFITVAL, COSTVAL)
prPerf$cumProfit <- cumsum(prPerf$profit)

#to compare against the default approach of investing in CD with 2% int (i.e. $6 profit out of $100 in 3 years)
prPerf$cdRet <- 6
prPerf$cumCDRet <- cumsum(prPerf$cdRet)
plot(prPerf$cumProfit)
lines(prPerf$cumCDRet, col='green')

#Or, we really do not need to have the cdRet and cumCDRet columns, since cdRet is $6 for every row
plot(prLifts$cumProfit)
abline(a=0, b=6)

########################################################################################
# 8. Save figures ######################################################################
# save as PDF
ggsave("MyBoxPlot.pdf", device = "pdf",
       width = 12, height = 8,
       units = "cm", dpi = 300) 
# save as TIFF
ggsave("MyBoxPlot.tiff", device = "tiff",
       width = 12, height = 8,
       units = "cm", dpi = 300)

# 9. Clean up ############################################################################
# clear packages
detach("package:ggplot2", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

rm(list = ls()) # removes variables from local environment (variables displayed on Top right quadrant under Environment)

# Clear console
cat("\014")  # ctrl+L

# FIN!