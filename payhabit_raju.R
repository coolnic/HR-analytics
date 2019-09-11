# Set the environment
rm(list = ls(all = T))
options(warn=-1)
in_path = "\\\\ingscsdowf101\\MSC_Rep_Grp\\PUBLIC\\Projects\\Analytics_WIP\\Invoice_Report_Analysis\\Invoice_Report_Analysis\\Loreal\\Pay Habit\\Payhabit Model for All customers\\IN_FILE"
setwd(in_path)
getwd()

## Libraries to install
library(sqldf)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)
library(data.table)
library(imputeMissings)
library(e1071)
library(lubridate)
library(stringr)
library(reshape2)
library(mice)
# install.packages('anytime', dependencies = TRUE, lib= "C:\\Users\\ND56397\\Desktop\\R_Programming\\Packages")
library(anytime)#,lib.loc ="C:\\Users\\ND56397\\Desktop\\R_Programming\\Packages")

## Files are available in the below path
## \\ingscsdowf101\MSC_Rep_Grp\Work In Progress\Development\Data
## Convert the file names from xlsb to csv format
## Change the file names manually below converted files

## Start Manually Change the file names
## loading the data into R
pay_habit_data <- read.csv("Pay Habit details_10th Sept.csv",stringsAsFactors = T,na.strings = c(''," ", "-", NA, "NA"))
#obs=48836
Invoice_dtl_rpt <- read.csv("Invoice details_9th_Sept.csv",stringsAsFactors = T,na.strings = c(''," ", "-", NA, "NA"))
#obs=159883
GIR_REPORT <- read.csv("GIR Detail_09th_Sept.csv",stringsAsFactors = T,na.strings = c(''," ", "-", NA, "NA"))
#obs = 57508
Master_block_data <- read.csv("Master-Blocks_10 th Sep.csv",stringsAsFactors = T,na.strings = c(''," ", "-", NA, "NA"))
#obs = 72538
Exchange_rate <- read.csv("Exchange_rate_3rd_sep_19.csv",stringsAsFactors = T,na.strings = c(''," ", "-", NA, "NA"))
#obs = 1976
payhabit_risk <- read.csv("Pay_habit_riskclass_10th_sep19.csv",stringsAsFactors = T,na.strings = c(''," ", "-", NA, "NA"))
#obs = 182
## End of file names manually


pay_habit_data <- pay_habit_data[which(pay_habit_data$Days.Slow != 'X'),]
#47286
pay_habit_data$Days.Slow <- as.numeric(levels(pay_habit_data$Days.Slow))[pay_habit_data$Days.Slow]
## Renaming the column Names
pay_habit_data <- setnames(pay_habit_data,old=c("AR.Document.Type","Posting.Date..PH.","AR.Document.Num","Profit.Center"), new=c("AR_DOCUMENT_TYPE","Posting_Date__PH_","AR_Document_Num","Profit_Center"))

## Filtering the data based on AR_DOCUMENT_TYPE =ECC/RV
pay_habit_data1 <- sqldf("select * from pay_habit_data  where AR_DOCUMENT_TYPE ='ECC/RV' ") 
# obs 36350

pay_habit_data1$Posting_Date__PH_ <- as.Date(pay_habit_data1$Posting_Date__PH_, format = ("%m/%d/%Y"))
#pay_habit_data1$Posting_Date__PH_ <- as.Date(factor(pay_habit_data1$Posting_Date__PH_), format ="%d-%m-%Y")
# pay_habit_data1$Posting_Date__PH_
pay_habit_data1_2 <- filter(pay_habit_data1,Posting_Date__PH_ >= as.Date("2018-02-01"))
# obs 32565

Invoice_dtl_rpt$Sales_Document_new <- as.character(paste("ECC/0",Invoice_dtl_rpt$Sales.Document, sep = ""))
Invoice_dtl_rpt$Shipment_Number_new <- paste("ECC/00",Invoice_dtl_rpt$Shipment.Number, sep = "")
# unique(Invoice_dtl_rpt$Shipment_Number_new)

# Invoice_dtl_rpt$Shipment_Number_new <- cbind("ECC/00",Invoice_dtl_rpt$Shipment.Number)
Invoice_dtl_rpt2 <- Invoice_dtl_rpt[which(Invoice_dtl_rpt$Payment.Terms..Master..1 != 'Not assigned'),]
#obs=76730
Invoice_dtl_rpt2$AR_Document_Num <- paste("ECC/0",Invoice_dtl_rpt2$Billing.Document,sep = "")
Invoice_dtl_rpt3 <- Invoice_dtl_rpt2[which(substring(Invoice_dtl_rpt2$AR_Document_Num,1,7) == 'ECC/091'),]
#obs= 66412
Invoice_dtl_rpt3_1 <- Invoice_dtl_rpt3[which(Invoice_dtl_rpt3$Shipment.Number != '#'),]
#obs= 62645

GIR_REPORT$Defect_Category <- GIR_REPORT$Defect.Category
GIR_REPORT$Sales_Doc_Num <- as.character(GIR_REPORT$Sales.Doc.Num)
GIR_REPORT$Shipment_Number <- as.character(GIR_REPORT$Shipment.Number)

GIR_REPORT1 <- GIR_REPORT %>% 
                filter(Defect_Category %in% c("No Defect","Late/Early Delivered")) %>% 
                distinct(Sales_Doc_Num, Shipment_Number, .keep_all = TRUE)
#obs = 41295

Invoice_Report_GIR_FLAG <- sqldf("select a.*,b.Defect_Category as GIR_FLAG
                                 from Invoice_dtl_rpt3_1 as a inner join GIR_REPORT1 as b
                                 on a.Sales_Document_new=b.Sales_Doc_Num and 
                                 a.Shipment_Number_new=b.Shipment_Number ")
#obs=57903

Invoice_Report_GIR_FLAG$Billing_Status <- as.factor(Invoice_Report_GIR_FLAG$STAS...Billing.Status..DI.)
Invoice_Report_GIR_FLAG$Profit_Center_1 <- as.factor(Invoice_Report_GIR_FLAG$Profit.Center.1)
Invoice_Report_GIR_FLAG_2 <- sqldf("select distinct AR_Document_Num, Profit_Center_1, Billing_Status, GIR_FLAG 
                                   from Invoice_Report_GIR_FLAG 
                                   order by AR_Document_Num, Profit_Center_1, Billing_Status, GIR_FLAG")
#obs=49347

Invoice_Report_GIR_FLAG_3 <- Invoice_Report_GIR_FLAG_2 %>% distinct(AR_Document_Num, Profit_Center_1, .keep_all = TRUE)
#obs=42987

# write.csv(Invoice_Report_GIR_FLAG_3,"Invoice_Report_GIR_FLAG_3.csv",row.names = F)

pay_habit_data1_3 <- sqldf("select a.*, b.GIR_FLAG 
                           from pay_habit_data1_2 a left join Invoice_Report_GIR_FLAG_3 b 
                           on a.AR_Document_Num=b.AR_Document_Num and a.Profit_Center=Profit_Center_1")
#obs = 32565

pay_habit_data1_4 <- pay_habit_data1_3 %>% filter(GIR_FLAG %in% c("No Defect","Late/Early Delivered"))
#obs =29041

## Dropping the variables with single category
pay_habit_data2 <- subset(pay_habit_data1_4,select = -c(Payer,Company.code,Reason.Code,Business.Group,Chart.of.accounts,Source.System,Chart.of.accounts.1,Management.Group,Calendar.year,Calendar.month,RCU,Quarter,Invoice.Amount,Ship.To.Party,Ship.To.Party,Rule.for.Credit.Check))
#obs =29041

##converting from factor to numeric values 
cols.num <- c("Currency.Days.Granted.USD","Currency.Days.Slow.USD","Currency.Days.Used.USD","Amount.in.USD")
pay_habit_data2[cols.num] <- sapply(pay_habit_data2[cols.num],as.numeric)

##converting from factor to date values 
cols.date <- c("Posting_Date__PH_","Net.Due.Date..PH.","Clearing.Date..PH.")
pay_habit_data2[cols.date] <- sapply(pay_habit_data2[cols.date], as.character.Date, format = '%m/%d/%Y')
#pay_habit_data2[cols.date] <- sapply(pay_habit_data2[cols.date], as.character.Date, format = '%d-%m-%Y')

## Renaming the payer001 to payer,Ship_To_Party_0001 to Ship_To_Party,Sold_to_party_0001 to Sold_to_party,
## Rule_for_Credit_Check_0001 to Rule_for_Credit_Check
pay_habit_data2 <- setnames(pay_habit_data2,old=c("Payer.1","Company.code.1","Reason.Code.1","Ship.To.Party.1","Sold.to.party.1","Rule.for.Credit.Check.1","Geo.Business.Region","Risk.Class","Days.Granted","Amount.in.USD","Performance.Center","Days.Slow","Global.Customer"), new=c("Payer","Company_code","Reason_Code","Ship_To_Party","Sold_to_party","Rule_for_Credit_Check","Geo_Business_Region","Risk_Class","Days_Granted","Amount_in_USD","Performance_Center","Days_Slow","Global_Customer"))

pay_habit_data2$Days_Granted <- as.numeric(pay_habit_data2$Days_Granted)

## Creating a new column EVENT_FLAG
pay_habit_data2$EVENT_FLAG <- ifelse(pay_habit_data2$Days_Slow<=0,0,1)

table(pay_habit_data2$EVENT_FLAG)

## Converting the EVENT_FLAG to factor
pay_habit_data2$EVENT_FLAG <- as.factor(pay_habit_data2$EVENT_FLAG)
pay_habit_data2$GIR_FLAG <- as.factor(pay_habit_data2$GIR_FLAG)

## Event Rate
(table(pay_habit_data2$EVENT_FLAG)[2])/nrow(pay_habit_data2)*100
#43.60%

table(pay_habit_data2$EVENT_FLAG, pay_habit_data2$GIR_FLAG)
#checking is there any missing values in the given data
colSums(is.na(pay_habit_data2))

str(pay_habit_data2)

## Divide the data into training and validation
set.seed(1000)

SplitIndex=createDataPartition(pay_habit_data2$EVENT_FLAG,p=0.8,list=FALSE)
# creating the training dataset
train_data=pay_habit_data2[SplitIndex,]
#validation dataset
valid_data=pay_habit_data2[-SplitIndex,]

# Building model on training dataset

rf.model.train<- randomForest(as.factor(EVENT_FLAG) ~ -1+Company_code+Geo_Business_Region+Risk_Class+Rule_for_Credit_Check+Days_Granted+Amount_in_USD+GIR_FLAG, ntree=200,mtry=(ncol(train_data)/3), data=train_data)

# checking the summary of the model
summary(rf.model.train)

# Variable importance
rf.model.train$importance

# creating the confusion matrix on train data
conf.train <- rf.model.train$confusion
conf.train
#Measuring the accuracy on Train Data
predicted_y<-predict(rf.model.train)
table(predicted_y)
confusionMatrix(predicted_y,train_data$EVENT_FLAG)
#Prediciting the probablity values for Train Data
prob_rf_train = predict(rf.model.train,train_data, type="prob")
#Adding the probability Scores to Train Data
train_data <- cbind(train_data,prob_rf_train)

#Measuring Accuracy on Validation data
predicted_test_rf<-predict(rf.model.train,valid_data, type="response")
# creating the confusion matrix on test data
confusionMatrix(predicted_test_rf,valid_data$EVENT_FLAG)
#Prediciting the probablity values for Train Data
prob_rf_test = predict(rf.model.train,valid_data, type="prob")
#Adding the probability Scores to Test Data
valid_data <- cbind(valid_data,prob_rf_test)

####################################################################################
####################### Model validation with invoice report #######################
####################################################################################


# Exclude the records which are there in payhabit report based on AR_Document_Num and Profit_center.

Invoice_Report_GIR_FLAG_4 <- Invoice_Report_GIR_FLAG %>% filter(GIR_FLAG %in% c("No Defect","Late/Early Delivered"))
#obs=57903
Invoice_Report_GIR_FLAG_4_1<- unique(Invoice_Report_GIR_FLAG_4,by=c("AR_Document_Num", "Profit_Center_1"))
#obs=57903
# Invoice_dtl_rpt4 <- Invoice_Report_GIR_FLAG_4_1[!((Invoice_Report_GIR_FLAG_4_1$AR_Document_Num %in% pay_habit_data1_4$AR_Document_Num)&(Invoice_Report_GIR_FLAG_4_1$Profit.Center.1 %in% pay_habit_data1_4$Profit_Center)),]

Invoice_Report_GIR_FLAG_4_1$Profit_Center <- Invoice_Report_GIR_FLAG_4_1$Profit.Center.1
#obs=57903
Invoice_dtl_rpt4 <- sqldf("select a.* from Invoice_Report_GIR_FLAG_4_1 a left outer join pay_habit_data1_4 b
                          on a.AR_Document_Num =b.AR_Document_Num and a.Profit_Center =b.Profit_Center
                          where b.AR_Document_Num is NULL or b.Profit_Center is NULL")
#obs=18945

# selecting the records with $ only for Net.Value.of.Billing.Item

Invoice_dtl_rpt4$Net_Value_of_Billing_Item = Invoice_dtl_rpt4$Net.Value.of.Billing.Item
Invoice_dtl_rpt4$Net.Value.of.Billing.Item<- NULL

Invoice_dtl_rpt5 <- sqldf("select * from Invoice_dtl_rpt4 where Net_Value_of_Billing_Item like '%$%' ")
#obs=9589


##write.csv(Invoice_dtl_rpt5,"Invoice_dtl_rpt5.csv",row.names = F)
Invoice_dtl_rpt5_1 <- sqldf("select * from Invoice_dtl_rpt5 where Net_Value_of_Billing_Item like '($%' ")
#obs=23
Invoice_dtl_rpt5_1$Net_Value_of_Billing_Item <- -1*as.numeric(gsub("[($),]", "", Invoice_dtl_rpt5_1$Net_Value_of_Billing_Item))
Invoice_dtl_rpt5_1$Tax.Amount <-  -1*as.numeric(gsub("[($),]", "", Invoice_dtl_rpt5_1$Tax.Amount))
Invoice_dtl_rpt5_2 <- sqldf("select * from Invoice_dtl_rpt5 where Net_Value_of_Billing_Item like '$%' ")
#obs=9566
Invoice_dtl_rpt5_2$Net_Value_of_Billing_Item <- as.numeric(gsub("[$,]", "", Invoice_dtl_rpt5_2$Net_Value_of_Billing_Item))
Invoice_dtl_rpt5_2$Tax.Amount <-  as.numeric(gsub("[$,]", "", Invoice_dtl_rpt5_2$Tax.Amount))

Invoice_dtl_rpt5 <-  rbind(Invoice_dtl_rpt5_1,Invoice_dtl_rpt5_2)
#obs=9589
Invoice_dtl_rpt5$currency <- "USD"

## selecting the records not equal to $ value for Net.Value.of.Billing.Item 
Invoice_dtl_rpt6 <- sqldf("select * from Invoice_dtl_rpt4 where Net_Value_of_Billing_Item not like '%$%' ")
#obs=9356
Invoice_dtl_rpt6$currency <- gsub("[^[:alpha:]]", "", Invoice_dtl_rpt6$Net_Value_of_Billing_Item)

Invoice_dtl_rpt6$Net_Value_of_Billing_Item <- as.numeric(Invoice_dtl_rpt6$Net_Value_of_Billing_Item)
Invoice_dtl_rpt6$Tax.Amount <-as.numeric(gsub("[^[:digit:].]", "",Invoice_dtl_rpt6$Tax.Amount))
# Merging the dataframes Invoice_dtl_rpt5 & Invoice_dtl_rpt6
Invoice_dtl_rpt7 <- rbind(Invoice_dtl_rpt5,Invoice_dtl_rpt6)
#obs=18945

# Changing the currency code from JPYC to JPY
Invoice_dtl_rpt7$currency <- ifelse(!is.na(Invoice_dtl_rpt7$currency) & Invoice_dtl_rpt7$currency == "JPYC","JPY",Invoice_dtl_rpt7$currency)
Invoice_dtl_rpt7$currency <- ifelse(Invoice_dtl_rpt7$currency == "Â","GBP",Invoice_dtl_rpt7$currency)

# Extracting the billing month from the dataframe
Invoice_dtl_rpt7$Dt_Billing_Date <- as.Date(Invoice_dtl_rpt7$Dt....BH..Billing.Date,format='%m/%d/%Y')
#Invoice_dtl_rpt7$Dt_Billing_Date <- as.Date(Invoice_dtl_rpt7$Dt....BH..Billing.Date,format='%d-%m-%Y')
Invoice_dtl_rpt7$Bill_Month <- format(Invoice_dtl_rpt7$Dt_Billing_Date,'%b')
Invoice_dtl_rpt7$Bill_Year <- format(Invoice_dtl_rpt7$Dt_Billing_Date,'%y')
Invoice_dtl_rpt7$currency_code <- toupper(paste(Invoice_dtl_rpt7$currency,Invoice_dtl_rpt7$Bill_Month,Invoice_dtl_rpt7$Bill_Year,sep='_'))

#Adding Net.Value.of.Billing.Item and Tax.Amount
Invoice_dtl_rpt7$Gross_Value <- Invoice_dtl_rpt7$Net_Value_of_Billing_Item+Invoice_dtl_rpt7$Tax.Amount

## merging the data with exchange rate 
Invoice_dtl_rpt7$exchange_rate <- Exchange_rate$EXCHANGE_RATE[match(Invoice_dtl_rpt7$currency_code,Exchange_rate$KEY)]
Invoice_dtl_rpt7$exchange_rate <- as.numeric(gsub("[^[:digit:].]", "",Invoice_dtl_rpt7$exchange_rate))

## Calculate Total Amount(GC) =NetValue+Tax.Amount/exchange_rate
Invoice_dtl_rpt7$Total_Amount_GC = (Invoice_dtl_rpt7$Net_Value_of_Billing_Item+Invoice_dtl_rpt7$Tax.Amount)/Invoice_dtl_rpt7$exchange_rate

## Calculate net due_date by using payment_terms from  billing date 
Invoice_dtl_rpt7$Dt....BH..Billing.Date <- as.Date(Invoice_dtl_rpt7$Dt....BH..Billing.Date,format = ("%m/%d/%Y"))
#Invoice_dtl_rpt7$Dt....BH..Billing.Date <- as.Date(Invoice_dtl_rpt7$Dt....BH..Billing.Date,format = ("%d-%m-%Y"))
#Invoice_dtl_rpt7$net_due_date <- as.Date(NA)

for (row in 1:nrow(Invoice_dtl_rpt7)){
  Invoice_dtl_rpt7[row,'net_due_date'] <- 
    if (Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "NET 30 DAYS FROM INVOICE DATE"){
      Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date']+30} else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "60 DAYS AFTER INVOICE DATE"){
        Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date']+60}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "NET 50 DAYS FROM INVOICE DATE"){
          Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date']+50}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "NET 28 DAYS FROM INVOICE DATE"){
            Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date']+28}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "3% 20 DAYS, 2% 30 DAYS NET 60 DAYS FROM"){
              Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date']+60}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "INV 1-15 DUE 25 INV 16-31 DUE 25TH MONTH"){
                ceiling_date(Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date'],"month")-days(1)+25}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "NET 45 DAYS FROM INVOICE DATE"){
                  Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date']+45}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "CASH IN ADVANCE NET"){
                    Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date']+0}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "CASH WITH ORDER"){
                      Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date']+0}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "NET 30 DAYS EOM"){
                        ceiling_date(Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date'],"month")-days(1)+30}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "NET 60 DAYS EOM"){
                          ceiling_date(Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date'],"month")-days(1)+60}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "NET 45 DAYS EOM"){
                            ceiling_date(Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date'],"month")-days(1)+45}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "NET EOM 45TH FLWG"){
                              ceiling_date(Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date'],"month")-days(1)+45}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "30 DAYS FROM B/L DATE - DRAFT"){
                                Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date']+30}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "NET 5TH DAY OF 2ND MONTH FLWG"){
                                  ceiling_date(Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date'],"month")-days(1)+15}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "2% EOM 15TH FLWG NET 30 DYS EOM 5TH FLG"){
                                    ceiling_date(Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date'],"month")-days(1)+15}else if(Invoice_dtl_rpt7[row,'Payment.Terms..Master..1'] == "2% EOM 15TH FLWG"){
                                      ceiling_date(Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date'],"month")-days(1)+15}else{Invoice_dtl_rpt7[row,'Dt....BH..Billing.Date']+0} 
}

# write.csv(Invoice_dtl_rpt7,"Invoice_dtl_rpt7.csv",row.names = F)

# str(Invoice_dtl_rpt7)
Invoice_dtl_rpt7$net_due_date_1<-as.Date(Invoice_dtl_rpt7$net_due_date, origin = "1970-01-01")
Invoice_dtl_rpt7$net_due_date <- NULL
Invoice_dtl_rpt7$net_due_date <- Invoice_dtl_rpt7$net_due_date_1

# write.csv(Invoice_dtl_rpt7,"Invoice_dtl_rpt7.csv",row.names = F)

# Invoice_dtl_rpt7_1 <- read.csv("\\\\ingscsdowf101\\MSC_Rep_Grp\\PUBLIC\\Projects\\Analytics_WIP\\Invoice_Report_Analysis\\Invoice_Report_Analysis\\Loreal\\Pay Habit\\R_AUTOMATION\\Invoice_dtl_rpt7_1.csv",stringsAsFactors = T,na.strings = c(''," ", "-", NA, "NA"))
# Invoice_dtl_rpt7$Dt_Billing_Date <- as.Date(Invoice_dtl_rpt7$Dt....BH..Billing.Date,format='%m/%d/%Y')
Invoice_dtl_rpt7$Dt_Billing_Date <- as.Date(Invoice_dtl_rpt7$Dt....BH..Billing.Date,format='%d-%m-%Y')
## Calculating the Days granted
Invoice_dtl_rpt7$Days_Granted <- as.numeric(Invoice_dtl_rpt7$net_due_date - Invoice_dtl_rpt7$Dt_Billing_Date)

## Merging the invoice data with payhabit_risk
Invoice_dtl_rpt8 <- merge(x=Invoice_dtl_rpt7,y=payhabit_risk,by.x="Payer.1",by.y="Payer")
#obs=14117

## Changing the date  from factor to date
Master_block_data$Dt....OH..Created.On <- as.Date(Master_block_data$Dt....OH..Created.On,format=("%m/%d/%Y"))
#Master_block_data$Dt....OH..Created.On <- as.Date(Master_block_data$Dt....OH..Created.On,format=("%d-%m-%Y"))
#obs=57779

## Filtering the master block data
Master_block_data_2 <- filter(Master_block_data, 
                              Global.Customer %in% c(418057,1646489,1646505,1653922,1647558,1670430,1753368,2065933,2192366,2214147,2181042,4329,169449,2086205,2259680,2049053 ), 
                              Dt....OH..Created.On >= as.Date("2018-02-01"), 
                              STAS...Delivery.Status..OI..1 %in% c("Completely processed"), 
                              STAS...Order.Status.1 %in% c("INVOICED"), 
                              Order.Type %in% c("Standard Order"))
#obs=41244



# merging the invoice data with payhabit data
Master_block_data_2$Sales_Document <- as.factor(Master_block_data_2$Sales.Document)
Master_block_data_3 <- sqldf("select distinct Sales_Document from Master_block_data_2" )
#obs=32726

Master_block_data_3$flag_3 <- 1

## merging the invoice data with Master Block data
Invoice_dtl_rpt9 <- left_join(Invoice_dtl_rpt8,Master_block_data_3 ,by = c("Sales.Document" = "Sales_Document"))
#obs=20081

Invoice_dtl_rpt10 <- Invoice_dtl_rpt9 %>% filter(flag_3==1)
#obs=16312
######


# Renaming the column names
Invoice_dtl_rpt10 <- setnames(Invoice_dtl_rpt10,old=c("Company.Code.1","Performance.Center","Ship.To...Region","Total_Amount_GC","Risk.Class"), new=c("Company_code","Performance_Center","Geo_Business_Region","Amount_in_USD","Risk_Class"))

Invoice_dtl_rpt10$Rule_for_Credit_Check <- NULL
Invoice_dtl_rpt10$Rule_for_Credit_Check <- Invoice_dtl_rpt10$Rule.for.Credit.Check.1

Invoice_dtl_rpt11 <- Invoice_dtl_rpt10 %>% select (Company_code,Performance_Center,Geo_Business_Region,Risk_Class,Rule_for_Credit_Check,Days_Granted,Amount_in_USD,GIR_FLAG)
#obs=13818
# Invoice_dtl_rpt11 <- Invoice_dtl_rpt10 %>% select (Company_code)
Invoice_dtl_rpt11$TEST_FLAG <-"OOS_VALIDATION"

Invoice_dtl_rpt11$GIR_FLAG_1 <- as.factor(Invoice_dtl_rpt11$GIR_FLAG)
Invoice_dtl_rpt11$GIR_FLAG <- NULL
Invoice_dtl_rpt11$GIR_FLAG <- Invoice_dtl_rpt11$GIR_FLAG_1
Invoice_dtl_rpt11$GIR_FLAG_1 <- NULL

pay_habit_data2_1 <- pay_habit_data2 %>% select (Company_code,Performance_Center,Geo_Business_Region,Risk_Class,Rule_for_Credit_Check,Days_Granted,Amount_in_USD,GIR_FLAG)
#obs=26309
# pay_habit_data2_1 <- pay_habit_data2 %>% select (Company_code)
pay_habit_data2_1$TEST_FLAG <-"TRAINING_DATA"


## Checking unique values for Performance_Center in Invoice_dtl_rpt11 dataframe
inv_comp <-  sqldf("select distinct company_code from Invoice_dtl_rpt11 ")
#obs=36
pay_comp <- sqldf("select distinct Company_code from pay_habit_data2_1" )
#obs=39

## Merging the dataframes
inv_company_merge <- sqldf("select  a.Company_code,b.Company_code as pay_Company_code
                           from inv_comp a left join pay_comp b 
                           on a.company_code=b.Company_code ")
#obs=36

## Merging the dataframes
Invoice_dtl_rpt_data11_1 <- sqldf("select a.* ,b.pay_Company_code
                                  from Invoice_dtl_rpt11 a left join inv_company_merge b 
                                  on a.company_code=b.Company_code ")
#obs=11153

Invoice_dtl_rpt_data11_1$Company_code <- NULL
Invoice_dtl_rpt_data11_1$Company_code <- Invoice_dtl_rpt_data11_1$pay_Company_code
Invoice_dtl_rpt_data11_1$pay_Company_code <- NULL

sum(is.na(Invoice_dtl_rpt_data11_1$Company_code))
#obs=20

inv_perf <-  sqldf("select distinct Performance_Center from Invoice_dtl_rpt11 ")
#obs=27
pay_perf <- sqldf("select distinct Performance_Center from pay_habit_data2_1" )
#obs=30

## Merging the dataframes
inv_perf_merge <- sqldf("select a.Performance_Center,b.Performance_Center  as pay_Performance_Center
                        from inv_perf a left join pay_perf b 
                        on a.Performance_Center=b.Performance_Center ")
#obs=27
## Merging the dataframes
Invoice_dtl_rpt_data11_2 <- sqldf("select a.* ,b.pay_Performance_Center
                                  from Invoice_dtl_rpt_data11_1 a left join inv_perf_merge b 
                                  on a.Performance_Center=b.Performance_Center ")
#obs=11153
Invoice_dtl_rpt_data11_2$Performance_Center <- NULL
Invoice_dtl_rpt_data11_2$Performance_Center <- Invoice_dtl_rpt_data11_2$pay_Performance_Center
Invoice_dtl_rpt_data11_2$pay_Performance_Center <- NULL

inv_geo <-  sqldf("select distinct Geo_Business_Region from Invoice_dtl_rpt11 ")
#obs=26
pay_geo <- sqldf("select distinct Geo_Business_Region from pay_habit_data2_1" )
#obs=26

## Merging the dataframes
inv_geo_merge <- sqldf("select a.Geo_Business_Region,b.Geo_Business_Region as pay_Geo_Business_Region
                       from inv_geo a left join pay_geo  b 
                       on a.Geo_Business_Region=b.Geo_Business_Region ")
#obs=26
## Merging the dataframes
Invoice_dtl_rpt_data11_3 <- sqldf("select a.* ,b.pay_Geo_Business_Region
                                  from Invoice_dtl_rpt_data11_2 a left join inv_geo_merge b 
                                  on a.Geo_Business_Region=b.Geo_Business_Region ")
#obs=11153

Invoice_dtl_rpt_data11_3$Geo_Business_Region <- NULL
Invoice_dtl_rpt_data11_3$Geo_Business_Region <- Invoice_dtl_rpt_data11_3$pay_Geo_Business_Region
Invoice_dtl_rpt_data11_3$pay_Geo_Business_Region <- NULL


inv_risk <-  sqldf("select distinct Risk_Class from Invoice_dtl_rpt11 ")
#obs=4
pay_risk <- sqldf("select distinct Risk_Class from pay_habit_data2_1" )
#obs=5

## Merging the dataframes
inv_risk_merge <- sqldf("select a.Risk_Class,b.Risk_Class as pay_Risk_Class
                        from inv_risk a left join pay_risk b  
                        on a.Risk_Class=b.Risk_Class ")
#obs=4
## Merging the dataframes
Invoice_dtl_rpt_data11_4 <- sqldf("select a.* ,b.pay_Risk_Class
                                  from Invoice_dtl_rpt_data11_3 a left join inv_risk_merge b 
                                  on a.Risk_Class=b.Risk_Class")
#obs=111533

Invoice_dtl_rpt_data11_4$Risk_Class <- NULL
Invoice_dtl_rpt_data11_4$Risk_Class <- Invoice_dtl_rpt_data11_4$pay_Risk_Class
Invoice_dtl_rpt_data11_4$pay_Risk_Class <- NULL

inv_Rule_for_Credit <-  sqldf("select distinct Rule_for_Credit_Check from Invoice_dtl_rpt11 ")
#obs=18
pay_Rule_for_Credit <- sqldf("select distinct Rule_for_Credit_Check from pay_habit_data2_1" )
#obs=19

## Merging the dataframes
inv_Rule_for_Credit_merge <- sqldf("select a.Rule_for_Credit_Check,b.Rule_for_Credit_Check as pay_Rule_for_Credit
                                   from inv_Rule_for_Credit a left join pay_Rule_for_Credit b 
                                   on a.Rule_for_Credit_Check=b.Rule_for_Credit_Check ")
#obs=18

## Merging the dataframes
Invoice_dtl_rpt_data11_5 <- sqldf("select a.* ,b.pay_Rule_for_Credit
                                  from Invoice_dtl_rpt_data11_4 a left join inv_Rule_for_Credit_merge b 
                                  on a.Rule_for_Credit_Check=b.Rule_for_Credit_Check")
#obs=111533

Invoice_dtl_rpt_data11_5$Rule_for_Credit_Check <- NULL
Invoice_dtl_rpt_data11_5$Rule_for_Credit_Check <- Invoice_dtl_rpt_data11_5$pay_Rule_for_Credit
Invoice_dtl_rpt_data11_5$pay_Rule_for_Credit <- NULL

inv_GIR_FLAG <-  sqldf("select distinct GIR_FLAG from Invoice_dtl_rpt11 ")
#obs=2
pay_GIR_FLAG <- sqldf("select distinct GIR_FLAG from pay_habit_data2_1" )
#obs=2

## Merging the dataframes
inv_GIR_FLAG_merge <- sqldf("select a.GIR_FLAG,b.GIR_FLAG as pay_GIR_FLAG
                            from inv_GIR_FLAG a left join pay_GIR_FLAG b 
                            on a.GIR_FLAG=b.GIR_FLAG ")
#obs=2

## Merging the dataframes
Invoice_dtl_rpt_data11_6 <- sqldf("select a.* ,b.pay_GIR_FLAG
                                  from Invoice_dtl_rpt_data11_5 a left join inv_GIR_FLAG_merge b 
                                  on a.GIR_FLAG=b.GIR_FLAG")
#obs=11153
Invoice_dtl_rpt_data11_6$GIR_FLAG <- NULL
Invoice_dtl_rpt_data11_6$GIR_FLAG <- Invoice_dtl_rpt_data11_6$pay_GIR_FLAG
Invoice_dtl_rpt_data11_6$pay_GIR_FLAG <- NULL

Invoice_dtl_rpt12 <- Invoice_dtl_rpt_data11_6 %>% select (Company_code,Performance_Center,Geo_Business_Region,Risk_Class,Rule_for_Credit_Check,Amount_in_USD,Days_Granted,GIR_FLAG,TEST_FLAG)

#obs= 11153

# write.csv(Invoice_dtl_rpt12,"Invoice_detail_report_With_prediction.csv",row.names = F)

# Invoice_dtl_rpt12 <- read.csv("Invoice_detail_report_With_prediction.csv",stringsAsFactors = T,na.strings = c(''," ", "-", NA, "NA"))


## Checking for missing values for each column
colSums(is.na(Invoice_dtl_rpt12)) 




Invoice_dtl_rpt12$Company_code<- as.factor(Invoice_dtl_rpt12$Company_code)
Invoice_dtl_rpt12$Geo_Business_Region<- as.factor(Invoice_dtl_rpt12$Geo_Business_Region)
Invoice_dtl_rpt12$Performance_Center<- as.factor(Invoice_dtl_rpt12$Performance_Center)
Invoice_dtl_rpt12$Rule_for_Credit_Check<- as.factor(Invoice_dtl_rpt12$Rule_for_Credit_Check)
Invoice_dtl_rpt12$Amount_in_USD <- as.factor(Invoice_dtl_rpt12$Amount_in_USD)


## Imputing the mising values 

levels(Invoice_dtl_rpt12$Performance_Center) -> levels(pay_habit_data2_1$Performance_Center)


##library(mice)
##library(mice,lib.loc ="C:\\Users\\ND56397\\Desktop\\R_Programming\\Packages")
##mice_imputes = mice(Invoice_dtl_rpt12, m=5, maxit = 40)
##mice_imputes$method
##Invoice_dtl_rpt12=mice::complete(mice_imputes,5)

Invoice_dtl_rpt12<- data.frame(impute(Invoice_dtl_rpt12))

## Merging the dataframe inv_pay_merge with All_Data
ALL_DATA <-rbind(pay_habit_data2_1,Invoice_dtl_rpt12)
#obs=29112

#Measuring Accuracy on Validation data
predicted_test_rf_1<-predict(rf.model.train,ALL_DATA)
# creating the confusion matrix on test data

table(predicted_test_rf_1)

ALL_DATA_validation <- ALL_DATA %>% filter(TEST_FLAG %in% c("OOS_VALIDATION"))

#Prediciting the probablity values for Train Data
prob_rf_test_1 = predict(rf.model.train,ALL_DATA_validation, type="prob")
#Adding the probability Scores to Test Data
ALL_DATA_validation_1 <- cbind(ALL_DATA_validation,prob_rf_test_1)
##View(ALL_DATA_validation_1)

table(ALL_DATA_validation_1$TEST_FLAG)

######
ALL_DATA_validation_2 <- ALL_DATA_validation_1 %>% select ("1") %>%
  rename(Predicted_Probability="1") %>% 
  mutate(Prediction_Flag=ifelse(Predicted_Probability>.5, "Delay_Payment","Early/Ontime Payment"))

Invoice_dtl_rpt13 <- cbind(Invoice_dtl_rpt10,ALL_DATA_validation_2)
# View(Invoice_dtl_rpt13)

## Replacing the NA values to Null
Invoice_dtl_rpt13[is.na(Invoice_dtl_rpt13)] <-" "

## Filtering the net_due_date.It should be greater than current date
Invoice_dtl_rpt13 <- Invoice_dtl_rpt13 %>% filter(Invoice_dtl_rpt13$net_due_date >= Sys.Date()+1 ) 

## Changing the directory to outfile
out_path = "\\\\ingscsdowf101\\MSC_Rep_Grp\\PUBLIC\\Projects\\Analytics_WIP\\Invoice_Report_Analysis\\Invoice_Report_Analysis\\Loreal\\Pay Habit\\Payhabit Model for All customers\\OUT_FILE"
setwd(out_path)

## Exporting the csv file 
write.csv(Invoice_dtl_rpt13,"Invoice_detail_report_With_prediction_10th_sep_19.csv",row.names = F)
  
