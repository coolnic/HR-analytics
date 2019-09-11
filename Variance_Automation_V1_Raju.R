
rm(list = ls(all = T))


## Loading the libraries 
library(sqldf)
library(dplyr)
library(data.table)
library(lubridate)

path = "\\\\ingscsdowf101\\msc_rep_grp\\PUBLIC\\Projects\\Analytics_WIP\\Invoice_Report_Analysis\\Invoice_Report_Analysis\\Loreal\\Ad-Hoc Analysis\\Variance_Bosch\\Variance_in_files"
setwd(path)
getwd()

GIR_DATA <- read.csv("GIR Detail _09th_Sep_without biz.csv")

## Filtering the data with #
GIR_DATA1 <- GIR_DATA[which(GIR_DATA$Dt...Latest.Cust.Req.Date != '#'),]

## Renaming the column Names
GIR_DATA1 <- setnames(GIR_DATA1,old=c("Business.Group","Global.Customer","Global.Customer.1","GMID","GMID.1","Dt...Latest.Cust.Req.Date","Dt...Delivery.Date..D.","OTC5e...Delivery.Item.Net.Weight"), new=c("Business_Group","Global_Customer","Global_Customer_Name","GMID","GMID_Name","Dt_Latest_Cust_Req_Date","Dt_Delivery_Date","Qty_Delivered"))

## Selecting required columns from the dataframe
GIR_DATA1_1 <- sqldf(" select Business_Group,Global_Customer,Global_Customer_Name,GMID,GMID_Name,Dt_Latest_Cust_Req_Date,Dt_Delivery_Date,Qty_Delivered from GIR_DATA1")

GIR_DATA1_1$Qty_Delivered <-  as.numeric(gsub("[KG,]", "", GIR_DATA1_1$Qty_Delivered))

## Filtering the data based on Qty_Delivered >=0
GIR_DATA1_1 <- GIR_DATA1_1[which(GIR_DATA1_1$Qty_Delivered >= 0),]

## Creating a new variable Global_Customer_Name
GIR_DATA1_1<- GIR_DATA1_1 %>% mutate(Global_Customer_New = ifelse(Global_Customer_Name %in% c("L'OREAL","L'OREAL CORP ACCT"), "L'OREAL", 
                                                                  ifelse(Global_Customer_Name %in% c("HENKEL & KGAA","INTER HARZ CORP ACCT","HENKEL ACCT"),"HENKEL",
                                                                         ifelse(Global_Customer_Name %in% c("KRAYDEN","BSH BOSCH & SIEMENS","DONG A CIS ACCT","BOSCH ENGINEERING CENTER"),"BOSCH",
                                                                                ifelse(Global_Customer_Name %in% c("UNILEVER NV","UNILEVER PLC"),"UNILEVER",
                                                                                       ifelse(Global_Customer_Name %in% c("PROCTER & GAMBLE","PROCTER & GAMBLE PHARMACEUTICA"),"PROCTER & GAMBLE",
                                                                                            ifelse(Global_Customer_Name %in% ("STAHL GROUP SA"),"STAHL","Global_Customer_Name")))))))                                                   

unique(GIR_DATA1_1$Global_Customer_New)

GIR_DATA1_1 <- GIR_DATA1_1[which(GIR_DATA1_1$Global_Customer_New != 'Global_Customer_Name'),]
## Changing the date format
GIR_DATA1_1$Dt_Latest_Cust_Req_Date <- as.Date(GIR_DATA1_1$Dt_Latest_Cust_Req_Date, format = ("%m/%d/%Y"))
#GIR_DATA1_1$Dt_Latest_Cust_Req_Date <- as.Date(GIR_DATA1_1$Dt_Latest_Cust_Req_Date, format = ("%d-%m-%Y"))
GIR_DATA1_1$Dt_Delivery_Date <- as.Date(GIR_DATA1_1$Dt_Delivery_Date, format = ("%m/%d/%Y"))
#GIR_DATA1_1$Dt_Delivery_Date <- as.Date(GIR_DATA1_1$Dt_Delivery_Date, format = ("%d-%m-%Y"))

## Creating a New Variable Precc or CC Logic
GIR_DATA1_1<- GIR_DATA1_1 %>% mutate(Pre_cc = ifelse(Dt_Latest_Cust_Req_Date > "2018-10-31","Post_cc","Pre_cc" ))


## Reading the data for critical GMID 
Critical_Gmid <- read.csv("Critical GMID list_May 2019.csv")

## rename Global Customer Name
setnames(Critical_Gmid,old="Global.Customer.New", new = "Global_Customer_New")

## Joining with critical gmid 
GIR_DATA1_2 <- left_join(GIR_DATA1_1, Critical_Gmid, by = c('GMID','Global_Customer_New'),all.x=TRUE)

GIR_DATA1_2$Critical_GMID_Flag_1 <- ifelse(is.na(GIR_DATA1_2$Critical_GMID_FLAG),"No", "Yes")

## Removing the Critical_GMID_Flag
GIR_DATA1_2$Critical_GMID_FLAG <- NULL
names(GIR_DATA1_2)[names(GIR_DATA1_2) == 'Critical_GMID_Flag_1'] <- 'Critical_GMID_Flag'

## creating new variable Diff CRDD-Dow Confirmed    
GIR_DATA1_2$Diff_CRDD_Dow_Confirmed = as.numeric(GIR_DATA1_2$Dt_Latest_Cust_Req_Date - GIR_DATA1_2$Dt_Delivery_Date)

## Arranging the data in ascending order
GIR_DATA1_3 <- GIR_DATA1_2[order(GIR_DATA1_2$'Diff_CRDD_Dow_Confirmed'),] 

## creating new variable CRDD_Dow Confirmed_Logic based on early and late
GIR_DATA1_3<- GIR_DATA1_3 %>% mutate(CRDD_Dow_Confirmed_Logic = ifelse(Dt_Latest_Cust_Req_Date < Dt_Delivery_Date,"Late",
                                                                       ifelse(Dt_Latest_Cust_Req_Date == Dt_Delivery_Date,"Ontime","Early")))

GIR_DATA1_4 <- sqldf("select * from GIR_DATA1_3 where CRDD_Dow_Confirmed_Logic='Early' and Critical_GMID_Flag ='Yes' ")


quantile(GIR_DATA1_4$'Diff_CRDD_Dow_Confirmed',probs = c(0,0.25, 0.5, 0.75,1))

GIR_DATA1_4$Quartile<-ifelse(GIR_DATA1_4$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_4$`Diff_CRDD_Dow_Confirmed`,c(0.25)),'Q1',
                             ifelse(GIR_DATA1_4$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_4$`Diff_CRDD_Dow_Confirmed`,c(0.5)),'Q2',
                                    ifelse(GIR_DATA1_4$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_4$`Diff_CRDD_Dow_Confirmed`,c(0.75)),'Q3',
                                           ifelse(GIR_DATA1_4$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_4$`Diff_CRDD_Dow_Confirmed`,c(1)),'Q4',NA))))

## Arranging the variables in order as per BI Report Late Customers
GIR_DATA2 <- sqldf("select Business_Group,Global_Customer,Global_Customer_Name,Global_Customer_New,GMID,GMID_Name,Dt_Latest_Cust_Req_Date,Pre_cc,Dt_Delivery_Date,CRDD_Dow_Confirmed_Logic,Diff_CRDD_Dow_Confirmed,Qty_Delivered,Quartile,Critical_GMID_Flag from GIR_DATA1_4 where CRDD_Dow_Confirmed_Logic='Early' ")

GIR_DATA3 <- setnames(GIR_DATA2,old=c("Business_Group","Global_Customer","Global_Customer_Name","Global_Customer_New","GMID","GMID_Name","Dt_Latest_Cust_Req_Date","Pre_cc","Dt_Delivery_Date","CRDD_Dow_Confirmed_Logic","Diff_CRDD_Dow_Confirmed"), new=c("Business Group","Global Customer","Global Customer Name","Global Customer New","GMID","GMIDName","Dt - Latest Cust Req Date - CRDD","Pre CC or CC Logic","Dt - Cust Req Date (OI)- DOW confirmed date","CRDD_Dow Confirmed_Logic","Diff CRDD-Dow Confirmed"))

GIR_DATA1_5 <- sqldf("select * from GIR_DATA1_3 where CRDD_Dow_Confirmed_Logic='Late' and Critical_GMID_Flag ='Yes' ")

quantile(GIR_DATA1_5$'Diff_CRDD_Dow_Confirmed',probs = c(0,0.25, 0.5, 0.75,1))

GIR_DATA1_5$Quartile<-ifelse(GIR_DATA1_5$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_5$`Diff_CRDD_Dow_Confirmed`,c(0.25)),'Q1',
                             ifelse(GIR_DATA1_5$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_5$`Diff_CRDD_Dow_Confirmed`,c(0.5)),'Q2',
                                    ifelse(GIR_DATA1_5$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_5$`Diff_CRDD_Dow_Confirmed`,c(0.75)),'Q3',
                                           ifelse(GIR_DATA1_5$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_5$`Diff_CRDD_Dow_Confirmed`,c(1)),'Q4',NA))))


## Arranging the variables in order as per BI Report Late Customers
GIR_DATA4 <- sqldf("select Business_Group,Global_Customer,Global_Customer_Name,Global_Customer_New,GMID,GMID_Name,Dt_Latest_Cust_Req_Date,Pre_cc,Dt_Delivery_Date,CRDD_Dow_Confirmed_Logic,Diff_CRDD_Dow_Confirmed,Qty_Delivered,Quartile,Critical_GMID_Flag from GIR_DATA1_5 where CRDD_Dow_Confirmed_Logic='Late' ")

GIR_DATA5 <- setnames(GIR_DATA4,old=c("Business_Group","Global_Customer","Global_Customer_Name","Global_Customer_New","GMID","GMID_Name","Dt_Latest_Cust_Req_Date","Pre_cc","Dt_Delivery_Date","CRDD_Dow_Confirmed_Logic","Diff_CRDD_Dow_Confirmed"), new=c("Business Group","Global Customer","Global Customer Name","Global Customer New","GMID","GMIDName","Dt - Latest Cust Req Date - CRDD","Pre CC or CC Logic","Dt - Cust Req Date (OI)- DOW confirmed date","CRDD_Dow Confirmed_Logic","Diff CRDD-Dow Confirmed"))

## Arranging the variables in order as per BI Report Ontime Customers
GIR_DATA1_6 <- sqldf("select * from GIR_DATA1_3 where CRDD_Dow_Confirmed_Logic='Ontime' and Critical_GMID_Flag ='Yes' ")

unique(GIR_DATA1_6$Critical_GMID_Flag)

quantile(GIR_DATA1_6$'Diff_CRDD_Dow_Confirmed',probs = c(0,0.25, 0.5, 0.75,1))

GIR_DATA1_6$Quartile<-ifelse(GIR_DATA1_6$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_6$`Diff_CRDD_Dow_Confirmed`,c(0.25)),'Q1',
                             ifelse(GIR_DATA1_6$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_6$`Diff_CRDD_Dow_Confirmed`,c(0.5)),'Q2',
                                    ifelse(GIR_DATA1_6$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_6$`Diff_CRDD_Dow_Confirmed`,c(0.75)),'Q3',
                                           ifelse(GIR_DATA1_6$`Diff_CRDD_Dow_Confirmed`<=quantile(GIR_DATA1_6$`Diff_CRDD_Dow_Confirmed`,c(1)),'Q4',NA))))


GIR_DATA7 <- sqldf("select Business_Group,Global_Customer,Global_Customer_Name,Global_Customer_New,GMID,GMID_Name,Dt_Latest_Cust_Req_Date,Pre_cc,Dt_Delivery_Date,CRDD_Dow_Confirmed_Logic,Diff_CRDD_Dow_Confirmed,Qty_Delivered,Quartile,Critical_GMID_Flag from GIR_DATA1_6 where CRDD_Dow_Confirmed_Logic='Ontime' ")

GIR_DATA7_1 <- setnames(GIR_DATA7,old=c("Business_Group","Global_Customer","Global_Customer_Name","Global_Customer_New","GMID","GMID_Name","Dt_Latest_Cust_Req_Date","Pre_cc","Dt_Delivery_Date","CRDD_Dow_Confirmed_Logic","Diff_CRDD_Dow_Confirmed"), new=c("Business Group","Global Customer","Global Customer Name","Global Customer New","GMID","GMIDName","Dt - Latest Cust Req Date - CRDD","Pre CC or CC Logic","Dt - Cust Req Date (OI)- DOW confirmed date","CRDD_Dow Confirmed_Logic","Diff CRDD-Dow Confirmed"))

## Combining all the Dataframes Early,Late and Ontime
GIR_DATA8 <- do.call("rbind", list(GIR_DATA3,GIR_DATA5,GIR_DATA7_1))

GIR_DATA8$Srno <- rownames(GIR_DATA8)


## Outfile path Location
out_path = "\\\\ingscsdowf101\\msc_rep_grp\\PUBLIC\\Projects\\Analytics_WIP\\Invoice_Report_Analysis\\Invoice_Report_Analysis\\Loreal\\Ad-Hoc Analysis\\Variance_Bosch\\Variance_out_files"
setwd(out_path)
getwd()
## Exporting the data to share folder
write.csv(GIR_DATA8,file = "Variance Analysis All Customers 10Sep2019.csv",row.names = F)
