library(aws.ec2metadata)
library(aws.s3)
library(readr)
library(tidyverse)
library(readxl)
library(gitcreds)
library(forecast)
library(MLmetrics)

bucket_exists("s3-ranch-034")
get_bucket("s3-ranch-034")

#importing HSE fee income data


inv21 <- s3read_using(FUN = read_xlsx,
                      bucket = "s3-ranch-034",
                      object = "FeeIncome/Copy of REACH invoices with Pivot Table.xlsx",
                      sheet=4)

inv22 <- s3read_using(FUN = read_xlsx,
                      bucket = "s3-ranch-034",
                      object = "FeeIncome/Copy of REACH invoices with Pivot Table.xlsx",
                      sheet=5)

#combine invoice sheets

Invoices <- inv21 %>%
  bind_rows(inv22)

#summing income and auth from Invoices

monthlysums <- aggregate(Invoices["Amount"], by=Invoices["General Ledger Date"], sum)

monthlysums <- monthlysums %>%
  complete(`General Ledger Date` = seq(as.Date('2021-05-01'), as.Date('2025-03-01'), 
                                       by = 'month') -1)

monthlysums$Amount <- monthlysums$Amount*sign(monthlysums$Amount)

#this command keeps only large authorisations, removes all missing values, and 
#then sums by date, repeats for all types of authorisations

AuthBaseLarge <- Invoices[Invoices$Type == "Authorisation - base - large",]
AuthBaseLarge <- AuthBaseLarge[rowSums(is.na(AuthBaseLarge))!= ncol(AuthBaseLarge), ]
AuthBaseLarge <- aggregate(AuthBaseLarge["Amount"], by=AuthBaseLarge["General Ledger Date"], sum)

AuthBaseMed <- Invoices[Invoices$Type == "Authorisation - base - medium",]
AuthBaseMed <- AuthBaseMed[rowSums(is.na(AuthBaseMed))!= ncol(AuthBaseMed), ]
AuthBaseMed <- aggregate(AuthBaseMed["Amount"], by=AuthBaseMed["General Ledger Date"], sum)

AuthBaseSmall <- Invoices[Invoices$Type == "Authorisation - base - small",]
AuthBaseSmall <- AuthBaseSmall[rowSums(is.na(AuthBaseSmall))!= ncol(AuthBaseSmall), ]
AuthBaseSmall <- aggregate(AuthBaseSmall["Amount"], by=AuthBaseSmall["General Ledger Date"], sum)

AuthBaseMic <- Invoices[Invoices$Type == "Authorisation - base - micro",]
AuthBaseMic <- AuthBaseMic[rowSums(is.na(AuthBaseMic))!= ncol(AuthBaseMic), ]
AuthBaseMic <- aggregate(AuthBaseMic["Amount"], by=AuthBaseMic["General Ledger Date"], sum)

#combines all authorisation income datasets into one then sum by date

AuthIncome <- AuthBaseLarge %>%
  bind_rows(AuthBaseMed) %>%
  bind_rows(AuthBaseSmall) %>%
  bind_rows(AuthBaseMic)

AuthIncome <- aggregate(AuthIncome["Amount"], by=AuthIncome["General Ledger Date"], sum)

#populating authorisation income with missing values such that have
#complete range of dates from Apr 21 to Sep 22

authinc <- AuthIncome %>%
  complete(`General Ledger Date` = seq(as.Date('2021-05-01'), as.Date('2025-03-01'), 
                                       by = 'month') -1)

#combining authorisation income dataframe with total income dataframe

totalnoauth <-  monthlysums %>% 
  cbind(authinc$Amount) %>%
  mutate_at(2:3, ~replace_na(.,0))

#creating row sum column to give total income without authorisation by date

totalnoauth$RowSums <- rowSums(totalnoauth[,c("Amount", "authinc$Amount")])

#SENSITIVITY GRAPH
#from April 2021

incomeapril <- cbind(totalnoauth$RowSums)

incomeapril[incomeapril==0] <- NA

income_ts_april <- ts(incomeapril, start=c(2021,4),frequency=12) %>%
  na.omit()

fcastapril <- tslm(income_ts_april ~ trend) %>%
  forecast(h=29)

plapril <- autoplot(fcastapril)


#from July 21

incomejuly <- cbind(totalnoauth$RowSums) %>%
  `[`(-c(1:3),)

incomejuly[incomejuly==0] <- NA

income_ts_july <- ts(incomejuly, start=c(2021,7),frequency=12) %>%
  na.omit()

fcastjuly <- tslm(income_ts_july ~ trend) %>%
  forecast(h=29)

pljuly <- autoplot(fcastjuly)


#from December 21

incomedec <- cbind(totalnoauth$RowSums) %>%
  `[`(-c(1:8),)

incomedec[incomedec==0] <- NA

income_ts_dec <- ts(incomedec, start=c(2021,12),frequency=12) %>%
  na.omit()

fcastdec <- tslm(income_ts_dec ~ trend) %>%
  forecast(h=29)

pldec <- autoplot(fcastdec)


#from November 21

incomenov <- cbind(totalnoauth$RowSums) %>%
  `[`(-c(1:7),)

incomenov[incomenov==0] <- NA

income_ts_nov <- ts(incomenov, start=c(2021,12),frequency=12) %>%
  na.omit()

fcastnov <- tslm(income_ts_nov ~ trend) %>%
  forecast(h=29)

plnov <- autoplot(fcastnov)


#plot with all four

sensitivityplot <- autoplot(income_ts_april) + autolayer(fcastapril, series="Apr 21", PI=FALSE) +
  autolayer(fcastjuly, series="Jul 21", PI=FALSE) + 
  autolayer(fcastdec, series="Dec 21", PI=FALSE) +
  autolayer(fcastnov, series="Nov 21", PI=FALSE) + xlim(2021,2025) +
  ylim(0,1200000) + ylab("Income (£)") +
  ggtitle("Non-auth Monthly Fee Income Forecast for 23-24 and 24-25") 


#MONTHLY FEE INCOME GRAPH APR 21 TO SEPT 22
#creating vectors of income without authorisation and just authorisation

noauthincomevector <- c(incomeapril)
authincomevector <- c((totalnoauth$`authinc$Amount`)*sign(totalnoauth$`authinc$Amount`))

dates <- c(seq(as.Date('2021-05-01'), as.Date('2025-03-01'), by = 'month') -1)
colours <- c("orange", "red", "blue")

table <- rbind(noauthincomevector, authincomevector)
barplot(table, main = "Monthly Fee Income Apr 21 - Sept 22", col = colours,
        names.arg = dates, las=2, ylim= c(0,1200000))

#MONTHLY FEE INCOME FORECAST PLOT
fcastjulyvector <- c(fcastjuly$`Forecast`)
table2 <- rbind(table, fcastjuly)