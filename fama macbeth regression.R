###
###   K/S 的moneyness 、
##    依stock排出每一天的warrant,算出每一档warrant moneyness and volume權重, 
##    加权平均得到avg moneyness（每一天只有一个）
##    open new data只有每一天的avg moneyness, Rt, Rt+1 return
##

##2021/3/15
"問題：
1：第一次回歸取出估計值然後第二次怎麽跑，Y要用什麽變數
"
rm(list=ls())
if(!require(data.table))install.packages("data.table")
if(!require(dplyr))install.packages("dpylr")
library(data.table)
library(xts)
library(stargazer)
library(dplyr)
library(plm)

getwd()
setwd("C:\\Users\\User\\Documents\\研究所\\論文")

####處理call資料
newcalldata2 <- read.csv("call_model_data.csv", sep = ",", stringsAsFactors = F) #控制變數的data
newcalldata1 <- as.data.frame(newcalldata2)
rm(newcalldata2)

newcalldata1 <- newcalldata1[,c(2:14)]
newcalldata1$MDATE <- as.Date(newcalldata1$MDATE)
newcalldata1 <- na.omit(newcalldata1)

newcalllist1 <- split(newcalldata1, f = newcalldata1$STOCK_CODE)
#coidlist <- split(newcalldata1, f = newcalldata1$COID.x)

##fama macbeth regression 資料處理
date_call_data <- split(newcalldata1, f = newcalldata1$MDATE)
tem_call_data <- list()
for (i in 1:length(date_call_data)) {
  
  cat(i, "/", length(date_call_data), "\n")
  
  tem_date <- date_call_data[[i]]
  
  tem_date <- tem_date[!duplicated(tem_date$STOCK_CODE),]
  
  tem_call_data[[i]] <- tem_date
}

fmb_call_data <- rbindlist(tem_call_data)

##fama macbeth regression  
newcalldata1$avg.moneynessIV <- newcalldata1$avg.moneyness*newcalldata1$IV
colnames(newcalldata1) <- c("MDATE", "COID.x", "STOCK_CODE", "IV", "STOCK_ROI", "CallAveMoney", "DATA_T1_RETURN", "PRIOR_DAY_RETURN", "T1_RETURN",
                            "ME", "ILLIQUIDITY", "MONTHTURNOVER", "MONTHRET", "CallAveMoney*IV")

call_fmb_model1 <- pmg(DATA_T1_RETURN~CallAveMoney+
                      STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                      index = c("STOCK_CODE"), data = newcalldata1) ##每檔股票在每天只留下一筆

call_fmb_model2 <- pmg(DATA_T1_RETURN~CallAveMoney+newcalldata1$`CallAveMoney*IV`+
                       STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                       index = c("STOCK_CODE"), data = newcalldata1) ##用原本的資料跑

sink("call_fmb_model.txt")
bind_model <- stargazer(call_fmb_model1, call_fmb_model2,
                        title="Results", digits = 3,  header = TRUE, type = "text", report = "vc*p",
                        model.names = TRUE)
sink()

####二階段回歸
'callbeta <- NULL
for (i in 1:length(newcalllist1)) {
  
  cat(i, "/", length(newcalllist1), "\n")
  
  tem <- newcalllist1[[i]]
  
  tem_model <- lm(data_t1_return~avg.moneyness+STOCK_ROI+prior_day_return+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, data = tem)
  
  callbeta <- rbind(callbeta, tem_model$coefficients)
  
}
colnames(callbeta) <- c("intercept", "AVG.MONEYNESS", "STOCK_ROI", "PRIOR_DAY_RETURN", "IV", "MONTHRET", "MONTHTURNOVER", "ILLIQUIDITY", "ME")
callbeta <- as.data.frame(callbeta)
call_model1 <- lm(intercept~., data = callbeta)

sink("two_step_call_model.txt")
bind_model <- stargazer(call_model1,
                        title="Results", digits = 3,  header = TRUE, type = "text", report = "vc*p",
                        model.names = TRUE)
sink()'
##################################################################################################################################################
###############################################################     put DATA    ##################################################################
##################################################################################################################################################

####二階段回歸或者fama macbeth regression
newputdata2 <- read.csv("put_model_data.csv", sep = ",", stringsAsFactors = F) #控制變數的data
newputdata1 <- as.data.frame(newputdata2)
rm(newputdata2)

newputdata1 <- newputdata1[,c(2:14)]
newputdata1$MDATE <- as.Date(newputdata1$MDATE)
newputdata1 <- na.omit(newputdata1)

newputlist1 <- split(newputdata1, f = newputdata1$STOCK_CODE)
#coidlist <- split(newputdata1, f = newputdata1$COID.x)

##fama macbeth regression 資料處理
date_put_data <- split(newputdata1, f = newputdata1$MDATE)
tem_put_data <- list()
for (i in 1:length(date_put_data)) {
  
  cat(i, "/", length(date_put_data), "\n")
  
  tem_date <- date_put_data[[i]]
  
  tem_date <- tem_date[!duplicated(tem_date$STOCK_CODE),]
  
  tem_put_data[[i]] <- tem_date
}

fmb_put_data <- rbindlist(tem_put_data)

##fama macbeth regression  
newputdata1$avg.moneynessIV <- newputdata1$avg.moneyness*newputdata1$IV
colnames(newputdata1) <- c("MDATE", "COID.x", "STOCK_CODE", "IV", "STOCK_ROI", "PutAveMoney", "DATA_T1_RETURN", "PRIOR_DAY_RETURN", "T1_RETURN",
                            "ME", "ILLIQUIDITY", "MONTHTURNOVER", "MONTHRET", "PutAveMoney*IV")

put_fmb_model1 <- pmg(DATA_T1_RETURN~PutAveMoney+
                  STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                  index = c("STOCK_CODE"), data = newputdata1) ##每檔股票在每天只留下一筆

put_fmb_model2 <- pmg(DATA_T1_RETURN~PutAveMoney+newputdata1$`PutAveMoney*IV`+
                  STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                  index = c("STOCK_CODE"), data = newputdata1) ##用原本的資料跑

sink("put_fmb_model.txt")
bind_model <- stargazer(put_fmb_model1, put_fmb_model2,
                        title="Results", digits = 3,  header = TRUE, type = "text", report = "vc*p",
                        model.names = TRUE)
sink()
'####二階段回歸
putbeta <- NULL
for (i in 1:length(newputlist1)) {
  
  cat(i, "/", length(newputlist1), "\n")
  
  tem <- newputlist1[[i]]
  
  tem_model <- lm(data_t1_return~avg.moneyness+STOCK_ROI+prior_day_return+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, data = tem)
  
  putbeta <- rbind(putbeta, tem_model$coefficients)
  
}
colnames(putbeta) <- c("intercept", "AVG.MONEYNESS", "STOCK_ROI", "PRIOR_DAY_RETURN", "IV", "MONTHRET", "MONTHTURNOVER", "ILLIQUIDITY", "ME")
putbeta <- as.data.frame(putbeta)
put_model1 <- lm(intercept~., data = putbeta)

sink("two_step_put_model.txt")
bind_model <- stargazer(put_model1,
                        title="Results", digits = 3,  header = TRUE, type = "text", report = "vc*p",
                        model.names = TRUE)
sink()'
##################################################################################################################################################
###############################################################     all DATA    ##################################################################
##################################################################################################################################################

##################################################################
#
#
#     全部data跑回歸
#
#
##################################################################
library(data.table)
setwd("C:\\Users\\User\\Documents\\研究所\\論文")
#allcalldata <- read.csv("call_model_data.csv", sep = ",", stringsAsFactors = F) #控制變數的data
#allputdata <- read.csv("put_model_data.csv", sep = ",", stringsAsFactors = F) #控制變數的data

allcalldata <- newcalldata1
allputdata <- newputdata1

#allcalldata <- allcalldata[,-1]
#allputdata <- allputdata[,-1]
alldata <- rbind(allcalldata, allputdata)
put.avemoney <- allputdata$AveMoney
put.avemoney <- c(rep(NA,646732),put.avemoney)
put.avemoney <- as.data.frame(put.avemoney)
call.avemoney <- allcalldata$AveMoney
call.avemoney <-  c(call.avemoney, rep(NA, 155098))
call.avemoney <- as.data.frame(call.avemoney)

alldata <- cbind(alldata,call.avemoney,put.avemoney)
alldata1 <- alldata[,-6]

avemoneyIV <- as.data.frame(alldata$AveMoney*alldata$IV)
colnames(avemoneyIV) = "AveMoneyIV"
call.avemoneyIV <- as.data.frame(alldata$call.avemoney*alldata$IV)
colnames(call.avemoneyIV) = "CallAveMoneyIV"
put.avemoneyIV <- as.data.frame(alldata$put.avemoney*alldata$IV)
colnames(put.avemoneyIV) = "PutAveMoneyIV"
alldata <- cbind(alldata, avemoneyIV, call.avemoneyIV, put.avemoneyIV)

write.csv(alldata, file = "alldata1218.csv", sep = ",", col.names = TRUE)
#alldata1 <- read.csv("alldata1218.csv", sep = ",", stringsAsFactors = F) #控制變數的data
#alldata <- alldata1[,-1]
alldata[is.na(alldata)] <- 0

newalllist1 <- split(alldata, f = alldata$STOCK_CODE) ##標的的list

##fama macbeth regression 資料處理
date_all_data <- split(alldata, f = alldata$MDATE)
tem_all_data <- list()
for (i in 1:length(date_all_data)) {
  
  cat(i, "/", length(date_all_data), "\n")
  
  tem_date <- date_all_data[[i]]
  
  tem_date <- tem_date[!duplicated(tem_date$STOCK_CODE),]
  
  tem_all_data[[i]] <- tem_date
}

fmb_all_data <- rbindlist(tem_all_data) ##每檔股票每一天只有一筆資料

##要做什麽模型
if(!require(plm))install.packages("plm")

##fama macbeth regression  
##檢查共綫性或者虛擬變數

'all_fmb_model1_1 <- pmg(DATA_T1_RETURN~AveMoney+
                          STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                      index = c("STOCK_CODE"), data = fmb_all_data) ##每檔股票在每天只留下一筆

all_fmb_model1_2 <- pmg(DATA_T1_RETURN~AveMoney+AveMoneyIV+
                          STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                        index = c("STOCK_CODE"), data = fmb_all_data)

all_fmb_model1_3 <- pmg(DATA_T1_RETURN~call.avemoney+
                          STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                        index = c("STOCK_CODE"), data = fmb_all_data)

all_fmb_model1_4 <- pmg(DATA_T1_RETURN~call.avemoney+CallAveMoneyIV+
                          STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                        index = c("STOCK_CODE"), data = fmb_all_data)

all_fmb_model1_5 <- pmg(DATA_T1_RETURN~put.avemoney+
                          STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME,  ##出問題
                        index = c("STOCK_CODE"), data = fmb_all_data)

all_fmb_model1_6 <- pmg(DATA_T1_RETURN~put.avemoney+PutAveMoneyIV+
                          STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                        index = c("STOCK_CODE"), data = fmb_all_data)

all_fmb_model1_7 <- pmg(DATA_T1_RETURN~call.avemoney+put.avemoney+
                          STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                        index = c("STOCK_CODE"), data = fmb_all_data)

all_fmb_model1_8 <- pmg(DATA_T1_RETURN~call.avemoney+CallAveMoneyIV+put.avemoney+PutAveMoneyIV+
                          STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                        index = c("STOCK_CODE"), data = fmb_all_data)


sink("fmb_model_unique.txt")
bind_model <- stargazer(all_fmb_model1_1, all_fmb_model1_2, all_fmb_model1_3, all_fmb_model1_4, all_fmb_model1_5, all_fmb_model1_6,  all_fmb_model1_7, all_fmb_model1_8,
                        title="Results", digits = 3,  header = TRUE, type = "text", report = "vc*p",
                        model.names = TRUE)
sink()'


all_fmb_model2_1 <- pmg(DATA_T1_RETURN~AveMoney+
                          STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                      index = c("STOCK_CODE"), data = alldata) ##用原本的資料跑

all_fmb_model2_2 <- pmg(DATA_T1_RETURN~AveMoney+AveMoneyIV+
                          STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, 
                        index = c("STOCK_CODE"), data = alldata)

sink("fmb_model.txt")
bind_model <- stargazer(all_fmb_model2_1, all_fmb_model2_2,
                        title="Results", digits = 3,  header = TRUE, type = "text", report = "vc*p",
                        model.names = TRUE)
sink()

####二階段回歸
allbeta <- NULL
for (i in 1:length(newalllist1)) {
  
  cat(i, "/", length(newalllist1), "\n")
  
  tem <- newalllist1[[i]]
  
  tem_model <- lm(DATA_T1_RETURN~AveMoney+STOCK_ROI+PRIOR_DAY_RETURN+IV+MONTHRET+MONTHTURNOVER+ILLIQUIDITY+ME, data = tem)
  
  allbeta <- rbind(allbeta, tem_model$coefficients)
  
}
colnames(allbeta) <- c("intercept", "AVG.MONEYNESS", "STOCK_ROI", "PRIOR_DAY_RETURN", "IV", "MONTHRET", "MONTHTURNOVER", "ILLIQUIDITY", "ME")
allbeta <- as.data.frame(allbeta)
all_model1 <- lm(intercept~., data = allbeta)

sink("two_step_model.txt")
bind_model <- stargazer(all_model1,
                        title="Results", digits = 3,  header = TRUE, type = "text", report = "vc*p",
                        model.names = TRUE)
sink()
##Y是t1_return
t1_return <- NULL
for (i in 1:length(newalllist1)) {
  
  cat(i, "/", length(newalllist1), "\n")
  
  tem_t1_return <- newalllist1[[i]][7]
  
  tem_t1_return <- lapply(tem_t1_return, as.numeric)
  
  avg_t1_return <- mean(tem_t1_return[[1]])
  
  t1_return <- rbind(t1_return, avg_t1_return)
  
}
t1_return <- as.data.frame(as.vector(t1_return))
colnames(t1_return) <- "t1_return"


allbeta1 <- cbind(t1_return, allbeta)
all_model2_t1 <- lm(t1_return~., data = allbeta1)

sink("two_step_model_t1return.txt")
bind_model <- stargazer(all_model2_t1,
                        title="Results", digits = 3,  header = TRUE, type = "text", report = "vc*p",
                        model.names = TRUE)
sink()











