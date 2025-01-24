#################################################################### 
## This file is used to replicate tables in the appendix for
## "Is liquidity provision informative? Evidence from agricultural
## futures markets" by Richie R. Ma and Teresa Serra
######################################################################

library(data.table)
library(lubridate)
library(nanotime)
library(purrr)

rm(list=ls())


summary.function <- function(x){
  summary <- list(mean=mean(x, na.rm=T), std=sd(x, na.rm=T),
                  min=min(x, na.rm=T), p25=quantile(x, 0.25, na.rm=T), median=median(x, na.rm=T),
                  p75=quantile(x, 0.75, na.rm=T), max=max(x, na.rm=T))
  return(summary)
  
}

#-------------------------- Table C1 -------------------------------

Table_C1 <- function(data,...){
  
  data[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                               substr(Time, 15, 16)))]
  data <- data[HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)]  ## continuous trading sessions and last market pause
  
  data[bidask==2 & Type==0, Type:=3]
  
   stat <- data[, {
     total=.N
     .SD[,.(percent=.N/total), by=.(Type)]}] 
   
  return(stat)
}

#--------------------------- Figure D1 -----------------------------

FigureD1 <- function(data,...){
  
  data[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                               substr(Time, 15, 16)))]
  #data <- data[HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)]
  
  data[, Session:=fifelse((HM %between% c(1900, 2359) | HM %between% c(0, 744)), "NIGHT", "DAY")]
  data[bidask==2 & Type==0, Type:=3]
  
  S.rate <- data[, {
    total=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==0 & Order_id %in% order_id, .(percent=length(unique(Order_id))/total), by=.(Date, Session)]
  }][, Type:="Submission"]
  
  E.rate <- data[, {
    total=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==3 & Order_id %in% order_id, .(percent=length(unique(Order_id))/total), by=.(Date, Session)]
  }][, Type:="Execution"]
  
  C.rate <- data[, {
    total=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==2 & Order_id %in% order_id,.(percent=length(unique(Order_id))/total), by=.(Date, Session)]
  }][, Type:="Cancellation"]
  
  M.rate <- data[, {
    total=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==1 & Order_id %in% order_id,.(percent=length(unique(Order_id))/total), by=.(Date, Session)]
  }][, Type:="Revision"]
  
  result2 <- rbindlist(list(S.rate=S.rate, E.rate=E.rate, C.rate=C.rate, M.rate=M.rate))
 
  return(result2)
  
}

#------------------------------ Figure D2 upper pannel --------------------------------


FigureD2 <- function(data, ...){
  
  
  data[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                               substr(Time, 15, 16)))]

  data <- data[HM%between%c(830, 1319)]
  data[bidask==2 & Type==0, Type:=3]
  
  data[HM%between%c(830, 859), int:=1]
  data[HM%between%c(900, 929), int:=2]
  data[HM%between%c(930, 959), int:=3]
  data[HM%between%c(1000, 1029), int:=4]
  data[HM%between%c(1030, 1059), int:=5]
  data[HM%between%c(1100, 1129), int:=6]
  data[HM%between%c(1130, 1159), int:=7]
  data[HM%between%c(1200, 1229), int:=8]
  data[HM%between%c(1230, 1259), int:=9]
  data[HM%between%c(1300, 1320), int:=10]
  
  
  S.rate <- data[, {
    total=data[Type==0, length(unique(Order_id))]
    .SD[Type==0, .(percent=length(unique(Order_id))/total), by=.(Date, int)]
  }][, Type:="Submission"]
  
  E.rate <- data[, {
    total=data[Type==0, length(unique(Order_id))]
    .SD[Type==3, .(percent=length(unique(Order_id))/total), by=.(Date, int)]
  }][, Type:="Execution"]
  
  C.rate <- data[, {
    total=data[Type==0, length(unique(Order_id))]
    .SD[Type==2,.(percent=length(unique(Order_id))/total), by=.(Date, int)]
  }][, Type:="Cancellation"]
  
  M.rate <- data[, {
    total=data[Type==0, length(unique(Order_id))]
    .SD[Type==1,.(percent=length(unique(Order_id))/total), by=.(Date, int)]
  }][, Type:="Revision"]
  
  result <- rbindlist(list(S.rate= S.rate, E.rate=E.rate, C.rate=C.rate, M.rate=M.rate))
  
  return(result)
  

}

#--------------------------- Figure D2 lower panel-------------------------------------

Figure2.2 <- function(data, ...){
  
  
  data[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                               substr(Time, 15, 16)))]
  
  data <- data[HM%between%c(1900, 2359) | HM%between%c(0, 744)]
  data[bidask==2 & Type==0, Type:=3]
  
  data[HM%between%c(1900, 1929), int:=1]
  data[HM%between%c(1930, 1959), int:=2]
  data[HM%between%c(2000, 2029), int:=3]
  data[HM%between%c(2030, 2059), int:=4]
  data[HM%between%c(2100, 2129), int:=5]
  data[HM%between%c(2130, 2159), int:=6]
  data[HM%between%c(2200, 2229), int:=7]
  data[HM%between%c(2230, 2259), int:=8]
  data[HM%between%c(2300, 2329), int:=9]
  data[HM%between%c(2330, 2359), int:=10]
  data[HM%between%c(0, 29), int:=11]
  data[HM%between%c(30, 59), int:=12]
  data[HM%between%c(100, 129), int:=13]
  data[HM%between%c(130, 159), int:=14]
  data[HM%between%c(200, 229), int:=15]
  data[HM%between%c(230, 259), int:=16]
  data[HM%between%c(300, 329), int:=17]
  data[HM%between%c(330, 359), int:=18]
  data[HM%between%c(400, 429), int:=19]
  data[HM%between%c(430, 459), int:=20]
  data[HM%between%c(500, 529), int:=21]
  data[HM%between%c(530, 559), int:=22]
  data[HM%between%c(600, 629), int:=23]
  data[HM%between%c(630, 659), int:=24]
  data[HM%between%c(700, 729), int:=25]
  data[HM%between%c(730, 745), int:=26]
  
  
  S.rate <- data[, {
    total=data[Type==0, length(unique(Order_id))]
    .SD[Type==0, .(percent=length(unique(Order_id))/total), by=.(Date, int)]
  }][, Type:="Submission"]
  
  E.rate <- data[, {
    total=data[Type==0, length(unique(Order_id))]
    .SD[Type==3, .(percent=length(unique(Order_id))/total), by=.(Date, int)]
  }][, Type:="Execution"]
  
  C.rate <- data[, {
    total=data[Type==0, length(unique(Order_id))]
    .SD[Type==2,.(percent=length(unique(Order_id))/total), by=.(Date, int)]
  }][, Type:="Cancellation"]
  
  M.rate <- data[, {
    total=data[Type==0, length(unique(Order_id))]
    .SD[Type==1,.(percent=length(unique(Order_id))/total), by=.(Date, int)]
  }][, Type:="Revision"]
  
  result <- rbindlist(list(S.rate= S.rate, E.rate=E.rate, C.rate=C.rate, M.rate=M.rate))
  return(result)
  
}

#-------------------------------  Figure E1 --------------------------------

FigureE1 <- function(data, ...){
  
  data[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                               substr(Time, 15, 16)))]
  #data <- data[HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)]
  data[, bidask:=as.numeric(bidask)]
  data[bidask==2 & Type==0, Type:=3]
  
  setkey(data, Order_id, MsgSeq, Seq, seq)
  
  total <- data[, {
    
    order.id=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Order_id %in% order.id, .(time=as.numeric(difftime(Time[.N,], Time[Type==0], units="secs"))), by=Order_id]
    
  }][, STAT:="total"]
  
  
  result <- rbind(total=total)
  return(result)
}

FigureE1.group <- function(data,...){
  
  data[time <1, Group:=1][time >=1 & time <2, Group:=2][time >=2 & time <5, Group:=3][time >=5 & time <10, Group:=4][time >=10 & time <20, Group:=5][time >=20 & time <30, Group:=6][time >=30 & time <60, Group:=7]
  data[time >=60 & time <120, Group:=8][time >=120 & time <180, Group:=9][time >=180 & time <240, Group:=10][time >=240 & time <300, Group:=11][time >=300 & time <600, Group:=12][time >= 600 & time <900, Group:=13]
  data[time >=900 & time <1200, Group:=14][time >=1200 & time <1500, Group:=15][time >=1500 & time <1800, Group:=16][time >=1800 & time <2100, Group:=17][time >=2100 & time <2400, Group:=18][time >= 2400 & time <2700, Group:=19]
  data[time >=2700 & time <3000, Group:=20][time >=3000 & time <3300, Group:=21][time >=3300 & time <3600, Group:=22][time >=3600, Group:=23]
  
  result <- data[, {
    
    order_total=data[,.N]
    .SD[, .(percent=.N/order_total), by=Group]
    
  }]
  
  return(result)
  
}
 
#---------------------------  Table G1 -----------------------------------

msg.fun <- function(data, ...){
  
  data <- data[HM %between% c(930,1230)]
  
  trade_same_price <- data[trade_same_px!=0, .N, by=HM]
  trade_same_price <- merge(data[, .N, by=HM], trade_same_price, by="HM", all.x=TRUE)[, .(HM, N.y)]
  
  trade_change_price <- data[trade_change_px!=0, .N, by=HM]
  trade_change_price <- merge(data[, .N, by=HM], trade_change_price, by="HM", all.x=TRUE)[, .(HM, N.y)]
  
  improve.submit <- data[submit_improve!=0, .N, by=HM]
  improve.submit <- merge(data[, .N, by=HM], improve.submit, by="HM", all.x=TRUE)[, .(HM, N.y)]
  
  worsen.cancel <- data[cancel_worsen!=0, .N, by=HM]
  worsen.cancel <- merge(data[, .N, by=HM], worsen.cancel, by="HM", all.x=TRUE)[, .(HM, N.y)]
  
  submit.BBO <- data[submit_BBO!=0, .N, by=HM]
  submit.BBO <- merge(data[, .N, by=HM], submit.BBO, by="HM", all.x=TRUE)[, .(HM, N.y)]
  
  cancel.BBO <- data[cancel_BBO!=0, .N, by=HM]
  cancel.BBO <- merge(data[, .N, by=HM], cancel.BBO, by="HM", all.x=TRUE)[, .(HM, N.y)]
  
  submit.Non.BBO <- data[submit_Non_BBO!=0, .N, by=HM]
  submit.Non.BBO <- merge(data[, .N, by=HM], submit.Non.BBO, by="HM", all.x=TRUE)[, .(HM, N.y)]
  
  cancel.Non.BBO <- data[cancel_Non_BBO!=0, .N, by=HM]
  cancel.Non.BBO <- merge(data[, .N, by=HM], cancel.Non.BBO, by="HM", all.x=TRUE)[, .(HM, N.y)]
  
  result <- data.table(HM=data[, .N, by=HM][, HM], Date=data[, unique(Date)], trade_same_px=trade_same_price$N.y, trade_change_px=trade_change_price$N.y,
                       submit_improve=improve.submit$N.y, cancel_worsen=worsen.cancel$N.y, submit_BBO=submit.BBO$N.y,
                       cancel_BBO=cancel.BBO$N.y, submit_Non_BBO=submit.Non.BBO$N.y,cancel_Non_BBO= cancel.Non.BBO$N.y)
  return(result)
  
}

########################## Tables H1 and H2 ########################################


library(data.table)
library(sandwich)
library(lmtest)
rm(list=ls())


set1 <- list()
set2 <- list()
set3 <- list()

for(i in 1:length(noise.file)){
  
  
  print(unique(message_day$Date))
  
  mid_returns_noise <- data.table(noises=noise)
  rm(noise)
  
  message_day[, `:=`(trade_same_px=abs(trade_same_px), trade_change_px=abs(trade_change_px),
                     submit_improve=abs(submit_improve), submit_BBO=abs(submit_BBO), submit_Non_BBO=abs(submit_Non_BBO),
                     cancel_worsen=abs(cancel_worsen), cancel_BBO=abs(cancel_BBO), cancel_Non_BBO=abs(cancel_Non_BBO))]
  
  data <- cbind(mid_returns_noise, message_day[, 10:17])
  
  rm(message_day, mid_returns_noise)
  
  data[, noises_lag:=shift(noises, 1, "lag", fill=NA)]
  
  ## setting 1
  
  setting_1 <- summary(lm(noises~noises_lag, data=data))
  
  result1 <- as.data.table(setting_1[["coefficients"]][,c(1,4)])
  result1[, variable:=c("intercept", "noise_lag")]
  
  result1[, Adj_R_square:=setting_1[["adj.r.squared"]]]
  
  set1[[i]] <- result1
  
  ### setting 2
  
  setting_2 <- lm(noises~noises_lag+trade_same_px+trade_change_px+
                    submit_improve+submit_BBO+submit_Non_BBO+
                    cancel_worsen+cancel_BBO+cancel_Non_BBO, data=data)
  
  
  setting_2_neweywest <- coeftest(setting_2, vcov. = NeweyWest)
  setting_2 <- summary(setting_2)
  
  result2 <- as.data.table(setting_2_neweywest[,c(1,4)])
  result2[, variable:=c("intercept", "noise_lag", "trade_same_px", "trade_change_px",
                        "submit_improve", "submit_BBO", "submit_Non_BBO",
                        "cancel_worsen", "cancel_BBO", "cancel_Non_BBO")]
  
  result2[, Adj_R_square:=setting_2[["adj.r.squared"]]]
  set2[[i]] <- result2
  
  ### setting 3
  
  setting_3 <- lm(noises~trade_same_px+trade_change_px+
                    submit_improve+submit_BBO+submit_Non_BBO+
                    cancel_worsen+cancel_BBO+cancel_Non_BBO, data=data)
  
  setting_3_neweywest <- coeftest(setting_3, vcov. = NeweyWest)
  setting_3 <- summary(setting_3)
  
  result3 <- as.data.table(setting_3_neweywest[,c(1,4)])
  result3[, variable:=c("intercept", "trade_same_px", "trade_change_px",
                        "submit_improve", "submit_BBO", "submit_Non_BBO",
                        "cancel_worsen", "cancel_BBO", "cancel_Non_BBO")]
  
  result3[, Adj_R_square:=setting_2[["adj.r.squared"]]]
  
  set3[[i]] <- result3
  
}


set1 <- rbindlist(set1)
setnames(set1, 'Pr(>|t|)', "pvalue")
set1_summary <- set1[, .(coef=round(median(Estimate),3)), by=variable]
set1_summary2 <- set1[pvalue<0.05, .(pvalue=round(.N/368, 4)), by=variable]
set1_summary3 <- set1[, .(R2=mean(unique(Adj_R_square)))]
set1_summary
set1_summary2
set1_summary3

set2 <- rbindlist(set2)
setnames(set2, 'Pr(>|t|)', "pvalue")
set2_summary <- set2[, .(coef=round(median(Estimate),3)), by=variable]
set2_summary2 <- set2[pvalue<0.05, .(pvalue=round(.N/368, 4)), by=variable]
set2_summary3 <- set2[, .(R2=mean(unique(Adj_R_square)))]
set2_summary
set2_summary2
set2_summary3

set3 <- rbindlist(set3)
setnames(set3, 'Pr(>|t|)', "pvalue")
set3_summary <- set3[, .(coef=round(median(Estimate),3)), by=variable]
set3_summary2 <- set3[pvalue<0.05, .(pvalue=round(.N/368, 4)), by=variable]
set3_summary3 <- set3[, .(R2=mean(unique(Adj_R_square)))]
set3_summary
set3_summary2
set3_summary3

###############################################################################
# In terms of Tables I1-I4, we use the same code for the SVAR estimation.

