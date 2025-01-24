#################################################################### 
## This file is used to replicate tables in the main text of
## "Is liquidity provision informative? Evidence from agricultural
## futures markets" by Richie R. Ma and Teresa Serra
######################################################################



rm(list=ls())

library(data.table)
library(lubridate)
library(nanotime)
library(purrr)


summary.function <- function(x){
  summary <- list(mean=round(mean(x, na.rm=T),4), std=round(sd(x, na.rm=T),4),
                  min=round(min(x, na.rm=T),4), p25=round(quantile(x, 0.25, na.rm=T),4), median=round(median(x, na.rm=T),4),
                  p75=round(quantile(x, 0.75, na.rm=T),4), max=round(max(x, na.rm=T), 4))
  return(summary)
  
}



#------------------------- Table 1 Panel A --------------------------------------

Table1.A <- function(data, ...){
  
  result <- data.table(non.day.order.rate=NA, day.order.rate=NA)
  
  data[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                               substr(Time, 15, 16)))]
  
  data <- data[HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)]
  
  total.order <- length(unique(data[Type==0, Order_id]))
  trades <- data[is.na(trade_id)==F, .N] 
 
  result <- list(total.order=total.order, trades=trades)
  
  return(result)
  
  
}

#------------------------- Table 1 Panel B --------------------------------------

Table1.B <- function(data,...){
  data[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                               substr(Time, 15, 16)))]

  data[bidask==2 & Type==0, Type:=3]
  
 # print(data[is.na(PX_depth)==T, unique(Type)])
  
  E.rate <- data[, {
    total=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==3 & Order_id %in% order_id,.(percent=length(unique(Order_id))/total), by=.(Date)]
  }][, Type:="Execution"]  
  
  C.rate <- data[, {
    total=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==2 & Order_id %in% order_id,.(percent=length(unique(Order_id))/total), by=.(Date)]
  }][, Type:="Cancellation"]   
  
  M.rate <- data[, {
    total=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==1 & Order_id %in% order_id,.(percent=length(unique(Order_id))/total), by=.(Date)]
  }][, Type:="Revision"] 
  
  result <- rbindlist(list(E.rate=E.rate, C.rate=C.rate, M.rate=M.rate))
  result <- dcast(result1, Date~Type, value.var = "percent")
  setkey(result, Date)
  
  return(result)
  
}

#------------------------- Table 1 Panel C --------------------------------------

Figure1.3 <- function(data,...){
  
  data[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                               substr(Time, 15, 16)))]
  #data <- data[HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)]
  data[, bidask:=as.numeric(bidask)]
  data[bidask==2 & Type==0, Type:=3]
  
  S.rate.BEST <- data[, {
    total=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==0 & Order_id %in% order_id, .(percent=length(unique(Order_id))/total), by=.(Date)]
  }][, `:=`(Type="Submit", Group="BEST")]
  
  S.rate.BEHIND <- data[, {
    total=data[Type==0& (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==0 & Order_id %in% order_id, .(percent=length(unique(Order_id))/total), by=.(Date)]
  }][,`:=`(Type="Submit", Group="BEHIND")]
  
  
  E.rate.BEST <- data[, {
    total=data[Type==0 & PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==3 & Order_id %in% order_id, .(percent=length(unique(Order_id))/total), by=.(Date)]
  }][, `:=`(Type="Execution", Group="BEST")]
  
  E.rate.BEHIND <- data[, {
    total=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==3 & Order_id %in% order_id, .(percent=length(unique(Order_id))/total), by=.(Date)]
  }][,`:=`(Type="Execution", Group="BEHIND")]
  
  C.rate.BEST <- data[, {
    total=data[Type==0 & PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==2 & Order_id %in% order_id,.(percent=length(unique(Order_id))/total), by=.(Date)]
  }][, `:=`(Type="Cancellation", Group="BEST")]
  
  C.rate.BEHIND <- data[, {
    total=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==2 & Order_id %in% order_id,.(percent=length(unique(Order_id))/total), by=.(Date)]
  }][, `:=`(Type="Cancellation", Group="BEHIND")]
  
  M.rate.BEST <- data[, {
    total=data[Type==0 & PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==1 & Order_id %in% order_id,.(percent=length(unique(Order_id))/total), by=.(Date)]
  }][, `:=`(Type="Revision", Group="BEST")]
  
  M.rate.BEHIND <- data[, {
    total=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), length(unique(Order_id))]
    order_id=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    .SD[Type==1 & Order_id %in% order_id,.(percent=length(unique(Order_id))/total), by=.(Date)]
  }][, `:=`(Type="Revision", Group="BEHIND")]
  
  result <- rbindlist(list(S.rate.BEST=S.rate.BEST,S.rate.BEHIND=S.rate.BEHIND,E.rate.BEST=E.rate.BEST,E.rate.BEHIND=E.rate.BEHIND, C.rate.BEST=C.rate.BEST, C.rate.BEHIND=C.rate.BEHIND, M.rate.BEST=M.rate.BEST, M.rate.BEHIND=M.rate.BEHIND))

  
  return(result)
  
}

###------------------------------- Table 2 ------------------------------------

Table2 <- function(data, ...){
  
  data[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                               substr(Time, 15, 16)))]
  #data <- data[HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)]
  data[, bidask:=as.numeric(bidask)]
  data[bidask==2 & Type==0, Type:=3]
  
  setkey(data, Order_id, Time, MsgSeq, Seq)
  
  data[, order.seq:=1:.N, by=Order_id]
  
  fill.after.revise.best.day <- data[, {
    best=data[Type==0 & PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    revise.order = data[Type==1 & Order_id %in% best, unique(Order_id)]
    total=data[Type==1 & Order_id %in% revise.order, length(unique(Order_id))]
    Seq.M = data[Type==1 & Order_id %in% revise.order, .(seq=min(order.seq)), by=.(Order_id)]
    E = data[Type==3 & Order_id %in% Seq.M$Order_id, Seq, by=.(Order_id)]
    E.merge = merge(Seq.M, E, by=c("Order_id"))
    E.merge[Seq > seq, .(percent=length(unique(Order_id))/total)]
  }][, STAT:= "fill.after.revise.best"]
  
  
  fill.after.revise.behind.day <- data[, {
    behind=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    revise.order = data[Type==1 & Order_id %in% behind, unique(Order_id)]
    total=data[Type==1 & Order_id %in% revise.order, length(unique(Order_id))]
    Seq.M = data[Type==1 & Order_id %in% revise.order, .(seq=min(order.seq)), by=.(Order_id)]
    E = data[Type==3 & Order_id %in% Seq.M$Order_id, Seq, by=.(Order_id)]
    E.merge = merge(Seq.M, E, by=c("Order_id"))
    E.merge[Seq > seq, .(percent=length(unique(Order_id))/total)]
  }][, STAT:= "fill.after.revise.behind"]
  
  
  fill.before.cancel.best.day <- data[, {
    best=data[Type==0 & PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    total=data[Type==2 & Order_id %in% best, length(unique(Order_id))]
    cancel.order=data[Type==2 & Order_id %in% best, unique(Order_id)]
    .SD[Type==3 & Order_id %in% cancel.order,.(percent=length(unique(Order_id))/total)]
  }][, STAT:= "fill.before.cancel.best"]
  
  fill.before.cancel.behind.day <- data[, {
    behind=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    total=data[Type==2 & Order_id %in% behind, length(unique(Order_id))]
    cancel.order=data[Type==2 & Order_id %in% behind, unique(Order_id)]
    .SD[Type==3 & Order_id %in% cancel.order,.(percent=length(unique(Order_id))/total)]
  }][, STAT:= "fill.before.cancel.behind"]
  
  fill.after.revise.day <- data[, {
    order_ID=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    revise.order = data[Type==1 & Order_id %in% order_ID, unique(Order_id)]
    total=data[Type==1 & Order_id %in% revise.order, length(unique(Order_id))]
    Seq.M = data[Type==1 & Order_id %in% revise.order, .(seq=min(order.seq)), by=.(Order_id)]
    E = data[Type==3 & Order_id %in% Seq.M$Order_id, Seq, by=.(Order_id)]
    E.merge = merge(Seq.M, E, by=c("Order_id"))
    E.merge[Seq > seq, .(percent=length(unique(Order_id))/total)]
  }][, STAT:= "fill.after.revise"]
  
  
  
  fill.before.cancel.day <- data[, {
    order_ID=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    total=data[Type==2 & Order_id %in% order_ID, length(unique(Order_id))]
    cancel.order=data[Type==2 & Order_id %in% order_ID, unique(Order_id)]
    .SD[Type==3 & Order_id %in% cancel.order,.(percent=length(unique(Order_id))/total)]
  }][, STAT:= "ill.before.cancel"]
  
  result <- rbindlist(list(fill.after.revise.day = fill.after.revise.day, fill.before.cancel.day=fill.before.cancel.day, 
                           fill.after.revise.best.day = fill.after.revise.best.day, fill.before.cancel.best.day=fill.before.cancel.best.day,
                           fill.after.revise.behind.day = fill.after.revise.behind.day, fill.before.cancel.behind.day=fill.before.cancel.behind.day))
 
  return(result)
  
}

t_test <- function(data, ...){
  
  test1 <- t.test(x=data[STAT=="fill.after.revise.best", percent], y=data[STAT=="fill.after.revise.behind", percent], alternative="two.sided", var.equal=FALSE)
  test2 <- t.test(x=data[STAT=="fill.before.cancel.best", percent], y=data[STAT=="fill.before.cancel.behind", percent], alternative="two.sided", var.equal=FALSE)
  
  result <- list(test1=test1, test2=test2)
  return(result)
  
}

##------------------------------- Table 3 -----------------------------------

Table3 <- function(data, ...){
  data[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                               substr(Time, 15, 16)))]
  #data <- data[HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)]
  data[, bidask:=as.numeric(bidask)]
  data[bidask==2 & Type==0, Type:=3]
  
  print(unique(data$Date))
  
  setkey(data, Order_id, Time, MsgSeq, Seq, seq)
  
  fill.latency <- data[, {
    
    order.id=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    fill.order=data[Type==3 & Order_id %in% order.id, unique(Order_id)]
    .SD[Order_id %in% fill.order, .(time=as.numeric(difftime(Time[Type==3][1,], Time[Type==0], units="secs"))), by=Order_id]
    
  }][, STAT:="fill.latency"]
  
  print("fill.latency.completed")
  
  
  fill.latency.best <- data[, {
    
    order.id=data[Type==0 & PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    fill.order=data[Type==3 & Order_id %in% order.id, unique(Order_id)]
    .SD[Order_id %in% fill.order, .(time=as.numeric(difftime(Time[Type==3][1,], Time[Type==0], units="secs"))), by=Order_id]
    
  }][, STAT:="fill.latency.best"]
  
  print("fill.latency.best.completed")
  
  fill.latency.behind <- data[, {
    
    order.id=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    fill.order=data[Type==3 & Order_id %in% order.id, unique(Order_id)]
    .SD[Order_id %in% fill.order, .(time=as.numeric(difftime(Time[Type==3][1,], Time[Type==0], units="secs"))), by=Order_id]
    
  }][, STAT:="fill.latency.behind"]
  
  print("fill.latency.behind.completed")
  
  
  cancel.latency <- data[, {
    
    order.id=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    cancel.order=data[Type==2 & Order_id %in% order.id, unique(Order_id)]
    .SD[Order_id %in% cancel.order, .(time=as.numeric(difftime(Time[Type==2], Time[Type==0], units="secs"))), by=Order_id]
    
  }][, STAT:="cancel.latency"]
  
  print("cancel.latency.completed")
  
  cancel.latency.best <- data[, {
    
    order.id=data[Type==0 &PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    cancel.order=data[Type==2 & Order_id %in% order.id, unique(Order_id)]
    .SD[Order_id %in% cancel.order, .(time=as.numeric(difftime(Time[Type==2], Time[Type==0], units="secs"))), by=Order_id]
    
  }][, STAT:="cancel.latency.best"]
  
  print("cancel.latency.best.completed")
  
  cancel.latency.behind <- data[, {
    
    order.id=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    cancel.order=data[Type==2 & Order_id %in% order.id, unique(Order_id)]
    .SD[Order_id %in% cancel.order, .(time=as.numeric(difftime(Time[Type==2], Time[Type==0], units="secs"))), by=Order_id]
    
  }][, STAT:="cancel.latency.behind"]
  
  print("cancel.latency.best.completed")
  
  
  revise.latency <- data[, {
    
    order.id=data[Type==0 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    revise.order=data[Type==1 & Order_id %in% order.id, unique(Order_id)]
    .SD[Order_id %in% revise.order, .(time=as.numeric(difftime(Time[Type==1][1,], Time[Type==0], units="secs"))), by=Order_id]
    
  }][, STAT:="revise.latency"]
  
  print("revise.latency.completed")
  
  revise.latency.best <- data[, {
    
    order.id=data[Type==0 & PX_depth==1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    revise.order=data[Type==1 & Order_id %in% order.id, unique(Order_id)]
    .SD[Order_id %in% revise.order, .(time=as.numeric(difftime(Time[Type==1][1,], Time[Type==0], units="secs"))), by=Order_id]
    
  }][, STAT:="revise.latency.best"]
  
  print("revise.latency.best.completed")
  
  revise.latency.behind <- data[, {
    
    order.id=data[Type==0 & PX_depth>1 & (HM %between% c(1900, 2359)|HM %between% c(0, 744) | HM %between% c(830, 1319)), unique(Order_id)]
    revise.order=data[Type==1& Order_id %in% order.id, unique(Order_id)]
    .SD[Order_id %in% revise.order, .(time=as.numeric(difftime(Time[Type==1][1,], Time[Type==0], units="secs"))), by=Order_id]
    
  }][, STAT:="revise.latency.behind"]
  
  print("revise.latency.behind.completed")
  
  result <- rbind(fill.latency, cancel.latency, revise.latency,
                  fill.latency.best, fill.latency.behind,
                  cancel.latency.best, cancel.latency.behind,
                  revise.latency.best, revise.latency.behind, fill=TRUE)
  return(result)
}

t_test <- function(data, ...){
  
  test1 <- t.test(x=data[STAT=="fill.after.revise.best", percent], y=data[STAT=="fill.after.revise.behind", percent], alternative="two.sided", var.equal=FALSE)
  test2 <- t.test(x=data[STAT=="fill.before.cancel.best", percent], y=data[STAT=="fill.before.cancel.behind", percent], alternative="two.sided", var.equal=FALSE)
  
  result <- list(test1=test1, test2=test2)
  return(result)
  
}

####----------------------------Table 5----------------------------------

event.time <- function(data, ...){
  
  
  time.stat <- data[, {
    total=.N
    trade_same_px=data[trade_same_px!=0, .(trade_same_px=.N/total)]
    trade_change_px=data[trade_change_px!=0, .(trade_change_px=.N/total)]
    submit_improve=data[submit_improve!=0, .(submit_improve=.N/total)]
    submit_BBO=data[submit_BBO!=0, .(submit_BBO=.N/total)]
    submit_Non_BBO=data[submit_Non_BBO!=0, .(submit_Non_BBO=.N/total)]
    cancel_worsen=data[cancel_worsen!=0, .(cancel_worsen=.N/total)]
    cancel_BBO=data[cancel_BBO!=0, .(cancel_BBO=.N/total)]
    cancel_Non_BBO=data[cancel_Non_BBO!=0, .(cancel_Non_BBO=.N/total)]
    result=cbind(trade_same_px,trade_change_px, submit_improve,submit_BBO, submit_Non_BBO, cancel_worsen, cancel_BBO, cancel_Non_BBO, total)
  }]
  
  return(time.stat)
}

contract.value <- function(data){
  
  
  value.stat <- data[, {
    total=sum(abs(trade_same_px), abs(trade_change_px), abs(submit_improve), abs(submit_BBO), abs(submit_Non_BBO), abs(cancel_worsen), abs(cancel_BBO), abs(cancel_Non_BBO))
    trade_same_px=data[trade_same_px!=0, .(trade_same_px=sum(abs(trade_same_px))/total)]
    trade_change_px=data[trade_change_px!=0, .(trade_change_px=sum(abs(trade_change_px))/total)]
    submit_improve=data[submit_improve!=0, .(submit_improve=sum(abs(submit_improve))/total)]
    submit_BBO=data[submit_BBO!=0, .(submit_BBO=sum(abs(submit_BBO))/total)]
    submit_Non_BBO=data[submit_Non_BBO!=0, .(submit_Non_BBO=sum(abs(submit_Non_BBO))/total)]
    cancel_worsen=data[cancel_worsen!=0, .(cancel_worsen=sum(abs(cancel_worsen))/total)]
    cancel_BBO=data[cancel_BBO!=0, .(cancel_BBO=sum(abs(cancel_BBO))/total)]
    cancel_Non_BBO=data[cancel_Non_BBO!=0, .(cancel_Non_BBO=sum(abs(cancel_Non_BBO))/total)]
    result=cbind(trade_same_px,trade_change_px, submit_improve,submit_BBO, submit_Non_BBO, cancel_worsen, cancel_BBO, cancel_Non_BBO, total)
  }]
  
  return(value.stat)
}

mid.return <- function(data){
  
  return.stat <- data[, {
    
    trade_same_px=data[trade_same_px!=0, .(trade_same_px=median(abs(mid_returns)))]
    trade_change_px=data[trade_change_px!=0, .(trade_change_px=median(abs(mid_returns)))]
    submit_improve=data[submit_improve!=0, .(submit_improve=median(abs(mid_returns)))]
    submit_BBO=data[submit_BBO!=0, .(submit_BBO=median(abs(mid_returns)))]
    submit_Non_BBO=data[submit_Non_BBO!=0, .(submit_Non_BBO=median(abs(mid_returns)))]
    cancel_worsen=data[cancel_worsen!=0, .(cancel_worsen=median(abs(mid_returns)))]
    cancel_BBO=data[cancel_BBO!=0, .(cancel_BBO=median(abs(mid_returns)))]
    cancel_Non_BBO=data[cancel_Non_BBO!=0, .(cancel_Non_BBO=median(abs(mid_returns)))]
    result=cbind(trade_same_px,trade_change_px, submit_improve,submit_BBO, submit_Non_BBO, cancel_worsen, cancel_BBO, cancel_Non_BBO)
  }]
  
  return(return.stat)
}

###-----------------------------------------------------------------------
### In terms of Table 6 and 7, we provide the codes for the SVAR model estimation.





