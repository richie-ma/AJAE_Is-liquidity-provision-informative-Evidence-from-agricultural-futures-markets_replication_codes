#######################################################################
## This file is used to replicate the variable constructions
## for the structural VAR model for
## "Is liquidity provision informative? Evidence from agricultural
## futures markets" by Richie R. Ma and Teresa Serra 
###################################################################


rm(list=ls())

library(data.table)
library(lubridate)
library(nanotime)

order.book.list <- list.files(path=, pattern = "xcbt")
message.list <- list.files(path=,  pattern = "xcbt")


for(i in 1:length(order.book.list)){

#----------------------------------------------------------------------------------------------------  
  
load(path=, )
  
LOB_date[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                             substr(Time, 15, 16)))]

## You need to calculate the midpoint returns at this stage

LOB_date[, mid_returns:=(log((Bid_PX_1+Ask_PX_1)/2)-shift(log((Bid_PX_1+Ask_PX_1)/2), 1, "lag", fill=NA))*10000]
LOB_date[, wm_mid_returns:=(log((Bid_PX_1*Ask_Qty_1+Ask_PX_1*Bid_Qty_1)/(Bid_Qty_1+Ask_Qty_1))-shift(log((Bid_PX_1*Ask_Qty_1+Ask_PX_1*Bid_Qty_1)/(Bid_Qty_1+Ask_Qty_1)), 1, "lag", fill=NA))*10000]
#LOB_date <- LOB_date[HM %between% c(830,1319) | HM %between% c(1900,2359) | HM %between% c(0,744)]

LOB_MBO <- LOB_date
rm(LOB_date)
#----------------------------------------------------------------------------------------------------------

### order messages

load(path=, )

print(as.Date(message[, unique(Date)], "%Y%m%d"))

message[,HM:= as.numeric(paste0(substr(Time, 12, 13),
                                 substr(Time, 15, 16)))]

setkey(message, Time, MsgSeq, Seq, seq)

#message <- message[HM%between%c(1900, 2359) | HM%between%c(0, 744) | HM%between%c(830, 1319)]

message.trade <- message[bidask==2]
message.trade <- message.trade[, .SD[.N], by=trade_id]


message_outright <- message[bidask=="0"|bidask=="1"]

message_implied <- message[bidask=="E"|bidask=="F"]

setkey(message_implied, Seq)

message_implied[, Qty.change:=Qty-shift(Qty, 1, "lag", fill=0), by=.(bidask,PX)]
message_implied[Type==2, Qty:=-Qty]

#-------------------------------------------------------------------------------------------------------

### Some trade summaries do not include trade aggressor, but it could be inferred by the matched orders. If the 
## matched orders are standing on the bid side, so the trade is sell-initiated. Conversely, if matched orders are
## standing on the ask side, so the trade is buy-initiated.

aggressor <- function(data, message_outright, ...){
  
  
  data[, bidask:=as.numeric(bidask)]
  
  if(dim(data[Type==0 & bidask==2 & agg==0 & is.na(Order_id)==T])[1]!=0){
  
  data <- data[-which(Type==0 & bidask==2 & agg==0 & is.na(Order_id)==T)]
  
  }
  ## If a trade only includes one order information, so this order would be matched orders rather than aggresor orders
  ## as if it was aggressor order, the trading direction can be known
  
  single.detail <- data[Type==0 & bidask==2 & agg==0 & is.na(Order_id)==F, .(Order_id=unique(Order_id)),by=trade_id]
  bidask.info <- message_outright[single.detail, on=.(Order_id)][Type==0|Type==1|Type==2, .(bidask.order=unique(bidask)), by=Order_id]
  single.detail <- bidask.info[single.detail, on=.(Order_id)]
  single.detail <- single.detail[!is.na(bidask.order)]
  data <-single.detail[data, on=.(Order_id, trade_id)]
  data[Type==0 & bidask==2 & agg==0 & is.na(Order_id)==F,agg:=fifelse(bidask.order==0, 2, 1)]
  
  data <- data[!is.na(agg)] 
  data <- data[, -c("bidask.order")]
  return(data)
}

message.trade <- aggressor(message.trade, message_outright)



##############################################################################
# In terms of trades, we need to use the MDP to accurately calculate the midpoint returns
# as the trades are disseminated by the MDP template

LOB_MBO <- merge(LOB_MBO, message[, .(seq, Seq)], by="seq", all.x = TRUE)
LOB_MBO_trade <- LOB_MBO[!is.na(Seq)]

### We need to merge the LOB data with the trade summary to calculate the immediate midquote price
### changes after each trade happens.

message.trade <- LOB_MBO[, .(Seq, wm_mid_returns,mid_returns)][message.trade, on=.(Seq), roll=-Inf]
rm(message)


### next, merge the Limit order book from MBO and message_implied, message_outright


message_outright <- LOB_MBO[, .(seq, wm_mid_returns, mid_returns)][message_outright, on=.(seq), mult="last"]
message_implied <- LOB_MBO[, .(seq, wm_mid_returns, mid_returns)][message_implied, on=.(seq)]

message <- rbind(message_outright,message_implied, message.trade)

setkey(message, Time, MsgSeq, Seq, seq)

### We need to delete messages when best bid price is greater than ask price


delete.seq <- LOB_MBO[Bid_PX_1 > Ask_PX_1, seq]

message <- message[-which(seq %in% delete.seq)]
rm(message.trade, message_implied, message_outright, LOB_MBO_trade)

### Note that in terms of modification messages, we merge the LOB_MBO and message based on the resubmission messages,
### However, we need to locate the initial cancellation as well.


Initial_cancellation <- function(data, LOB_MBO,...){
  
  message_revise <- LOB_MBO[message, on=.(seq), mult="first"]
  message_revise <- message_revise[Type==1 & PX.change.lag!=0 & is.na(PX.change.lag)==F]
  
  message_revise[bidask==0,PX.change.lag.depth:=fifelse(PX.change.lag >= Bid_PX_1, 1, fifelse(PX.change.lag >= Bid_PX_2, 2,
                                                fifelse(PX.change.lag >= Bid_PX_3, 3, fifelse(PX.change.lag >= Bid_PX_4, 4, 
                                                fifelse(PX.change.lag >= Bid_PX_5, 5, fifelse(PX.change.lag >= Bid_PX_6, 6,
                                                fifelse(PX.change.lag >= Bid_PX_7, 7, fifelse(PX.change.lag >= Bid_PX_8, 8, 
                                                fifelse(PX.change.lag >= Bid_PX_9, 9, fifelse(PX.change.lag >= Bid_PX_10, 10, 11))))))))))]
  
  message_revise[bidask==1,PX.change.lag.depth:=fifelse(PX.change.lag <= Ask_PX_1, 1, fifelse(PX.change.lag <= Ask_PX_2, 2,
                                                fifelse(PX.change.lag <= Ask_PX_3, 3, fifelse(PX.change.lag <= Ask_PX_4, 4, 
                                                fifelse(PX.change.lag <= Ask_PX_5, 5, fifelse(PX.change.lag <= Ask_PX_6, 6,
                                                fifelse(PX.change.lag <= Ask_PX_7, 7, fifelse(PX.change.lag <= Ask_PX_8, 8, 
                                                fifelse(PX.change.lag <= Ask_PX_9, 9, fifelse(PX.change.lag <= Ask_PX_10, 10, 11))))))))))]
  
  
  return(message_revise)
}

message_revise <- Initial_cancellation(message, LOB_MBO)
setnames(message_revise, "mid_returns", "mid_returns_2")
setnames(message_revise, "wm_mid_returns", "wm_mid_returns_2")


message_revise <- message_revise[, .(seq, mid_returns_2, wm_mid_returns_2, PX.change.lag.depth)]
message <- message_revise[message, on=.(seq)]

rm(message_revise)


setkey(message, Time, MsgSeq, Seq, seq)
message[, PX.change.lag.depth:=fifelse(is.na(PX.change.lag.depth), 0, PX.change.lag.depth)]
rm(LOB_MBO)

message <- message[HM%between%c(1900, 2359) | HM%between%c(0, 744) | HM%between%c(830, 1319)]

##########################################################################
## we need to construct the following variables
## mid-quote returns in basis points. r_t=log(m_t)-log(m_t-1)

## Trade variables: Marketable orders that change the BBO (Change midquote); Marketable orders that
## do not change the BBO (do not change the midquote) (2 variables)

## Order submission: Improving submission (change the midquote); order submission at BBO (does not change the midquote)
## Order submission at Non-BBO (does not change midquote) (3 variables)

## Order cancellation: Worsening cancellation (change the midquote); order cancellation at BBO (does not change the midquote)
## Order cancellation at Non-BBO (does not change midquote) (3 variables)



if(as.Date(message[, unique(Date)], "%Y%m%d")=="2019-07-03" | as.Date(message[, unique(Date)], "%Y%m%d")=="2019-12-24"){
  
  message_day <- message[HM %between% c(830, 1204)]
  
}else{

message_day <- message[HM %between% c(830, 1319)]

}

message_night <- message[HM %between% c(1900, 2359) | HM %between% c(0, 744)]
rm(message)

print("data processing")


data_preparation <- function(data, ...){

  ## trade variables

  data[,trade:=fifelse(bidask==2 & agg==1, (PX/100)*(Qty*5000)/(10^6), fifelse(bidask==2 & agg==2, -(PX/100)*(Qty*5000)/(10^6), 0))]
  data[,`:=`(trade_same_px=fifelse(mid_returns==0 & trade!=0, trade, 0),
             trade_change_px=fifelse(mid_returns!=0 & trade!=0, trade, 0))]

  ## submission variables

  data[, submit:=fifelse(Type==0 & bidask==0,(PX/100)*(Qty*5000)/(10^6),
                         fifelse(Type==0 & bidask==1, -(PX/100)*(Qty*5000)/(10^6), 0))]


  data[, `:=`(submit_improve=fifelse(mid_returns!=0 & submit!=0, submit, 0),
              submit_BBO=fifelse(mid_returns==0 & submit!=0 & PX_depth==1, submit, 0),
              submit_Non_BBO=fifelse(mid_returns==0 & submit!=0 & PX_depth>1, submit, 0))]

  ### modification variables

  data[, revise_resubmit:=fifelse(Type==1 & bidask==0 & Qty.change>0,(PX/100)*(Qty.change*5000)/(10^6),
                                  fifelse(Type==1 & bidask==1 & Qty.change>0, -(PX/100)*(Qty.change*5000)/(10^6), 0))]

  data[, revise_cancel:=fifelse(Type==1 & bidask==0 & Qty.change<0,(PX/100)*(Qty.change*5000)/(10^6),
                                fifelse(Type==1 & bidask==0 & Qty.change.lag<0, (PX.change.lag/100)*(Qty.change.lag*5000)/(10^6),
                                        fifelse(Type==1 & bidask==1 & Qty.change<0, -(PX/100)*(Qty.change*5000)/(10^6),
                                                fifelse(Type==1 & bidask==1 & Qty.change.lag<0, -(PX.change.lag/100)*(Qty.change.lag*5000)/(10^6),0))))]

  data[, `:=`(revise_submit_improve=fifelse(mid_returns!=0 & revise_resubmit!=0, revise_resubmit, 0),
              revise_submit_BBO=fifelse(mid_returns==0 & revise_resubmit!=0 & PX_depth==1, revise_resubmit, 0),
              revise_submit_Non_BBO=fifelse(mid_returns==0 & revise_resubmit!=0 & PX_depth>1, revise_resubmit, 0))]

  data[, `:=`(revise_cancel_worsen=fifelse(is.na(mid_returns_2)==F & mid_returns_2!=0 & revise_cancel!=0, revise_cancel,
                                           fifelse(is.na(mid_returns_2)==T & mid_returns!=0 & revise_cancel!=0, revise_cancel, 0)),
              revise_cancel_BBO=fifelse(is.na(mid_returns_2)==T & mid_returns==0 & revise_cancel!=0 & PX_depth==1 & Qty.change<0, revise_cancel,
                                        fifelse(is.na(mid_returns_2)==F & mid_returns_2==0 & revise_cancel!=0 & PX.change.lag.depth==1 & Qty.change.lag<0, revise_cancel, 0)),
              revise_cancel_Non_BBO=fifelse(is.na(mid_returns_2)==T & mid_returns==0 & revise_cancel!=0 & PX_depth>1 & Qty.change<0, revise_cancel,
                                            fifelse(is.na(mid_returns_2)==F & mid_returns_2==0 & revise_cancel!=0 & PX.change.lag.depth>1 & Qty.change.lag<0, revise_cancel, 0)))]

  ### deletion variables

  data[, delete:=fifelse(Type==2 & bidask==0,(PX/100)*(Qty*5000)/(10^6),
                         fifelse(Type==2 & bidask==1, -(PX/100)*(Qty*5000)/(10^6), 0))]


  data[, `:=`(delete_worsen=fifelse(mid_returns!=0 & delete!=0, delete, 0),
              delete_BBO=fifelse(mid_returns==0 & delete!=0 & PX_depth==1, delete, 0),
              delete_Non_BBO=fifelse(mid_returns==0 & delete!=0 & PX_depth>1, delete, 0))]


  ### implied liquidity

  data[, implied_provision:=fifelse(Type==0 & bidask=="E",(PX/100)*(Qty*5000)/(10^6),
                                    fifelse(Type==0 & bidask=="F", -(PX/100)*(Qty*5000)/(10^6),
                                            fifelse(Type==1 & bidask=="E" & Qty.change>0,(PX/100)*(Qty.change*5000)/(10^6),
                                                    fifelse(Type==1 & bidask=="F" & Qty.change>0, -(PX/100)*(Qty.change*5000)/(10^6),0))))]

  data[, implied_withdraw:=fifelse(Type==2 & bidask=="E",(PX/100)*(Qty*5000)/(10^6),
                                   fifelse(Type==2 & bidask=="F", -(PX/100)*(Qty*5000)/(10^6),
                                           fifelse(Type==1 & bidask=="E" & Qty.change<0,(PX/100)*(Qty.change*5000)/(10^6),
                                                   fifelse(Type==1 & bidask=="F" & Qty.change<0, -(PX/100)*(Qty.change*5000)/(10^6),0))))]



  data[, `:=`(implied_provision_improve=fifelse(mid_returns!=0 & implied_provision!=0, implied_provision, 0),
              implied_provision_BBO=fifelse(mid_returns==0 & implied_provision!=0 & PX_depth==1, implied_provision, 0),
              implied_provision_Non_BBO=fifelse(mid_returns==0 & implied_provision!=0 & PX_depth>1, implied_provision, 0))]

  data[, `:=`(implied_withdraw_worsen=fifelse(mid_returns!=0 & implied_withdraw!=0, implied_withdraw, 0),
              implied_withdraw_BBO=fifelse(mid_returns==0 & implied_withdraw!=0 & PX_depth==1, implied_withdraw, 0),
              implied_withdraw_Non_BBO=fifelse(mid_returns==0 & implied_withdraw!=0 & PX_depth>1, implied_withdraw, 0))]

  ### Now we are doing the aggregation to reduce the dimension.

  data[, `:=`(submit_improve=fifelse(submit_improve!=0, submit_improve, fifelse(revise_submit_improve!=0, revise_submit_improve,
                                                                                fifelse(implied_provision_improve!=0, implied_provision_improve, 0))),
              submit_BBO=fifelse(submit_BBO!=0, submit_BBO, fifelse(revise_submit_BBO!=0, revise_submit_BBO,
                                                                    fifelse(implied_provision_BBO!=0, implied_provision_BBO, 0))),
              submit_Non_BBO=fifelse(submit_Non_BBO!=0, submit_Non_BBO, fifelse(revise_submit_Non_BBO!=0, revise_submit_Non_BBO,
                                                                                fifelse(implied_provision_Non_BBO!=0, implied_provision_Non_BBO, 0))))]



  data[, `:=`(cancel_worsen=fifelse(delete_worsen!=0, delete_worsen, fifelse(revise_cancel_worsen!=0, revise_cancel_worsen,
                                                                             fifelse(implied_withdraw_worsen!=0, implied_withdraw_worsen, 0))),
              cancel_BBO=fifelse(delete_BBO!=0, delete_BBO, fifelse(revise_cancel_BBO!=0, revise_cancel_BBO,
                                                                    fifelse(implied_withdraw_BBO!=0, implied_withdraw_BBO, 0))),
              cancel_Non_BBO=fifelse(delete_Non_BBO!=0, delete_Non_BBO, fifelse(revise_cancel_Non_BBO!=0, revise_cancel_Non_BBO,
                                                                                fifelse(implied_withdraw_Non_BBO!=0, implied_withdraw_Non_BBO, 0))))]

  data <- data[, .(Date, Time, MsgSeq, Seq, seq, HM,Type, wm_mid_returns,mid_returns,
                   trade_same_px, trade_change_px,
                   submit_improve, submit_BBO, submit_Non_BBO,
                   cancel_worsen, cancel_BBO, cancel_Non_BBO)]

  setkey(data, Time, MsgSeq, Seq, seq)

  return(data)

}


message_day <- data_preparation(message_day)
message_night <- data_preparation(message_night)



}


