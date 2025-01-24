###########################################################################
## This file is used to replicate figures in the main text of
## "Is liquidity provision informative? Evidence from agricultural
## futures markets" by Richie R. Ma and Teresa Serra
############################################################################

rm(list=ls())

library(data.table)


######################################################################
ann.days <-  as.Date(c("2019-02-08","2019-03-08","2019-04-09","2019-05-10",
                       "2019-06-11", "2019-07-11", "2019-08-12","2019-09-12",
                       "2019-10-10","2019-11-08","2019-12-10","2020-01-10",
                       "2020-02-11","2020-03-10","2020-04-09","2020-05-12","2020-06-11"), "%Y-%m-%d")


#############################################################################
## Since this code is same for each market, we just take the corn futures
## market as an example
###############################################################################


############################ PRE announcement ###############################################

##--------------------------------------- 15min ---------------------------------------------
corn.pre.irf.15min <- corn.pre.irf[which(substr(corn.pre.irf, 14, 15)=="15")]
corn.pre.irf.date <- as.Date(substr(corn.pre.irf.15min, 1, 8), "%Y%m%d")
corn.pre.irf.ann <- corn.pre.irf.15min[corn.pre.irf.date %in% ann.days]
corn.pre.irf.non.ann <- corn.pre.irf.15min[corn.pre.irf.date %in% ann.days==F]


corn.pre.irf.ann.list <- list()

for(i in 1:length(corn.pre.irf.ann)){
  
  #load(paste0("P:/announcement/corn/pre_irf/", corn.pre.irf.ann[i])) ## you need to change the path based on your own data, hereafter.
  corn.pre.irf.ann.list[[i]] <- IRF.result
}
corn.pre.irf.ann.list <- rbindlist(corn.pre.irf.ann.list)
corn.pre.irf.ann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.pre.ann.summary <- corn.pre.irf.ann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.pre.ann.summary

corn.pre.irf.nonann.list <- list()

for(i in 1:length(corn.pre.irf.non.ann)){
  
  #load(paste0("P:/announcement/corn/pre_irf/", corn.pre.irf.non.ann[i]))
  corn.pre.irf.nonann.list[[i]] <- IRF.result
}
corn.pre.irf.nonann.list <- rbindlist(corn.pre.irf.nonann.list)
corn.pre.irf.nonann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.pre.non.ann.summary <- corn.pre.irf.nonann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.pre.non.ann.summary

variable <- unique(corn.pre.irf.ann.list$variables)

pvalue <- c()

for(i in 1:length(variable)){

test <- wilcox.test(x=corn.pre.irf.ann.list[variables==variable[i], price_impacts], y=corn.pre.irf.nonann.list[variables==variable[i],price_impacts], paired=F, alternative="two.sided")
pvalue[i] <- test$p.value

}
test_15min <- data.table(vars=variable, pvalue=pvalue, interval=-15)

summary_15min <- merge(corn.pre.ann.summary, corn.pre.non.ann.summary, by="variables")
summary_15min <- summary_15min[, .(diff=price.impacts.x-price.impacts.y), by=.(variables)]
summary_15min <- summary_15min[, interval:=-15]



#-------------------------------------- 30min -----------------------------------
corn.pre.irf.30min <- corn.pre.irf[which(substr(corn.pre.irf, 14, 15)=="30")]
corn.pre.irf.date <- as.Date(substr(corn.pre.irf.30min, 1, 8), "%Y%m%d")
corn.pre.irf.ann <- corn.pre.irf.30min[corn.pre.irf.date %in% ann.days]
corn.pre.irf.non.ann <- corn.pre.irf.30min[corn.pre.irf.date %in% ann.days==F]


corn.pre.irf.ann.list <- list()

for(i in 1:length(corn.pre.irf.ann)){
  
  #load(paste0("P:/announcement/corn/pre_irf/", corn.pre.irf.ann[i]))
  corn.pre.irf.ann.list[[i]] <- IRF.result
}
corn.pre.irf.ann.list <- rbindlist(corn.pre.irf.ann.list)
corn.pre.irf.ann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.pre.ann.summary <- corn.pre.irf.ann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.pre.ann.summary

corn.pre.irf.nonann.list <- list()

for(i in 1:length(corn.pre.irf.non.ann)){
  
  #load(paste0("P:/announcement/corn/pre_irf/", corn.pre.irf.non.ann[i]))
  corn.pre.irf.nonann.list[[i]] <- IRF.result
}
corn.pre.irf.nonann.list <- rbindlist(corn.pre.irf.nonann.list)
corn.pre.irf.nonann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.pre.non.ann.summary <- corn.pre.irf.nonann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.pre.non.ann.summary

variable <- unique(corn.pre.irf.ann.list$variables)

pvalue <- c()

for(i in 1:length(variable)){
  
  test <- wilcox.test(x=corn.pre.irf.ann.list[variables==variable[i], price_impacts], y=corn.pre.irf.nonann.list[variables==variable[i],price_impacts], paired=F, alternative="two.sided")
  pvalue[i] <- test$p.value
  
}
test_30min <- data.table(vars=variable, pvalue=pvalue, interval=-30)


summary_30min <- merge(corn.pre.ann.summary, corn.pre.non.ann.summary, by="variables")
summary_30min <- summary_30min[, .(diff=price.impacts.x-price.impacts.y), by=.(variables)]
summary_30min <- summary_30min[, interval:=-30]

##----------------------------------------------- 60min--------------------------------------------------
corn.pre.irf.60min <- corn.pre.irf[which(substr(corn.pre.irf, 14, 15)=="60")]
corn.pre.irf.date <- as.Date(substr(corn.pre.irf.60min, 1, 8), "%Y%m%d")
corn.pre.irf.ann <- corn.pre.irf.60min[corn.pre.irf.date %in% ann.days]
corn.pre.irf.non.ann <- corn.pre.irf.60min[corn.pre.irf.date %in% ann.days==F]


corn.pre.irf.ann.list <- list()

for(i in 1:length(corn.pre.irf.ann)){
  
  #load(paste0("P:/announcement/corn/pre_irf/", corn.pre.irf.ann[i]))
  corn.pre.irf.ann.list[[i]] <- IRF.result
}
corn.pre.irf.ann.list <- rbindlist(corn.pre.irf.ann.list)
corn.pre.irf.ann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.pre.ann.summary <- corn.pre.irf.ann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.pre.ann.summary

corn.pre.irf.nonann.list <- list()

for(i in 1:length(corn.pre.irf.non.ann)){
  
  #load(paste0("P:/announcement/corn/pre_irf/", corn.pre.irf.non.ann[i]))
  corn.pre.irf.nonann.list[[i]] <- IRF.result
}
corn.pre.irf.nonann.list <- rbindlist(corn.pre.irf.nonann.list)
corn.pre.irf.nonann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.pre.non.ann.summary <- corn.pre.irf.nonann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.pre.non.ann.summary

variable <- unique(corn.pre.irf.ann.list$variables)

pvalue <- c()

for(i in 1:length(variable)){
  
  test <- wilcox.test(x=corn.pre.irf.ann.list[variables==variable[i], price_impacts], y=corn.pre.irf.nonann.list[variables==variable[i],price_impacts], paired=F, alternative="two.sided")
  pvalue[i] <- test$p.value
  
}
test_60min <- data.table(vars=variable, pvalue=pvalue, interval=-60)


summary_60min <- merge(corn.pre.ann.summary, corn.pre.non.ann.summary, by="variables")
summary_60min <- summary_60min[, .(diff=price.impacts.x-price.impacts.y), by=.(variables)]
summary_60min <- summary_60min[, interval:=-60]

##----------------------------------------------- 90min--------------------------------------------------
corn.pre.irf.90min <- corn.pre.irf[which(substr(corn.pre.irf, 14, 15)=="90")]
corn.pre.irf.date <- as.Date(substr(corn.pre.irf.90min, 1, 8), "%Y%m%d")
corn.pre.irf.ann <- corn.pre.irf.90min[corn.pre.irf.date %in% ann.days]
corn.pre.irf.non.ann <- corn.pre.irf.90min[corn.pre.irf.date %in% ann.days==F]


corn.pre.irf.ann.list <- list()

for(i in 1:length(corn.pre.irf.ann)){
  
  #load(paste0("P:/announcement/corn/pre_irf/", corn.pre.irf.ann[i]))
  corn.pre.irf.ann.list[[i]] <- IRF.result
}
corn.pre.irf.ann.list <- rbindlist(corn.pre.irf.ann.list)
corn.pre.irf.ann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.pre.ann.summary <- corn.pre.irf.ann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.pre.ann.summary

corn.pre.irf.nonann.list <- list()

for(i in 1:length(corn.pre.irf.non.ann)){
  
 # load(paste0("P:/announcement/corn/pre_irf/", corn.pre.irf.non.ann[i]))
  corn.pre.irf.nonann.list[[i]] <- IRF.result
}
corn.pre.irf.nonann.list <- rbindlist(corn.pre.irf.nonann.list)
corn.pre.irf.nonann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.pre.non.ann.summary <- corn.pre.irf.nonann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.pre.non.ann.summary


variable <- unique(corn.pre.irf.ann.list$variables)

pvalue <- c()

for(i in 1:length(variable)){
  
  test <- wilcox.test(x=corn.pre.irf.ann.list[variables==variable[i], price_impacts], y=corn.pre.irf.nonann.list[variables==variable[i],price_impacts], paired=F, alternative="two.sided")
  pvalue[i] <- test$p.value
  
}
test_90min <- data.table(vars=variable, pvalue=pvalue, interval=-90)


summary_90min <- merge(corn.pre.ann.summary, corn.pre.non.ann.summary, by="variables")
summary_90min <- summary_90min[, .(diff=price.impacts.x-price.impacts.y), by=.(variables)]
summary_90min <- summary_90min[, interval:=-90]

summary.pre.irf <- rbind(summary_15min, summary_30min, summary_60min, summary_90min)
test.pre.irf <- rbind(test_15min, test_30min, test_60min, test_90min)


############################ POST announcement ###############################################

ann.days <-  as.Date(c("2019-02-08","2019-03-08","2019-04-09","2019-05-10",
                       "2019-06-11", "2019-07-11", "2019-08-12","2019-09-12",
                       "2019-10-10","2019-11-08","2019-12-10","2020-01-10",
                       "2020-02-11","2020-03-10","2020-04-09","2020-05-12","2020-06-11"), "%Y-%m-%d")



##--------------------------------------- 15min ---------------------------------------------
corn.post.irf.15min <- corn.post.irf[which(substr(corn.post.irf, 15, 16)=="15")]
corn.post.irf.date <- as.Date(substr(corn.post.irf.15min, 1, 8), "%Y%m%d")
corn.post.irf.ann <- corn.post.irf.15min[corn.post.irf.date %in% ann.days]
corn.post.irf.non.ann <- corn.post.irf.15min[corn.post.irf.date %in% ann.days==F]


corn.post.irf.ann.list <- list()

for(i in 1:length(corn.post.irf.ann)){
  
 # load(paste0("P:/announcement/corn/post_irf/", corn.post.irf.ann[i]))
  corn.post.irf.ann.list[[i]] <- IRF.result
}
corn.post.irf.ann.list <- rbindlist(corn.post.irf.ann.list)
corn.post.irf.ann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.post.ann.summary <- corn.post.irf.ann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.post.ann.summary

corn.post.irf.nonann.list <- list()

for(i in 1:length(corn.post.irf.non.ann)){
  
 # load(paste0("P:/announcement/corn/post_irf/", corn.post.irf.non.ann[i]))
  corn.post.irf.nonann.list[[i]] <- IRF.result
}
corn.post.irf.nonann.list <- rbindlist(corn.post.irf.nonann.list)
corn.post.irf.nonann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.post.non.ann.summary <- corn.post.irf.nonann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.post.non.ann.summary

variable <- unique(corn.post.irf.ann.list$variables)

pvalue <- c()

for(i in 1:length(variable)){
  
  test <- wilcox.test(x=corn.post.irf.ann.list[variables==variable[i], price_impacts], y=corn.post.irf.nonann.list[variables==variable[i],price_impacts], paired=F, alternative="two.sided")
  pvalue[i] <- test$p.value
  
}
test_15min <- data.table(vars=variable, pvalue=pvalue, interval=15)

summary_15min <- merge(corn.post.ann.summary, corn.post.non.ann.summary, by="variables")
summary_15min <- summary_15min[, .(diff=price.impacts.x-price.impacts.y), by=.(variables)]
summary_15min <- summary_15min[, interval:=15]


#-------------------------------------- 30min -----------------------------------
corn.post.irf.30min <- corn.post.irf[which(substr(corn.post.irf, 15, 16)=="30")]
corn.post.irf.date <- as.Date(substr(corn.post.irf.30min, 1, 8), "%Y%m%d")
corn.post.irf.ann <- corn.post.irf.30min[corn.post.irf.date %in% ann.days]
corn.post.irf.non.ann <- corn.post.irf.30min[corn.post.irf.date %in% ann.days==F]


corn.post.irf.ann.list <- list()

for(i in 1:length(corn.post.irf.ann)){
  
 # load(paste0("P:/announcement/corn/post_irf/", corn.post.irf.ann[i]))
  corn.post.irf.ann.list[[i]] <- IRF.result
}
corn.post.irf.ann.list <- rbindlist(corn.post.irf.ann.list)
corn.post.irf.ann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.post.ann.summary <- corn.post.irf.ann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.post.ann.summary

corn.post.irf.nonann.list <- list()

for(i in 1:length(corn.post.irf.non.ann)){
  
#  load(paste0("P:/announcement/corn/post_irf/", corn.post.irf.non.ann[i]))
  corn.post.irf.nonann.list[[i]] <- IRF.result
}
corn.post.irf.nonann.list <- rbindlist(corn.post.irf.nonann.list)
corn.post.irf.nonann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.post.non.ann.summary <- corn.post.irf.nonann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.post.non.ann.summary

variable <- unique(corn.post.irf.ann.list$variables)

pvalue <- c()

for(i in 1:length(variable)){
  
  test <- wilcox.test(x=corn.post.irf.ann.list[variables==variable[i], price_impacts], y=corn.post.irf.nonann.list[variables==variable[i],price_impacts], paired=F, alternative="two.sided")
  pvalue[i] <- test$p.value
  
}
test_30min <- data.table(vars=variable, pvalue=pvalue, interval=30)


summary_30min <- merge(corn.post.ann.summary, corn.post.non.ann.summary, by="variables")
summary_30min <- summary_30min[, .(diff=price.impacts.x-price.impacts.y), by=.(variables)]
summary_30min <- summary_30min[, interval:=30]

##----------------------------------------------- 60min--------------------------------------------------
corn.post.irf.60min <- corn.post.irf[which(substr(corn.post.irf, 15, 16)=="60")]
corn.post.irf.date <- as.Date(substr(corn.post.irf.60min, 1, 8), "%Y%m%d")
corn.post.irf.ann <- corn.post.irf.60min[corn.post.irf.date %in% ann.days]
corn.post.irf.non.ann <- corn.post.irf.60min[corn.post.irf.date %in% ann.days==F]


corn.post.irf.ann.list <- list()

for(i in 1:length(corn.post.irf.ann)){
  
#  load(paste0("P:/announcement/corn/post_irf/", corn.post.irf.ann[i]))
  corn.post.irf.ann.list[[i]] <- IRF.result
}
corn.post.irf.ann.list <- rbindlist(corn.post.irf.ann.list)
corn.post.irf.ann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.post.ann.summary <- corn.post.irf.ann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.post.ann.summary

corn.post.irf.nonann.list <- list()

for(i in 1:length(corn.post.irf.non.ann)){
  
 # load(paste0("P:/announcement/corn/post_irf/", corn.post.irf.non.ann[i]))
  corn.post.irf.nonann.list[[i]] <- IRF.result
}
corn.post.irf.nonann.list <- rbindlist(corn.post.irf.nonann.list)
corn.post.irf.nonann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.post.non.ann.summary <- corn.post.irf.nonann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.post.non.ann.summary

variable <- unique(corn.post.irf.ann.list$variables)

pvalue <- c()

for(i in 1:length(variable)){
  
  test <- wilcox.test(x=corn.post.irf.ann.list[variables==variable[i], price_impacts], y=corn.post.irf.nonann.list[variables==variable[i],price_impacts], paired=F, alternative="two.sided")
  pvalue[i] <- test$p.value
  
}
test_60min <- data.table(vars=variable, pvalue=pvalue, interval=60)

summary_60min <- merge(corn.post.ann.summary, corn.post.non.ann.summary, by="variables")
summary_60min <- summary_60min[, .(diff=price.impacts.x-price.impacts.y), by=.(variables)]
summary_60min <- summary_60min[, interval:=60]

##----------------------------------------------- 90min--------------------------------------------------
corn.post.irf.90min <- corn.post.irf[which(substr(corn.post.irf, 15, 16)=="90")]
corn.post.irf.date <- as.Date(substr(corn.post.irf.90min, 1, 8), "%Y%m%d")
corn.post.irf.ann <- corn.post.irf.90min[corn.post.irf.date %in% ann.days]
corn.post.irf.non.ann <- corn.post.irf.90min[corn.post.irf.date %in% ann.days==F]


corn.post.irf.ann.list <- list()

for(i in 1:length(corn.post.irf.ann)){
  
  #load(paste0("P:/announcement/corn/post_irf/", corn.post.irf.ann[i]))
  corn.post.irf.ann.list[[i]] <- IRF.result
}
corn.post.irf.ann.list <- rbindlist(corn.post.irf.ann.list)
corn.post.irf.ann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.post.ann.summary <- corn.post.irf.ann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.post.ann.summary

corn.post.irf.nonann.list <- list()

for(i in 1:length(corn.post.irf.non.ann)){
  
 # load(paste0("P:/announcement/corn/post_irf/", corn.post.irf.non.ann[i]))
  corn.post.irf.nonann.list[[i]] <- IRF.result
}
corn.post.irf.nonann.list <- rbindlist(corn.post.irf.nonann.list)
corn.post.irf.nonann.list[p0.025cum <0 & p0.975cum>0, price_impacts:=0]
corn.post.non.ann.summary <- corn.post.irf.nonann.list[, .(price.impacts=median(price_impacts)), by=.(variables)]
corn.post.non.ann.summary

variable <- unique(corn.post.irf.ann.list$variables)

pvalue <- c()

for(i in 1:length(variable)){
  
  test <- wilcox.test(x=corn.post.irf.ann.list[variables==variable[i], price_impacts], y=corn.post.irf.nonann.list[variables==variable[i],price_impacts], paired=F, alternative="two.sided")
  pvalue[i] <- test$p.value
  
}
test_90min <- data.table(vars=variable, pvalue=pvalue, interval=90)

summary_90min <- merge(corn.post.ann.summary, corn.post.non.ann.summary, by="variables")
summary_90min <- summary_90min[, .(diff=price.impacts.x-price.impacts.y), by=.(variables)]
summary_90min <- summary_90min[, interval:=90]


summary.post.irf <- rbind(summary_15min, summary_30min, summary_60min, summary_90min)
test.post.irf <- rbind(test_15min, test_30min, test_60min, test_90min)

###########################################################################################

summary.irf <- rbind(summary.pre.irf, summary.post.irf)
summary.irf <- dcast(summary.irf, interval~variables, value.var = "diff")

test.irf <- rbind(test.pre.irf, test.post.irf)
test.irf <- dcast(test.irf, interval~vars, value.var = "pvalue")

