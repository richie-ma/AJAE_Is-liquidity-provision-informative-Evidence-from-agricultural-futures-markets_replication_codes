#################################################################### 
## This file is used to estimate the structural VAR model for
## "Is liquidity provision informative? Evidence from agricultural
## futures markets" by Richie R. Ma and Teresa Serra
######################################################################

library(data.table)
library(vars)
library(urca)
library(svars)
library(nanotime)

############################################################################################################### 
### In terms of impulse response function, you need to edit the corresponding source code of svars package to
### improve the calculation speed. Specifically, find the following block in the source file, and then add
### temp$B <- B_matrix_new in the block of  else if (x$method == "Cholesky") {}. 
### Using trace(wild.boot, edit = T) to edit the source code.

# bootf <- function(Ustar1) {
# if (design == "fixed") {

#  ...

#   else if (x$method == "Cholesky") {
#     temp <- id.chol(varb, order_k = x$order_k)
#     temp$B <- B_matrix_new
#   }
###############################################################################################################



#trace(wild.boot, edit = T)

rm(list=ls())


##-------------------------------------------------------------------------
## In terms of SVAR estimation, we should estimate the VAR model at first

var <- VAR(message[,..cols], p = lag, type = "none", lag.max = 10, ic="SC")

gc()
##-----------------------------------------------------------------------------

A_matrix <- diag(1, 13) 

### set the estimated elements in the A matrix to NA

A_matrix[1, c(3, 4, 7)] <- NA

svar.1 <- id.chol(var)
svar <- SVAR(var, estmethod="direct", Amat = A_matrix, Bmat=NULL, hessian=TRUE)

B_matrix_new <- solve(svar$A)%*%svar$B

svar.1$B <- B_matrix_new

#---------------- Information shares ---------------------------------------

information.shares <- function(SVAR,message, ...){
  
  
  VMA <- Phi(svar, nstep = 150) 
  
  VMA <- as.array(VMA)
  

  VMA_cum <- VMA[,,1]
  
  for(d in 1:(dim(VMA)[3]-1)){
    
    VMA_cum <- VMA_cum+VMA[,,(d+1) ]
    
    
  }  
  
  ## we could get the Wold representation of SVAR model
  ## SVAR to VMA 
  
  ##-----------------------------------------------------------------
  ## Next, according to Beveridge and Nelson (1981), then the BN decomposition of
  ## VMA is 
  ## y_t=C(1)et+nt
  ### Note that the first element is the part of returns coming from
  ## the permanent random walk process, which could reflect the efficient price
  ## the second part is the noise part
  
  ## Now, we calculate the variance-covariance of the above VMA process,
  
  Omega <- VMA_cum %*% t(VMA_cum)
  
  ## transient price noise is the error term subtracted from the efficient part
  ## generating multivariate normal random sample as epsilon_t
  
  noise_list <- list()
  
  for(p in 1:1000){
    epsilon <- mvrnorm(n=dim(message)[1], mu=rep(0,9),
                       Sigma = diag(1, nrow=9, ncol=9))
    
    noise <- message[, ..cols] - VMA_cum %*% t(epsilon)
    noise <- noise[, 1]
    noise_list[[i]] <- noise
  }
  
  noise <- do.call(cbind, noise_list)
  noise <- rowMeans(noise)
  
  
  ### Here, applying a similar logic of variance decomposition, we calculate the 
  ## variance coming from the Omega matrix, which is the sum of the elements 
  ## at the diagonal
  
  efficient_variance <- Omega[1,1]
  
  info.share <- as.data.table((VMA_cum[1,]^2/efficient_variance)*100) ##shown as percentage
  
  info.share <- data.table::transpose(info.share)
  colnames(info.share)[1:13] <-  cols
  
  result <- list(info.share=info.share, noise=noise)
  
  return(result)
  
}

result.list <- information.shares(svar, message)

info.result <- result.list[[1]]
info.result[, Date:=as.Date(date, "%Y%m%d")]

mid_returns_noise <- result.list[[2]]

#--------------------------- impluse response function ----------------------------------

replication <- 1000
nstep <- 150


IRF <- irf(svar.1, n.ahead = nstep)


irf.cols <- c()

cols.irf <- cols[2:length(cols)]

for (j in 1:length(cols.irf)){
  
  irf.cols[j] <- paste0("epsilon[", " ", cols.irf[j], " ", "]", " ", "%->% mid_returns")
  
  
}

IRF <- IRF$irf
IRF <- as.data.table(IRF)[, ..irf.cols]
colnames(IRF)[1:12] <- cols[2:13]
IRF <- IRF[, lapply(.SD, cumsum), .SDcols=cols[2:13]][.N]
  
ci <- wild.boot(svar.1, design = "fixed", distr = "gaussian", n.ahead = nstep, nboot=replication)


  
  
  ci.sig <- function(ci, nstep, replication, ...){
    
    ci.all <- list()
    
    for(b in 1:length(irf.cols)){
      
      ci.all.variable <- matrix(NA, nrow=nstep , ncol = replication )
      
      
      
      for(a in 1:replication){
        
        ci.each <- c(ci$bootstrap[[a]][["irf"]][[irf.cols[b]]])
        ci.all.variable[1:(nstep), a] <-ci.each
        
        
      }
      
      
      ci.all.variable.025 <- apply(ci.all.variable, 1, function(x) quantile(x, 0.025, na.rm=TRUE))
      ci.all.variable.975 <- apply(ci.all.variable, 1, function(x) quantile(x, 0.975, na.rm=TRUE))
      
      ci.all[[b]] <- data.table(var=irf.cols[b], p0.025=ci.all.variable.025, p0.975=ci.all.variable.975)
      
    
      
      
    }
    
    
    ci.all.cumu <- rbindlist(ci.all)
    ci.all.cumu <- ci.all.cumu[, .(p0.025cum=cumsum(p0.025), p0.975cum=cumsum(p0.975)), by=var]
    
    return(ci.all.cumu)
    
  }
  
  ci.result <- ci.sig(ci, nstep, replication)
  
  ci.result <- ci.result[, .SD[.N, .(p0.025cum, p0.975cum)], by=var]
  
  IRF.result <- cbind(t(IRF), ci.result[, .(p0.025cum, p0.975cum)])
  setnames(IRF.result, "V1", "price_impacts")
  IRF.result[, variables:=cols[2:15]]
  
  IRF.result[, Date:=as.Date(date, "%Y%m%d")]







