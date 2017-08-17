##set path
setwd("E:/Rfile/testfile")

# install.packages("openxlsx")
library(openxlsx)
# install.packages("plyr")
library(plyr)

# install.packages("randomForest")
library(randomForest)
###########################
##get data
n=8
k=4
for (i in 1:n){
  d <- read.xlsx("zj_1M_2M_new.xlsx",i)[,c(2,4:31)]
  colnames(d) <- c("time","monitor","monitorIp","visit",
                   "file_size","trans_time",colnames(d)[7:29])
  
  # d$group <- substr(d$time, 1, 2)

  a <- floor(as.numeric(substr(d$time, 4, 5)) / 3)  #Group by hour (3hours)
  d$groupByH <- paste0(substr(d$time,1,3),a)
  d <- d[,c(1,30,2:29)]

  
  ###set filesize: delete NA
  if (i<=k){
    t = which(d$file_size == 1027.813)        ##10M =10246.5   1M=1027.813 200k=205.563
    d <- d[t,]
  }else{
    t = which(d$file_size == 2055.625)        ##10M =10246.5   2M=2055.625
    d <- d[t,]
  }
 
  
  d$trans_time <- round(as.numeric(d$trans_time),3)
  t <- which(is.na(d$trans_time))
  if(length(t)>=1){
    d <- d[-t,]
  }  
  
  # t <- which(d$trans_time>50) 
  # d$trans_time[t] <- 50 
  
  d$retrans_rate <- round(d$retrans_bytes/d$trans_bytes,3)
  d <- d[,c(1:10,31,11:30)]
  assign(paste0("d", i), d)
}

load("E:/Rfile/testfile/rf_model.Rdata")
for(i in 1:n){
  # test <- as.data.frame(D[i])
  test <- get(paste0("d",i))
  
  pred_y <- round(predict(model.rf, test),3)
  # t <- which(pred_y>50)
  # pred_y[t] <- 50
  pred <- data.frame(time=test$time, group=test$group, visit=test$visit, 
                     file_size=test$file_size, real_y=test$trans_time, pred_y=pred_y)
  assign(paste0("pred", i), pred)
}

###################################################
#转为分数
m=n/k
for(i in 1:m){
  for(j in 1:k){
    t <- get(paste0("pred",(i-1)*4+j))
    if(j==1){
      groupT <- as.matrix(t$group)
      real <- as.matrix(t$real_y) 
      pred <- as.matrix(t$pred_y)
    }else{
      groupT <- rbind(groupT,as.matrix(t$group))
      real <- rbind(real,as.matrix(t$real_y))
      pred <- rbind(pred,as.matrix(t$pred_y))
    }
  }
 
  
  d<-data.frame(groupT=groupT, real=real, pred=pred)
  
  ##count percent by group on pred
  a=0.1
  p <- aggregate(d$pred,list(d$groupT),function(x){
    return(quantile(x,a))
  })
  
  for (j in 2:9) {
    a=a+0.1
    p[,j+1] <- aggregate(d$pred,list(d$groupT),function(x){
      return(quantile(x,a))
    })[,2]
    
  }
  names(p) <- c("groupH","10%","20%","30%","40%","50%","60%","70%","80%","90%")
  
  if(i==1){
    assign(paste0("p_", i), p)
  }else if(i==2){
    assign(paste0("p_", i), p)
  }else if(i==3){
    assign(paste0("p_", i), p)
  }else if(i==4){
    assign(paste0("p_", i), p)
  }
  ##count percent by group on real
  a=0.1
  r <- aggregate(d$real,list(d$groupT),function(x){
    return(quantile(x,a))
  })
  
  for (j in 2:9) {
    a=a+0.1
    r[,j+1] <- aggregate(d$real,list(d$groupT),function(x){
      return(quantile(x,a))
    })[,2]
    
  }
  names(r) <- c("groupH","10%","20%","30%","40%","50%","60%","70%","80%","90%")
  
  if(i==1){
    assign(paste0("r_", i), r)
  }else if(i==2){
    assign(paste0("r_", i), r)
  }else if(i==3){
    assign(paste0("r_", i), r)
  }else if(i==4){
    assign(paste0("r_", i), r)
  }
}
rm(d, t, groupT, real, pred)

#####transtime -> score
for(i in 1:n){
  d <- get(paste0("pred",i))
  ###real
  for (j in 1:nrow(d)) {
    if(i<=k){
      l <- which(r_1[,1]==d[j,2])
      u <- r_1[l,]
    }
    else if(i<=2*k){
      l <- which(r_2[,1]==d[j,2])
      u <- r_2[l,]      
    }
    else if(i<=3*k){
      l <- which(r_3[,1]==d[j,2])
      u <- r_3[l,]      
    }else{
      l <- which(r_4[,1]==d[j,2])
      u <- r_4[l,]      
    }
    
    ###real
    x <- d[j,5]
    if(x < u[2]){
      d[j,5] = 5
    }
    else if(x < u[3]){
      d[j,5] = 4
    }
    else if(x < u[4]){
      d[j,5] = 3
    }
    else if(x < u[5]){
      d[j,5] = 2
    }
    else if(x < u[6]){
      d[j,5] = 1
    }
    else if(x < u[7]){
      d[j,5] = 0
    }
    else if(x < u[8]){
      d[j,5] = -1
    }
    else if(x < u[9]){
      d[j,5] = -2
    }
    else if(x < u[10]){
      d[j,5] = -3
    }
    else{
      d[j,5] = -4
    }
  }
  
  ###pred
  for (j in 1:nrow(d)) {
    if(i<=k){
      l <- which(p_1[,1]==d[j,2])
      u <- p_1[l,]
    }
    else if(i<=2*k){
      l <- which(p_2[,1]==d[j,2])
      u <- p_2[l,]      
    }
    else if(i<=3*k){
      l <- which(p_3[,1]==d[j,2])
      u <- p_3[l,]      
    }else{
      l <- which(p_4[,1]==d[j,2])
      u <- p_4[l,]
    }
    
    x <- d[j,6]
    if(x < u[2]){
      d[j,6] = 5
    }
    else if(x < u[3]){
      d[j,6] = 4
    }
    else if(x < u[4]){
      d[j,6] = 3
    }
    else if(x < u[5]){
      d[j,6] = 2
    }
    else if(x < u[6]){
      d[j,6] = 1
    }
    else if(x < u[7]){
      d[j,6] = 0
    }
    else if(x < u[8]){
      d[j,6] = -1
    }
    else if(x < u[9]){
      d[j,6] = -2
    }
    else if(x < u[10]){
      d[j,6] = -3
    }
    else{
      d[j,6] = -4
    }
  }
  
  assign(paste0("d_rp", i), d)
}

####


for(i in 1:n){
  d <- get(paste0("d_rp",i))
  t <- aggregate(d[,c(4:6)], list(d$group), mean)
  assign(paste0("t", i), t)
}

if(n==8){
  ##real_y
  F_1_r <- data.frame(groupT=t1$Group.1, A = t1$real_y, B = t2$real_y, 
                      C = t3$real_y, D = t4$real_y)
  
  F_2_r <- data.frame(groupT=t5$Group.1, A = t5$real_y, B = t6$real_y, 
                      C = t7$real_y, D = t8$real_y)
  ##pred_y
  F_1_p <- data.frame(groupT=t1$Group.1, A = t1$pred_y, B = t2$pred_y, 
                      C = t3$pred_y, D = t4$pred_y)
  
  F_2_p <- data.frame(groupT=t5$Group.1, A = t5$pred_y, B = t6$pred_y, 
                      C = t7$pred_y, D = t8$pred_y)
}
if(n==12){
  ##real
  F_1_r <- data.frame(groupH=t1$Group.1, ls_200k = t1$real_y, nb_200k = t2$real_y, 
                      sx_200k = t3$real_y, wz_200k = t4$real_y)
  
  F_2_r <- data.frame(groupH=t5$Group.1, ls_1M = t5$real_y, nb_1M = t6$real_y, 
                      sx_1M = t7$real_y, wz_1M = t8$real_y)
  
  F_3_r <- data.frame(groupH=t9$Group.1, ls_5M = t9$real_y, nb_5M = t10$real_y, 
                      sx_5M = t11$real_y, wz_5M = t12$real_y)
  
  ##pred
  F_1_p <- data.frame(groupH=t1$Group.1, ls_200k = t1$pred_y, nb_200k = t2$pred_y, 
                      sx_200k = t3$pred_y, wz_200k = t4$pred_y)
  
  F_2_p <- data.frame(groupH=t5$Group.1, ls_1M = t5$pred_y, nb_1M = t6$pred_y, 
                      sx_1M = t7$pred_y, wz_1M = t8$pred_y)
  
  F_3_p <- data.frame(groupH=t9$Group.1, ls_5M = t9$pred_y, nb_5M = t10$pred_y, 
                      sx_5M = t11$pred_y, wz_5M = t12$pred_y)  
}


for(i in 1:n){
  d <- get(paste0("pred",i))
  t <- aggregate(d[,c(5,6)], list(d$group), mean)
  assign(paste0("t", i), t)
}


if(n==8){
  ##real_y
  T_1_r <- data.frame(groupT=t1$Group.1, A = t1$real_y, B = t2$real_y, 
                      C = t3$real_y, D = t4$real_y)
  
  T_2_r <- data.frame(groupT=t5$Group.1, A = t5$real_y, B = t6$real_y, 
                      C = t7$real_y, D = t8$real_y)
  ##pred_y
  T_1_p <- data.frame(groupT=t1$Group.1, A = t1$pred_y, B = t2$pred_y, 
                      C = t3$pred_y, D = t4$pred_y)
  
  T_2_p <- data.frame(groupT=t5$Group.1, A = t5$pred_y, B = t6$pred_y, 
                      C = t7$pred_y, D = t8$pred_y)
}else{
  ##real
  T_1_r <- data.frame(groupH=t1$Group.1, ls_200k = t1$real_y, nb_200k = t2$real_y, 
                      sx_200k = t3$real_y, wz_200k = t4$real_y)
  
  T_2_r <- data.frame(groupH=t5$Group.1, ls_1M = t5$real_y, nb_1M = t6$real_y, 
                      sx_1M = t7$real_y, wz_1M = t8$real_y)
  
  T_3_r <- data.frame(groupH=t9$Group.1, ls_5M = t9$real_y, nb_5M = t10$real_y, 
                      sx_5M = t11$real_y, wz_5M = t12$real_y)
  
  ##pred
  T_1_p <- data.frame(groupH=t1$Group.1, ls_200k = t1$pred_y, nb_200k = t2$pred_y, 
                      sx_200k = t3$pred_y, wz_200k = t4$pred_y)
  
  T_2_p <- data.frame(groupH=t5$Group.1, ls_1M = t5$pred_y, nb_1M = t6$pred_y, 
                      sx_1M = t7$pred_y, wz_1M = t8$pred_y)
  
  T_3_p <- data.frame(groupH=t9$Group.1, ls_5M = t9$pred_y, nb_5M = t10$pred_y, 
                      sx_5M = t11$pred_y, wz_5M = t12$pred_y)  
}

# D <-list(d1, d2, d3, d4)
# P <- list(pred1,pred2,pred3,pred4)
# for(i in 1:n){
#   d <- as.data.frame(D[i])
#   p <- as.data.frame(P[i])
#   d <- cbind(d[,c(1:8)], pred_y=p$pred_y, d[,c(9:32)])
#   assign(paste0("d", i), d)
# }
# 
# D <-list(d1, d2, d3, d4)
# P <- list(d_rp1, d_rp2, d_rp3, d_rp4)
# for(i in 1:n){
#   d <- as.data.frame(D[i])
#   p <- as.data.frame(P[i])
#   d <- cbind(d[,c(1:9)], score_real=p$real_y, score_pred=p$pred_y, d[,c(10:33)])
#   assign(paste0("d", i), d)
# }

#################################################################################????
p1 = getwd()
mypath <- "/savexlsx/"

fn  = "zj_1M_2M_avgTranstimeGroupBy3H"           
fname <- paste0(p1, mypath, fn, ".xlsx")              
wb <- createWorkbook()  

T1 <- cbind(T_1_r,T_1_p)
f='T_5M_rp'
addWorksheet(wb, f)                
writeData(wb, f, T1)           

T1 <- cbind(T_2_r,T_2_p)
f='T_10M_rp'
addWorksheet(wb, f)   
writeData(wb, f, T1)           

if(n==12){
  T1 <- cbind(T_3_r,T_3_p)
  f='T_5M_rp'
  addWorksheet(wb, f)   
  writeData(wb, f, T1)           
}


F1 <- cbind(F_1_r,F_1_p)
f='F_5M_rp'
addWorksheet(wb, f)                 
writeData(wb, f, F1)          

F2 <- cbind(F_2_r,F_2_p)
f='F_10M_rp'
addWorksheet(wb, f)   
writeData(wb, f, F2)            

if(n==12){
  F1 <- cbind(F_3_r,F_3_p)
  f='F_5M_rp'
  addWorksheet(wb, f)   
  writeData(wb, f, F1)           
}

saveWorkbook(wb, fname, overwrite = TRUE)
rm(f,fn,fname,mypath,p1,wb)



