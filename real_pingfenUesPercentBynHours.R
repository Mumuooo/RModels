
# setwd("E:/Rworks/1116apply")
# library(openxlsx)
# library(plyr)
# library(randomForest)
###########################
##Ԥ??
# load("E:/Rworks/1116apply/join_15000_model.Rdata")

#
fname="newdata"  #文件夹
allfilesname=list.files(fname)  #文件列表
n=length(allfilesname)

fn <- vector(mode = "character",n)   #获取列表各文件名
for (i in 1:n){
  str=allfilesname[i]
  areaname=unlist(strsplit(str,split="[.]"))
  fn[i] = substring(areaname[1],8)
}

D <- list()
for (i in 1:n){
  d <- read.csv(file=paste(fname,'/',allfilesname[i],sep=''))
  d$retrans_rate <- round(d$retrans_bytes/d$trans_bytes,3)
  d <- d[,c(1:5,26,6:25)]
  
  a=as.character(d[,1])       #获取时间
  a=unlist(strsplit(a,split="-")) #将日期和时间分开
  a=matrix(a,ncol=2,byrow=T)
  
  b=unlist(strsplit(as.character(a[,2]), split = ":"))    #GroupBy3H
  b=matrix(b,ncol=3,byrow=T)                              #ֵ
  b=floor(as.numeric(b[,1])/3)
  b=paste0(a[,1],"-",b)
  
  d <- data.frame(groupBy3H=b,day=a[,1],d)
  d <- d[,c(3,1:2,4:28)]
  D <- c(D,list(d))
}


##估算传输时间
P <- list()
for(i in 1:n){
  d <- as.data.frame(D[i])
  pred_y <- round(predict(model.rf, d),3)
  p <- data.frame(time=d[,1], groupH=d[,2], day=d[,3], trans_bytes=d$trans_bytes, pred_y=pred_y)
  P <- c(P,list(p))
}

##将pred_y加到列表中
DP <- list()
for(i in 1:n){
  d <- as.data.frame(D[i])
  p <- as.data.frame(P[i])
  d <- cbind(d[,c(1:4)], pred_y=p$pred_y, d[,c(5:28)])
  DP <- c(DP,list(d))
}


##计算平均传输时间
Avg_t = list()
for(i in 1:n){
  p <- as.data.frame(P[i])
  t <- aggregate(p[,c(4,5)], list(p$day), mean)
  Avg_t <- c(Avg_t, list(t))
}

for(i in 1:n){
  t <- data.frame(Avg_t[i])
  t[,3] <- round(t[,3],3)
  if(i==1){
    avg_allt <- t[,c(1,3)]
  }else{
    avg_allt <- left_join(avg_allt,t[,c(1,3)],by="Group.1")
  }
}

colnames(avg_allt) <- c("group",fn)


###############################################################################################################################

#####ת??Ϊ????

###??????ʱ????ͬһ?ļ???С???з??ʵ???ƽ????
k=2
m=n/k;

for(i in 1:m){
  t1 <- as.data.frame(P[(i-1)*k+1]) 
  if(k>=2){
    t2 <- as.data.frame(P[(i-1)*k+2])     
  }
  if(k>=3){
    t3 <- as.data.frame(P[(i-1)*k+3])
  }
  if(k>=4){
    t4 <- as.data.frame(P[(i-1)*k+4])
  }
  if(k==2){
    groupH <- rbind(as.matrix(t1$groupH), as.matrix(t2$groupH))
    pred<-rbind(as.matrix(t1$pred_y), as.matrix(t2$pred_y)) 
    
  }else if(k==3){
    groupH <- rbind(as.matrix(t1$groupH), as.matrix(t2$groupH), as.matrix(t3$groupH))
    pred<-rbind(as.matrix(t1$pred_y), as.matrix(t2$pred_y), as.matrix(t3$pred_y))    
    
  }else if(k==4){
    groupH <- rbind(as.matrix(t1$groupH), as.matrix(t2$groupH),
                  as.matrix(t3$groupH), as.matrix(t4$groupH))
    pred <- rbind(as.matrix(t1$pred_y), as.matrix(t2$pred_y),
                as.matrix(t3$pred_y), as.matrix(t4$pred_y))
  }

  d<-data.frame(groupH=groupH, pred=pred)
  
 #????ÿnHours??10????λ??
  a=0.1
  t <- aggregate(d$pred,list(d$groupH),function(x){
    return(quantile(x,a))
  })
  
  for (j in 2:9) {
    a=a+0.1
    t[,j+1] <- aggregate(d$pred,list(d$groupH),function(x){
      return(quantile(x,a))
    })[,2]
    
  }
  names(t) <- c("groupH","10%","20%","30%","40%","50%","60%","70%","80%","90%")


  if(i==1){
    assign(paste0("t_", i), t)
    t_1$groupH = as.character(t_1$groupH)
  }else if(i==2){
    assign(paste0("t_", i), t)
    t_2$groupH = as.character(t_2$groupH)
  }else if(i==3){
    assign(paste0("t_", i), t)
    t_3$groupH = as.character(t_3$groupH)
  }else if(i==4){
    assign(paste0("t_", i), t)
    t_4$groupH = as.character(t_4$groupH)
  }else if(i==5){
    assign(paste0("t_", i), t)
    t_5$groupH = as.character(t_5$groupH)
  }else if(i==6){
    assign(paste0("t_", i), t)
    t_6$groupH = as.character(t_6$groupH)
  }
  
}
rm(d, t, t1, t2, groupH, pred)



#####ת??Ϊ????????
Pc <- list()
for(i in 1:n){
  p <- as.data.frame(P[i])
  p$groupH <- as.character(p$groupH)
  ###real
  for (j in 1:nrow(p)) {
    ###ȡ????Ӧʱ????ƽ????
    if(i<=k){
      l <- which(t_1[,1]==p[j,2])
      u <- t_1[l,]
    }else if(i<=2*k){
      l <- which(t_2[,1]==p[j,2])
      u <- t_2[l,]      
    }else if(i<=3*k){
      l <- which(t_3[,1]==p[j,2])
      u <- t_3[l,]      
    }else if(i<=4*k){
      l <- which(t_4[,1]==p[j,2])
      u <- t_4[l,]
    }else if(i<=5*k){
      l <- which(t_5[,1]==p[j,2])
      u <- t_5[l,]
    }else if(i<=6*k){
      l <- which(t_6[,1]==p[j,2])
      u <- t_6[l,]
    }
    
    x <- p[j,5]
    if(x < u[2]){
      p[j,5] = 5
    }
    else if(x < u[3]){
      p[j,5] = 4
    }
    else if(x < u[4]){
      p[j,5] = 3
    }
    else if(x < u[5]){
      p[j,5] = 2
    }
    else if(x < u[6]){
      p[j,5] = 1
    }
    else if(x < u[7]){
      p[j,5] = 0
    }
    else if(x < u[8]){
      p[j,5] = -1
    }
    else if(x < u[9]){
      p[j,5] = -2
    }
    else if(x < u[10]){
      p[j,5] = -3
    }
    else{
      p[j,5] = -4
    }
  }
  
  Pc <- c(Pc,list(p))
}

##???ӷ?????ԭʼ??????
D <- list()
for(i in 1:n){
  d <- as.data.frame(DP[i])
  p <- as.data.frame(Pc[i])
  d <- cbind(d[,c(1:5)], score=p$pred_y, d[,c(6:29)])
  D <- c(D,list(d))
}

####ͳ??ƽ??????
Avg_f = list()
for(i in 1:n){
  p <- as.data.frame(Pc[i])
  t <- aggregate(p[,c(4,5)], list(p$day), mean)
  Avg_f <- c(Avg_f,list(t))
}

l=nrow(t)
avg_allf <- matrix(0,l,n+1)
avg_allf[,1] <- as.character(t[,1]) 
for(i in 1:n){
  t <- data.frame(Avg_f[i]) 
  avg_allf[,i+1] <- round(t[,3],3)
}
colnames(avg_allf) <- c("groupf",fn)



#################################################################################????
##????ƽ??????
path1 = getwd()
mypath <- "/saveResults/"

str  = "dxjs11~13_FTPred"           #?ļ?????????????????
fname <- paste0(path1, mypath, str, ".xlsx")     #???屣?????ļ?·?????ļ???
wb <- createWorkbook()  

f='avg_allf'
addWorksheet(wb, f)                   #????sheet
writeData(wb, f, avg_allf)            #д??????

f='avg_allt'
addWorksheet(wb, f)   
writeData(wb, f, avg_allt)            #д????
     
saveWorkbook(wb, fname, overwrite = TRUE)

################################################
##????Ԥ??????
str  = "dxjs11~13_pred"           
fname <- paste0(path1, mypath, str, ".xlsx")              
wb <- createWorkbook()  

for (i in 1:n){
  d <- as.data.frame(D[i])
  addWorksheet(wb, fn[i])                  
  writeData(wb, fn[i], d)
}

saveWorkbook(wb, fname, overwrite = TRUE)
rm(fname)
rm(mypath)
rm(path1)
rm(wb)

