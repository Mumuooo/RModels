#all attr cluster: all node together
install.packages("cluster")
library(cluster)

n=8
k=4
for (i in 1:n){
  d <- read.xlsx("zj_1M_2M_new.xlsx",i)[,c(2,5:32)]
  colnames(d) <- c("time","monitor","monitorIp","visit",
                   "file_size","trans_time",colnames(d)[7:29])
  
  d$group <- substr(d$time, 1, 2)
  d <- d[,c(1,30,2:29)]
  # a <- floor(as.numeric(substr(d$time, 4, 5)) / 6)  #Group by hour (3hours)
  # b <- paste0(substr(d$time,1,3),a)
  # d$groupByH <- b
  # d <- d[,c(1,30,2:29)]
  
  
  ###set filesize: delete NA
  if (i<=k){
    t = which(d$file_size == 5123.25)        ##10M =10246.5   2M=2055.625 200k=205.563
    d <- d[t,]
  }else{
    t = which(d$file_size == 10246.5)        ##10M =10246.5   2M=2055.625 200k=205.563
    d <- d[t,]
  }
  
  
  d$trans_time <- round(as.numeric(d$trans_time),3)
  t <- which(is.na(d$trans_time))
  if(length(t)>=1){
    d <- d[-t,]
  }  
  
  d$retrans_rate <- round(d$retrans_bytes/d$trans_bytes,3)
  d <- d[,c(1:10,31,11:30)]
  assign(paste0("d", i), d)
}

#######################################################################
#绘制3维散点图
# install.packages("rgl")
library(rgl)
d<- d[,c(2,5,7,16,20)]
# d[,j] <- (d[,j]-min(d[,j]))/(max(d[,j]-min(d[,j]))) #特征归一化
fitm <- kmeans(d[,3],15,20)
d$cluster <- fitm$cluster
# t <- which(d$visit==d3$visit[2])
# d$visit[t] <- 3
open3d()
plot3d(d$rtt_avg,d$rto_avg,d$trans_time,col = d$cluster, size = 5)

#######################################################################
##归一化
for(i in 1:n){
  d <- get(paste0("d",i))
  for(j in 8:31){
    if(max(d[,j])!=min(d[,j])){
      d[,j] <- (d[,j]-min(d[,j]))/(max(d[,j]-min(d[,j])))
    }
  }
  assign(paste0("d",i),d)
}
######################################################################

D1 <- rbind(d1,d2,d3,d4)
D2 <- rbind(d5,d6,d7,d8)

m=n/k
for(i in 1:m){
  t <- get(paste0("D",i))
  groups <- unique(t$group)
  for(j in 1:length(groups)){
    d <- t[which(t$group==groups[j]),]
    fitm <- kmeans(d[,7],10,20)  #根据trans_time聚类
    d$cluster <- fitm$cluster
    # fitm <- pam(d[,c(8:31)],10)
    # d$cluster <- fitm$clustering
    d <- d[,c(1:7,32,8:31)]
    
    x <- data.frame(visit=d$visit, filesize=d$file_size,transtime=d$trans_time,cluster=d$cluster)
   
    TX <- ddply(x,"cluster",summarise,count=length(filesize),mean=(mean(transtime)))  #分组统计信息
    TX <- TX[order(TX$mean),] #根据同一cluster的均值排序
    TX$score <- c(1:nrow(TX)) #根据均值给cluster评分
    assign(paste0("T_",i,"_",groups[j]),TX)
    
    TX1 <- ddply(x,c("visit","cluster"),summarise,count=length(filesize),mean=(mean(transtime)))
    g <- aggregate(TX1$count,list(TX1$visit),function(x){
      return(sum(x))              #分节点，统计每一cluster的数量
    })   
    
    TX1$rate=0                   #分节点，计算每一cluster所占样本数比例
    for(ii in 1:nrow(g)){
       d0 <- TX1[which(TX1$visit==g[ii,1]),]
       TX1[which(TX1$visit==g[ii,1]),5] <- d0$count/g[ii,2]
    }
    TX1 <- TX1[,c(1,2,5,3,4)]
    assign(paste0("TX_",i,"_",groups[j]),TX1)
    # plot(d$trans_time[1:50],col=km$cluster[1:50])
    # plot(d[,c(7,12,31)],col=km$cluster[1:50])
    # plot(t,col=km$cluster)
    # points(v,km$centers,col=1:10,pch=8,cex=2)
  }
}

##############################

p1 = getwd()
mypath <- "/savexlsx/"

fn  = "zj_1M_2M_kmeansTo10c_pred_info"          
fname <- paste0(p1, mypath, fn, ".xlsx")              
wb <- createWorkbook()                   
f='TX_1_08'
addWorksheet(wb, f)                   
writeData(wb, f, TX_1_08)     

f='TX_1_09'
addWorksheet(wb, f)                   
writeData(wb, f, TX_1_09)

f='TX_1_10'
addWorksheet(wb, f)                   
writeData(wb, f, TX_1_10)

f='TX_1_11'
addWorksheet(wb, f)                   
writeData(wb, f, TX_1_11)

f='TX_2_08'
addWorksheet(wb, f)                   
writeData(wb, f, TX_2_08)     

f='TX_2_09'
addWorksheet(wb, f)                   
writeData(wb, f, TX_2_09)

f='TX_2_10'
addWorksheet(wb, f)                   
writeData(wb, f, TX_2_10)

f='TX_2_11'
addWorksheet(wb, f)                   
writeData(wb, f, TX_2_11)          

saveWorkbook(wb, fname, overwrite = TRUE)
rm(f,fn,fname,mypath,p1,wb)
