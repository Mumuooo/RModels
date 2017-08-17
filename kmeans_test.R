# setwd("E:/Rfile/testfile")
# # # install.packages("openxlsx")
# library(openxlsx)
# # # install.packages("dplyr")
# library(dplyr)
# library(plyr)
# library(cluster)
# library(randomForest)

# flag=1  # 1-对real_y聚类，否则对pred_y聚类

n=8
k=4
D <- list()
for (i in 1:n){
  d <- read.xlsx("data/zj_5M_10M_0904.xlsx",i)[,c(2,5:32)]
  colnames(d) <- c("time","monitor","monitorIp","visit",
                   "file_size","trans_time",colnames(d)[7:29])
  
  # d$group <- substr(d$time, 1, 2)

  a <- floor(as.numeric(substr(d$time, 4, 5)) / 6)  #Group by hour (6hours)
  d$group <- paste0(substr(d$time,1,3),a)
  d <- d[,c(1,30,2:29)]
  
  
  ###set filesize: delete NA
  if (i/k<=1){
    t = which(d$file_size == 5123.25)        ##10M =10246.5   2M=2055.625 200k=205.563
    d <- d[t,]

  }else if(i/k<=2){
    t = which(d$file_size == 10246.5)        ##5M =5123.25   1M=1027.813 200kx=205.563
    d <- d[t,]
  }else{
    t = which(d$file_size == 205.563)        ##5M =5123.25   1M=1027.813 200kx=205.563
    d <- d[t,]
  }
  
  
  d$trans_time <- round(as.numeric(d$trans_time),3)
  t <- which(is.na(d$trans_time))
  if(length(t)>=1){
    d <- d[-t,]
  }  
  
  d$retrans_rate <- round(d$retrans_bytes/d$trans_bytes,3)
  d <- d[,c(1:10,31,11:30)]
  D <- c(D,list(d))
  # assign(paste0("d", i), d)
}


load("E:/Rfile/testfile/rf_model.Rdata")

for(i in 1:n){
  test <- data.frame(D[i])
  # test <- get(paste0("d",i))
  
  pred_y <- round(predict(model.rf, test),3)
  pred <- data.frame(time=test$time, group=test$group, visit=test$visit, 
                     file_size=test$file_size, real_y=test$trans_time, pred_y=pred_y)
  assign(paste0("pred", i), pred)
}

D1 <- rbind(pred1,pred2,pred3,pred4)
D2 <- rbind(pred5,pred6,pred7,pred8)
D3 <- rbind(pred9,pred10,pred11,pred12)


############################################################################################
#real
allNodeClusterInfoReal <- list()
allvisitClusterInfoReal <- list()
m=n/k
for(i in 1:m){
  t <- get(paste0("D",i))
  groups <- unique(t$group)
  t$score <- 0
  for(j in 1:length(groups)){
    d <- t[which(t$group==groups[j]),]
  
    ## 对real_y聚类
    set.seed(5)
    fitm <- kmeans(d[,5],15,20)  #根据trans_time聚类
    
    # fitm <- pam(d[,7],10)
    # d$cluster <- fitm$clustering
    
    x <- data.frame(visit=d$visit, filesize=d$file_size,transtime=d$real_y,cluster=fitm$cluster)
  
   
    ###################################################################################################################
    TX <- ddply(x,"cluster",summarise,count=length(filesize),mean=(mean(transtime)))  #分组统计信息:统计每一簇的均值
    TX <- TX[order(TX$mean),] #根据同一cluster的均值排序
    TX$score <- c(nrow(TX):1) #根据均值给cluster评分
    allNodeClusterInfoReal <- c(allNodeClusterInfoReal, list(TX))
    # assign(paste0("T_",i,"_",groups[j]),TX)
    x <- left_join(x,TX[,c(1,4)], by= "cluster")
    t$score[which(t$group==groups[j])] <- x$score
    
    TX1 <- ddply(x,c("visit","cluster"),summarise,count=length(filesize),mean=(mean(transtime)))
    g <- aggregate(TX1$count,list(TX1$visit),function(x){
      return(sum(x))              #分节点，统计每一cluster的数量
    })   
    # 
    TX1$rate=0                   #分节点，计算每一cluster所占样本数比例
    for(ii in 1:nrow(g)){
      d0 <- TX1[which(TX1$visit==g[ii,1]),]
      TX1[which(TX1$visit==g[ii,1]),5] <- d0$count/g[ii,2]
    }
    TX1 <- TX1[,c(1,2,5,3,4)]
    TX1 <- left_join(TX1,TX,by="cluster")   #根据cluster，连接分数
    TX1$finalscore <- TX1$rate*TX1$score
    g <- aggregate(TX1$finalscore,list(TX1$visit),function(x){
      return(sum(x))              #分节点，统计每一cluster的数量
    })
    g$group <- groups[j]
    g <- g[,c(3,1,2)]
    if(j==1){
      visitScore <- g
    }else{
      visitScore <- rbind(visitScore, g)
    }
    allvisitClusterInfoReal <- c(allvisitClusterInfoReal, list(TX1))
    # assign(paste0("TX_",i,"_",groups[j]),TX1)
  }
  assign(paste0("D",i),t)
  assign(paste0("visitRealScore",i),visitScore)
}

#############################################################################################
#pred
allNodeClusterInfoPred <- list()
allvisitClusterInfoPead <- list()
for(i in 1:m){
  t <- get(paste0("D",i))
  groups <- unique(t$group)
  t$score2 <- 0
  for(j in 1:length(groups)){
    d <- t[which(t$group==groups[j]),]

    ## 对pred_y聚类
    set.seed(5)
    fitm <- kmeans(d[,6],15,20)
    
    # fitm <- pam(d[,6],10)
    # d$cluster <- fitm$clustering
    x <- data.frame(visit=d$visit, filesize=d$file_size,transtime=d$pred_y, cluster=fitm$cluster)

    
    #####################################################################################################################
    TX <- ddply(x,"cluster",summarise,count=length(filesize),mean=(mean(transtime)))  #分组统计信息:统计每一簇的均值
    # TX <- data.frame(cluster=c(1:15),count=fitm$size,mean=fitm$centers)
    TX <- TX[order(TX$mean),] #根据同一cluster的均值排序
    TX$score <- c(nrow(TX):1) #根据均值给cluster评分
    allNodeClusterInfoPred <- c(allNodeClusterInfoPred, list(TX))
    # assign(paste0("P_",i,"_",groups[j]),TX)
    x <- left_join(x, TX[,c(1,4)], by = "cluster")
    t$score2[which(t$group==groups[j])] <- x$score
    
    
    TX1 <- ddply(x,c("visit","cluster"),summarise,count=length(filesize),mean=(mean(transtime)))
    g <- aggregate(TX1$count,list(TX1$visit),function(x){
      return(sum(x))              #分节点，统计每一cluster的数量
    })   
    # 
    TX1$rate=0                   #分节点，计算每一cluster所占样本数比例
    for(ii in 1:nrow(g)){
      d0 <- TX1[which(TX1$visit==g[ii,1]),]
      TX1[which(TX1$visit==g[ii,1]),5] <- d0$count/g[ii,2]
    }
    TX1 <- TX1[,c(1,2,5,3,4)]
    TX1 <- left_join(TX1,TX,by="cluster")   #根据cluster，连接分数
    TX1$finalscore <- TX1$rate*TX1$score
    g <- aggregate(TX1$finalscore,list(TX1$visit),function(x){
      return(sum(x))              #分节点，统计每一cluster的数量
    })
    g$group <- groups[j]
    g <- g[,c(3,1,2)]
    if(j==1){
      visitScore <- g
    }else{
      visitScore <- rbind(visitScore, g)
    }
    
    allvisitClusterInfoPead <- c(allvisitClusterInfoPead, list(TX1))
    # assign(paste0("PX_",i,"_",groups[j]),TX1)
    
    # plot(d$trans_time[1:50],col=km$cluster[1:50])  #聚类结果绘图
    # plot(d[,c(7,12,31)],col=km$cluster[1:50])
    # plot(t,col=km$cluster)
    # points(v,km$centers,col=1:10,pch=8,cex=2)
  }
  assign(paste0("D",i),t)
  assign(paste0("visitPredScore",i),visitScore)
}

# train <- fitm$centers               #knn测试
# test <- as.matrix(x$transtime) 
# cl <- c(1:15)
# fitknn <- knn(train = train, test = test, cl=cl, k=1)
# 
# x$knn <- fitknn

# visitPredScore1 <- visitPredScore1[c(-9,-22),]
visitScore <- cbind(visitRealScore1,visitRealScore2,visitRealScore3)
visitScore$sum <- visitScore[,3]+visitScore[,6]+visitScore[,9]

visitScoreP <- cbind(visitPredScore1,visitPredScore2,visitPredScore3)
visitScoreP$sum <- visitScoreP[,3]+visitScoreP[,6]+visitScoreP[,9]

#################################################################################################
p1 = getwd()
mypath <- "/savexlsx/zj_200k_1M_5M_1030/"

fn  = "zj_200k_1M_5M_1030_kmeansTo15cBytranstimeGroupBy6H_plus1and2_client"
fname <- paste0(p1, mypath, fn, ".xlsx")
wb <- createWorkbook()

f='visitScore'
addWorksheet(wb, f)
writeData(wb, f, visitScore)


f='visitScoreP'
addWorksheet(wb, f)
writeData(wb, f, visitScoreP)

saveWorkbook(wb, fname, overwrite = TRUE)
rm(f,fn,fname,mypath,p1,wb)
# # 
# 
# ###############################################################################################################################
# 
# p1 = getwd()
# mypath <- "/savexlsx/"
# 
# fn  = "zj_1M_2M_kmeansTo10cBy6H_ALL_info"
# fname <- paste0(p1, mypath, fn, ".xlsx")
# wb <- createWorkbook()
# 
# f='TX_1_09-1'
# addWorksheet(wb, f)
# writeData(wb, f, get(f))
# 
# f='TX_1_09-2'
# addWorksheet(wb, f)
# writeData(wb, f, get(f))
# 
# f='TX_1_09-3'
# addWorksheet(wb, f)
# writeData(wb, f, get(f))
# 
# f='TX_1_09-4'
# addWorksheet(wb, f)
# writeData(wb, f, get(f))
# 
# f='TX_2_09-1'
# addWorksheet(wb, f)
# writeData(wb, f, get(f))
# 
# f='TX_2_09-2'
# addWorksheet(wb, f)
# writeData(wb, f, get(f))
# 
# f='TX_2_09-3'
# addWorksheet(wb, f)
# writeData(wb, f, get(f))
# 
# f='TX_2_09-4'
# addWorksheet(wb, f)
# writeData(wb, f, get(f))
# 
# saveWorkbook(wb, fname, overwrite = TRUE)
# rm(f,fn,fname,mypath,p1,wb)