
n=12
k=4
D <- list()
for (i in 1:n){
  d <- read.xlsx("data/zj_1M_5M_0904_log.xlsx",i)[,c(2,5:28)]

  
  # d$group <- substr(d$hour, 1, 2)
  
  a <- floor(as.numeric(substr(d$hour, 4, 5)) / 6)  #Group by hour (6hours)
  d$group <- paste0(substr(d$hour,1,3),a)
  d <- d[,c(1,26,2:25)]
 
  
  d$retrans_rate <- round(d$retrans_bytes/d$trans_bytes,3)
  if(i%%k==0){
    d$visit <- 4
  }else{
    d$visit <- i%%k
  }
  d <- d[,c(1:3,28,4:6,27,7:26)]
  D <- c(D,list(d))
  # assign(paste0("d", i), d)
}

load("E:/Rfile/testfile/rf_model.Rdata")
for(i in 1:n){
  # test <- get(paste0("d",i))
  test <- data.frame(D[i])
  pred_y <- round(predict(model.rf, test),3)
  pred <- data.frame(time=test$hour, group=test$group, file_size=test$trans_bytes, visit=test$visit, pred_y=pred_y)
  assign(paste0("pred", i), pred)
}

D1 <- rbind(pred1,pred2,pred3,pred4)
D2 <- rbind(pred5,pred6,pred7,pred8)
D3 <- rbind(pred9,pred10,pred11,pred12)

m=n/k
allvisitClusterInfolog <- list()
for(i in 1:m){
  t <- get(paste0("D",i))
  groups <- unique(t$group)
  
  for(j in 1:length(groups)){
    d <- t[which(t$group==groups[j]),]
    
    ## 对Log pred_y聚类
    set.seed(5)
    fitm <- kmeans(d[,5],15,20)  #根据trans_time聚类
    d$cluster <- fitm$cluster
    
    # fitm <- pam(d[,7],10)
    # d$cluster <- fitm$clustering
    
    x <- data.frame(visit=d$visit, filesize=d$file_size,transtime=d$pred_y,cluster=d$cluster)
    
    
    #####################################################################
    TX <- ddply(x,"cluster",summarise,count=length(filesize),mean=(mean(transtime)))  #分组统计信息:统计每一簇的均值
    TX <- TX[order(TX$mean),] #根据同一cluster的均值排序
    TX$score <- c(nrow(TX):1) #根据均值给cluster评分
    assign(paste0("T_",i,"_",groups[j]),TX)
    
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
    
    # assign(paste0("TX_",i,"_",groups[j]),TX1)
    allvisitClusterInfolog <- c(allvisitClusterInfolog, list(TX1))
  }
  assign(paste0("visitlogScore",i),visitScore)
}

visitScoreL <- cbind(visitlogScore1,visitlogScore2,visitlogScore3)
visitScoreL$sum <- visitScoreL[,3]+visitScoreL[,6]+visitScoreL[,9]

#############
p1 = getwd()
mypath <- "/savexlsx/zj_200k_1M_5M_1030/"

fn  = "zj_200k_1M_5M_1030_kmeansTo15cBytranstimeGroupBy6H_log1and2"
fname <- paste0(p1, mypath, fn, ".xlsx")
wb <- createWorkbook()

# f='visitScore1'
# addWorksheet(wb, f)
# writeData(wb, f, visitScore1)


f='visitScoreL'
addWorksheet(wb, f)
writeData(wb, f, visitScoreL)

saveWorkbook(wb, fname, overwrite = TRUE)
rm(f,fn,fname,mypath,p1,wb)