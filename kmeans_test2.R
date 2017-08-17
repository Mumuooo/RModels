#trans_time   cluster
for(i in 1:8){
  t <- get(paste0("pred",i))
  groups <- unique(t$group)
  for(j in 1:length(groups)){
    d <- t[which(t$group==groups[j]),]
    km <- kmeans(d$real_y,10,20,3)
    x <- sort(km$centers)
    tx <- data.frame(t(x))
    colnames(tx) <- as.character(c(1:10))
    tx <- cbind(group=groups[j],tx)
    if(j==1){
        TX <- tx
    }else{
      TX <- rbind(TX,tx)
    }

    # plot(t,col=km$cluster)
    # points(v,km$centers,col=1:10,pch=8,cex=2)
    
  }
  assign(paste0("TX",i),TX)
}

#all attr cluster
for(i in 1:8){
  t <- get(paste0("d",i))
  groups <- unique(t$day)
  for(j in 1:length(groups)){
    d <- t[which(t$day==groups[j]),]
    km <- kmeans(d[,c(8:31)],10,20)
    d$cluster <- km$cluster
    d <- d[,c(1:7,32,8:31)]
    
    x1 <- data.frame(A=d$file_size,transtime=d$trans_time,C=d$cluster)
    TX <- ddply(x1,"C",summarise,count=length(A),mean=(mean(transtime)))
    TX <- TX[order(TX$mean),]
    # x<- userdata【order(userdata$username,1)】
    assign(paste0("T_",i,"_",groups[j]),TX)
    # plot(d$trans_time[1:50],col=km$cluster[1:50])
    # plot(d[,c(7,12,31)],col=km$cluster[1:50])
    # plot(t,col=km$cluster)
    # points(v,km$centers,col=1:10,pch=8,cex=2)
    
  }
  
}

ALL09 <- cbind(T_1_09,T_2_09,T_3_09,T_4_09)
ALL10 <- cbind(T_1_10,T_2_10,T_3_10,T_4_10)
ALL11 <- cbind(T_1_11,T_2_11, T_3_11,T_4_11)
ALL12 <- cbind(T_1_12,T_2_12, T_3_12,T_4_12)

ALL2_09 <- cbind(T_5_09,T_6_09, T_7_09,T_8_09)
ALL2_10 <- cbind(T_5_10,T_6_10, T_7_10,T_8_10)
ALL2_11 <- cbind(T_5_11,T_6_11, T_7_11,T_8_11)
ALL2_12 <- cbind(T_5_12,T_6_12, T_7_12,T_8_12)

p1 = getwd()
mypath <- "/savexlsx/"

fn  = "sx_5M_10M_kmeansBytranstime"          
fname <- paste0(p1, mypath, fn, ".xlsx")              
wb <- createWorkbook()                   
f='TX_1_09'
addWorksheet(wb, f)                   
writeData(wb, f, TX_1_09)            

f='TX_1_10'
addWorksheet(wb, f)   
writeData(wb, f, TX_1_10)            

f='TX_1_11'
addWorksheet(wb, f)   
writeData(wb, f, TX_1_11)

f='TX_1_12'
addWorksheet(wb, f)   
writeData(wb, f, TX_1_12)

f='TX_2_09'
addWorksheet(wb, f)                   
writeData(wb, f, TX_2_09)            

f='TX_2_10'
addWorksheet(wb, f)   
writeData(wb, f, TX_2_10)            

f='TX_2_11'
addWorksheet(wb, f)   
writeData(wb, f, TX_2_11)

f='TX_2_12'
addWorksheet(wb, f)   
writeData(wb, f, TX_2_12)


saveWorkbook(wb, fname, overwrite = TRUE)
rm(f,fn,fname,mypath,p1,wb)


