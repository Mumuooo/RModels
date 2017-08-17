install.packages("cluster")
library(cluster)

grepip="59.48.9.122"

for(i in 1:2){
  d <- read.xlsx("savexlsx/sx_5M_10M/grepipsx_59.48.9.122(datong).xlsx",i)
  assign(paste0("d", i), d)
}

v <- c(1:10)
v[1:10] <-1
for(i in 1:4){
  t <- d1[,8+(i-1)*4]
  pa <- pam(t,10)
  # plot(t,col=pa$clustering)
  # points(v,pa$medoids,col=1:10,pch=8,cex=2)
  x <- sort(pa$medoids)
  assign(paste0("x",i), x)
}

Finv_1 <- data.frame(jz_1=x1,cz_1=x2,yc_1=x3,sz_1=x4)

for(i in 1:4){
  t <- d2[,8+(i-1)*4]
  pa <- pam(t,10)
  # plot(t,col=km$cluster)
  # points(v,km$centers,col=1:10,pch=8,cex=2)
  x <- sort(pa$medoids)
  assign(paste0("x",i), x)
}


Finv_2 <- data.frame(jz_2=x1,cz_2=x2,yc_2=x3,sz_2=x4)
##############################
####可视化
n=1
m=6
x=c(n:m)
y = Finv_1
windows()
mainstr <- "5M"
plot(x, y[n:m,1], main = mainstr, type="o", lwd=2, col=1, pch=1, #cex符号的大小，包括字体 lwd 线条宽度 col绘图颜色：col.main标题颜色 col.sub副标题颜色 等等
     xlab="x",ylab="transtime", ylim=c(8,12), xaxt="n", yaxt="n")

lines(y[n:m,2]~x, col = 2, type = "o", lwd = 2, pch=2) #添加曲线 #pch 点的形状
lines(y[n:m,3]~x, col = 3, type = "o", lwd = 2, pch=3) #添加曲线 #pch 点的形状
lines(y[n:m,4]~x, col = 4, type = "o", lwd = 2, pch=4) #添加曲线 #pch 点的形状

legend("topleft",legend=c("jz","cz","yc","sz"),col=c(1:4), pch=c(1:4),  #pch 点的符号
       lty=1, cex=0.8, bty="n") #bty 图形边框 lty 线条类型 lwd 线条宽度
axis(1, at=seq(n, m, 1))  #, labels =T1[n:m,1]
axis(2, at=seq(8, 12, 0.5) )

####可视化
n=1
m=6
x=c(n:m)
y = Finv_2
windows()
mainstr <- "10M"
plot(x, y[n:m,1], main = mainstr, type="o", lwd=2, col=1, pch=1, #cex符号的大小，包括字体 lwd 线条宽度 col绘图颜色：col.main标题颜色 col.sub副标题颜色 等等
     xlab="x",ylab="transtime", ylim=c(17,24), xaxt="n", yaxt="n")

lines(y[n:m,2]~x, col = 2, type = "o", lwd = 2, pch=2) #添加曲线 #pch 点的形状
lines(y[n:m,3]~x, col = 3, type = "o", lwd = 2, pch=3) #添加曲线 #pch 点的形状
lines(y[n:m,4]~x, col = 4, type = "o", lwd = 2, pch=4) #添加曲线 #pch 点的形状

legend("topleft",legend=c("jz","cz","yc","sz"),col=c(1:4), pch=c(1:4),  #pch 点的符号
       lty=1, cex=0.8, bty="n") #bty 图形边框 lty 线条类型 lwd 线条宽度
axis(1, at=seq(n, m, 1))  #, labels =T1[n:m,1]
axis(2, at=seq(17, 24, 0.5) )

###############
#分组统计
t <- d2[,20]
pa <- pam(t,10)
x1 <- data.frame(A=d2$file_size,transtime=d2$jz_trans_time,C=pa$clustering)
t <- aggregate(x1$A, list(x1$C), function(x){
  return(length(x))
})

t[,3] <- pa$medoids
colnames(t) <- c("Lable","count","mean")
t <- t[order(t$mean),2:3]

T <- data.frame(T,t)
colnames(T) <- c("jz_count","jz_mean","cz_count","cz_mean"
                 ,"yc_count","yc_mean","sz_count","sz_mean")


##############

T$jz_mean <- round(T$jz_mean,3)
T$cz_mean <- round(T$cz_mean,3)
T$yc_mean <- round(T$yc_mean,3)
T$sz_mean <- round(T$sz_mean,3)
wb <- createWorkbook()
wb <- loadWorkbook("E:/Rfile/testfile/savexlsx/Kmeans_sx.xlsx")
addWorksheet(wb, "5M")
writeData(wb, "5M", T)            #写入数据
saveWorkbook(wb, "E:/Rfile/testfile/savexlsx/kmeans_sx.xlsx",overwrite = TRUE)