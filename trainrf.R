setwd("E:/Rfile/testfile")
# getwd()

##导入安装包
install.packages("randomForest")
library(randomForest)

install.packages("openxlsx")
library(openxlsx)

rm(list=ls())
train <- read.xlsx("train_data.xlsx",1)
test <- read.xlsx("train_data.xlsx",2)

train <- train[,c(6:30)]
test <- test[,c(6:30)]

train <- train[,c(-19)]
test <- test[,c(-19)]

set.seed(5)
t <- sample(nrow(train))
train <- train[t,]


#训练rf
set.seed(5)
model.rf <- randomForest(trans_time~.,data=train, improtance=TRUE, mtry=8, ntree=300)
windows()
plot(model.rf,col=1:1)
print(model.rf)

pred <- predict(model.rf,test)
d_val <- abs(pred-test$trans_time)/test$trans_time
mean_dval <- mean(d_val)
re <- cbind(y=test$trans_time,pred,d_val,is_bad=(d_val>0.2))
  
save(model.rf, file = "E:/Rfile/testfile/train_model.Rdata") 
 
#绘制拟合线 

n=101
m=200
x=c(n:m)
y=re[n:m,1]
windows()
mainstr <- paste("rf:",n,"~",m)
plot(x, y, main = mainstr, type="o", lwd=2, col=2, pch=1, #cex符号大小，包括字体 lwd 线条宽度 col绘图颜色：col.main标题颜色 col.sub副标题颜色 等等
     xlab="x",ylab="transtime", ylim=c(0,20), xaxt="n", yaxt="n")

lines(re[n:m,2]~x, col = 4, type = "o", lwd = 2, pch=5) #添加曲线 #pch 点的形状
# bad_x <- pre_bad[which(pre_bad<=100)]
#bad_x <- pre_bad[which(pre_bad>=n & pre_bad<=m)]
#points(bad_x, re[bad_x,2], col=3, pch=24, cex=3, lwd=2)   # 标出拟合不好的点
legend("topleft",legend=c("real_y","predict_y"),col=c("red","blue"), pch=c(1,5), 
       lty=1, cex=0.8, bty="o", text.width = 8)
  