setwd("E:/Rfile/testfile")
# # # getwd()
# # 
# # ##导入安装包
# install.packages("randomForest")
# library(randomForest)
#
install.packages("openxlsx")
library(openxlsx)

install.packages("plyr")
library(plyr)
install.packages("dplyr")  #表连接
library(dplyr)


# rm(list=ls())
grepip = "115.238.248.216"

for(i in 1:8){
  d <- read.xlsx("zj_1M_2M_new.xlsx",i)[,1:8]
  names(d) <- c("time1","time2","timegroup","monitor_site","monitor_ip","visit_info","file_size","trans_time")
  d$trans_time <- round(as.numeric(d$trans_time),3)
  
  t <- which(is.na(d$trans_time))
  if(length(t) != 0){
    d <- d[-t,]
  }
  
  dip <- d[which(d$monitor_ip==grepip),]
  assign(paste0("dip", i), dip)
}


####筛选
for(j in 1:4){
  x <- get(paste0("dip", j))
  for(i in 1:4){
    if(i!=j){
      y <- get(paste0("dip", i))
      x <- semi_join(x,y,by="time1")    #筛选字段time1值匹配的记录
    }
  }
  assign(paste0("x", j), x)
  rm(x,y)
}

for(j in 5:8){
  x <- get(paste0("dip", j))
  for(i in 5:8){
    if(i!=j){
      y <- get(paste0("dip", i))
      x <- semi_join(x,y,by="time1")    #筛选字段time1值匹配的记录
    }
  }
  assign(paste0("x", j), x)
  rm(x,y)
}

F1 <- rbind(x1,x2,x3,x4)
F2 <- rbind(x5,x6,x7,x8)

#################################################################################储存
path1 = getwd()
mypath <- "/savexlsx/"

fn  = paste0("grepipzj_",grepip)          #文件保存名字在这里改
fname <- paste0(path1, mypath, fn, ".xlsx")                #定义保存的文件路径及文件名
wb <- createWorkbook()

f=paste0(grepip,"_1M")
addWorksheet(wb, f)                   #添加sheet
writeData(wb, f, F1)            #写入数据

f=paste0(grepip,"_2M")
addWorksheet(wb, f)                   #添加sheet
writeData(wb, f, F2)            #写入数据

saveWorkbook(wb, fname, overwrite = TRUE)
rm(f)
rm(fn)
rm(fname)
rm(mypath)
rm(path1)
rm(wb)



# ####左连接
# x <- get(paste0("dip", 1))
# for(i in 2:4){
#   y <- get(paste0("dip", i))
#   x <- inner_join(x,y,by="time1")
# }



# #############
# T$jz_mean <- round(T$jz_mean,3)
# T$cz_mean <- round(T$cz_mean,3)
# T$yc_mean <- round(T$yc_mean,3)
# T$sz_mean <- round(T$sz_mean,3)
# wb <- createWorkbook()
# wb <- loadWorkbook("E:/Rfile/testfile/savexlsx/Kmeans_sx.xlsx")
# addWorksheet(wb, "5M")
# writeData(wb, "5M", T)            #写入数据
# saveWorkbook(wb, "E:/Rfile/testfile/savexlsx/kmeans_sx.xlsx",overwrite = TRUE)



# 
# #####################################################
# ##library(xlsx) write.xlsx(x,file,sheet name) eg. write.xlsx(incometsforecasts2,"/Users/ap/Desktop/incometsforecast.xlsx","sheet1")
# write.xlsx(T, asTable =TRUE, "E:/Rfile/testfile/savexlsx/Kmeans_1M.xlsx",sheetName="Sheet1", overwrite = TRUE)