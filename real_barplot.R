p=c("jx: ", "qz: ", "nb ", "ls: ")


#################################################################Ԥ??
day = '08'
windows()
split.screen(c(1,2))
m=n/k

for(j in 1:k){
  screen(j)
  d <- data.frame(P_1_08)
  for (i in 2:m) {
    t <- data.frame(Pc[(i-1)*k+j])
    d <- rbind(d,t)
  }
  t <- which(d[,3]==day)
  r <- d[t,]
  if(nrow(r)!=0){
    m_y <- round(mean(r$pred_y),3)
    x=table(r$pred_y)
    yl=ceiling(max(x)/100)*100+100
    b=barplot(x, ylim=c(0,yl), xlab=paste(p[j], day),col = "lightblue", border = "blue", main=paste('Score',m_y)) 
    text(b, x, labels = x, cex=0.6, pos=3)
  }
}
