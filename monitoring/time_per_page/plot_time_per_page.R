library(lattice)

ts<-read.table('time_per_page.txt')
is.na(ts$V1[ts$V1>3600])<-T
w<-which(ts$V1<0.1)
is.na(ts$V1[w])<-T
pdf(file='time_per_page.pdf',width=8,height=6,pointsize=12)
histogram(ts$V1,nint=100,xlab='Time for classification (S)')
mean(ts$V1,na.rm=T)
median(ts$V1,na.rm=T)
pdf(file='log_time_per_page.pdf',width=8,height=6,pointsize=12)
tics<-c(0.1,0.2,0.5,1,2,5,10,20,50,100,200,500,2000,5000)
ticl<-as.character(tics)
histogram(log(ts$V1),nint=100,xlab='Time for classification (S)',
		  scales=list(x=list(at=log(tics),labels=ticl)))
