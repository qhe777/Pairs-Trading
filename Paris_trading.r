library(zoo)
library(tseries)
#O-U 


BHP<-get.hist.quote(instrument = 'BHP',start = '2011-01-01',end = '2016-01-04',retclass='ts',quote="Adjusted",compression = 'd')
BLL<-get.hist.quote(instrument ='BBL',start = '2011-01-01',end = '2016-01-04',retclass='ts',quote="Adjusted",compression = 'd')


sprd <- log(BHP)-log(BLL)
BHP <- BHP[!is.na(sprd)]
BLL <- BLL[!is.na(sprd)]
sprd <- sprd[!is.na(sprd)]

interval=21

spread=sprd[interval:length(sprd)]
mean=c()
beta=c()
sigma=c()

for (i in interval: length(sprd) )
{
  
  spread_log<-sprd[(i-interval+1):i]
  prev_sprd <- c(spread_log[2:length(spread_log)], 0)
  
  d_sprd <- spread_log - prev_sprd
  u=mean(prev_sprd)
  mean=c(mean,u)
  prev_sprd_mean <- prev_sprd - u
  spread_log.zoo <- merge(d_sprd, prev_sprd_mean)
  sprd_t <- as.data.frame(spread_log.zoo)
  
  result <- lm(d_sprd ~ prev_sprd_mean+0, data = sprd_t)
  beta_1=coef(result)[1]
  beta=c(beta,beta_1)
  sigma_1=sqrt(var(d_sprd-beta*prev_sprd_mean))
  sigma=c(sigma,sigma_1)
}

enter=c()
close=c()
postion=c()
i <- 2
while(i<=length(spread))
{
  if ( (mean[i]+sigma[i]-spread[i])<=0 & (mean[i-1]+sigma[i-1]-spread[i-1])>0 ) 
  {
    enter=c(enter,i)
    postion=c(postion,1)
    for (j in (i+1):length(spread)) 
    {
      if ( (mean[j]-spread[j])>=0 & (mean[j-1]-spread[j-1])<0 )
      {
        close=c(close,j)
        break
      }
    }
    i <- j
  }
  if ( (mean[i]-sigma[i]-spread[i])>=0 & (mean[i-1]-sigma[i-1]-spread[i-1])<0 ) 
  {
    enter=c(enter,i)
    postion=c(postion,-1)
    for (j in (i+1):length(spread)) 
    {
      if ( (mean[j]-spread[j])<=0 & (mean[j-1]-spread[j-1])>0 )
      {
        close=c(close,j)
        break
      }
    }
    i <- j
  }
  i <-i+1
}

vol1=c()
vol2=c()
r1=c()
r2=c()
average=21
for (i in 1:length(close))
{
  vol1[i]=sqrt(var(diff(log(BHP[(enter[i]-average+interval-1):(enter[i]+interval-1)]))))
  vol2[i]=sqrt(var(diff(log(BLL[(enter[i]-average+interval-1):(enter[i]+interval-1)]))))
  r1[i]=(BHP[(close[i]+interval-1)]/BHP[(enter[i]+interval-1)])-1
  r2[i]=(BLL[(close[i]+interval-1)]/BLL[(enter[i]+interval-1)])-1
}
postion=postion[1:length(close)]
r=(r2-r1)*postion
mydata=data.frame(postion,vol1,vol2,r1,r2,r)

y=c()
for (i in 1:22)
{
  if ( abs(r1[i])>abs(r2[i]) )
{
y=c(y,1)
}
if ( abs(r1[i])<abs(r2[i]) )
{
y=c(y,0)
}
}
myprobit <- glm(y~ vol1+ vol2, family = binomial(link = "probit"), data = mydata)
