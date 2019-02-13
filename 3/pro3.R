simulate<-function(n,theta)
{
  list<-runif(n,0,theta)#generate uniform number
  moment<-2*mean(list) # get moment
  mle<-max(list) # get mle
  data<-c(moment,mle)  #return the vector
  return(data)
}
se<-function(x,theta)
{
    return ((x-theta)*(x-theta))   #se
}
compare<-function(n,theta)
{
  cat(sprintf("Combination of: %s,%s\n", n,theta))
  data_m<-replicate(1000,simulate(n,theta))#do it 1000 times 
  #matrix<-matrix(data_m,nrow=2,byrow=FALSE)   #set the matrix row=2 and fill the matrix by column
  se_temp<-apply(data_m,1,se,theta) #calculate  (head_theta-theta)^2
  sum<-apply(se_temp,2,sum) #get sum of each row
  m_o<-sum[1]/1000;
  m_m<-sum[2]/1000;  #get mean of se
  cat(sprintf("moment: %s   ",m_o))
  cat(sprintf("mle: %s\n",m_m))
  return(c(m_o,m_m))
}

n<-c(1,2,3,5,10,30)
theta<-c(1,5,50,100)
combination<-expand.grid(n,theta)  # get all combination
v = as.vector(unlist(combination))
combination<-matrix(v,ncol=2,byrow=FALSE)

output<-apply(combination,1,function(x)do.call(compare,as.list(x)))# use each row as input to use function

#########get combination to draw graphic

n<-c(1,2,3,5,10,30)
theta<-c(100)
combination<-expand.grid(n,theta)  # get combination
v = as.vector(unlist(combination))
combination<-matrix(v,ncol=2,byrow=FALSE)
output<-apply(combination,1,function(x)do.call(compare,as.list(x)))

region<-c("moment","mle")
barplot(output,names.arg=n,ylab="mse",xlab="theta=100",beside=T,legend.text = region)# draw barplot
###########################
compare(30,1)

data_m<-replicate(10,simulate(30,1))
matrix<-matrix(data_m,nrow=2,byrow=FALSE)
se_temp<-apply(matrix,1,se,1)
sum<-apply(se_temp,2,sum) #get sum of each row
m_o<-sum[1]/10
m_m<-sum[2]/10

###################Q2
data<-c(21.72,14.65,50.42,28.78,11.23)
dim(data)=c(1,5)
ln<-apply(data,1,log)
meanln<-apply(ln,2,mean)
theta=1/meanln


data<-c(21.72,14.65,50.42,28.78,11.23)
f <- function(theta,x) # set log funtion
  {
   result<-(length(x)*log(theta)-(theta+1)*sum(log(x)))
   return (-result)
}
ml.est <- optim(1, fn=f,method = "L-BFGS-B", lower=0.1, hessian=TRUE,x=data) #get estimate

theta<-ml.est$par  #mle theta
s<-sqrt(diag(solve(ml.est$hessian))) # approximate stand error



l<-theta-qnorm(0.975)*s
r<-theta+qnorm(0.975)*s
cat(sprintf("[%s,%s]",l,r))

