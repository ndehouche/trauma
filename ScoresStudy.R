i<-0
j<-0
k<-0

n<-1
B<-0
C<-0
score <-matrix(nrow=216, ncol=3)
mode(score)<-"integer"
reversal12<-matrix(nrow=216, ncol=216)
reversal13<-matrix(nrow=216, ncol=216)
reversal23<-matrix(nrow=216, ncol=216)
for (i in seq(0, 5, by=1))
{for (j in seq(0, 5, by=1))
{for (k in seq(0, 5, by=1))
{
if ((i>=j) && (j>=k))
{  score[n,1] <-as.integer(i)
  score[n,2] <-as.integer(j)
  score[n,3] <-as.integer(k)
  
  n<-n+1}
}
  
}
  
}


for (i in seq(1, n-1, by=1))
{
  for (j in seq(i+1, n-1, by=1))
  {
    
    if (((score[i,1]+score[i,2]+score[i,3]>score[j,1]+score[j,2]+score[j,3]) & (score[i,1]^2+score[i,2]^2+score[i,3]^2<score[j,1]^2+score[j,2]^2+score[j,3]^2))
        || ((score[i,1]+score[i,2]+score[i,3]<score[j,1]+score[j,2]+score[j,3]) & (score[i,1]^2+score[i,2]^2+score[i,3]^2>score[j,1]^2+score[j,2]^2+score[j,3]^2)))
      
    {
      reversal12[i,j]<-1
      
    }
    
    if (((score[i,1]+score[i,2]+score[i,3]>score[j,1]+score[j,2]+score[j,3]) & (score[i,1]^3+score[i,2]^3+score[i,3]^3<score[j,1]^3+score[j,2]^3+score[j,3]^3))
        || ((score[i,1]+score[i,2]+score[i,3]<score[j,1]+score[j,2]+score[j,3]) & (score[i,1]^3+score[i,2]^3+score[i,3]^3>score[j,1]^3+score[j,2]^3+score[j,3]^3)))
      
    {
      reversal13[i,j]<-1
      
    }
    
    if (((score[i,1]^2+score[i,2]^2+score[i,3]^2>score[j,1]^2+score[j,2]^2+score[j,3]^2) & (score[i,1]^3+score[i,2]^3+score[i,3]^3<score[j,1]^3+score[j,2]^3+score[j,3]^3))
        || ((score[i,1]^2+score[i,2]^2+score[i,3]^2<score[j,1]^2+score[j,2]^2+score[j,3]^2) & (score[i,1]^3+score[i,2]^3+score[i,3]^3>score[j,1]^3+score[j,2]^3+score[j,3]^3)))
      
    {
      reversal23[i,j]<-1
      
    }
    
  }   
  
  
}

which(reversal12==1, arr.ind=TRUE)
which(reversal23==1, arr.ind=TRUE)
which(reversal13==1, arr.ind=TRUE)

df12<-data.frame(score[which(reversal12==1, arr.ind=TRUE)[,1],],score[which(reversal12==1, arr.ind=TRUE)[,2],])
df13<-data.frame(score[which(reversal13==1, arr.ind=TRUE)[,1],],score[which(reversal13==1, arr.ind=TRUE)[,2],])
df23<-data.frame(score[which(reversal23==1, arr.ind=TRUE)[,1],],score[which(reversal23==1, arr.ind=TRUE)[,2],])

print.xtable(xtable(df12), file="./x12.txt", digits=1)
print.xtable(xtable(df13), file="./x13.txt", digits=1)
print.xtable(xtable(df23), file="./x23.txt", digits=1)