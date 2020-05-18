#The Injury Severity Score: A Decision-Theoretic Perspective

number <-0 #number of simulations of duration t.end
average1 <-c(100)
average<-c(100)
average3 <-c(100)

#p<-c(.05,.2,.49,.15,.15) #AIS grades distribution

p<-c(.05,.2,.49,.15,.15) #AIS grades distribution
for (number in seq(1, 10, by=1))
{
  
  t.end   <- 100 # duration of sim
  t.clock <- 0    # sim time
  Ta <- .5 # interarrival period
  Ts <- 1  # service period
  t1 <- 0         # time for next arrival
  t2 <- t.end     # time for next departure
  tn <- t.clock   # tmp var for last event time
  tb <- 0         # tmp var for last busy-time start
  n <- 0          # number in system
  s <- 0          # cumulative number-time product
  b <- 0          # total busy time
  c <- 0          # total completions
  qc <- 0         # plot instantaneous q size
  tc <- 0         # plot time delta
  patient <-0     # Regular ISSS Scores list
  patient1 <-0     # Sum AIS Scores list
  patient3 <-0     # Cube AIS Scores list
  plotSamples<-100
  temp<-0
  time <-0        # Arrival time of patient i
  waiting <-0 #waiting times regular
  waiting1 <-0 #waiting times sum
  waiting3 <-0 #waiting times cube
  
  arrival <-0 #arrival times
  critical <-0 #patients witih critical injuries
  
  A <-0
  B <-0
  C <-0
  ISS <-0
  ISS1 <-0
  ISS3 <-0
  
  
  score <-c(6)
  priority <-0
  j<-1
  i<- 0
  k <-0
  while (t.clock < t.end) {
    if (t1 < t2) {      # arrival event
      
      
      t.clock <- t1
      s <- s + n * (t.clock - tn)  # delta time-weighted number in queue
      n <- n + 1
      
      # score[1] <-sample(1:5,1,replace=TRUE,prob=c(.04,.2,.5,.13,.13))
      
      score[1] <-sample(1:5,1,replace=TRUE,prob=p)
      score[2] <-sample(1:5,1,replace=TRUE,prob=p)
      score[3] <-sample(1:5,1,replace=TRUE,prob=p)
      score[4] <-sample(1:5,1,replace=TRUE,prob=p)
      score[5] <-sample(1:5,1,replace=TRUE,prob=p)
      score[6] <-sample(1:5,1,replace=TRUE,prob=p)
      
      if ((score[6]==5)||(score[5]==5)||(score[4]==5)||(score[3]==5)||(score[2]==5)||(score[1]==5))
      {critical <-append(critical,i)
      }  
      i<-i+1
      
      A <-max(score)
      B <-sort(score, partial=5)[5]
      C <-sort(score, partial=4)[4]
      ISS <-A^2 +B^2 +C^2
      ISS1 <-A+B+C
      ISS3 <-(A)^3 +(B)^3 +(C)^3
      patient <- append(patient, ISS)
      patient1 <- append(patient1, ISS1) 
      patient3 <- append(patient3, ISS3) 
      arrival<-append(arrival,t.clock)
      
      
      if (t.clock < plotSamples) { 
        
        qc <- append(qc,n)
        
        tc <- append(tc,t.clock)
      }
      tn <- t.clock
      t1 <- t.clock + rexp(1, 1/Ta)
      if(n == 1) { 
        
        tb <- t.clock
        t2 <- t.clock + rexp(1, 1/Ts)  # exponential  interarrival period
      }
    } else {            # departure event
      
      t.clock <- t2
      s <- s + n * (t.clock - tn)  # delta time-weighted number in queue
      n <- n - 1
      priority<-which.max(patient)
      priority1<-which.max(patient1)
      priority3<-which.max(patient3)
      
      waiting<-append(waiting,(t.clock-arrival[priority]))
      waiting1<-append(waiting1,(t.clock-arrival[priority1]))
      waiting3<-append(waiting3,(t.clock-arrival[priority3]))
      
      patient[priority]<-0
      patient1[priority1]<-0
      patient3[priority3]<-0
      
      
      if (t.clock < plotSamples) { 
        
        qc <- append(qc,n)
        
        tc <- append(tc,t.clock)
      }
      tn <- t.clock
      c <- c + 1
      if (n > 0) { 
        t2 <- t.clock + rexp(1, 1/Ts)  # exponential  service period
      }
      else { 
        t2 <- t.end
        b <- b + t.clock - tb
      }
    }   
  }
  u <- b/t.clock       # utilization B/T
  N <- s/t.clock       # mean queue length (see the Load Average notes)
  x <- c/t.clock       # mean throughput C/T
  r <- N/x             # mean residence time (from Little's law: Q = XR)
  q <- sum(qc)/max(tc) # estimated queue length for plot
  
  #waiting<-waiting[-length(waiting)]
  #waiting1<-waiting1[-length(waiting1)]
  #waiting3<-waiting3[-length(waiting3)]
  
  waiting<-waiting[is.finite(waiting)]
  waiting1<-waiting1[is.finite(waiting1)]
  waiting3<-waiting3[is.finite(waiting3)]
  
  
  #par(mfrow=c(2,3))
  #plot(waiting[critical], ylim=c(0,i), type="o", col="blue", pch=20, main="Waiting time for patients with critical injuries (sum)", xlab="Person with critical injuries", ylab="Waiting time")
  #plot(waiting1[critical], ylim=c(0,i), type="o", col="blue", pch=20, main="Waiting time for patients with critical injuries (sum of Squares)", xlab="Person with critical injuries", ylab="Waiting time")
  #plot(waiting3[critical], ylim=c(0,i), type="o", col="blue", pch=20, main="Waiting time for patients with critical injuries (sum of cubes)", xlab="Person with critical injuries", ylab="Waiting time")
  
  #plot(waiting, ylim=c(0,k), type="o", col="blue", pch=20, main="Waiting time for patients (sum)", xlab="Patient", ylab="Waiting time")
  #plot(waiting1, ylim=c(0,k), type="o", col="blue", pch=20, main="Waiting time for patients (sum of Squares)", xlab="Patient", ylab="Waiting time")
  #plot(waiting3, ylim=c(0,k), type="o", col="blue", pch=20, main="Waiting time for patients (sum of cubes)", xlab="Patient", ylab="Waiting time")
  
  #plot(tc, type="o", col="blue", pch=20, main="Wait times", xlab="Person", ylab="Wait time")
  average[number]= mean(waiting)
  average1[number]=mean(waiting1)
  average3[number]=mean(waiting3)
  
}
i<-min(average,average1,average3)-1
j<-max(average,average1,average3)+1
plot(average, type="l", col="blue", ylim=c(i,j))
lines(average1, type="l", col="red")
lines(average3, type="l", col="yellow")

#library(ggplot2)
#library(dplyr)
#library(tidyr)
#df<- data.frame(seq(1,100,by=1),average,average1,average3)
#ggplot(df, mapping=aes(x=(seq(1,100,by=1)), y=average, color="red")) +geom_line()
