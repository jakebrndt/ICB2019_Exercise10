# 1. 
#assigning variable values, time(days) will equal 1000, to start both growth rates are 0.1, the normal group will start with 1 cell with the mutant group starting 100 total cells, and carrying capacity is 1 million
timesteps=999
N=numeric(length=timesteps)
M=numeric(length=timesteps)
N[1]=1
M[1]=99
rN=.1
rM=.1
K=1000000
#assigning the new rates for post-drug treatment
rN2=0.05
rM2=-0.1
#We ran two for-loops, one (first) that occurred before drug treatment leading up to equilibrium, and then (second) after drug treatment
for (t in 1:249){
  N[t+1] <- N[t]+rN*N[t]*(1-((N[t]+M[t])/K))
  M[t+1] <- M[t]+rM*M[t]*(1-((N[t]+M[t])/K))
}
for (t in 249:999){
  N[t+1] <- N[t]+rN2*N[t]*(1-((N[t]+M[t])/K))
  M[t+1] <- M[t]+rM2*M[t]*(1-((N[t]+M[t])/K))
}
#here we create an empty vector "time" that will be from 1 to 1000
time <- c()
time <- seq(1,1000)
#at this point, we load ggplot2, make our vectors from the for-loops into dataframes and then plot them using ggplot
library(ggplot2)
Npop <- data.frame(time=1:length(N), mutant=N)
Mpop <- data.frame(time=1:length(M), nonmutant=M)
ggplot()+
  geom_line(data=Npop, aes(x=time, y=N, color="blue"))+
  geom_line(data=Mpop, aes(x=time, y=M, color="red"))+
  ylab("Population")+
  xlab("Time (days)")+
  scale_color_discrete(name = "Sub-Population", labels = c("Mutant", "Non-Mutant"))
