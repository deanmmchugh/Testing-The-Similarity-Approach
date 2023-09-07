library(tidyverse)
library(broom)
library(lme4)
library(mclogit)

data(Transport) 

Transport 
model<- mclogit(cbind(resp,suburb) ~ distance+cost, data=Transport )

summary(model)

data(electors) 
electors<-within(electors,{party.time<-interaction(party,time)})
electors<-within(electors,{time.class<-interaction(time,class)})
#Timepointsnestedwithinparties 
summary(mclogit( Freq|time.class~econ.left/class+welfare/class+auth/class, random=~1|party/time, data=electors)) 
#Party-levelrandominterceptsandrandomslopesvaryingovertimepoints 
summary(mclogit( Freq|time.class~econ.left/class+welfare/class+auth/class, random=list(~1|party,~econ.left+0|time), data=electors))

electors
