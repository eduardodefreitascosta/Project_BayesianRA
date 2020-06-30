
##Packages###
library(meta)
library(bayesmeta)
library(ggplot2)
library(here)
library(readxl)
library(tidyverse)
##creating function##


back<-function(x){exp(x)/(1+exp(x))}



##Importing data##


claudia<-read_excel(here("Dados","claudia_ma.xlsx"))

colnames(claudia)<-c("total","pos","prev","region","part","fresh","local","author")


####general analysis#####

crins<-escalc(measure = "PLO",xi=pos,ni=total,data=claudia,add.measure = T,slab = author)

meta1<-bayesmeta(y=crins[,"yi"],sigma=sqrt(crins[,"vi"]),mu.prior.mean = 1,mu.prior.sd =4,tau.prior=function(t){dhalfnormal(t,scale=0.5)} ,labels = claudia$author)

#Forest plot


forest(meta1, atransf=back, xlab="Prevalence", at =(c(-8,-5,-2, 0, 2)),cex=0.8,xlim=c(-12,6),main=" ")
text(-12, -0.5, "Author(s) and Year", pos = 4, cex=0.8)
text(6, -0.5, "Prevalence [95% CI]", pos = 2, cex=0.8)
text(0, -0.5, "Study Outcome", pos = 2, cex=0.8)
abline(h=c(-length(crins$yi)-1.8,-length(crins$yi)-4.2,0))

#density plot
plot(1, type="n", xlab="Prevalence", ylab="Density", xlim=c(0.05, 0.3), ylim=c(0, 22))

lines(density(back(rnorm(100000,meta1$summary[3,2],meta1$summary[4,2]))),col=1:6)





###Region analysis####

N<-c(1,2,3,4,5,6)
id<-c("Brazil","Latin America", "North America", "Africa","Asia","Europe")
meta1<-list()
crins<-list()
for (i in 1:length(N)){
  
crins[[i]]<-escalc(measure = "PLO",xi=pos,ni=total,data=claudia,add.measure = T,slab = author, subset = region==N[i])

meta1[[i]]<-bayesmeta(y=crins[[i]][,"yi"],sigma=sqrt(crins[[i]][,"vi"]),mu.prior.mean = 1,mu.prior.sd =4,tau.prior=function(t){dhalfnormal(t,scale=0.5)},labels = claudia$author[claudia$region==N[i]] )

}




data<-data.frame(matrix(rep(NA,10000*6), nrow=10000,ncol=6,byrow=T))

for (j in 1: length(N)){
  for(i in 1:10000){
 data[i,j]<-rnorm(1,meta1[[j]]$summary[3,2],meta1[[j]]$summary[4,2]) 

  }
}
 data$id<-seq(1:10000)
   
 colnames(data)<-id
 
 data2 <- gather(data = data,
                        key = "Pais",
                        value = "Prev",-id)
 

 ggplot(data2, aes(x = Pais, y = back(Prev)*100)) +
   geom_boxplot()+
   scale_y_continuous(name = "Prevalence %",breaks = seq(0, 100, 15),
                      limits=c(0, 85))+
   scale_x_discrete(name = "Region")
 
 
saida<-array(rep(NA,30),dim=c(6,5,1))

for (i in 1:length(N)){
  
lines(density(back(rnorm(10000,meta1[[i]]$summary[3,2],meta1[[i]]$summary[4,2]))),col=1,lty=N[i],cex.axis=1.5, cex.lab=1.5,lwd=3)


legend(0.7, 6, bty='n', id, col=1,lty=1:6,lwd=3)


saida[i, ,1]<-cbind( (meta1[[i]]$summary[3,2]),
                     (meta1[[i]]$summary[4,2]),
                     (meta1[[i]]$summary[5,2]),
                     (meta1[[i]]$summary[2,2]),
                     (meta1[[i]]$summary[6,2]) )
}


##Cut vs carcass analysis##
id1<-c("Carcass","Cut")
meta2<-list()
crins1<-list()
corte<-c(1,2)
for (i in 1:length(corte)){
  
  crins1[[i]]<-escalc(measure = "PLO",xi=pos,ni=total,data=claudia, add=1/2,add.measure = T,slab = author, subset = part==corte[i])
  
  meta2[[i]]<-bayesmeta(y=crins1[[i]][,"yi"],sigma=sqrt(crins1[[i]][,"vi"]),mu.prior.mean = 1,mu.prior.sd =4,tau.prior=function(t){dhalfnormal(t,scale=0.5)},labels = claudia$author[claudia$part==corte[i]],rel.tol.integrate = 1.45e-5 )
  
}



data3<-data.frame(matrix(rep(NA,10000*2), nrow=10000,ncol=2,byrow=T))

for (j in 1:length(corte)){
  for(i in 1:10000){
    data3[i,j]<-rnorm(1,meta2[[j]]$summary[3,2],meta2[[j]]$summary[4,2]) 
    
  }
}


colnames(data3)<-id1
data3$id<-seq(1:10000)

data4 <- gather(data = data3,
                key = "Cut",
                value = "Prev",-id)


ggplot(data4, aes(x = Cut, y = back(Prev)*100)) +
  geom_boxplot()+
  scale_y_continuous(name = "Prevalence %",breaks = seq(0, 100, 10),
                     limits=c(5, 40))+
  scale_x_discrete(name = "")


saida1<-array(rep(NA,10),dim=c(2,5,1))

for (i in 1:length(corte)){
  
  
  saida1[i, ,1]<-cbind((meta2[[i]]$summary[3,2]),
                       (meta2[[i]]$summary[4,2]),
                       (meta2[[i]]$summary[5,2]),
                       (meta2[[i]]$summary[2,2]),
                       (meta2[[i]]$summary[6,2]) )
}

##Local analysis##


meta3<-list()
crins2<-list()
loc<-c(1,2)

claudia1<-na.omit(claudia)

for (i in 1:length(loc)){
  
  crins2[[i]]<-escalc(measure = "PLO",xi=pos,ni=total,data=claudia1,subset = local==loc[i])
  
  meta3[[i]]<-bayesmeta(y=na.omit(crins2[[i]][,"yi"]),sigma=sqrt(na.omit(crins2[[i]][,"vi"])),mu.prior.mean = 1,mu.prior.sd =4,tau.prior=function(t){dhalfnormal(t,scale=0.5)} )
  
}



data5<-data.frame(matrix(rep(NA,10000*2), nrow=10000,ncol=2,byrow=T))
id2<-c("Retail","Slaugterhouse")
for (j in 1:length(loc)){
  for(i in 1:10000){
    data5[i,j]<-rnorm(1,meta3[[j]]$summary[3,2],meta3[[j]]$summary[4,2]) 
    
  }
}


colnames(data5)<-id2
data5$id<-seq(1:10000)

data6 <- gather(data = data5,
                key = "local",
                value = "Prev",-id)


ggplot(data6, aes(x = local, y = back(Prev)*100)) +
  geom_boxplot()+
  scale_y_continuous(name = "Prevalence %",breaks = seq(0, 100, 10),
                     limits=c(5, 40))+
  scale_x_discrete(name = "")





saida2<-array(rep(NA,10),dim=c(2,5,1))


for (i in 1:length(loc)){
 
  
  saida2[i, ,1]<-cbind((meta3[[i]]$summary[3,2]),
                       (meta3[[i]]$summary[4,2]),
                       (meta3[[i]]$summary[5,2]),
                       (meta3[[i]]$summary[2,2]),
                       (meta3[[i]]$summary[6,2]) )
}




##fresh vs frozen##


meta4<-list()
crins3<-list()
fro<-c(1,2)

claudia1<-na.omit(claudia)

for (i in 1:length(fro)){
  
  crins3[[i]]<-escalc(measure = "PLO",xi=pos,ni=total,data=claudia1,subset = fresh==loc[i])
  
  meta4[[i]]<-bayesmeta(y=na.omit(crins3[[i]][,"yi"]),sigma=sqrt(na.omit(crins3[[i]][,"vi"])),mu.prior.mean = 1,mu.prior.sd =4,tau.prior=function(t){dhalfnormal(t,scale=0.5)} )
  
}


data7<-data.frame(matrix(rep(NA,10000*2), nrow=10000,ncol=2,byrow=T))
id3<-c("Fresh","Frozen")
for (j in 1:length(fro)){
  for(i in 1:10000){
    data7[i,j]<-rnorm(1,meta4[[j]]$summary[3,2],meta4[[j]]$summary[4,2]) 
    
  }
}


colnames(data7)<-id3
data7$id<-seq(1:10000)

data8 <- gather(data = data7,
                key = "fro",
                value = "Prev",-id)


ggplot(data8, aes(x = fro, y = back(Prev)*100)) +
  geom_boxplot()+
  scale_y_continuous(name = "Prevalence %",breaks = seq(0, 100, 10),
                     limits=c(5, 40))+
  scale_x_discrete(name = "")





saida3<-array(rep(NA,10),dim=c(2,5,1))


for (i in 1:length(loc)){
  
  
  lines(density(back(rnorm(10000,meta4[[i]]$summary[3,2],meta4[[i]]$summary[4,2]))),col=loc[i])
  
  
  legend(0.3, 9, bty='n', id2, col=c(1:i),lty=1)
  
  
  saida3[i, ,1]<-cbind((meta4[[i]]$summary[3,2]),
                       (meta4[[i]]$summary[4,2]),
                       (meta4[[i]]$summary[5,2]),
                       (meta4[[i]]$summary[2,2]),
                       (meta4[[i]]$summary[6,2]) )
}




save.image(file.path(here(),"Resultados","resultados.RData",sep=""))
