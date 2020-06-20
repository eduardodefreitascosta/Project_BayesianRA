
##Packages###
library(meta)
library(bayesmeta)
library(ggplot2)
library(here)
library(readxl)
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

plot(1, type="n", xlab="Prevalence", ylab="Density", xlim=c(0, 1), ylim=c(0, 15),main="Posterior prevalence density")
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

meta2<-list()
crins1<-list()
corte<-c(1,2)
for (i in 1:length(corte)){
  
  crins1[[i]]<-escalc(measure = "PLO",xi=pos,ni=total,data=claudia, add=1/2,add.measure = T,slab = author, subset = part==corte[i])
  
  meta2[[i]]<-bayesmeta(y=crins1[[i]][,"yi"],sigma=sqrt(crins1[[i]][,"vi"]),mu.prior.mean = 1,mu.prior.sd =4,tau.prior=function(t){dhalfnormal(t,scale=0.5)},labels = claudia$author[claudia$part==corte[i]],rel.tol.integrate = 1.45e-5 )
  
}


plot(1, type="n", xlab="Prevalence", ylab="Density", xlim=c(0, 0.4), ylim=c(0, 16),main="Posterior prevalence density")
saida1<-array(rep(NA,10),dim=c(2,5,1))
id1<-c("Carcass","Cut")

for (i in 1:length(corte)){
  
  lines(density(back(rnorm(10000,meta2[[i]]$summary[3,2],meta2[[i]]$summary[4,2]))),col=corte[i])
  
  
  legend(0.3, 6, bty='n', id1, col=c(1:i),lty=1)
  
  
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


plot(1, type="n", xlab="Prevalence", ylab="Density", xlim=c(0, 0.4), ylim=c(0, 17),main="Posterior prevalence density")

saida2<-array(rep(NA,10),dim=c(2,5,1))

id2<-c("Retail","Slaugterhouse")
for (i in 1:length(loc)){
  
  
  lines(density(back(rnorm(10000,meta3[[i]]$summary[3,2],meta3[[i]]$summary[4,2]))),col=loc[i])
  
  
  legend(0.3, 9, bty='n', id2, col=c(1:i),lty=1)
  
  
  saida2[i, ,1]<-cbind((meta3[[i]]$summary[3,2]),
                       (meta3[[i]]$summary[4,2]),
                       (meta3[[i]]$summary[5,2]),
                       (meta3[[i]]$summary[2,2]),
                       (meta3[[i]]$summary[6,2]) )
}



save.image(file.path(here(),"Resultados","resultados.RData",sep=""))
