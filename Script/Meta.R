
##Packages###

#Packages to be used
packages<-c("meta","bayesmeta","ggplot2","here","readxl",
            "tidyverse","grid","ggridges","ggthemes","extrafont")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


##creating function##

back<-function(x){exp(x)/(1+exp(x))}



#Set graphics
theme_set(theme_minimal())
#font_import()

#loadfonts(device="win")

#Register fonts for Windows bitmap output
#fonts()



##Importing data##


claudia<-read_excel(here("Data","claudia_ma.xlsx"))

colnames(claudia)<-c("total","pos","prev","region","part","fresh","local","author")


####general analysis#####

crins<-escalc(measure = "PLO",xi=pos,ni=total,data=claudia,add.measure = T,slab = author)

meta<-bayesmeta(y=crins[,"yi"],sigma=sqrt(crins[,"vi"]),
                mu.prior.mean = 0,mu.prior.sd =4,tau.prior=function(t){dhalfnormal(t,scale=0.5)},labels = claudia$author)


#density plot
plot(1, type="n", xlab="Prevalence", ylab="Density", xlim=c(0.05, 0.3), ylim=c(0, 22))

lines(density(back(rnorm(100000,meta$summary[3,2],meta$summary[4,2]))),col=1:6)





###Region analysis####

N<-c(1,2,3,4,5,6)
id<-c("Brazil","Latin America", "North America", "Africa","Asia","Europe")
meta1<-list()
crins<-list()
for (i in 1:length(N)){
  
crins[[i]]<-escalc(measure = "PLO",xi=pos,ni=total,data=claudia,add.measure = T,slab = author, subset = region==N[i])

meta1[[i]]<-bayesmeta(y=crins[[i]][,"yi"],sigma=sqrt(crins[[i]][,"vi"]),
                      mu.prior.mean = 0,mu.prior.sd =4,
                      tau.prior=function(t){dhalfnormal(t,scale=0.5)},labels = claudia$author[claudia$region==N[i]] )

}

names(meta1)<-id  

data<-data.frame(matrix(rep(NA,10000*6), nrow=10000,ncol=6,byrow=T))

for (j in 1: length(N)){
  for(i in 1:10000){
 data[i,j]<-rnorm(1,meta1[[j]]$summary[3,2],meta1[[j]]$summary[4,2]) 

  }
}
colnames(data)<-id

data$id<-seq(1:10000)
   
#Density plots
data2 <- gather(data = data,
                        key = "Pais",
                        value = "Prev",-id)

tiff(here("Figures","Density_1.jpg"), units="in", width=8, height=5, res=300)

 print(
   ggplot(data2,aes(x = back(Prev), y = Pais)) +
   geom_density_ridges(from = 0, to = 0.7) +
   #geom_boxplot(aes(fill = Pais), width = 0.06, outlier.shape = NA)+
   xlab(substitute(paste("Posterior distribution of ",italic('Salmonella '),"sp."," prevalence") ))+
   ylab("World geographic Country")+
   theme(
     axis.title.x = element_text( size=13),
     axis.title.y = element_text( size=13))+
   theme(text=element_text(family="Times New Roman", face="bold", size=15))+
   theme(axis.text.x = element_text(vjust = 5))+
   theme(legend.position = "none")+
   scale_x_continuous(labels = scales::percent_format(accuracy = 1))
 )
 dev.off()
 

#Outputs from the model
output<-array(rep(NA,30),dim=c(6,5,1))

for (i in 1:length(N)){
  
output[i, ,1]<-cbind((meta1[[i]]$summary[3,2]),
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
  
  meta2[[i]]<-bayesmeta(y=crins1[[i]][,"yi"],sigma=sqrt(crins1[[i]][,"vi"]),
                        mu.prior.mean = 0,mu.prior.sd =4,tau.prior=function(t){dhalfnormal(t,scale=0.5)},labels = claudia$author[claudia$part==corte[i]],rel.tol.integrate = 1.45e-5 )
  
}

names(meta2)<-id1

data3<-data.frame(matrix(rep(NA,10000*2), nrow=10000,ncol=2,byrow=T))

for (j in 1:length(corte)){
  for(i in 1:10000){
    data3[i,j]<-rnorm(1,meta2[[j]]$summary[3,2],meta2[[j]]$summary[4,2]) 
    
  }
}


colnames(data3)<-id1
data3$id<-seq(1:10000)


#Density plots

data4 <- gather(data = data3,
                key = "Cut",
                value = "Prev",-id)

tiff(here("Figures","Density_2.jpg"), units="in", width=8, height=5, res=300)
print(
ggplot(data4,aes(x = back(Prev), y = Cut)) +
  geom_density_ridges(from = 0.05, to = 0.3) +
  #geom_boxplot(aes(fill = Cut), width = 0.06, outlier.shape = NA)+
  xlab(substitute(paste("Posterior distribution of ",italic('Salmonella '),"sp."," prevalence") )) + 
  ylab("Cuts")+
  theme(
    axis.title.x = element_text( size=13),
    axis.title.y = element_text( size=13))+
  theme(text=element_text(family="Times New Roman", face="bold", size=15))+
  theme(axis.text.x = element_text(vjust = 5))+
  #theme(axis.text.x = element_text(hjust = 5))+
  theme(legend.position = "none")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 1))
)
dev.off()



#Outputs of the model

output1<-array(rep(NA,10),dim=c(2,5,1))

for (i in 1:length(corte)){
  
  
  output1[i, ,1]<-cbind((meta2[[i]]$summary[3,2]),
                       (meta2[[i]]$summary[4,2]),
                       (meta2[[i]]$summary[5,2]),
                       (meta2[[i]]$summary[2,2]),
                       (meta2[[i]]$summary[6,2]) )
}



##Local analysis##
meta3<-list()
crins2<-list()
loc<-c(1,2)
id2<-c("Retail","Slaugterhouse")

claudia1<-na.omit(claudia)

for (i in 1:length(loc)){
  
  crins2[[i]]<-escalc(measure = "PLO",xi=pos,ni=total,data=claudia1,subset = local==loc[i])
  
  meta3[[i]]<-bayesmeta(y=na.omit(crins2[[i]][,"yi"]),
                        sigma=sqrt(na.omit(crins2[[i]][,"vi"])),mu.prior.mean = 0,mu.prior.sd =4,tau.prior=function(t){dhalfnormal(t,scale=0.5)} )
  
}


names(meta3)<-id2

data5<-data.frame(matrix(rep(NA,10000*2), nrow=10000,ncol=2,byrow=T))

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

#Density plot
tiff(here("Figures","Density_3.jpg"), units="in", width=8, height=5, res=300)

print(
  ggplot(data6,aes(x = back(Prev), y = local)) +
  geom_density_ridges(from=0.05,to=0.35) +
  #geom_boxplot(aes(fill = Cut), width = 0.06, outlier.shape = NA)+
  xlab(substitute(paste("Posterior distribution of ",italic('Salmonella '),"sp."," prevalence") )) + 
  ylab("Place of collection")+
  theme(
    axis.title.x = element_text( size=13),
    axis.title.y = element_text( size=13))+
  theme(text=element_text(family="Times New Roman", face="bold", size=15))+
  theme(axis.text.x = element_text(vjust = 5))+
  #theme(axis.text.x = element_text(hjust = 5))+
  theme(legend.position = "none")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1))
)
  dev.off()


#Outputs of the model
output2<-array(rep(NA,10),dim=c(2,5,1))


for (i in 1:length(loc)){
 
  
  output2[i, ,1]<-cbind((meta3[[i]]$summary[3,2]),
                       (meta3[[i]]$summary[4,2]),
                       (meta3[[i]]$summary[5,2]),
                       (meta3[[i]]$summary[2,2]),
                       (meta3[[i]]$summary[6,2]) )
}




##fresh vs frozen##
meta4<-list()
crins3<-list()
fro<-c(1,2)
id3<-c("Fresh","Frozen")

claudia1<-na.omit(claudia)

for (i in 1:length(fro)){
  
  crins3[[i]]<-escalc(measure = "PLO",xi=pos,ni=total,data=claudia1,subset = fresh==loc[i])
  
  meta4[[i]]<-bayesmeta(y=na.omit(crins3[[i]][,"yi"]),
                        sigma=sqrt(na.omit(crins3[[i]][,"vi"])),
                        mu.prior.mean = 0,mu.prior.sd =4,tau.prior=function(t){dhalfnormal(t,scale=0.5)} )
  
}


names(meta4)<-id3

data7<-data.frame(matrix(rep(NA,10000*2), nrow=10000,ncol=2,byrow=T))
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

#Density plot

tiff(here("Figures","Density_4.jpg"), units="in", width=8, height=5, res=300)

print(
ggplot(data8,aes(x = back(Prev), y = fro)) +
  geom_density_ridges(from = 0.05, to = 0.35) +
  #geom_boxplot(aes(fill = Cut), width = 0.06, outlier.shape = NA)+
  xlab(substitute(paste("Posterior distribution of ",italic('Salmonella '),"sp."," prevalence") )) + 
  ylab("Physical state")+
  theme(
    axis.title.x = element_text( size=13),
    axis.title.y = element_text( size=13))+
  theme(text=element_text(family="Times New Roman", face="bold", size=15))+
  theme(axis.text.x = element_text(vjust = 5))+
  #theme(axis.text.x = element_text(hjust = 5))+
  theme(legend.position = "none")+
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1))
)
dev.off()



#Outputs of the model
output3<-array(rep(NA,10),dim=c(2,5,1))


for (i in 1:length(loc)){
  
  
  output3[i, ,1]<-cbind((meta4[[i]]$summary[3,2]),
                       (meta4[[i]]$summary[4,2]),
                       (meta4[[i]]$summary[5,2]),
                       (meta4[[i]]$summary[2,2]),
                       (meta4[[i]]$summary[6,2]) )
}




save.image(here("Results","results.RData"))
