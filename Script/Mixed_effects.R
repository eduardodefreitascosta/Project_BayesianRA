
##Importing data##

claudia2<-read_excel(here("Data","claudia_ma2.xlsx"),sheet="Prevalencia-metaanalise")

# Logit transformation
crins<-escalc(xi=claudia2$pos,ni=claudia2$total,measure = "PLO")

#set back trans
back<-function(x){1/(1+exp(-x))}

##Tidy into a dataframe
crins$regiao<-factor(claudia2$regiao)
crins$part<-factor(claudia2$part)
crins$cond<-factor(claudia2$cond)
crins$local<-factor(claudia2$local)
crins$id<-1:104


priors <-list(  base=c(prior(normal(0,100), class = Intercept),
                       prior(gamma(0.001,0.001), class = sd),
                       prior(normal(0,100),class=b)),
           
               cen1= c(prior(normal(0,1), class = Intercept),
                       prior(gamma(0.001,0.001), class = sd),
                       prior(normal(0,1),class=b)),
          
               cen2= c(prior(normal(-2,1), class = Intercept),
                       prior(gamma(0.001,0.001), class = sd),
                       prior(normal(-2,1),class=b)),
          
               cen3= c(prior(normal(-2,100), class = Intercept),
                       prior(gamma(0.001,0.001), class = sd),
                       prior(normal(-2,100),class=b)),
               
               cen4= c(prior(normal(2,1), class = Intercept),
                       prior(gamma(0.001,0.001), class = sd),
                       prior(normal(2,1),class=b)),
               
               cen5= c(prior(normal(2,100), class = Intercept),
                       prior(gamma(0.001,0.001), class = sd),
                       prior(normal(2,100),class=b))

           )

m.brm<-list()

for (j in 1:6){
  
m.brm[[j]] <- brm(yi|se(vi) ~ relevel(regiao,ref="3")+part+local+cond+ (1|id),
              data = crins,
              prior = priors[[j]],
              iter = 100000,
              chains = 2,
              thin=50
              )
}



#Baseline regression output
capture.output(m.brm[[1]], file = here("Results","Table4.txt"))

#Model convergency
#plots<-plot(m.brm[[1]])


# Posterior distributions including scenarios

post<-posterior_samples(m.brm[[1]])

post<-post[,1:9]
colnames(post)<-c("North America", "Brazil", "Latin America", "Africa", "Asia", "Europe","Cuts","Frozen","Slaughterhouse")



X=matrix(c(
  x1=rep(1,48),
  x2<-c(c(0,0,0,0,0,0,0,0),c(1,1,1,1,1,1,1,1),rep(c(0,0,0,0,0,0,0,0),4)),
  x3<-c(rep(c(0,0,0,0,0,0,0,0),2),c(1,1,1,1,1,1,1,1),rep(c(0,0,0,0,0,0,0,0),3)),
  x4<-c(rep(c(0,0,0,0,0,0,0,0),3),c(1,1,1,1,1,1,1,1),rep(c(0,0,0,0,0,0,0,0),2)),
  x5<-c(rep(c(0,0,0,0,0,0,0,0),4),c(1,1,1,1,1,1,1,1),rep(c(0,0,0,0,0,0,0,0),1)),
  x6<-c(rep(c(0,0,0,0,0,0,0,0),5),c(1,1,1,1,1,1,1,1)),
  x7<-rep(c(0,1,1,1,1,0,0,0),6),
  x8<-rep(c(0,0,0,1,1,1,1,0),6),
  x9<-rep(c(0,0,1,0,1,0,1,1),6)
  
),nrow=48,
)


posterior<-X%*%t(post)


posterior2<-cbind.data.frame(posterior,X[,7:9])

d1<-dim(posterior)[2]+1
d2<-dim(posterior)[2]+3

colnames(posterior2)[d1:d2]<-c("Cuts","Frozen","Slaughterhouse")
posterior2$country<-c(rep("N. America",8),rep("Brazil",8),rep("L. America",8),rep("Africa",8),rep("Asia",8),rep("Europe",8))
posterior2$id<-1:48

posterior3<-posterior2%>%
  gather(key="test",value="logit",-id,-Cuts,-Frozen,-Slaughterhouse,-country)

posterior3$Cuts <- factor(posterior3$Cuts, labels = c("Carcass", "Cuts"))
posterior3$Frozen <- factor(posterior3$Frozen, labels = c("Fresh", "Frozen"))
posterior3$Slaughterhouse <- factor(posterior3$Slaughterhouse, labels = c("Retail", "Slaughterhouse"))


ggplot(posterior3,aes(x=back(logit),
                      y=fct_reorder(country,logit),fill=country))+
  theme_minimal()+
  geom_density_ridges(alpha = 0.8) +
  facet_grid(Cuts~Frozen+Slaughterhouse,switch="y")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5)) +
  guides(fill = guide_legend(title="Countries"))+
  labs(x = "Prevalence")+
  theme(legend.position = "right",
        legend.text = element_text(size = 12, colour = "black"),
        text = element_text(size=15),
        axis.title.x = element_text(color = "Black", size = 14, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.5),
    
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
        scale_x_continuous(labels = scales::percent_format(accuracy = 1))

ggsave(here("Figures","Fig2.jpg"),dpi=300)


table<-posterior3%>%
  group_by(country,Cuts,Frozen,Slaughterhouse)%>%
  summarise(media1=mean(logit),sd1=sd(logit),mean2=mean(back(logit)),sd2=sd(back(logit)))
  
#capture.output(table, file = here("Results","Table5.txt"),sep)

write.table(table,here("Results","table5.csv"),sep=";",row.names=FALSE)


#Sensitivity analysis



sens_sce<-list(
  Baseline=posterior_samples(m.brm[[1]]),
  Scenario1=posterior_samples(m.brm[[2]]),
  Scenario2=posterior_samples(m.brm[[3]]),
  Scenario3=posterior_samples(m.brm[[4]]),
  Scenario4=posterior_samples(m.brm[[5]]),
  Scenario5=posterior_samples(m.brm[[6]])
)

sens_sce1<-list(
  Baseline=cbind.data.frame(scenario=rep("Baseline",2000),sens_sce$Baseline[,1:9]),
  Scenario1=cbind.data.frame(scenario=rep("Scenario 1",2000),sens_sce$Scenario1[,1:9]),
  Scenario2=cbind.data.frame(scenario=rep("Scenario 2",2000),sens_sce$Scenario2[,1:9]),
  Scenario3=cbind.data.frame(scenario=rep("Scenario 3",2000),sens_sce$Scenario3[,1:9]),
  Scenario4=cbind.data.frame(scenario=rep("Scenario 4",2000),sens_sce$Scenario4[,1:9]),
  Scenario5=cbind.data.frame(scenario=rep("Scenario 5",2000),sens_sce$Scenario5[,1:9])
)


final_sce<-rbind.data.frame(sens_sce1$Baseline,
                            sens_sce1$Scenario1,
                            sens_sce1$Scenario2,
                            sens_sce1$Scenario3,
                            sens_sce1$Scenario4,
                            sens_sce1$Scenario5)

col_names<-c("scenario","bo","bbr","bla","baf","bas","beu","bport","bcond","blocal")



colnames(final_sce)<-col_names


sce_plot<-final_sce%>%
  gather(key="parameter",value="estimate",-scenario)


sce_plot$parameter1 <- factor(
  sce_plot$parameter,
  levels = c("baf", "bas", "bbr","bcond","beu","bla","blocal","bo","bport"),
  labels = c(
    "beta[Africa]", 
    "beta[Asia]", 
    "beta[Brazil]",
    "beta[cold]",
    "beta[Europe]",
    "beta[Latin_America]",
    "beta[sample]",
    "beta[0]",
    "beta[processing]"
  ))




ggplot(sce_plot,aes(x=estimate,fill=scenario))+
  theme_minimal()+
  geom_density(alpha=0.7)+
  facet_wrap(~ parameter1, ncol = 3,
             labeller = label_parsed)+
  guides(fill = guide_legend(title="Scenarios"))+
  labs(x = "Estimate (logit scale)")+
  theme(legend.position = "right",
        legend.text = element_text(size = 12, colour = "black"),
        text = element_text(size=15),
        axis.title.x = element_text(color = "Black", size = 14, face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1.5),
        
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_fill_discrete(name = "Dose", labels = c("Baseline", "N(0,1)", "N(-2,1)","N(-2,100)","N(2,1)","N(2,100)"))
ggsave(here("Figures","Fig3.jpg"),dpi=300)  
  

save.image(here("Results","mixed_model.RData"))
