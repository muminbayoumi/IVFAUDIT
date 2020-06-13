source("load-clean-data.R")



fluidstatuscap<- iv$FluidStatusDOC %>% table %>% names()

levels(iv$FluidStatusDOC) <-c('Not Documented','Euovolemia','Hypovolemia')




library(reshape2)
melt(iv,measure.vars =c('AppMain','AppReplace','AppResus'),value.name = "Indication") %>% filter(Indication!="Not Required") %>%
        ggplot(aes(variable,fill=fct_infreq(Indication)))+geom_bar(position = 'dodge')



ggplot(iv,aes(Weight7D))+ geom_bar()
ggplot(iv,aes(UE24H))+geom_bar()
ggplot(iv,aes(FluidIndication))+geom_bar()
ggplot(iv,aes(IVF24.FPDOC))+geom_bar()
ggplot(iv,aes(FluidBalance24H))+geom_bar()


prop1 <- melt(iv,measure.vars = names(iv[c(8,10:12)]),variable.name = "measuredindicator",value.name ="achieved" ) %>%
        group_by(measuredindicator,achieved) %>%
        summarise(count=sum((as.numeric(as.factor(achieved)))),prop=round(count/32,2)) %>% filter(achieved=="Yes")

melt(iv,measure.vars = names(iv[c(8,10:12)]),variable.name = "measuredindicator",value.name ="achieved" ) %>%
        ggplot(aes(x=measuredindicator,fill=achieved))+geom_bar(position = "fill")+
        scale_fill_viridis_d(option="cividis")+
        geom_label(data=prop1,aes(x=measuredindicator,y=prop),label=prop1$prop)+
        scale_x_discrete(labels=str_wrap(c("Weighed every 7 days","U&E's every 24 hours","Fluid Indication Noted In Plan","24 hour fluids prescribed or fluid plan documented"),15))+
        labs(x="Measured Indicator",
             y="Proportion of patients",
             caption="Fluid Indication is one of 3 as per NICE guidance. Resus. Replace. Maintain",
             fill="Has it been\nachieved?")


prop2 <- melt(iv,measure.vars = names(iv[c(14:16)]),variable.name = "measuredindicator",value.name ="achieved" ) %>%
        group_by(measuredindicator,achieved) %>%
        summarise(count=sum((as.numeric(as.factor(achieved)))),prop=round(count/32,2)) %>% filter(achieved=="Yes")

melt(iv,measure.vars = names(iv[c(14:16)]),variable.name = "measuredindicator",value.name ="achieved" ) %>%
        ggplot(aes(x=measuredindicator,fill=achieved))+geom_bar(position = "fill")+
        scale_fill_viridis_d(option="cividis")+
        geom_label(data=prop2,aes(x=measuredindicator,y=prop),label=prop2$prop)+
        scale_x_discrete(labels=str_wrap(c("IVF Volume Noted","IVF Rate Noted","Maintaince dose IV K+ Prescribed"),15))+
        labs(x="Measured Indicator",
             y="Proportion of patients",
             caption="Daily Fluid Requirment: 25-30ml/Kg (20-25ml/Kg if frail or allowing for HF)
             \nDaily Na+ & K+ Requirments : 1 mmol/Kg",
             fill="Has it been\nachieved?")
