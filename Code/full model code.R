library(Boruta)
library(readxl)
library(caret)
library(tidyverse)
library(randomForest)
library("ggbiplot", lib.loc="~/R/R-3.5.1/library")
library(lattice)
library(ranger)
library(mice)
library(cowplot)
library(devtools) 
library(randomForestCI)
library(ggpmisc)
library(forcats)


####load data###
whole_ch <- read_csv("data/whole.ch.csv")

####Impute data####
measured_ch<-whole_ch[,-c(7:10)]

tempdata<-mice(measured_ch,m=5,maxit=50,meth='cart',seed=500)

tempdata$imp$port.revenue

NN.data<-complete(tempdata,1)

####VIFs#### 

head(NN.data)

corvif(NN.data[,-c(1:6)])

corvif(NN.data[,-c(1:7,13,14)]) #final variables under 5

#########Boruta Variable selection####

##select art linear data
NN.art.data<-NN.data[,-c(3:6)]


art.boruta.full<-Boruta(artificial.linear ~ .,
                        data=NN.art.data,
                        maxRuns=200,
                        doTrace=0)

art.borutaVars <- getSelectedAttributes(art.boruta.full)

plot(art.boruta.full, 
     whichShadow = c(FALSE, FALSE, FALSE), 
     cex.axis = .7, 
     las = 2, 
     boxwex = 0.6, 
     xlab = "", main = "Variable Importance")

art.boruta.formula <- formula(paste("artificial.linear ~ ", 
                                    paste(art.borutaVars, collapse=" + ")))

art.boruta.full<-Boruta(art.boruta.formula,
                        data=NN.art.data,
                        maxRuns=200,
                        doTrace=0)

art.boruta.formula


####Prediction artifical linear

fitControl = trainControl(method = "cv",
                          number = 10,
                          #repeats = 10,
                          returnResamp = "all",
                          savePredictions = "all")

ind <- index(NN.art.data,0.8)
length(ind$train); length(ind$test)



rf.art.fit2 <- train(art.boruta.formula,
                     data =na.omit(NN.art.data[ind$train,]), 
                     trControl = fitControl,
                     importance=T,
                     tuneLength = 4,  # final value was mtry = 4
                     preProcess = c("center", "scale"),
                     method = "rf",
                     keep.inbag=TRUE,
                     metric="RMSE")

png('art.importnace.png')
varImpPlot(rf.art.fit2$finalModel)
dev.off()

ggsave("total.linear.imp.plot.png",dpi=500, height=6, width=5)


predict<-predict(rf.art.fit2,newdata=NN.art.data)

obs<- NN.art.data$artificial.linear

mean(obs)

39/71


postResample(pred=predict,obs= NN.art.data$artificial.linear)


#####SEs
art.rf<-randomForest(art.boruta.formula,data = na.omit(NN.art.data), ntree=1000,keep.forest=T, keep.inbag=T,keep.importance=T)

comp.art.var<-randomForestInfJack(art.rf,newdata=NN.art.data,calibrate = TRUE)
comp.art.se<-sqrt(comp.art.var)

art.ses<-comp.art.se$var.hat
plot.pred<-cbind(predict,obs,art.ses)

my.formula<-y~x

linear.comp.plot<-ggplot(as.data.frame(plot.pred),aes(x=obs, y=predict))+geom_point()+
  geom_smooth(method="lm", se =FALSE,colour="black",formula=my.formula )+
  geom_errorbar(aes(ymax=predict+art.ses,ymin=predict-art.ses))+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  scale_y_continuous(name="Predicted linear hardening")+
  scale_x_continuous(name="Observed linear hardening")+
  theme_classic()

ggsave("linear.comparison.plot.png",dpi=500, height=5, width=5)



#####BW 
NN.BW.data<-NN.data[,-c(2,4:6)]


BW.boruta.full<-Boruta(CH.BW ~ .,
                        data=NN.BW.data,
                        maxRuns=200,
                        doTrace=0)

BW.borutaVars <- getSelectedAttributes(BW.boruta.full)

plot(BW.boruta.full, 
     whichShadow = c(FALSE, FALSE, FALSE), 
     cex.axis = .7, 
     las = 2, 
     boxwex = 0.6, 
     xlab = "", main = "Variable Importance")

BW.boruta.formula <- formula(paste("CH.BW ~ ", 
                                    paste(art.borutaVars, collapse=" + ")))

BW.boruta.full<-Boruta(BW.boruta.formula,
                        data=NN.BW.data,
                        maxRuns=200,
                        doTrace=0)

BW.boruta.formula


####Prediction BW 

fitControl = trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 10,
                          returnResamp = "all",
                          savePredictions = "all")

rf.BW.fit2 <- train(BW.boruta.formula,
                     data =NN.BW.data, 
                     trControl = fitControl,
                     importance=T,
                     
                     tuneLength = 4,  # final value was mtry = 4
                     preProcess = c("center", "scale"),
                     method = "rf",
                     metric="RMSE")


png('BW.importnace.png')
varImpPlot(rf.BW.fit2$finalModel)
dev.off()



BW.predict<-predict(rf.BW.fit2,newdata=NN.BW.data)

BW.obs<- NN.BW.data$CH.BW

postResample(pred=BW.predict,obs= BW.obs)

BW.plot.pred<-cbind(BW.obs,BW.predict)

summary(BW.obs)

###SEs

BW.rf<-randomForest(BW.boruta.formula,data = na.omit(NN.BW.data), ntree=1000,keep.forest=T, keep.inbag=T,keep.importance=T)

comp.BW.var<-randomForestInfJack(BW.rf,newdata=NN.art.data,calibrate = TRUE)
comp.BW.se<-sqrt(comp.BW.var)

BW.ses<-comp.BW.se$var.hat
BW.plot.pred<-cbind(BW.predict,BW.obs,BW.ses)


my.formula<-y~x

BW.comp.plot<-ggplot(as.data.frame(BW.plot.pred),aes(x=BW.obs, y=BW.predict))+geom_point()+
  geom_smooth(method="lm", se =FALSE,colour="black",formula=my.formula )+
  geom_errorbar(aes(ymax=BW.predict+BW.ses,ymin=BW.predict-BW.ses))+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  scale_y_continuous(name="Predicted Breakwater")+
  scale_x_continuous(name="Observed Breakwater")+
  theme_classic()

ggsave("BW.comparison.plot.png",dpi=500, height=5, width=5)

##### Jetty  
View(NN.data)

NN.jetty.data<-NN.data[,-c(2:3,5:6)]


jetty.boruta.full<-Boruta(CH.jetty ~ .,
                       data=NN.jetty.data,
                       maxRuns=200,
                       doTrace=0)

jetty.borutaVars <- getSelectedAttributes(jetty.boruta.full)

plot(jetty.boruta.full, 
     whichShadow = c(FALSE, FALSE, FALSE), 
     cex.axis = .7, 
     las = 2, 
     boxwex = 0.6, 
     xlab = "", main = "Variable Importance")

jetty.boruta.formula <- formula(paste("CH.jetty ~ ", 
                                   paste(art.borutaVars, collapse=" + ")))

jetty.boruta.full<-Boruta(jetty.boruta.formula,
                       data=NN.jetty.data,
                       maxRuns=200,
                       doTrace=0)

jetty.boruta.formula


####Prediction jetty 

fitControl = trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 10,
                          returnResamp = "all",
                          savePredictions = "all")

rf.jetty.fit2 <- train(jetty.boruta.formula,
                    data =NN.jetty.data, 
                    trControl = fitControl,
                    importance=T,
                    ntree=1000,
                    tuneLength = 4,  # final value was mtry = 4
                    preProcess = c("center", "scale"),
                    method = "rf",
                    metric="RMSE")


png('jetty.importnace.png')
varImpPlot(rf.jetty.fit2$finalModel)
dev.off()


jetty.predict<-predict(rf.jetty.fit2,newdata=NN.jetty.data)

jetty.obs<- NN.jetty.data$CH.jetty

postResample(pred=jetty.predict,obs= jetty.obs)

jetty.plot.pred<-cbind(jetty.obs,jetty.predict)

###SEs

jetty.rf<-randomForest(jetty.boruta.formula,data = na.omit(NN.jetty.data), ntree=1000,keep.forest=T, keep.inbag=T,keep.importance=T)

comp.jetty.var<-randomForestInfJack(jetty.rf,newdata=NN.jetty.data,calibrate = TRUE)
comp.jetty.se<-sqrt(comp.jetty.var)

jetty.ses<-comp.jetty.se$var.hat
jetty.plot.pred<-cbind(jetty.predict,jetty.obs,jetty.ses)

my.formula<-y~x

jetty.comp.plot<-ggplot(as.data.frame(jetty.plot.pred),aes(x=jetty.obs, y=jetty.predict))+geom_point()+
  geom_smooth(method="lm", se =FALSE,colour="black",formula=my.formula )+
  geom_errorbar(aes(ymax=jetty.predict+jetty.ses,ymin=jetty.predict-jetty.ses))+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  scale_y_continuous(name="Predicted Jetty")+
  scale_x_continuous(name="Observed Jetty")+
  theme_classic()

ggsave("jetty.comparison.plot.png",dpi=500, height=5, width=5)

#####Pontoon
NN.pontoon.data<-NN.data[,-c(2:4,6)]


pontoon.boruta.full<-Boruta(ch.pontoon ~ .,
                       data=NN.pontoon.data,
                       maxRuns=200,
                       doTrace=0)

pontoon.borutaVars <- getSelectedAttributes(pontoon.boruta.full)

plot(pontoon.boruta.full, 
     whichShadow = c(FALSE, FALSE, FALSE), 
     cex.axis = .7, 
     las = 2, 
     boxwex = 0.6, 
     xlab = "", main = "Variable Importance")

pontoon.boruta.formula <- formula(paste("ch.pontoon ~ ", 
                                   paste(art.borutaVars, collapse=" + ")))

pontoon.boruta.full<-Boruta(pontoon.boruta.formula,
                       data=NN.pontoon.data,
                       maxRuns=200,
                       doTrace=0)

pontoon.boruta.formula


####Prediction pontoon 

fitControl = trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 10,
                          returnResamp = "all",
                          savePredictions = "all")

rf.pontoon.fit2 <- train(pontoon.boruta.formula,
                    data =NN.pontoon.data, 
                    trControl = fitControl,
                    importance=T,
                    
                    tuneLength = 4,  # final value was mtry = 4
                    preProcess = c("center", "scale"),
                    method = "rf",
                    metric="RMSE")


png('pontoon.importnace.png')
varImpPlot(rf.pontoon.fit2$finalModel)
dev.off()


pontoon.predict<-predict(rf.pontoon.fit2,newdata=NN.pontoon.data)

pontoon.obs<- NN.pontoon.data$ch.pontoon

postResample(pred=pontoon.predict,obs= pontoon.obs)

pontoon.plot.pred<-cbind(pontoon.obs,pontoon.predict)

###SEs

pontoon.rf<-randomForest(pontoon.boruta.formula,data = na.omit(NN.pontoon.data), ntree=1000,keep.forest=T, keep.inbag=T,keep.importance=T)

comp.pontoon.var<-randomForestInfJack(pontoon.rf,newdata=NN.pontoon.data,calibrate = TRUE)
comp.pontoon.se<-sqrt(comp.pontoon.var)

pontoon.ses<-comp.pontoon.se$var.hat
pontoon.plot.pred<-cbind(pontoon.predict,pontoon.obs,pontoon.ses)

my.formula<-y~x

pontoon.comp.plot<-ggplot(as.data.frame(pontoon.plot.pred),aes(x=pontoon.obs, y=pontoon.predict))+geom_point()+
  geom_smooth(method="lm", se =FALSE,colour="black",formula=my.formula )+
  geom_errorbar(aes(ymax=pontoon.predict+pontoon.ses,ymin=pontoon.predict-pontoon.ses))+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  scale_y_continuous(name="Predicted Pontoon")+
  scale_x_continuous(name="Observed Pontoon")+
  theme_classic()

ggsave("pontoon.comparison.plot.png",dpi=500, height=5, width=5)

#####wharf 
NN.wharf.data<-NN.data[,-c(2:5)]


wharf.boruta.full<-Boruta(ch.wharf ~ .,
                       data=NN.wharf.data,
                       maxRuns=200,
                       doTrace=0)

wharf.borutaVars <- getSelectedAttributes(wharf.boruta.full)

plot(wharf.boruta.full, 
     whichShadow = c(FALSE, FALSE, FALSE), 
     cex.axis = .7, 
     las = 2, 
     boxwex = 0.6, 
     xlab = "", main = "Variable Importance")

wharf.boruta.formula <- formula(paste("ch.wharf ~ ", 
                                   paste(art.borutaVars, collapse=" + ")))

wharf.boruta.full<-Boruta(wharf.boruta.formula,
                       data=NN.wharf.data,
                       maxRuns=200,
                       doTrace=0)

wharf.boruta.formula


####Prediction wharf 

fitControl = trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 10,
                          returnResamp = "all",
                          savePredictions = "all")

rf.wharf.fit2 <- train(wharf.boruta.formula,
                    data =NN.wharf.data, 
                    trControl = fitControl,
                    importance=T,
                    ntree=500,
                    tuneLength = 4,  # final value was mtry = 4
                    preProcess = c("center", "scale"),
                    method = "rf",
                    metric="RMSE")


png('wharf.importnace.png')
varImpPlot(rf.wharf.fit2$finalModel)
dev.off()


library(mgcv)

(rf.wharf.fit2$finalModel)

wharf.predict<-predict(rf.wharf.fit2,newdata=NN.wharf.data,interval="prediction")

wharf.obs<- NN.wharf.data$ch.wharf

postResample(pred=wharf.predict,obs= wharf.obs)

wharf.plot.pred<-cbind(wharf.obs,wharf.predict)

summary(wharf.obs)

###SEs

wharf.rf<-randomForest(wharf.boruta.formula,data = na.omit(NN.wharf.data), ntree=1000,keep.forest=T, keep.inbag=T,keep.importance=T)

comp.wharf.var<-randomForestInfJack(wharf.rf,newdata=NN.wharf.data,calibrate = TRUE)
comp.wharf.se<-sqrt(comp.wharf.var)

wharf.ses<-comp.wharf.se$var.hat
wharf.plot.pred<-cbind(wharf.predict,wharf.obs,wharf.ses)

my.formula<-y~x

wharf.comp.plot<-ggplot(as.data.frame(wharf.plot.pred),aes(x=wharf.obs, y=wharf.predict))+geom_point()+
  geom_smooth(method="lm", se =FALSE,colour="black",formula=my.formula )+
  geom_errorbar(aes(ymax=wharf.predict+wharf.ses,ymin=wharf.predict-wharf.ses))+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)+
  scale_y_continuous(name="Predicted Wharf")+
  scale_x_continuous(name="Observed Wharf")+
  theme_classic()

ggsave("wharf.comparison.plot.png",dpi=500, height=5, width=5)

####

g<-grid.arrange(linear.comp.plot,BW.comp.plot,jetty.comp.plot,pontoon.comp.plot,wharf.comp.plot,nrow=3)

ggsave(file="comp.png",g,dpi=500, height=15, width=10)



ylabs(name="% MSE increase")+
###Predict art
mysheets2 <- read_excel_allsheets("input2.xlsx")

mysheets2<-lapply(mysheets2, dplyr::rename, port.revenue = port.rev)

art.preds<-lapply(mysheets2, function(x){predict.train(rf.art.fit2, newdata=x)})

df <- data.frame(matrix(unlist(art.preds), nrow=length(art.preds), byrow=T))

colnames(df)<-LO_2043$location

df <- tibble::rownames_to_column(df, "Time")
df$Time<-names(art.preds)


plot.pred.table<-df%>%
  gather(key = "Location", value = "measurement", -Time)

plot.pred.table$Time2<-factor(plot.pred.table$Time, levels=c("LO_2043", "MED_2043", "HI_2043" , "LO_2068" , "MED_2068" ,"HI_2068"))


ggplot(plot.pred.table, aes(x=Location, y=measurement,fill=Time2))+
  geom_bar(position="dodge", stat="identity")

##Generate SEs
art.rf<-randomForest(art.boruta.formula,data = na.omit(NN.art.data), ntree=1000,keep.forest=T, keep.inbag=T,keep.importance=T)
art.var<-lapply(mysheets2,function(x){randomForestInfJack(art.rf,newdata=x,calibrate = TRUE)})
art.se<- lapply(art.var,function(x){sqrt(x)})

art.se.col<-lapply(art.se,`[`,2)
art.se.col<-data.frame(matrix(unlist(art.se.col), nrow=length(art.se.col), byrow=T))
colnames(art.se.col)<-LO_2043$location
art.se.col<-tibble::rownames_to_column(art.se.col, "Time")
art.se.table<-art.se.col%>%
  gather(key = "Location", value = "se", -Time)
plot.pred.table$se<-art.se.table$se


plot.pred.table2<-plot.pred.table %>% separate(Time, c("ID","Year"))

plot.pred.table2$ID<-factor(plot.pred.table2$ID,levels=c("LO","MED","HI"))

NZ.art.obs<-test_data[1:14,1:2]
NZ.art.obs$ID<-"Current"
NZ.art.obs$Year<-2068


NZ.art<-rbind(NZ.art.obs,NZ.art.obs.blank)

NZ.art<-dplyr::rename(NZ.art, measurement = artificial.linear)

NZ.art$Time2<-"Control"
NZ.art$se<-0

plot.pred.table3<-rbind(plot.pred.table2,NZ.art)

plot.pred.table3$ID<-factor(plot.pred.table3$ID,levels=c("Current", "LO","MED","HI"))

levels(plot.pred.table3$ID)

plot.pred.table4<-filter(plot.pred.table3, Location !="all_NZ") 

write.csv(plot.pre)

library(forcats)
theme_set(theme_cowplot())

ggplot(plot.pred.table4, aes(x=fct_rev(Location), y=measurement,fill=fct_rev(ID)))+
  geom_bar(position="dodge", stat="identity")+
  facet_grid(Year~.)+
  labs(x="Location", y="Total Coastal Hardening")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.title=element_blank())+
  coord_flip()+
  geom_errorbar(aes(ymax=measurement+se,ymin=measurement-se),
                position = "dodge",alpha=0.5,size=.25)

ggsave("art.preds.facet flip.png",dpi=500,height=10,width=6)


##### Predict BW
BW.preds<-lapply(mysheets2, function(x){predict(rf.BW.fit2, newdata=x,interval = "confidence")})


BW.df <- data.frame(matrix(unlist(BW.preds), nrow=length(BW.preds), byrow=T))
colnames(BW.df)<-LO_2043$location

BW.df <- tibble::rownames_to_column(BW.df, "Time")

BW.df$Time<-names(BW.preds)

BW.pred.table<-BW.df%>%
  gather(key = "Location", value = "measurement", -Time)

BW.pred.table$Time2<-factor(BW.pred.table$Time, levels=c("LO_2043", "MED_2043", "HI_2043" , "LO_2068" , "MED_2068" ,"HI_2068"))


ggplot(BW.pred.table, aes(x=Location, y=measurement,fill=Time2))+
  geom_bar(position="dodge", stat="identity")

##Generate SEs
BW.rf<-randomForest(BW.boruta.formula,data = na.omit(NN.BW.data), ntree=1000,keep.forest=T, keep.inbag=T,keep.importance=T)
BW.var<-lapply(mysheets2,function(x){randomForestInfJack(BW.rf,newdata=x,calibrate = TRUE)})
BW.se<- lapply(BW.var,function(x){sqrt(x)})

BW.se.col<-lapply(BW.se,`[`,2)
BW.se.col<-data.frame(matrix(unlist(BW.se.col), nrow=length(BW.se.col), byrow=T))
colnames(BW.se.col)<-LO_2043$location
BW.se.col<-tibble::rownames_to_column(BW.se.col, "Time")
BW.se.table<-BW.se.col%>%
  gather(key = "Location", value = "se", -Time)
BW.pred.table$se<-BW.se.table$se

BW.pred.table2<-BW.pred.table %>% separate(Time, c("ID","Year"))

BW.pred.table2$ID<-factor(BW.pred.table2$ID,levels=c("LO","MED","HI"))

NZ.BW.obs<-test_data[1:14,c(1,3)]
NZ.BW.obs$ID<-"Current"
NZ.BW.obs.blank<-NZ.BW.obs

NZ.BW.obs$Year<-2068
NZ.BW.obs$Time2<-"Control"
NZ.BW.obs.blank$Year<-2043
NZ.BW.obs.blank$Time2<-"Control"


NZ.BW<-rbind(NZ.BW.obs,NZ.BW.obs.blank)

NZ.BW<-dplyr::rename(NZ.BW, measurement = CH.BW)
NZ.BW$se<-0

BW.pred.table3<-rbind(BW.pred.table2,NZ.BW)

BW.pred.table3$ID<-factor(BW.pred.table3$ID,levels=c("Current", "LO","MED","HI"))

BW.pred.table4<-filter(BW.pred.table3, Location !="all_NZ") 

theme_set(theme_cowplot())

library(forcats)
theme_set(theme_cowplot())

ggplot(BW.pred.table4, aes(x=fct_rev(Location), y=measurement,fill=fct_rev(ID)))+
  geom_bar(position="dodge", stat="identity")+
  facet_grid(Year~.)+
  labs(x="Location", y="Total Breakwater")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.title=element_blank())+
  coord_flip()+
  geom_errorbar(aes(ymax=measurement+se,ymin=measurement-se),
                position = "dodge",alpha=0.5,size=.25)

ggsave("BW.preds.facet flip.png",dpi=500,height=10,width=6)

##### Predict jetty
jetty.preds<-lapply(mysheets2, function(x){predict(rf.jetty.fit2, newdata=x,interval = "confidence")})


jetty.df <- data.frame(matrix(unlist(jetty.preds), nrow=length(jetty.preds), byrow=T))
colnames(jetty.df)<-LO_2043$location

jetty.df <- tibble::rownames_to_column(jetty.df, "Time")

jetty.df$Time<-names(jetty.preds)

jetty.pred.table<-jetty.df%>%
  gather(key = "Location", value = "measurement", -Time)

jetty.pred.table$Time2<-factor(jetty.pred.table$Time, levels=c("LO_2043", "MED_2043", "HI_2043" , "LO_2068" , "MED_2068" ,"HI_2068"))


ggplot(jetty.pred.table, aes(x=Location, y=measurement,fill=Time2))+
  geom_bar(position="dodge", stat="identity")

###Generate SEs
jetty.rf<-randomForest(jetty.boruta.formula,data = na.omit(NN.jetty.data), ntree=1000,keep.forest=T, keep.inbag=T,keep.importance=T)
jetty.var<-lapply(mysheets2,function(x){randomForestInfJack(jetty.rf,newdata=x,calibrate = TRUE)})
jetty.se<- lapply(jetty.var,function(x){sqrt(x)})

jetty.se.col<-lapply(jetty.se,`[`,2)
jetty.se.col<-data.frame(matrix(unlist(jetty.se.col), nrow=length(jetty.se.col), byrow=T))
colnames(jetty.se.col)<-LO_2043$location
jetty.se.col<-tibble::rownames_to_column(jetty.se.col, "Time")
jetty.se.table<-jetty.se.col%>%
  gather(key = "Location", value = "se", -Time)
jetty.pred.table$se<-jetty.se.table$se

jetty.pred.table2<-jetty.pred.table %>% separate(Time, c("ID","Year"))

jetty.pred.table2$ID<-factor(jetty.pred.table2$ID,levels=c("LO","MED","HI"))

NZ.jetty.obs<-test_data[1:14,c(1,4)]
NZ.jetty.obs$ID<-"Current"
NZ.jetty.obs.blank<-NZ.jetty.obs

NZ.jetty.obs$Year<-2068
NZ.jetty.obs$Time2<-"Control"
NZ.jetty.obs.blank$Year<-2043
NZ.jetty.obs.blank$Time2<-"Control"


NZ.jetty<-rbind(NZ.jetty.obs,NZ.jetty.obs.blank)

NZ.jetty<-dplyr::rename(NZ.jetty, measurement = CH.jetty)

NZ.jetty$se<-0

jetty.pred.table3<-rbind(jetty.pred.table2,NZ.jetty)

jetty.pred.table3$ID<-factor(jetty.pred.table3$ID,levels=c("Current", "LO","MED","HI"))

jetty.pred.table4<-filter(jetty.pred.table3, Location !="all_NZ") 



theme_set(theme_cowplot())

ggplot(jetty.pred.table4, aes(x=fct_rev(Location), y=measurement,fill=fct_rev(ID)))+
  geom_bar(position="dodge", stat="identity")+
  facet_grid(Year~.)+
  labs(x="Location", y="Total Jetty")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.title=element_blank())+
  coord_flip()+
  geom_errorbar(aes(ymax=measurement+se,ymin=measurement-se),
                              position = "dodge",alpha=0.5,size=.25)

ggsave("jetty.preds.facet flip.png",dpi=500,height=10,width=6)

##### Predict pontoon
pontoon.preds<-lapply(mysheets2, function(x){predict(rf.pontoon.fit2, newdata=x,interval = "confidence")})


pontoon.df <- data.frame(matrix(unlist(pontoon.preds), nrow=length(pontoon.preds), byrow=T))
colnames(pontoon.df)<-LO_2043$location

pontoon.df <- tibble::rownames_to_column(pontoon.df, "Time")

pontoon.df$Time<-names(pontoon.preds)

pontoon.pred.table<-pontoon.df%>%
  gather(key = "Location", value = "measurement", -Time)

pontoon.pred.table$Time2<-factor(pontoon.pred.table$Time, levels=c("LO_2043", "MED_2043", "HI_2043" , "LO_2068" , "MED_2068" ,"HI_2068"))


ggplot(pontoon.pred.table, aes(x=Location, y=measurement,fill=Time2))+
  geom_bar(position="dodge", stat="identity")

###Generate SEs
pontoon.rf<-randomForest(pontoon.boruta.formula,data = na.omit(NN.pontoon.data), ntree=1000,keep.forest=T, keep.inbag=T,keep.importance=T)
pontoon.var<-lapply(mysheets2,function(x){randomForestInfJack(pontoon.rf,newdata=x,calibrate = TRUE)})
pontoon.se<- lapply(pontoon.var,function(x){sqrt(x)})

pontoon.se.col<-lapply(pontoon.se,`[`,2)
pontoon.se.col<-data.frame(matrix(unlist(pontoon.se.col), nrow=length(pontoon.se.col), byrow=T))
colnames(pontoon.se.col)<-LO_2043$location
pontoon.se.col<-tibble::rownames_to_column(pontoon.se.col, "Time")
pontoon.se.table<-pontoon.se.col%>%
  gather(key = "Location", value = "se", -Time)
pontoon.pred.table$se<-pontoon.se.table$se

pontoon.pred.table2<-pontoon.pred.table %>% separate(Time, c("ID","Year"))

pontoon.pred.table2$ID<-factor(pontoon.pred.table2$ID,levels=c("LO","MED","HI"))

NZ.pontoon.obs<-test_data[1:14,c(1,5)]
NZ.pontoon.obs$ID<-"Current"
NZ.pontoon.obs.blank<-NZ.pontoon.obs

NZ.pontoon.obs$Year<-2068
NZ.pontoon.obs$Time2<-"Control"
NZ.pontoon.obs.blank$Year<-2043
NZ.pontoon.obs.blank$Time2<-"Control"


NZ.pontoon<-rbind(NZ.pontoon.obs,NZ.pontoon.obs.blank)

NZ.pontoon<-dplyr::rename(NZ.pontoon, measurement = ch.pontoon)
NZ.pontoon$se<-0

pontoon.pred.table3<-rbind(pontoon.pred.table2,NZ.pontoon)

pontoon.pred.table3$ID<-factor(pontoon.pred.table3$ID,levels=c("Current", "LO","MED","HI"))

pontoon.pred.table4<-filter(pontoon.pred.table3, Location !="all_NZ") 

theme_set(theme_cowplot())

library(forcats)
theme_set(theme_cowplot())

ggplot(pontoon.pred.table4, aes(x=fct_rev(Location), y=measurement,fill=fct_rev(ID)))+
  geom_bar(position="dodge", stat="identity")+
  facet_grid(Year~.)+
  labs(x="Location", y="Total pontoon")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.title=element_blank())+
  coord_flip()+
  geom_errorbar(aes(ymax=measurement+se,ymin=measurement-se),
                position = "dodge",alpha=0.5,size=.25)

ggsave("pontoon.preds.facet flip.png",dpi=500,height=10,width=6)


##### Predict wharf

wharf.preds<-lapply(mysheets2, function(x){predict(rf.wharf.fit2, newdata=x)})



wharf.df <- data.frame(matrix(unlist(wharf.preds), nrow=length(wharf.preds), byrow=T))
colnames(wharf.df)<-LO_2043$location
 

wharf.df <- tibble::rownames_to_column(wharf.df, "Time")


wharf.df$Time<-names(wharf.preds)
wharf.se.col$Time<-names(wharf.preds)

wharf.pred.table<-wharf.df%>%
  gather(key = "Location", value = "measurement", -Time)

##Generate SEs
wharf.rf<-randomForest(wharf.boruta.formula,data = na.omit(NN.wharf.data), ntree=1000,keep.forest=T, keep.inbag=T,keep.importance=T)
wharf.var<-lapply(mysheets2,function(x){randomForestInfJack(wharf.rf,newdata=x,calibrate = TRUE)})
wharf.se<- lapply(wharf.var,function(x){sqrt(x)})

wharf.se.col<-lapply(wharf.se,`[`,2)
wharf.se.col<-data.frame(matrix(unlist(wharf.se.col), nrow=length(wharf.se.col), byrow=T))
colnames(wharf.se.col)<-LO_2043$location
wharf.se.col<-tibble::rownames_to_column(wharf.se.col, "Time")
wharf.se.table<-wharf.se.col%>%
  gather(key = "Location", value = "se", -Time)
wharf.pred.table$se<-wharf.se.table$se


wharf.pred.table$Time2<-factor(wharf.pred.table$Time, levels=c("LO_2043", "MED_2043", "HI_2043" , "LO_2068" , "MED_2068" ,"HI_2068"))


ggplot(wharf.pred.table, aes(x=Location, y=measurement,fill=Time2))+
  geom_bar(position="dodge", stat="identity")


wharf.pred.table2<-wharf.pred.table %>% separate(Time, c("ID","Year"))

wharf.pred.table2$ID<-factor(wharf.pred.table2$ID,levels=c("LO","MED","HI"))

NZ.wharf.obs<-test_data[1:14,c(1,6)]
NZ.wharf.obs$ID<-"Current"
NZ.wharf.obs.blank<-NZ.wharf.obs

NZ.wharf.obs$Year<-2068
NZ.wharf.obs$Time2<-"Control"
NZ.wharf.obs.blank$Year<-2043
NZ.wharf.obs.blank$Time2<-"Control"


NZ.wharf<-rbind(NZ.wharf.obs,NZ.wharf.obs.blank)

NZ.wharf<-dplyr::rename(NZ.wharf, measurement = ch.wharf)

NZ.wharf$se<-0

wharf.pred.table3<-rbind(wharf.pred.table2,NZ.wharf)

wharf.pred.table3$ID<-factor(wharf.pred.table3$ID,levels=c("Current", "LO","MED","HI"))

wharf.pred.table4<-filter(wharf.pred.table3, Location !="all_NZ") 



library(forcats)
theme_set(theme_cowplot())

ggplot(wharf.pred.table4, aes(x=fct_rev(Location), y=measurement,fill=fct_rev(ID)))+
  geom_bar(position="dodge", stat="identity")+
  facet_grid(Year~.)+
  labs(x="Location", y="Total wharf")+
  guides(fill = guide_legend(reverse = TRUE))+
  theme(legend.title=element_blank())+
  coord_flip()+
  geom_errorbar(aes(ymax=measurement+se,ymin=measurement-se),
                position = "dodge",alpha=0.5,size=.25)
  

ggsave("wharf.preds.facet flip.bers.png",dpi=500,height=10,width=6)

####Make excel
install.packages("xlsx")
library("xlsx")

# Write the first data set in a new workbook
write.xlsx(plot.pred.table4, file="predictions.xlsx",
           sheetName="total art lienar", append=FALSE)
# Add a second data set in a new worksheet
write.xlsx(BW.pred.table4, file="predictions.xlsx", sheetName="BW", 
           append=TRUE)
# Add a third data set
write.xlsx(jetty.pred.table4, file="predictions.xlsx", sheetName="Jetty", 
           append=TRUE)
write.xlsx(wharf.pred.table4, file="predictions.xlsx", sheetName="wharf", 
           append=TRUE)
write.xlsx(pontoon.pred.table4, file="predictions.xlsx", sheetName="pontoon", 
           append=TRUE)
###Group
all.ch.preds<-cbind(plot.pred.table4[,c(1:4)],BW.pred.table4[,4],jetty.pred.table4[,4],pontoon.pred.table4[,4],wharf.pred.table4[,4])

names(all.ch.preds)

all.ch.preds<-dplyr::rename(all.ch.preds,"art"="measurement","BW"="BW.pred.table4[, 4]","jetty"="jetty.pred.table4[, 4]","pontoon"= "pontoon.pred.table4[, 4]","wharf"= "wharf.pred.table4[, 4]")

all.ch.preds%>%
  mutate(total=BW+jetty+pontoon+wharf)

gather.all.ch.preds<-all.ch.preds%>%
  gather(key = "Ch.type", value = "measurement",-ID,-Year,-Location)

preds.noart<-gather.all.ch.preds%>%
  filter(Ch.type!="art",Year==2043)

ggplot(preds.noart,aes(x=fct_rev(Location),y=measurement,fill=Ch.type))+
  geom_bar(position ="fill",stat = "identity")+
  labs(x="Location", y="Percentage")+
  facet_wrap(ID~.,1)+
  coord_flip()

ggsave("percentages.png",dpi=500,height=10,width=6)


###Variable improtance plots

wharf.importance<-importance(rf.wharf.fit2$finalModel)%>%
  data.frame() %>% 
  mutate(feature = row.names(.), Type="Wharf") 

wharf.importance$feature


jetty.importance<-importance(rf.jetty.fit2$finalModel)%>%
  data.frame() %>% 
  mutate(feature = row.names(.), Type="Jetty") 

pontoon.importance<-importance(rf.pontoon.fit2$finalModel)%>%
  data.frame() %>% 
  mutate(feature = row.names(.), Type="Pontoon") 

BW.importance<-importance(rf.BW.fit2$finalModel)%>%
  data.frame() %>% 
  mutate(feature = row.names(.),  Type="BW") 

art.importance<-importance(rf.art.fit2$finalModel)%>%
  data.frame() %>% 
  mutate(feature = row.names(.), Type="art") 

all.var.imps<-rbind(art.importance,BW.importance,jetty.importance,pontoon.importance,wharf.importance)


all.var.imps%>%
  ggplot(.,aes(x=feature,y=X.IncMSE,fill=Type))+
  geom_bar(position="dodge",stat="identity")+
  coord_flip()+
  scale_x_discrete(name = NULL, breaks=c("reg.GDP","pop.council","port.cargo","port.teu", "port.visits",  "port.revenue", "marina.no", "coast.length"),
                   labels=c("Regional GDP","Council Population", "Port Cargo", "Port TEU", "Ship Visits", "Port Revenue","Number of Marinas", "Coast Length"))+
  scale_fill_discrete(name =NULL, breaks=c('art','BW','Jetty','Pontoon','Wharf'), labels=c('Total linear','Breakwater','Jetty','Pontoon','Wharf'))+
  scale_y_continuous(name = "% MSE increase")+
  theme_classic()

ggsave("all vars import.png",dpi=500,height=5,width=8)
