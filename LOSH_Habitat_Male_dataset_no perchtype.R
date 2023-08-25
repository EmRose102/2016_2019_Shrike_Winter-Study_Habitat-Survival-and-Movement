#laptop directory
setwd("C:/Users/emily/Desktop/Data Analysis with R/LOSH Scripts")
LOSH16<-read.csv("C:/Users/emily/Desktop/Data Analysis with R/LOSH Scripts/LOSH habitat by group/LOSHtotal_Males_for R.csv", sep=",", header=T)
head(LOSH16)

LOSH16$Wire250

####Named LOSH16 but really includes data from 2106-17 and 2017-18 seasons!!!!!

#desktop directory
setwd("C:/Users/Emily.Donahue/Desktop/Data Analysis with R/LOSH Scripts")
LOSH16<-read.csv("C:/Users/Emily.Donahue/Desktop/Data Analysis with R/LOSH Scripts/LOSH16HabitatforR.csv", sep=",", header=T)
head(LOSH16)

library(lme4)
library(AICcmodavg)

#Check Correlation


LOSHcorsubset<-read.csv("C:/Users/emily/Desktop/Data Analysis with R/LOSH Scripts/LOSH habitat total/LOSHtotal_for R_corsubset.csv", sep=",", header=T)
LOSHcoversubset<-read.csv("C:/Users/emily/Desktop/Data Analysis with R/LOSH Scripts/LOSH habitat total/LOSHtotal_for R_landcoversubset.csv", sep=",", header=T)


source("MyLibrary.R")
pairs(LOSHcorsubset,lower.panel=panel.cor, upper.panel=panel.smooth2)
pairs(LOSHcoversubset,lower.panel=panel.cor, upper.panel=panel.smooth2)


#Rescale Variables

#Wire Length
rescale<-function(x) (x-min(x)) / (max(x) - min(x)) * 100
Wire250scale<-rescale(LOSH16$Wire250)
Wire100scale<-rescale(LOSH16$Wire100)


####________________________ 500 m scale ____________________________####

##FULL MODEL
## glmer(Use ~ Corn500 + Cotton500 + Rice500 + Soy500 +
## Fallow500 + Develop500 +Forest500 + (1|PointID) + (1|BirdID) + (1|Year), 
## family = binomial,nAGQ =0,  data = LOSH16, nAGQ=0)

#__________Random Effects_____________

random500.2<- glmer(Use ~ Corn500 + Cotton500 + Rice500 + Soy500 +
                      Fallow500 + Develop500 +Forest500 + (1|PointID) + (1|BirdID), 
                    family = binomial, nAGQ =0,  data = LOSH16)

random500.1a<- glmer(Use ~ Corn500 + Cotton500 + Rice500 + Soy500 +
                       Fallow500 + Develop500 +Forest500 + (1|PointID), 
                     family = binomial, nAGQ =0, data = LOSH16)

random500.1b<- glmer(Use ~ Corn500 + Cotton500 + Rice500 + Soy500 +
                       Fallow500 + Develop500 +Forest500 + (1|Year), 
                     family = binomial, nAGQ =0, data = LOSH16)


anova(random500.1a,random500.2) #BirdID does improve model
anova(random500.1b,random500.2) #PointID does not improve model
summary(random500.2)
summary(random500.1b)
summary(random500.1a)

#__________Fixed Effects_____________

#Round1

null<- glmer(Use ~ 1 + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
nullYR<- glmer(Use ~ Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
Corn500<- glmer(Use ~ Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
Cotton500<- glmer(Use ~ Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
Rice500<- glmer(Use ~ Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
Soy500<- glmer(Use ~ Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
Fallow500<- glmer(Use ~ Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
Develop500<- glmer(Use ~ Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
Forest500<- glmer(Use ~ Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

AIC500set1<-AIC(null,nullYR,Corn500,Cotton500,Rice500,Soy500,Fallow500,Develop500,Forest500)


deltaAIC500.1<-AIC500set1-(AIC(Develop500))
table500AIC1<-cbind(AIC500set1,deltaAIC500.1)
table500AIC1

summary(Develop500)

confint(Develop500,level=0.85, method="Wald")

#Round 1: Develop is best predictor

#Round2

null<- glmer(Use ~ 1 + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
nullYR<- glmer(Use ~ Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
Develop500<- glmer(Use ~ Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
D.Corn500<- glmer(Use ~ Corn500 + Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
D.Cotton500<- glmer(Use ~ Cotton500 + Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
D.Rice500<- glmer(Use ~ Rice500 + Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
D.Soy500<- glmer(Use ~ Soy500 + Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
D.Fallow500<- glmer(Use ~ Fallow500 + Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
D.Forest500<- glmer(Use ~ Develop500 + Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

AIC500set2<-AIC(null,nullYR,Develop500,D.Corn500,D.Cotton500,D.Rice500,D.Soy500,D.Fallow500,D.Forest500)

deltaAIC500.2<-AIC500set2-(AIC(Develop500))
table500AIC2<-cbind(AIC500set2,deltaAIC500.2)
table500AIC2


summary(Develop500)

confint(Develop500,level=0.85, method="Wald")
confint(D.Corn500,level=0.85, method="Wald")
confint(D.Cotton500,level=0.85, method="Wald")
confint(D.Rice500,level=0.85, method="Wald")
confint(D.Soy500,level=0.85, method="Wald")
confint(D.Fallow500,level=0.85, method="Wald")
confint(D.Forest500,level=0.85, method="Wald")


#Develop and fallow the best


#### end ####

####____________________ 250 m Scale _________________________####


##Hab.FULL<- glmer(Use ~ Wire250scale + WireAvg250 + WoodyPerches250 + WoodyAvg250
## + NonnatPerches250 + NonnatAvg250 + Ditches250 + DitchWavg250 + GrassWavg250 + GrassHavg250
## + (1|PointIDtied) + (1|BirdID), family = binomial, data = LOSHhab)


#_________Random Effects__________

random250.2<- glmer(Use ~ Wire250scale + WireAvg250 + WoodyPerches250 + WoodyAvg250 + 
                  NonnatPerches250 + NonnatAvg250 + Ditches250 + DitchWavg250 + 
                  GrassWavg250 + GrassHavg250 + (1|PointID) + (1|BirdID), 
                  family = binomial, nAGQ=0, data = LOSH16)
random250.1<- glmer(Use ~ Wire250scale + WireAvg250 + WoodyPerches250 + WoodyAvg250 + 
                  NonnatPerches250 + NonnatAvg250 + Ditches250 + DitchWavg250 + 
                  GrassWavg250 + GrassHavg250 + (1|PointID), 
                   family = binomial, nAGQ =0, data = LOSH16)

anova(random250.1,random250.2)
summary(random250.1)



#__________250 Fixed Effects_____________

#250Round1

null<- glmer(Use ~ 1 + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wire250<- glmer(Use ~ Wire250scale + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
woody250<- glmer(Use ~ WoodyPerches250 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
manmade250<- glmer(Use ~ NonnatPerches250 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250<- glmer(Use ~ Ditches250 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditch.w250<- glmer(Use ~ DitchWavg250 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.w250<- glmer(Use ~ GrassWavg250 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h250<- glmer(Use ~ GrassHavg250 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

AIC250set1<-AIC(null,wire250,woody250,manmade250,ditches250,ditch.w250,grass.w250,grass.h250)

deltaAIC250.1<-AIC250set1-(AIC(wire250))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1

#wire250 best predictor

#Round2

null<- glmer(Use ~ 1 + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
nullYR<- glmer(Use ~ Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
Develop500<- glmer(Use ~ Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)



wi250.D.C<- glmer(Use ~ Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Cot<- glmer(Use ~ Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.R<- glmer(Use ~ Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.S<- glmer(Use ~ Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Fa<- glmer(Use ~ Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Fo<- glmer(Use ~ Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D<- glmer(Use ~ Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

wi250.D.C<- glmer(Use ~ Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
woody250.D.Cot<- glmer(Use ~ WoodyPerches250 +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
nonnat250.D.R<- glmer(Use ~ NonnatPerches250 +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditch.w250.D.S<- glmer(Use ~ DitchWavg250 +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.w250.D.Fa<- glmer(Use ~ GrassWavg250 +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h250.D.Fo<- glmer(Use ~ GrassHavg250 +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D<- glmer(Use ~ Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


AIC250set1<-AIC(wi250.D.C, wi250.D.Cot, wi250.D.R, wi250.D.S, wi250.D.Fa, wi250.D.Fo, wi250.D,
                woody250.D.Cot, nonnat250.D.R, ditch.w250.D.S, grass.w250.D.Fa, grass.h250.D.Fo)



deltaAIC250.1<-AIC250set1-(AIC(wi250.D))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1


####Round 3

wi250.D.C<- glmer(Use ~ Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Cot<- glmer(Use ~ Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.R<- glmer(Use ~ Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.S<- glmer(Use ~ Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Fa<- glmer(Use ~ Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Fo<- glmer(Use ~ Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D<- glmer(Use ~ Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

wi250.C<- glmer(Use ~ Wire250scale +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.Cot<- glmer(Use ~ Wire250scale +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.R<- glmer(Use ~ Wire250scale +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.S<- glmer(Use ~ Wire250scale +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.Fa<- glmer(Use ~ Wire250scale +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.Fo<- glmer(Use ~ Wire250scale +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D<- glmer(Use ~ Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

ditches250.D.C<- glmer(Use ~ Ditches250 +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250.D.Cot<- glmer(Use ~ Ditches250 +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250.D.R<- glmer(Use ~ Ditches250 +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250.D.S<- glmer(Use ~ Ditches250 +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250.D.Fa<- glmer(Use ~ Ditches250 +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250.D.Fo<- glmer(Use ~ Ditches250 +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250.D<- glmer(Use ~ Ditches250 +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


ditches250.C<- glmer(Use ~ Ditches250 +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250.Cot<- glmer(Use ~ Ditches250 +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250.R<- glmer(Use ~ Ditches250 +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250.S<- glmer(Use ~ Ditches250 +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250.Fa<- glmer(Use ~ Ditches250 +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches250.Fo<- glmer(Use ~ Ditches250 +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)




#___________________________

AIC250set1<-AIC(wi250.D.C, wi250.D.Cot, wi250.D.R, wi250.D.S, wi250.D.Fa, wi250.D.Fo,
wi250.C, wi250.Cot, wi250.R, wi250.S, wi250.Fa, wi250.Fo, wi250.D,
ditches250.D.C, ditches250.D.Cot, ditches250.D.R, ditches250.D.S, ditches250.D.Fa, ditches250.D.Fo, ditches250.D,
ditches250.C, ditches250.Cot, ditches250.R, ditches250.S, ditches250.Fa, ditches250.Fo)


deltaAIC250.1<-AIC250set1-(AIC(wi250.D))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1




confint(wire250.develop500,level=0.85, method="Wald")
confint(wi.di.wi.h250.d500,level=0.85, method="Wald")


####Round 4

wi250.D.C<- glmer(Use ~ Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Cot<- glmer(Use ~ Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.R<- glmer(Use ~ Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.S<- glmer(Use ~ Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Fa<- glmer(Use ~ Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Fo<- glmer(Use ~ Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


wi250.C<- glmer(Use ~ Wire250scale +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.Cot<- glmer(Use ~ Wire250scale +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.R<- glmer(Use ~ Wire250scale +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.S<- glmer(Use ~ Wire250scale +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.Fa<- glmer(Use ~ Wire250scale +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.Fo<- glmer(Use ~ Wire250scale +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D<- glmer(Use ~ Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

di.wi250.D.C<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.D.Cot<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.D.R<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.D.S<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.D.Fa<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.D.Fo<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

di.wi250.C<- glmer(Use ~ Ditches250 +Wire250scale +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.Cot<- glmer(Use ~ Ditches250 +Wire250scale +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.R<- glmer(Use ~ Ditches250 +Wire250scale +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.S<- glmer(Use ~ Ditches250 +Wire250scale +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.Fa<- glmer(Use ~ Ditches250 +Wire250scale +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.Fo<- glmer(Use ~ Ditches250 +Wire250scale +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.D<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

woody250.wi250.D.<- glmer(Use ~ WoodyPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
nonnat250.wi250.D.<- glmer(Use ~ NonnatPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di250.wi250.D.<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditch.w250.wi250.D.<- glmer(Use ~ DitchWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.w250.wi250.D.<- glmer(Use ~ GrassWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h250.wi250.D.<- glmer(Use ~ GrassHavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)



AIC250set1<-AIC(wi250.D.C, wi250.D.Cot, wi250.D.R, wi250.D.S, wi250.D.Fa, wi250.D.Fo,
wi250.C, wi250.Cot, wi250.R, wi250.S, wi250.Fa, wi250.Fo, wi250.D,
di.wi250.D.C, di.wi250.D.Cot, di.wi250.D.R, di.wi250.D.S, di.wi250.D.Fa, di.wi250.D.Fo,
di.wi250.C, di.wi250.Cot, di.wi250.R, di.wi250.S, di.wi250.Fa, di.wi250.Fo, di.wi250.D,
ditch.w.wi250.D.C, ditch.w.wi250.D.Cot, ditch.w.wi250.D.R, ditch.w.wi250.D.S, ditch.w.wi250.D.Fa, ditch.w.wi250.D.Fo, 
woody250.wi250.D., nonnat250.wi250.D., di250.wi250.D., ditch.w250.wi250.D., grass.w250.wi250.D., grass.h250.wi250.D.)


deltaAIC250.1<-AIC250set1-(AIC(wi250.D))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1

#Round 5

wi250.D<- glmer(Use ~ Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

wi250.D.C<- glmer(Use ~ Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Cot<- glmer(Use ~ Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.R<- glmer(Use ~ Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.S<- glmer(Use ~ Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Fa<- glmer(Use ~ Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wi250.D.Fo<- glmer(Use ~ Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


di.wi250.D.C<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.D.Cot<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.D.R<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.D.S<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.D.Fa<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di.wi250.D.Fo<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


woody250.wi250.D.<- glmer(Use ~ WoodyPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
nonnat250.wi250.D.<- glmer(Use ~ NonnatPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di250.wi250.D.<- glmer(Use ~ Ditches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditch.w250.wi250.D.<- glmer(Use ~ DitchWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.w250.wi250.D.<- glmer(Use ~ GrassWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h250.wi250.D.<- glmer(Use ~ GrassHavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

grass.h250.wi250.D.C<- glmer(Use ~ GrassHavg250 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h250.wi250.D.Cot<- glmer(Use ~ GrassHavg250 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h250.wi250.D.R<- glmer(Use ~ GrassHavg250 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h250.wi250.D.S<- glmer(Use ~ GrassHavg250 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h250.wi250.D.Fa<- glmer(Use ~ GrassHavg250 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h250.wi250.D.Fo<- glmer(Use ~ GrassHavg250 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


AIC250set1<-AIC(wi250.D,
  wi250.D.C, wi250.D.Cot, wi250.D.R, wi250.D.S, wi250.D.Fa, wi250.D.Fo,
di.wi250.D.C, di.wi250.D.Cot, di.wi250.D.R, di.wi250.D.S, di.wi250.D.Fa, di.wi250.D.Fo,
woody250.wi250.D., nonnat250.wi250.D., di250.wi250.D., ditch.w250.wi250.D., grass.w250.wi250.D., grass.h250.wi250.D.,
grass.h250.wi250.D.C, grass.h250.wi250.D.Cot, grass.h250.wi250.D.R, grass.h250.wi250.D.S, grass.h250.wi250.D.Fa, grass.h250.wi250.D.Fo)


deltaAIC250.1<-AIC250set1-(AIC(wi250.D))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1

#Looking at your model's CI of beta coefficients, do they cross zero, if so it's not an informative model. - intercept means the variable was avoided, + means variable was selected for
#confint("model")
#confint("model",level=0.85) 

#wire250.develop500 moves on

####____________end_______________####

####____________________ 100 m Scale _________________________####


##Hab.FULL<- glmer(Use ~ Wire100scale + WireAvg100 + WoodyPerches100 + WoodyAvg100
## + NonnatPerches100 + NonnatAvg100 + Ditches100 + DitchWavg100 + GrassWavg100 + GrassHavg100
## + (1|PointIDtied) + (1|BirdID), family = binomial, data = LOSHhab)


#_________Random Effects__________

random100.2<- glmer(Use ~ Wire100scale + WireAvg100 + WoodyPerches100 + WoodyAvg100 + 
                      NonnatPerches100 + NonnatAvg100 + Ditches100 + DitchWavg100 + 
                      GrassWavg100 + GrassHavg100 + (1|PointID) + (1|BirdID), 
                    family = binomial, nAGQ=0, data = LOSH16)
random100.1<- glmer(Use ~ Wire100scale + WireAvg100 + WoodyPerches100 + WoodyAvg100 + 
                      NonnatPerches100 + NonnatAvg100 + Ditches100 + DitchWavg100 + 
                      GrassWavg100 + GrassHavg100 + (1|PointID), 
                    family = binomial, nAGQ =0, data = LOSH16)

anova(random100.1,random100.2)
summary(random100.1)



#__________100 Fixed Effects_____________


#100Round1

woody100<- glmer(Use ~ WoodyPerches100 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
manmade100<- glmer(Use ~ NonnatPerches100 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditches100<- glmer(Use ~ Ditches100 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditch.w100<- glmer(Use ~ DitchWavg100 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.w100<- glmer(Use ~ GrassWavg100 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100<- glmer(Use ~ GrassHavg100 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

AIC250set1<-AIC(null,woody100,manmade100,ditches100,ditch.w100,grass.w100,grass.h100)

deltaAIC250.1<-AIC250set1-(AIC(ditches100))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1

#wire100 best predictor

#Round2

wi250.D<- glmer(Use ~ Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
woody100.di100.wi250.D<- glmer(Use ~ WoodyPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
manmade100.di100.wi250.D<- glmer(Use ~ NonnatPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ditch.w100.di100.wi250.D<- glmer(Use ~ DitchWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.w100.di100.wi250.D<- glmer(Use ~ GrassWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100.di100.wi250.D<- glmer(Use ~ GrassHavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


di100.wi250.D.C<- glmer(Use ~ Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di100.wi250.D.Cot<- glmer(Use ~ Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di100.wi250.D.R<- glmer(Use ~ Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di100.wi250.D.S<- glmer(Use ~ Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di100.wi250.D.Fa<- glmer(Use ~ Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di100.wi250.D.Fo<- glmer(Use ~ Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

grass.h100.di100.wi250.D.C<- glmer(Use ~ GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100.di100.wi250.D.Cot<- glmer(Use ~ GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100.di100.wi250.D.R<- glmer(Use ~ GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100.di100.wi250.D.S<- glmer(Use ~ GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100.di100.wi250.D.Fa<- glmer(Use ~ GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100.di100.wi250.D.Fo<- glmer(Use ~ GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


di100.woody250.wi250.D<- glmer(Use ~ Ditches100 +WoodyPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di100.nonnat250.wi250.D<- glmer(Use ~ Ditches100 +NonnatPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di100.ditch.w250.wi250.D<- glmer(Use ~ Ditches100 +DitchWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di100.grass.w250.wi250.D<- glmer(Use ~ Ditches100 +GrassWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
di100.grass.h250.wi250.D<- glmer(Use ~ Ditches100 +GrassHavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


grass.h100.wi250.D.C<- glmer(Use ~ GrassHavg100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100.wi250.D.Cot<- glmer(Use ~ GrassHavg100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100.wi250.D.R<- glmer(Use ~ GrassHavg100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100.wi250.D.S<- glmer(Use ~ GrassHavg100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100.wi250.D.Fa<- glmer(Use ~ GrassHavg100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
grass.h100.wi250.D.Fo<- glmer(Use ~ GrassHavg100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

ditch.w.di100.wi250.D.S<- glmer(Use ~ DitchWavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


AIC250set1<-AIC(wi250.D, woody100.di100.wi250.D, manmade100.di100.wi250.D, ditch.w100.di100.wi250.D, grass.w100.di100.wi250.D, grass.h100.di100.wi250.D,
                di100.wi250.D.C, di100.wi250.D.Cot, di100.wi250.D.R, di100.wi250.D.S, di100.wi250.D.Fa, di100.wi250.D.Fo,
grass.h100.di100.wi250.D.C, grass.h100.di100.wi250.D.Cot, grass.h100.di100.wi250.D.R, grass.h100.di100.wi250.D.S, grass.h100.di100.wi250.D.Fa, grass.h100.di100.wi250.D.Fo,
di100.woody250.wi250.D, di100.nonnat250.wi250.D, di100.ditch.w250.wi250.D, di100.grass.w250.wi250.D, di100.grass.h250.wi250.D,
grass.h100.wi250.D.C, grass.h100.wi250.D.Cot, grass.h100.wi250.D.R, grass.h100.wi250.D.S, grass.h100.wi250.D.Fa, grass.h100.wi250.D.Fo,
ditch.w.di100.wi250.D.S)




deltaAIC250.1<-AIC250set1-(AIC(grass.h100.di100.wi250.D.S))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1


ditch.w.di100.wi250.D.S<- glmer(Use ~ DitchWavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
AIC(grass.h100.di100.wi250.D.S)


confint(nonnat.di.wi250.grass.w250.D,level=0.85, method="Wald")
confint(wi.di.wi.h250.d500,level=0.85, method="Wald")




####____________end_______________####


####____________________ 11 m Scale _________________________####


##Hab.FULL<- glmer(Use ~ PerchType11 + PerchH11 + Water11 + WaterType11
## + DitchWavg11 + DitchDbelowAvg11 + WaterDavg11 + Crops11 + CropType11 
## + Tilled11 + Burned11
## + (1|PointID) + (1|BirdID), family = binomial, data = LOSH16)


#_________Random Effects__________

random11.2<- glmer(Use ~ PerchType11 + PerchH11 + Water11 +
                    DitchWavg11 + DitchDbelowAvg11 + WaterDavg11 + Crops11 + CropType11 + 
                    Tilled11 + Burned11 +
                    (1|PointID) + (1|BirdID), family = binomial,nAGQ = 0, data = LOSH16)
random11.1<- glmer(Use ~ PerchType11 + PerchH11 + Water11 +
                    DitchWavg11 + DitchDbelowAvg11 + WaterDavg11 + Crops11 + CropType11 + 
                    Tilled11 + Burned11 +
                    (1|PointID), family = binomial, nAGQ = 0, data = LOSH16)

anova(random11.1,random11.2)
summary(random11.1)



#__________11 Fixed Effects_____________

#11Round1

#1) Take Forest500 and Wire250 and Wire100 and Manmade100 to compare to 11 m parameters

wa11<- glmer(Use ~ Water11 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
crop11<- glmer(Use ~ Crops11 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11<- glmer(Use ~Water11 +Crops11 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


AIC250set1<-AIC(wa11,crop11,wa.crop11)

deltaAIC250.1<-AIC250set1-(AIC(wa11))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1

#####2) 11 m Round 2 Compare best single 100 variables in bivariate models

#_________________________________________________________________________

#wa and crop
wa.crop11.wi250.D<- glmer(Use ~ Water11 +Crops11 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.woody100.di100.wi250.D<- glmer(Use ~ Water11 +Crops11 +WoodyPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.manmade100.di100.wi250.D<- glmer(Use ~ Water11 +Crops11 +NonnatPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.ditch.w100.di100.wi250.D<- glmer(Use ~ Water11 +Crops11 +DitchWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.w100.di100.wi250.D<- glmer(Use ~ Water11 +Crops11 +GrassWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.h100.di100.wi250.D<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


wa.crop11.di100.wi250.D.C<- glmer(Use ~ Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.di100.wi250.D.Cot<- glmer(Use ~ Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.di100.wi250.D.R<- glmer(Use ~ Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.di100.wi250.D.S<- glmer(Use ~ Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.di100.wi250.D.Fa<- glmer(Use ~ Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.di100.wi250.D.Fo<- glmer(Use ~ Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

wa.crop11.grass.h100.di100.wi250.D.C<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.h100.di100.wi250.D.Cot<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.h100.di100.wi250.D.R<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.h100.di100.wi250.D.S<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.h100.di100.wi250.D.Fa<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.h100.di100.wi250.D.Fo<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


wa.crop11.di100.woody250.wi250.D<- glmer(Use ~ Water11 +Crops11 +Ditches100 +WoodyPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.di100.nonnat250.wi250.D<- glmer(Use ~ Water11 +Crops11 +Ditches100 +NonnatPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.di100.ditch.w250.wi250.D<- glmer(Use ~ Water11 +Crops11 +Ditches100 +DitchWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.di100.grass.w250.wi250.D<- glmer(Use ~ Water11 +Crops11 +Ditches100 +GrassWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.di100.grass.h250.wi250.D<- glmer(Use ~ Water11 +Crops11 +Ditches100 +GrassHavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


wa.crop11.grass.h100.wi250.D.C<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.h100.wi250.D.Cot<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.h100.wi250.D.R<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.h100.wi250.D.S<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.h100.wi250.D.Fa<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.crop11.grass.h100.wi250.D.Fo<- glmer(Use ~ Water11 +Crops11 +GrassHavg100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


wa.crop11.ditch.w100.di100.wi250.D.S<- glmer(Use ~ Water11 +Crops11 +DitchWavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

#wa only
wa.wi250.D<- glmer(Use ~ Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.woody100.di100.wi250.D<- glmer(Use ~ Water11 +WoodyPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.manmade100.di100.wi250.D<- glmer(Use ~ Water11 +NonnatPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.ditch.w100.di100.wi250.D<- glmer(Use ~ Water11 +DitchWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.w100.di100.wi250.D<- glmer(Use ~ Water11 +GrassWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.h100.di100.wi250.D<- glmer(Use ~ Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

wa.di100.wi250.D.C<- glmer(Use ~ Water11 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.di100.wi250.D.Cot<- glmer(Use ~ Water11 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.di100.wi250.D.R<- glmer(Use ~ Water11 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.di100.wi250.D.S<- glmer(Use ~ Water11 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.di100.wi250.D.Fa<- glmer(Use ~ Water11 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.di100.wi250.D.Fo<- glmer(Use ~ Water11 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

grass.h100.di100.wi250.D.C<- glmer(Use ~ Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.h100.di100.wi250.D.Cot<- glmer(Use ~ Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.h100.di100.wi250.D.R<- glmer(Use ~ Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.h100.di100.wi250.D.S<- glmer(Use ~ Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.h100.di100.wi250.D.Fa<- glmer(Use ~ Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.h100.di100.wi250.D.Fo<- glmer(Use ~ Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


wa.di100.woody250.wi250.D<- glmer(Use ~ Water11 +Ditches100 +WoodyPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.di100.nonnat250.wi250.D<- glmer(Use ~ Water11 +Ditches100 +NonnatPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.di100.ditch.w250.wi250.D<- glmer(Use ~ Water11 +Ditches100 +DitchWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.di100.grass.w250.wi250.D<- glmer(Use ~ Water11 +Ditches100 +GrassWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.di100.grass.h250.wi250.D<- glmer(Use ~ Water11 +Ditches100 +GrassHavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


wa.grass.h100.wi250.D.C<- glmer(Use ~ Water11 +GrassHavg100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.h100.wi250.D.Cot<- glmer(Use ~ Water11 +GrassHavg100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.h100.wi250.D.R<- glmer(Use ~ Water11 +GrassHavg100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.h100.wi250.D.S<- glmer(Use ~ Water11 +GrassHavg100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.h100.wi250.D.Fa<- glmer(Use ~ Water11 +GrassHavg100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
wa.grass.h100.wi250.D.Fo<- glmer(Use ~ Water11 +GrassHavg100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


wa.ditch.w100.di100.wi250.D.S<- glmer(Use ~ Water11 +DitchWavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

#____________________________________
AIC250set1<-AIC(
  
  wa.crop11.wi250.D, wa.crop11.woody100.di100.wi250.D, wa.crop11.manmade100.di100.wi250.D, wa.crop11.ditch.w100.di100.wi250.D, wa.crop11.grass.w100.di100.wi250.D, wa.crop11.grass.h100.di100.wi250.D,
  wa.crop11.di100.wi250.D.C, wa.crop11.di100.wi250.D.Cot, wa.crop11.di100.wi250.D.R, wa.crop11.di100.wi250.D.S, wa.crop11.di100.wi250.D.Fa, wa.crop11.di100.wi250.D.Fo,
wa.crop11.grass.h100.di100.wi250.D.C, wa.crop11.grass.h100.di100.wi250.D.Cot, wa.crop11.grass.h100.di100.wi250.D.R, wa.crop11.grass.h100.di100.wi250.D.S, wa.crop11.grass.h100.di100.wi250.D.Fa, wa.crop11.grass.h100.di100.wi250.D.Fo,
wa.crop11.di100.woody250.wi250.D, wa.crop11.di100.nonnat250.wi250.D, wa.crop11.di100.ditch.w250.wi250.D, wa.crop11.di100.grass.w250.wi250.D, wa.crop11.di100.grass.h250.wi250.D,
wa.crop11.grass.h100.wi250.D.C, wa.crop11.grass.h100.wi250.D.Cot, wa.crop11.grass.h100.wi250.D.R, wa.crop11.grass.h100.wi250.D.S, wa.crop11.grass.h100.wi250.D.Fa, wa.crop11.grass.h100.wi250.D.Fo,
wa.crop11.ditch.w100.di100.wi250.D.S,
wa.wi250.D, wa.woody100.di100.wi250.D, wa.manmade100.di100.wi250.D, wa.ditch.w100.di100.wi250.D, wa.grass.w100.di100.wi250.D, wa.grass.h100.di100.wi250.D,
wa.di100.wi250.D.C, wa.di100.wi250.D.Cot, wa.di100.wi250.D.R, wa.di100.wi250.D.S, wa.di100.wi250.D.Fa, wa.di100.wi250.D.Fo,
grass.h100.di100.wi250.D.C, wa.grass.h100.di100.wi250.D.Cot, wa.grass.h100.di100.wi250.D.R, wa.grass.h100.di100.wi250.D.S, wa.grass.h100.di100.wi250.D.Fa, wa.grass.h100.di100.wi250.D.Fo,
wa.di100.woody250.wi250.D, wa.di100.nonnat250.wi250.D, wa.di100.ditch.w250.wi250.D, wa.di100.grass.w250.wi250.D, wa.di100.grass.h250.wi250.D,
wa.grass.h100.wi250.D.C, wa.grass.h100.wi250.D.Cot, wa.grass.h100.wi250.D.R, wa.grass.h100.wi250.D.S, wa.grass.h100.wi250.D.Fa, wa.grass.h100.wi250.D.Fo,
wa.ditch.w100.di100.wi250.D.S)




deltaAIC250.1<-AIC250set1-(AIC(wa.di100.wi250.D.S))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1


wa.grass.h.di.wi250.F.D<- glmer(Use ~ Water11 +GrassHavg100 +Ditches100 +Wire250scale +Fallow500 +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
AIC(wa.grass.h.di.wi250.D)


#water presence is best model


####____________end_______________####


####____________________ 5 m Scale _________________________####

##Hab.FULL<- glmer(Use ~ PerchType5 + PerchH5 + Water5 + WaterType5
## + DitchWavg5 + DitchDbelowAvg5 + WaterDavg5 + Crops5 + CropType5 
## + Tilled5 + Burned5
## + (1|PointID) + (1|BirdID), family = binomial, data = LOSH16)


#_________Random Effects__________

random5.2<- glmer(Use ~ PerchType5 + PerchH5 + Water5 +
                     DitchWavg5 + DitchDbelowAvg5 + WaterDavg5 + Crops5 + CropType5 + 
                     Tilled5 + Burned5 +
                     (1|PointID) + (1|BirdID), family = binomial,nAGQ = 1, data = LOSH16)
random5.1<- glmer(Use ~ PerchType5 + PerchH5 + Water5 +
                     DitchWavg5 + DitchDbelowAvg5 + WaterDavg5 + Crops5 + CropType5 + 
                     Tilled5 + Burned5 +
                     (1|PointID), family = binomial, nAGQ = 1, data = LOSH16)

anova(random5.1,random5.2)
summary(random5.1)




#__________5 Fixed Effects_____________

#5Round1

#1) Take Forest500 and Wire250 and Wire100 and Manmade100 and Perch Type 11 to compare to 5 m parameters

perch.t5<- glmer(Use ~ PerchType5 + (1|PointID), family = binomial, nAGQ =1, data = LOSH16)
perch.h5<- glmer(Use ~ PerchH5 + (1|PointID), family = binomial, nAGQ =1, data = LOSH16)
water5<- glmer(Use ~ Water5 + (1|PointID), family = binomial, nAGQ =1, data = LOSH16)
crops5<- glmer(Use ~ Crops5 + (1|PointID), family = binomial, nAGQ =1, data = LOSH16)
crop.t5<- glmer(Use ~ CropType5 + (1|PointID), family = binomial, nAGQ =1, data = LOSH16)


AIC5set1<-AIC(perch.t5, perch.h5, water5, 
           crops5, crop.t5)

deltaAIC5.1<-AIC5set1-(AIC(perch.t5))
table5AIC1<-cbind(AIC5set1,deltaAIC5.1)
table5AIC1

confint(perch.t5,level=0.85, method="Wald")
confint(water11,level=0.85)

summary(perch.t11)




#2) Compare best single 100 variables in bivariate models

#ph and pt
#wa and crop



pt.ph.wa.crop11.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.woody100.di100.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +WoodyPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.manmade100.di100.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +NonnatPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.ditch.w100.di100.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +DitchWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.grass.w100.di100.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +GrassWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.grass.h100.di100.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


pt.ph.wa.crop11.di100.wi250.D.C<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.di100.wi250.D.Cot<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.di100.wi250.D.R<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.di100.wi250.D.S<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.di100.wi250.D.Fa<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.di100.wi250.D.Fo<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

pt.ph.wa.crop11.grass.h100.di100.wi250.D.C<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.grass.h100.di100.wi250.D.Cot<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.grass.h100.di100.wi250.D.R<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.grass.h100.di100.wi250.D.S<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.grass.h100.di100.wi250.D.Fa<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.grass.h100.di100.wi250.D.Fo<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


pt.ph.wa.crop11.di100.woody250.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Ditches100 +WoodyPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.di100.nonnat250.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Ditches100 +NonnatPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.di100.ditch.w250.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Ditches100 +DitchWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.di100.grass.w250.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Ditches100 +GrassWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.crop11.di100.grass.h250.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +Ditches100 +GrassHavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


pt.ph.wa.crop11.ditch.w100.di100.wi250.D.S<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Crops11 +DitchWavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

#wa only
pt.ph.wa.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.woody100.di100.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +WoodyPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.manmade100.di100.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +NonnatPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.ditch.w100.di100.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +DitchWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.grass.w100.di100.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +GrassWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.grass.h100.di100.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

pt.ph.wa.di100.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.di100.wi250.D.C<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.di100.wi250.D.Cot<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.di100.wi250.D.R<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.di100.wi250.D.S<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.di100.wi250.D.Fa<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.di100.wi250.D.Fo<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

pt.ph.wa.grass.h100.di100.wi250.D.<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.grass.h100.di100.wi250.D.C<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.grass.h100.di100.wi250.D.Cot<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.grass.h100.di100.wi250.D.R<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.grass.h100.di100.wi250.D.S<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.grass.h100.di100.wi250.D.Fa<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.grass.h100.di100.wi250.D.Fo<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


pt.ph.wa.di100.woody250.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +WoodyPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.di100.nonnat250.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +NonnatPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.di100.ditch.w250.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +DitchWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.di100.grass.w250.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +GrassWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.ph.wa.di100.grass.h250.wi250.D<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +Ditches100 +GrassHavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


pt.ph.wa.ditch.w100.di100.wi250.D.S<- glmer(Use ~ PerchH5 +PerchType5 +Water11 +DitchWavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)





#pt only
#wa and crop



pt.wa.crop11.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.woody100.di100.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Crops11 +WoodyPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.manmade100.di100.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Crops11 +NonnatPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.ditch.w100.di100.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Crops11 +DitchWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.grass.w100.di100.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Crops11 +GrassWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.grass.h100.di100.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


pt.wa.crop11.di100.wi250.D.<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.di100.wi250.D.C<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.di100.wi250.D.Cot<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.di100.wi250.D.R<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.di100.wi250.D.S<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.di100.wi250.D.Fa<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.di100.wi250.D.Fo<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

pt.wa.crop11.grass.h100.di100.wi250.D.C<- glmer(Use ~ PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.grass.h100.di100.wi250.D.Cot<- glmer(Use ~ PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.grass.h100.di100.wi250.D.R<- glmer(Use ~ PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.grass.h100.di100.wi250.D.S<- glmer(Use ~ PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.grass.h100.di100.wi250.D.Fa<- glmer(Use ~ PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.grass.h100.di100.wi250.D.Fo<- glmer(Use ~ PerchType5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


pt.wa.crop11.di100.woody250.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +WoodyPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.di100.nonnat250.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +NonnatPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.di100.ditch.w250.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +DitchWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.di100.grass.w250.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +GrassWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.crop11.di100.grass.h250.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Crops11 +Ditches100 +GrassHavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


pt.wa.crop11.ditch.w100.di100.wi250.D.S<- glmer(Use ~ PerchType5 +Water11 +Crops11 +DitchWavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

#wa only
pt.wa.wi250.D<- glmer(Use ~ PerchType5 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.woody100.di100.wi250.D<- glmer(Use ~ PerchType5 +Water11 +WoodyPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.manmade100.di100.wi250.D<- glmer(Use ~ PerchType5 +Water11 +NonnatPerches100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.ditch.w100.di100.wi250.D<- glmer(Use ~ PerchType5 +Water11 +DitchWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.grass.w100.di100.wi250.D<- glmer(Use ~ PerchType5 +Water11 +GrassWavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.grass.h100.di100.wi250.D<- glmer(Use ~ PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

pt.wa.di100.wi250.D.C<- glmer(Use ~ PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.di100.wi250.D.Cot<- glmer(Use ~ PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.di100.wi250.D.R<- glmer(Use ~ PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.di100.wi250.D.S<- glmer(Use ~ PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.di100.wi250.D.Fa<- glmer(Use ~ PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.di100.wi250.D.Fo<- glmer(Use ~ PerchType5 +Water11 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

pt.wa.grass.h100.di100.wi250.D.C<- glmer(Use ~ PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Corn500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.grass.h100.di100.wi250.D.Cot<- glmer(Use ~ PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.grass.h100.di100.wi250.D.R<- glmer(Use ~ PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Rice500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.grass.h100.di100.wi250.D.S<- glmer(Use ~ PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.grass.h100.di100.wi250.D.Fa<- glmer(Use ~ PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.grass.h100.di100.wi250.D.Fo<- glmer(Use ~ PerchType5 +Water11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


pt.wa.di100.woody250.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Ditches100 +WoodyPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.di100.nonnat250.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Ditches100 +NonnatPerches250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.di100.ditch.w250.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Ditches100 +DitchWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
pt.wa.di100.grass.w250.wi250.D<- glmer(Use ~ PerchType5 +Water11 +Ditches100 +GrassWavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)



wa.wi250.D<- glmer(Use ~ Water11 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
AIC(wa.wi250.D)
#________________________________

AIC250set1<-AIC(
  pt.ph.wa.crop11.wi250.D, pt.ph.wa.crop11.woody100.di100.wi250.D, pt.ph.wa.crop11.manmade100.di100.wi250.D, pt.ph.wa.crop11.ditch.w100.di100.wi250.D, pt.ph.wa.crop11.grass.w100.di100.wi250.D, pt.ph.wa.crop11.grass.h100.di100.wi250.D,
  pt.ph.wa.crop11.di100.wi250.D.C, pt.ph.wa.crop11.di100.wi250.D.Cot, pt.ph.wa.crop11.di100.wi250.D.R, pt.ph.wa.crop11.di100.wi250.D.S, pt.ph.wa.crop11.di100.wi250.D.Fa, pt.ph.wa.crop11.di100.wi250.D.Fo,
pt.ph.wa.crop11.grass.h100.di100.wi250.D.C, pt.ph.wa.crop11.grass.h100.di100.wi250.D.Cot, pt.ph.wa.crop11.grass.h100.di100.wi250.D.R, pt.ph.wa.crop11.grass.h100.di100.wi250.D.S, pt.ph.wa.crop11.grass.h100.di100.wi250.D.Fa, pt.ph.wa.crop11.grass.h100.di100.wi250.D.Fo,
pt.ph.wa.crop11.di100.woody250.wi250.D, pt.ph.wa.crop11.di100.nonnat250.wi250.D, pt.ph.wa.crop11.di100.ditch.w250.wi250.D, pt.ph.wa.crop11.di100.grass.w250.wi250.D, pt.ph.wa.crop11.di100.grass.h250.wi250.D,
pt.ph.wa.crop11.ditch.w100.di100.wi250.D.S,
pt.ph.wa.wi250.D, pt.ph.wa.woody100.di100.wi250.D, pt.ph.wa.manmade100.di100.wi250.D, pt.ph.wa.ditch.w100.di100.wi250.D, pt.ph.wa.grass.w100.di100.wi250.D, pt.ph.wa.grass.h100.di100.wi250.D,
pt.ph.wa.di100.wi250.D., pt.ph.wa.di100.wi250.D.C, pt.ph.wa.di100.wi250.D.Cot, pt.ph.wa.di100.wi250.D.R, pt.ph.wa.di100.wi250.D.S, pt.ph.wa.di100.wi250.D.Fa, pt.ph.wa.di100.wi250.D.Fo,
pt.ph.wa.grass.h100.di100.wi250.D., pt.ph.wa.grass.h100.di100.wi250.D.C, pt.ph.wa.grass.h100.di100.wi250.D.Cot, pt.ph.wa.grass.h100.di100.wi250.D.R, pt.ph.wa.grass.h100.di100.wi250.D.S, pt.ph.wa.grass.h100.di100.wi250.D.Fa, pt.ph.wa.grass.h100.di100.wi250.D.Fo,
pt.ph.wa.di100.woody250.wi250.D, pt.ph.wa.di100.nonnat250.wi250.D, pt.ph.wa.di100.ditch.w250.wi250.D, pt.ph.wa.di100.grass.w250.wi250.D, pt.ph.wa.di100.grass.h250.wi250.D,
pt.ph.wa.ditch.w100.di100.wi250.D.S,
pt.wa.crop11.wi250.D, pt.wa.crop11.woody100.di100.wi250.D, pt.wa.crop11.manmade100.di100.wi250.D, pt.wa.crop11.ditch.w100.di100.wi250.D, pt.wa.crop11.grass.w100.di100.wi250.D, pt.wa.crop11.grass.h100.di100.wi250.D,
pt.wa.crop11.di100.wi250.D., pt.wa.crop11.di100.wi250.D.C, pt.wa.crop11.di100.wi250.D.Cot, pt.wa.crop11.di100.wi250.D.R, pt.wa.crop11.di100.wi250.D.S, pt.wa.crop11.di100.wi250.D.Fa, pt.wa.crop11.di100.wi250.D.Fo,
pt.wa.crop11.grass.h100.di100.wi250.D.C, pt.wa.crop11.grass.h100.di100.wi250.D.Cot, pt.wa.crop11.grass.h100.di100.wi250.D.R, pt.wa.crop11.grass.h100.di100.wi250.D.S, pt.wa.crop11.grass.h100.di100.wi250.D.Fa, pt.wa.crop11.grass.h100.di100.wi250.D.Fo,
pt.wa.crop11.di100.woody250.wi250.D, pt.wa.crop11.di100.nonnat250.wi250.D, pt.wa.crop11.di100.ditch.w250.wi250.D, pt.wa.crop11.di100.grass.w250.wi250.D, pt.wa.crop11.di100.grass.h250.wi250.D,
pt.wa.crop11.ditch.w100.di100.wi250.D.S,
pt.wa.wi250.D, pt.wa.woody100.di100.wi250.D, pt.wa.manmade100.di100.wi250.D, pt.wa.ditch.w100.di100.wi250.D, pt.wa.grass.w100.di100.wi250.D, pt.wa.grass.h100.di100.wi250.D,
pt.wa.di100.wi250.D.C, pt.wa.di100.wi250.D.Cot, pt.wa.di100.wi250.D.R, pt.wa.di100.wi250.D.S, pt.wa.di100.wi250.D.Fa, pt.wa.di100.wi250.D.Fo,
pt.wa.grass.h100.di100.wi250.D.C, pt.wa.grass.h100.di100.wi250.D.Cot, pt.wa.grass.h100.di100.wi250.D.R, pt.wa.grass.h100.di100.wi250.D.S, pt.wa.grass.h100.di100.wi250.D.Fa, pt.wa.grass.h100.di100.wi250.D.Fo,
pt.wa.di100.woody250.wi250.D, pt.wa.di100.nonnat250.wi250.D, pt.wa.di100.ditch.w250.wi250.D, pt.wa.di100.grass.w250.wi250.D, pt.wa.di100.grass.h250.wi250.D,
pt.wa.ditch.w100.di100.wi250.D.S
)




deltaAIC250.1<-AIC250set1-(AIC(pt.ph.wa.di100.wi250.D.Fa))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1

##Extra Models (pt.ph.D, pt.ph.wi250.D,pt.ph.wa11.D)
pt.ph.D<- glmer(Use ~ PerchH5 +PerchType5 +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
AIC(pt.ph.D)
AICc(pt.ph.D)





confint(pt.ph.wa.crop11.di.grass.w100.wi250.woody250.F.D,level=0.85, method="Wald")
confint(water11,level=0.85)

summary(pt.ph.wa.crop11.di.grass.w100.wi250.woody250.F.D)



#ph only

null<- glmer(Use ~ 1 + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
nullYR<- glmer(Use ~ Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)


ph.wa.crop11.wi250.D<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.grass.h100.di100.wi250.D<- glmer(Use ~ PerchH5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.wi250.D.<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.wi250.D.Cot<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.wi250.D.S<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.wi250.D.Fa<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.wi250.D.Fo<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.grass.h100.di100.wi250.D.Cot<- glmer(Use ~ PerchH5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.grass.h100.di100.wi250.D.S<- glmer(Use ~ PerchH5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.grass.h100.di100.wi250.D.Fa<- glmer(Use ~ PerchH5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.grass.h100.di100.wi250.D.Fo<- glmer(Use ~ PerchH5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.grass.h250.wi250.D<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +GrassHavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)




#________________________________

AIC250set1<-AIC(null, nullYR,
  ph.wa.crop11.wi250.D, 
  ph.wa.crop11.grass.h100.di100.wi250.D,
  ph.wa.crop11.di100.wi250.D., 
  ph.wa.crop11.di100.wi250.D.Cot,
  
  ph.wa.crop11.di100.wi250.D.S, 
  ph.wa.crop11.di100.wi250.D.Fa,
  
  ph.wa.crop11.di100.wi250.D.Fo,
  ph.wa.crop11.grass.h100.di100.wi250.D.Cot, 
  ph.wa.crop11.grass.h100.di100.wi250.D.S, 
  ph.wa.crop11.grass.h100.di100.wi250.D.Fa,
  ph.wa.crop11.grass.h100.di100.wi250.D.Fo,
  
  ph.wa.crop11.di100.grass.h250.wi250.D
)




deltaAIC250.1<-AIC250set1-(AIC(pt.wa.crop11.grass.h100.di100.wi250.D.S))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1





####TOP MODELS####______________________________________________________________________________________________####

#laptop directory
setwd("C:/Users/emily/Desktop/Data Analysis with R/LOSH Scripts")
LOSH16<-read.csv("C:/Users/emily/Desktop/Data Analysis with R/LOSH Scripts/LOSH habitat by group/LOSHtotal_Males_for R.csv", sep=",", header=T)
head(LOSH16)

LOSH16$Wire250


library(lme4)
library(AICcmodavg)


#Rescale Variables

#Wire Length
rescale<-function(x) (x-min(x)) / (max(x) - min(x)) * 100
Wire250scale<-rescale(LOSH16$Wire250)
Wire100scale<-rescale(LOSH16$Wire100)




##___________________________________________________________
#______________________________________________
#________________________________________________

#TOP MODELS

null<- glmer(Use ~ 1 + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
nullYR<- glmer(Use ~ Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)

ph.wa.crop11.wi250.D<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.grass.h100.di100.wi250.D<- glmer(Use ~ PerchH5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.wi250.D.<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.wi250.D.Cot<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.wi250.D.S<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.wi250.D.Fa<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.wi250.D.Fo<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.grass.h100.di100.wi250.D.Cot<- glmer(Use ~ PerchH5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Cotton500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.grass.h100.di100.wi250.D.S<- glmer(Use ~ PerchH5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Soy500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.grass.h100.di100.wi250.D.Fa<- glmer(Use ~ PerchH5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Fallow500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.grass.h100.di100.wi250.D.Fo<- glmer(Use ~ PerchH5 +Water11 +Crops11 +GrassHavg100 +Ditches100 +Wire250scale +Develop500 +Forest500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)
ph.wa.crop11.di100.grass.h250.wi250.D<- glmer(Use ~ PerchH5 +Water11 +Crops11 +Ditches100 +GrassHavg250 +Wire250scale +Develop500 + Year + (1|PointIDtied) + (1|BirdID), family = binomial, nAGQ =1, data = LOSH16)



AIC250set1<-AIC(null, nullYR,
                ph.wa.crop11.wi250.D, 
                ph.wa.crop11.grass.h100.di100.wi250.D,
                ph.wa.crop11.di100.wi250.D., 
                ph.wa.crop11.di100.wi250.D.Cot,
                
                ph.wa.crop11.di100.wi250.D.S, 
                ph.wa.crop11.di100.wi250.D.Fa,
                
                ph.wa.crop11.di100.wi250.D.Fo,
                ph.wa.crop11.grass.h100.di100.wi250.D.Cot, 
                ph.wa.crop11.grass.h100.di100.wi250.D.S, 
                ph.wa.crop11.grass.h100.di100.wi250.D.Fa,
                ph.wa.crop11.grass.h100.di100.wi250.D.Fo,
                ph.wa.crop11.di100.grass.h250.wi250.D
)




deltaAIC250.1<-AIC250set1-(AIC(ph.wa.crop11.grass.h100.di100.wi250.D.S))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1


#AICc

confint(ph.wa.crop11.wi250.D,level=0.85, method="Wald")
confint(ph.wa.crop11.grass.h100.di100.wi250.D,level=0.85, method="Wald")
confint(ph.wa.crop11.di100.wi250.D.,level=0.85, method="Wald")
confint(ph.wa.crop11.di100.wi250.D.Cot,level=0.85, method="Wald")
confint(ph.wa.crop11.di100.wi250.D.S,level=0.85, method="Wald") #top model
confint(ph.wa.crop11.di100.wi250.D.Fa,level=0.85, method="Wald")
confint(ph.wa.crop11.di100.wi250.D.Fo,level=0.85, method="Wald")
confint(ph.wa.crop11.grass.h100.di100.wi250.D.Cot,level=0.85, method="Wald")
confint(ph.wa.crop11.grass.h100.di100.wi250.D.S,level=0.85, method="Wald")
confint(ph.wa.crop11.grass.h100.di100.wi250.D.Fa,level=0.85, method="Wald")
confint(ph.wa.crop11.grass.h100.di100.wi250.D.Fo,level=0.85, method="Wald")
confint(ph.wa.crop11.di100.grass.h250.wi250.D,level=0.85, method="Wald")

summary(ph.wa.crop11.di100.wi250.D.S) #top model


AIC250set1<-c(
  AICc(null),
  AICc(nullYR),
  AICc(ph.wa.crop11.wi250.D),
  AICc(ph.wa.crop11.grass.h100.di100.wi250.D),
  AICc(ph.wa.crop11.di100.wi250.D.),
  AICc(ph.wa.crop11.di100.wi250.D.Cot),
  AICc(ph.wa.crop11.di100.wi250.D.S),
  AICc(ph.wa.crop11.di100.wi250.D.Fa),
  AICc(ph.wa.crop11.di100.wi250.D.Fo),
  AICc(ph.wa.crop11.grass.h100.di100.wi250.D.Cot),
  AICc(ph.wa.crop11.grass.h100.di100.wi250.D.S),
  AICc(ph.wa.crop11.grass.h100.di100.wi250.D.Fa),
  AICc(ph.wa.crop11.grass.h100.di100.wi250.D.Fo),
  AICc(ph.wa.crop11.di100.grass.h250.wi250.D))
  
                                               

deltaAIC250.1<-AIC250set1-(AICc(ph.wa.crop11.di100.wi250.D.S))
table250AIC1<-cbind(AIC250set1,deltaAIC250.1)
table250AIC1






#AIC Table
null
1 perch.t.perch.h5.water11.grass.h.wire100.wire250.D.fallow500      12 759.0234 -746.2849   0.7384651
2 perch.t.perch.h5.crop.water11.grass.h.wire100.wire250.D.fallow500 13 759.9901 -745.2849   1.7051634
3 perch.t.crop5.crop.water11.grass.h.wire100.wire250.D.fallow500    13 758.2849 -745.2849   0.0000000

> AICc Table
AIC5set1 deltaAIC5.1
null 1139.2655  376.3806307
[1,] 759.4119   0.6731428
[2,] 760.4439   1.7051634
[3,] 758.7388   0.0000000


#weights calculations

null<-exp(-0.5*376.3806307)
A<-exp(-0.5*0.6731428)
B<-exp(-0.5*1.7051634)
C<-exp(-0.5*0.00)

sum(null,A,B,C)
=2.140528

null/2.140528
A/2.140528
B/2.140528
C/2.140528

#Weights

> null/2.140528
[1] 8.698885e-83
> A/2.140528
[1] 0.3336629
> B/2.140528
[1] 0.1991625
> C/2.140528
[1] 0.4671745


confint(perch.t.perch.h5.water11.grass.h.wire100.wire250.D.fallow500,level=0.85, method="Wald")
confint(perch.t.perch.h5.crop.water11.grass.h.wire100.wire250.D.fallow500,level=0.85, method="Wald")
confint(perch.t.crop5.crop.water11.grass.h.wire100.wire250.D.fallow500,level=0.85, method="Wald")


summary(perch.t.perch.h5.water11.grass.h.wire100.wire250.D.fallow500)



####_________end____________####



