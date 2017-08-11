library(readr)
library(ggplot2)
library(ggthemes)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
df <- read_csv("~/cpe/Pitchfork/Pitchforkcleaned.csv")
df$X1 <- NULL
df=df[df['artists']!="Various Artists",]
df['EarlyEra']=1
df[df['Release Year']>2009,'EarlyEra']=0
df['trend']=df['Prev2']-df['Prev1']
df[df['Prev2']==-1,'trend']=-1
ggplot(df[(df['Prev1']==-1) & (df['Release Year']>2001),],aes(scores))+
  geom_histogram(bins=20,color="black",fill="red")+
  ggtitle("Distribution of first reviewed releases")+xlab("Score")
ggplot(df[df['Prev1']!=-1,],aes(Prev1,scores))+geom_point(alpha=.1,color="blue")+
  ggtitle("Score plotted vs current score")+xlab("Previous Score")+ylab("Current Score")+
  geom_smooth(method=lm,color='red',se=FALSE,alpha=.5)+
  theme(plot.background=element_rect(fill="white"),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(color = 'white'),
        panel.grid.minor = element_line(color = 'white'))

firstyears<-df[(df['Prev1']==-1) & (df['Release Year']>2001),]

firstlmfit<-aov(scores~MajorIndie+Major+electronic+
                 pop+experimental+global+folk+jazz+EarlyEra,data=firstyears)
summary(firstlmfit)


firstdtfit<-rpart(scores~MajorIndie+Major+Other+electronic+pop+experimental+
                 global+folk+jazz+EarlyEra,data=firstyears,method="anova",
                 control=rpart.control(minsplit=30, cp=0.00075))
fancyRpartPlot(firstdtfit)

qqnorm(firstyears$scores)
qqline(firstyears$scores)
qqnorm(firstyears$scores**2)
qqline(firstyears$scores**2)

firstlmfit<-aov(scores**2~MajorIndie+Major+electronic+
                  pop+experimental+global+folk+jazz+EarlyEra,data=firstyears)
summary(firstloglmfit)
#does worse

sophs<-df[(df['Prev1']!=-1) & (df['Prev2']==-1),]
sophs['Prev1a']=sophs['Prev1']**2
secondredlmfit<-lm(scores~MajorIndie+Major+electronic+
                  pop+experimental+global+folk+jazz+EarlyEra+Prev1,data=sophs)
summary(secondlmfit)


secondredlmfit<-lm(scores~MajorIndie+Major+electronic+
                  pop+experimental+global+folk+jazz+Prev1+Prev1a,data=sophs)
summary(secondredlmfit)


secondqmfit<-lm(scores~Prev1+Prev1a,data=sophs)
summary(secondqmfit)

seconddtfit<-rpart(scores~MajorIndie+Major+Other+electronic+pop+experimental+
                    global+folk+jazz+EarlyEra+Prev1,data=sophs,method="anova",
                   control=rpart.control(minsplit=30, cp=0.00095))
fancyRpartPlot(seconddtfit)

thirds<-df[(df['Prev1']!=-1) & (df['Prev2']!=-1) & (df['Prev3']==-1),]

thirdlmfit<-lm(scores~MajorIndie+Major+electronic+
                     pop+experimental+global+folk+
                 jazz+EarlyEra+Prev1+Prev2+`Label Switch`+Wait,data=thirds)
summary(thirdlmfit)

thirddtfit<-rpart(scores~MajorIndie+Major+Other+electronic+pop+experimental+
                     global+folk+jazz+EarlyEra+Prev1+Prev2+trend,data=thirds,method="anova",
                   control=rpart.control(minsplit=30,cp=.0009))
fancyRpartPlot(thirddtfit)

others<-df[(df['Prev1']!=-1) & (df['Prev2']!=-1) & (df['Prev3']!=-1),]
thirdlmfit<-lm(scores~MajorIndie+Major+electronic+
                 pop+experimental+global+folk+
                 jazz+EarlyEra+Prev1+Prev2+Prev3+
                 `Label Switch`+Wait,data=others)
summary(thirdlmfit)


thirdqmfit<-lm(scores**2~MajorIndie+Major+electronic+
                 pop+experimental+global+folk+
                 jazz+EarlyEra+Prev1+Prev2+Prev3+
                 `Label Switch`+Wait,data=others)
summary(thirdqmfit)

lastdtfit<-rpart(scores~MajorIndie+Major+Other+electronic+pop+experimental+
                    global+folk+jazz+EarlyEra+Prev1+Prev2
                  +Prev3+Wait+`Label Switch`,data=others,method="anova",
                 control=rpart.control(minsplit=30,cp=.003))
fancyRpartPlot(lastdtfit)


# Now building basics of clustering
df['GreaterThan8.3%']=0.0
df['7to8.3']=0.0
df['6to7']=0.0
df['4to6']=0.0
df['0to3']=0.0
# debuts 
simpledist<-function(df,column,i){
  return(abs(df[,column]-df[i,column][[1]]))
}
genredistance=simpledist(firstyears,'rap',1)+simpledist(firstyears,'rock',1)+
  simpledist(firstyears,'electronic',1)+simpledist(firstyears,'experimental',1)+
  simpledist(firstyears,'pop',1)+simpledist(firstyears,'global',1)+
  simpledist(firstyears,'jazz',1)+simpledist(firstyears,'folk',1)
labeldistance=simpledist(firstyears,'Major',1)+simpledist(firstyears,'MajorIndie',1)+
  simpledist(firstyears,'Other',1)
totaldist=genredistance+labeldistance
rels<-firstyears[totaldist==0,]



for (i in 1:nrow(firstyears)){
  genredistance=simpledist(firstyears,'rap',i)+simpledist(firstyears,'rock',i)+
    simpledist(firstyears,'electronic',i)+simpledist(firstyears,'experimental',i)+
    simpledist(firstyears,'pop',i)+simpledist(firstyears,'global',i)+
    simpledist(firstyears,'jazz',i)+simpledist(firstyears,'folk',i)
  labeldistance=simpledist(firstyears,'Major',i)+simpledist(firstyears,'MajorIndie',i)+
    simpledist(firstyears,'Other',i)
  otherdist=simpledist(firstyears,'EarlyEra',i)
  totaldist=genredistance+labeldistance
  rels<-firstyears[totaldist==0,]
  coun<-nrow(rels)
  firstyears[i,'GreaterThan8.3%']=nrow(rels[rels[,'scores']>8.3,])/coun
  firstyears[i,'7to8.3']=nrow(rels[(rels[,'scores']<=8.3) & (rels[,'scores']>7),])/coun
  firstyears[i,'6to7']=nrow(rels[(rels[,'scores']<=7) & (rels[,'scores']>6),])/coun
  firstyears[i,'4to6']=nrow(rels[(rels[,'scores']<=6) & (rels[,'scores']>3),])/coun
  firstyears[i,'0to3']=nrow(rels[(rels[,'scores']<=3) & (rels[,'scores']>=0.0),])/coun
}

for (i in 1:nrow(firstyears)){
  genredistance=simpledist(firstyears,'rap',i)+simpledist(firstyears,'rock',i)+
    simpledist(firstyears,'electronic',i)+simpledist(firstyears,'experimental',i)+
    simpledist(firstyears,'pop',i)+simpledist(firstyears,'global',i)+
    simpledist(firstyears,'jazz',i)+simpledist(firstyears,'folk',i)
  labeldistance=simpledist(firstyears,'Major',i)+simpledist(firstyears,'MajorIndie',i)+
    simpledist(firstyears,'Other',i)
  otherdist=simpledist(firstyears,'EarlyEra',i)
  totaldist=genredistance+labeldistance+otherdist
  rels<-firstyears[totaldist==0,]
  coun<-nrow(rels)
  firstyears[i,'GreaterThan8.3%']=nrow(rels[rels[,'scores']>8.3,])/coun
  firstyears[i,'7to8.3']=nrow(rels[(rels[,'scores']<=8.3) & (rels[,'scores']>7),])/coun
  firstyears[i,'6to7']=nrow(rels[(rels[,'scores']<=7) & (rels[,'scores']>6),])/coun
  firstyears[i,'4to6']=nrow(rels[(rels[,'scores']<=6) & (rels[,'scores']>3),])/coun
  firstyears[i,'0to3']=nrow(rels[(rels[,'scores']<=3) & (rels[,'scores']>=0.0),])/coun
}
sophs['GreaterThan8.3%']=0.0
sophs['7to8.3']=0.0
sophs['6to7']=0.0
sophs['4to6']=0.0
sophs['0to3']=0.0
sophs['Prev1n']=(sophs['Prev1']-mean(sophs$'Prev1'))/sd(sophs$'Prev1')
for (i in 1:nrow(sophs)){
  genredistance=simpledist(sophs,'rap',i)+simpledist(sophs,'rock',i)+
    simpledist(sophs,'electronic',i)+simpledist(sophs,'experimental',i)+
    simpledist(sophs,'pop',i)+simpledist(sophs,'global',i)+
    simpledist(sophs,'jazz',i)+simpledist(sophs,'folk',i)
  labeldistance=simpledist(sophs,'Major',i)+simpledist(sophs,'MajorIndie',i)+
    simpledist(sophs,'Other',i)
  scoredist=simpledist(sophs,'Prev1n',i)
  otherdist=simpledist(sophs,'EarlyEra',i)+simpledist(sophs,'Wait',i)+simpledist(sophs,'Label Switch',i)
  totaldist=genredistance+labeldistance+otherdist+scoredist
  rels<-sophs[totaldist<=sort(totaldist$rap)[100],]
  coun<-nrow(rels)
  sophs[i,'GreaterThan8.3%']=nrow(rels[rels[,'scores']>8.3,])/coun
  sophs[i,'7to8.3']=nrow(rels[(rels[,'scores']<=8.3) & (rels[,'scores']>7),])/coun
  sophs[i,'6to7']=nrow(rels[(rels[,'scores']<=7) & (rels[,'scores']>6),])/coun
  sophs[i,'4to6']=nrow(rels[(rels[,'scores']<=6) & (rels[,'scores']>3),])/coun
  sophs[i,'0to3']=nrow(rels[(rels[,'scores']<=3) & (rels[,'scores']>=0.0),])/coun
}

thirds['Prev1n']=(thirds['Prev1']-mean(thirds$'Prev1'))/sd(thirds$'Prev1')
thirds['Prev2n']=(thirds['Prev2']-mean(thirds$'Prev2'))/sd(thirds$'Prev2')
for (i in 1:nrow(thirds)){
  genredistance=simpledist(thirds,'rap',i)+simpledist(thirds,'rock',i)+
    simpledist(thirds,'electronic',i)+simpledist(thirds,'experimental',i)+
    simpledist(thirds,'pop',i)+simpledist(thirds,'global',i)+
    simpledist(thirds,'jazz',i)+simpledist(thirds,'folk',i)
  labeldistance=simpledist(thirds,'Major',i)+simpledist(thirds,'MajorIndie',i)+
    simpledist(thirds,'Other',i)
  scoredist=simpledist(thirds,'Prev1n',i)+simpledist(thirds,'Prev2n',i)
  otherdist=simpledist(thirds,'EarlyEra',i)+simpledist(thirds,'Wait',i)+
    simpledist(thirds,'Label Switch',i)
  totaldist=genredistance+labeldistance+otherdist+scoredist
  rels<-thirds[totaldist<=sort(totaldist$rap)[100],]
  coun<-nrow(rels)
  thirds[i,'GreaterThan8.3%']=nrow(rels[rels[,'scores']>8.3,])/coun
  thirds[i,'7to8.3']=nrow(rels[(rels[,'scores']<=8.3) & (rels[,'scores']>7),])/coun
  thirds[i,'6to7']=nrow(rels[(rels[,'scores']<=7) & (rels[,'scores']>6),])/coun
  thirds[i,'4to6']=nrow(rels[(rels[,'scores']<=6) & (rels[,'scores']>3),])/coun
  thirds[i,'0to3']=nrow(rels[(rels[,'scores']<=3) & (rels[,'scores']>=0.0),])/coun
}

others['Prev1n']=(others['Prev1']-mean(others$'Prev1'))/sd(others$'Prev1')
others['Prev2n']=(others['Prev2']-mean(others$'Prev2'))/sd(others$'Prev2')
others['Prev3n']=(others['Prev3']-mean(others$'Prev3'))/sd(others$'Prev3')
for (i in 1:nrow(others)){
  genredistance=simpledist(others,'rap',i)+simpledist(others,'rock',i)+
    simpledist(others,'electronic',i)+simpledist(others,'experimental',i)+
    simpledist(others,'pop',i)+simpledist(others,'global',i)+
    simpledist(others,'jazz',i)+simpledist(others,'folk',i)
  labeldistance=simpledist(others,'Major',i)+simpledist(others,'MajorIndie',i)+
    simpledist(others,'Other',i)
  scoredist=simpledist(others,'Prev1n',i)+simpledist(others,'Prev2n',i)+
    simpledist(others,'Prev3n',i)
  otherdist=simpledist(others,'EarlyEra',i)+simpledist(others,'Wait',i)+
    simpledist(others,'Label Switch',i)
  totaldist=genredistance+labeldistance+otherdist+scoredist
  rels<-others[totaldist<=sort(totaldist$rap)[100],]
  coun<-nrow(rels)
  others[i,'GreaterThan8.3%']=nrow(rels[rels[,'scores']>8.3,])/coun
  others[i,'7to8.3']=nrow(rels[(rels[,'scores']<=8.3) & (rels[,'scores']>7),])/coun
  others[i,'6to7']=nrow(rels[(rels[,'scores']<=7) & (rels[,'scores']>6),])/coun
  others[i,'4to6']=nrow(rels[(rels[,'scores']<=6) & (rels[,'scores']>3),])/coun
  others[i,'0to3']=nrow(rels[(rels[,'scores']<=3) & (rels[,'scores']>=0.0),])/coun
}