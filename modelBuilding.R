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
fancyRpartPlot(seconddtfit)