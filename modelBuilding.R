library(readr)
library(ggplot2)
library(ggthemes)
df <- read_csv("~/cpe/Pitchfork/Pitchforkcleaned.csv")
df$X1 <- NULL
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

