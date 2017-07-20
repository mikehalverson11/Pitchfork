library(rvest)
library(magritr)
library(XML)
artists<-c()
album<-c()
genre<-c()
scores<-c()
labels<-c()
years<-c()
dates<-c()
reviewer<-c()
n=1 # starting page
N=1591 # what page of reviews you want to go to
# iterate over all specified pages
for (i in n:N){
  link <-paste("http://pitchfork.com/reviews/albums/?page=",i,sep="")
  htmlinit <- read_html(link)
  message(i)
  review <- html_attr(html_nodes(htmlinit,css="a"),"href")
  review<-review[grepl("/reviews/albums/[0-9]",review)]
  for (j in review){
    link<-paste("http://pitchfork.com",j,sep="")
    htmlrev<-read_html(link)
    art<- html_text(html_nodes(htmlrev, css=".artist-list"))
    artists <- c(artists,art)
    genreht<-"N/A"
    genrehts<-html_text(html_nodes(htmlrev,css=".before"))
    if (length(genrehts)!=0){
      genreht=genrehts
    }
    genre<-c(genre,rep(genreht,length(art)))
    album <- c(album,html_text(html_nodes(htmlrev, css=".review-title")))
    score<-html_text(html_nodes(htmlrev,css=".score"))
    date<-html_text(html_nodes(htmlrev,css=".pub-date"))
    dates<-c(dates,rep(date,length(art)))
    label<-html_text(html_nodes(htmlrev,css=".label-list li"))
    if ((length(label)!=1) && (length(art)!=length(label))) {
      label<-rep(paste(label[1],label[2],sep="/"),length(art))
    } else if (length(label)==0){label=""} else if ((length(label)==1) && (length(art)!=1)){label<-rep(label,length(art))}
    labels<-c(labels,label)
    year<-gsub(" ","",html_text(html_nodes(htmlrev,css=".year")))
    year<-substring(year,2,10)
    years<-c(years,year)
    scores<-c(scores,score)
    writer<-html_text(html_nodes(htmlrev,css=".authors-detail__display-name"))
    if (length(writer)!=1){
      writer="N/A"
    }
    reviewer<-c(reviewer,rep(writer,length(art)))
    if (length(labels)!=length(scores)){message("WRONG",i,album[length(album)])}
  }
}
# convert string scores to scores 
scores <-as.numeric(scores)
#create dataframe
df<-data.frame(artists,album,genre,scores,years,labels,dates,reviewer)
write.csv(df,"Pitchfork2.csv")