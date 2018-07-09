library(stringr)
library(corrgram)
library(corrplot)
library(ggplot2)
x<-read.csv("mainfile.csv",stringsAsFactors = FALSE)

c1<-x$gender
y<-sum(str_count(c1,"male"))
y
z<-sum(str_count(c1,"female"))
z
a<-sum(str_count(c1,"unknown"))
a
v<-c(y,z,a)
v
p<-barplot(v, main="Gender", horiz=TRUE,
        names.arg=c("Male", "Female", "Un"))


c2<-unique(x$gender.confidence)
c2
c<-(x$gender.confidence)
c

c3 <- table(cut(as.numeric(as.character(c2)),
          breaks = 2))
c3

f<-barplot(c3, main="Gender Confidences", verti=TRUE,
           names.arg=c("0.0----0.5", "0.5---1"))

c4<-sum(x$gender.confidence>0.00&x$gender.confidence<0.5&x$gender=="male")
c4

c5<-sum(x$gender.confidence>0.00&x$gender.confidence<0.5&x$gender=="female")
c5

c6<-sum(x$gender.confidence>0.5&x$gender.confidence<1.01&x$gender=="female")
c6

c7<-sum(x$gender.confidence>0.5&x$gender.confidence<1.01&x$gender=="male")
c7

c8<-cbind(c4,c5,c6,c7)
c8
h<-barplot(c8, main="Gender Vs Gender Confidences", horiz=TRUE,
    names.arg=c("0-0.5 Male","0-0.5 F","0.5-1 Male","0.5-1 F"))
h


c9<-(x$profile_yn.confidence)

c10<- table(cut(as.numeric(as.character(c9)),
                    seq(0.5,1.0,by=0.1)))

c10

c11<- (round(c10/sum(c10) * 100, 2))
c11

clabels<-paste(c11, "%", sep="")
clabels
library(plotrix)

colors <- c("white","red","blue","black")
colors
c12<-pie(c11, main="Profile Confidence Ranges",labels=clabels, col=colors)
c12
legend(1.5, 0.5, c("0.9-1.0","0.6-0.7","0.5-0.6","0.7-0.8","0.8-0.9"), cex=0.8, 
       fill=colors)
l1<-tail(sort(table(x$tweet_location)),30)

l2<-sapply(x,is.numeric)
cor.data<- cor(x[,l2])
corrplot(cor.data,method = 'color', main="Correlation B/W Data Variablrs")
corrgram(x,main="Corrgram Relation of each Data Variable")
corrgram(x, type = NULL, order = T,lower.panel = panel.shade,
         upper.panel = panel.pie, main="Corrgram Relation of each Data Variable")
