
setwd("C:\\Users\\eunju\\Desktop\\3학년 2학기\\학부연구생\\가동률지수")

data1 <- read.csv("data.csv",header=FALSE)
datal1 = data.frame()
for (j in 1:ncol(data1)) {
  for (i in 1:nrow(data1)) {
    val = data.frame(data1[i,j])
    
    datal1 = dplyr::bind_rows(datal1, val)
  }
}
str(datal1)
head(datal1)
dim(data1)

data2 <- read.csv("name.csv",header=FALSE)
name = data.frame()
for (i in 1:ncol(data1)) {
  val = data.frame(data2)
  name = dplyr::bind_rows(name, val)
}
str(name)
head(name)
dim(name)

data3 <- read.csv("date.csv",header=FALSE)
date1 = data.frame()
for (i in 1:ncol(data1)) {
  val = data.frame(rep(data3[i,1],nrow(data1)))
  date1 = dplyr::bind_rows(date1, val)
}
str(date1)
head(date1)
dim(date1)

datare = data.frame(date1,name,datal1)
colnames(datare)=c("날짜", "name", "가동률지수")
head(datare)
tail(datare)
dplyr::glimpse(datare)
class(datare)


library(quantmod)
library(plotly)
datare %>% 
  plot_ly(x= ~name, y = ~가동률지수, type="box",
          text=paste(datare$날짜)) %>%
  layout(boxmode = "group")



library("dplyr")
install.packages("hrbrthemes")
library("hrbrthemes")
library(ggplot2)
##################################################
#담배제조업
datare %>%
  mutate( highlight=ifelse(name=="담배 제조업", "담배 제조업", "Other")) %>%
  ggplot( aes(x=date1, y=datal1, group=name, color=highlight, size=highlight)) +
  geom_line()+
  scale_color_manual(values = c("lightgrey","#69b3a2")) +
  scale_size_manual(values=c(0.2,1.5))+
  theme(legend.position="none") +
  theme_ipsum()

#의료용 기기
datare %>%
  mutate( highlight=ifelse(name=="의료용 기기 제조업", "의료용 기기 제조업", "Other")) %>%
  ggplot( aes(x=date1, y=datal1, group=name, color=highlight, size=highlight)) +
  geom_line()+
  scale_color_manual(values = c("lightgrey","#69b3a2")) +
  scale_size_manual(values=c(0.2,1.5))+
  theme(legend.position="none") +
  theme_ipsum()
#선박
datare %>%
  mutate( highlight=ifelse(name=="선박 및 보트 건조업", "선박 및 보트 건조업", "Other")) %>%
  ggplot( aes(x=date1, y=datal1, group=name, color=highlight, size=highlight)) +
  geom_line()+
  scale_color_manual(values = c("lightgrey","#69b3a2")) +
  scale_size_manual(values=c(0.2,1.5))+
  theme(legend.position="none") +
  theme_ipsum()
#항공기
datare %>%
  mutate( highlight=ifelse(name=="항공기 우주선 및 부품 제조업", "항공기 우주선 및 부품 제조업", "Other")) %>%
  ggplot( aes(x=date1, y=datal1, group=name, color=highlight, size=highlight)) +
  geom_line()+
  scale_color_manual(values = c("lightgrey","#69b3a2")) +
  scale_size_manual(values=c(0.2,1.5))+
  theme(legend.position="none") +
  theme_ipsum()





####################################
datare$날짜 = as.Date(datare$날짜)
datare %>%
  ggplot( aes(x=날짜, y=가동률지수)) +
  geom_line( data=datare %>% filter(name != "담배 제조업"), aes(group=name), color="grey", size=0.2) +
  geom_line( data=datare %>% filter(name == "담배 제조업"), aes(group=name), color="#69b3a2", size=1.5 )+
  theme(legend.position="none") +
  theme_ipsum()

datare %>%
  ggplot( aes(x=날짜, y=가동률지수)) +
  geom_line( data=datare %>% filter(name != "의료용 기기 제조업"), aes(group=name), color="grey", size=0.2) +
  geom_line( data=datare %>% filter(name == "의료용 기기 제조업"), aes(group=name), color="#69b3a2", size=1.5 )+
  theme(legend.position="none") +
  theme_ipsum()

datare %>%
  ggplot( aes(x=날짜, y=가동률지수)) +
  geom_line( data=datare %>% filter(name != "선박 및 보트 건조업"), aes(group=name), color="grey", size=0.2) +
  geom_line( data=datare %>% filter(name == "선박 및 보트 건조업"), aes(group=name), color="#69b3a2", size=1.5 )+
  theme(legend.position="none") +
  theme_ipsum()

datare %>%
  ggplot( aes(x=날짜, y=가동률지수)) +
  geom_line( data=datare %>% filter(name != "항공기 우주선 및 부품 제조업"), aes(group=name), color="grey", size=0.2) +
  geom_line( data=datare %>% filter(name == "항공기 우주선 및 부품 제조업"), aes(group=name), color="#69b3a2", size=1.5 )+
  theme(legend.position="none") +
  theme_ipsum()
