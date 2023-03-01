
###통계학과 201811526 이은주
###상관관계 분석

#######################################################1.주식 그래프
library(stringr)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1, stringsAsFactors = FALSE)
KOR_ticker$'종목코드' = as.factor(str_pad(KOR_ticker$'종목코드', 6,'left', 0))
KOR_price = read.csv('data/KOR_price_week.csv',stringsAsFactors = FALSE)

date = as.Date(KOR_price[,1])
KG동부제철 = KOR_price[,"X016380"]
POSCO = KOR_price[,"X005490"]
동국제강 = KOR_price[,"X001230"]
현대제철 = KOR_price[,"X004020"]
고려제강 = KOR_price[,"X002240"]
data = data.frame(date,KG동부제철,POSCO,동국제강,현대제철,고려제강)
head(data)
glimpse(data)

data$KG동부제철 = log(data$KG동부제철)
data$POSCO = log(data$POSCO)
data$동국제강 = log(data$동국제강)
data$현대제철 = log(data$현대제철)
data$고려제강 = log(data$고려제강)

library(ggplot2)
library(dplyr)
data %>% 
  ggplot() +
  geom_line(aes(x = date, y = KG동부제철, col="red")) + 
  geom_line(aes(x = date, y = POSCO, col="yellow")) +
  geom_line(aes(x = date, y = 동국제강, col="green")) +
  geom_line(aes(x = date, y = 현대제철, col="blue")) +
  geom_line(aes(x = date, y = 고려제강, col ="purple")) +
  xlab("Date") + ylab("Stock Price") + theme_bw() +
  ggtitle("철강_주가")+
  scale_color_discrete(name = "종목", 
                       labels = c("KG동부제철", "POSCO","동국제강","현대제철","고려제강"))

date = as.Date(KOR_price[,1])
FaF = KOR_price[,"X007700"]
LF = KOR_price[,"X093050"]
한섬 = KOR_price[,"X020000"]
한세실업 = KOR_price[,"X105630"]
신세계인터내셔날 = KOR_price[,"X031440"]
data = data.frame(date,FaF,LF,한섬,한세실업,신세계인터내셔날)
head(data)

library(ggplot2)
library(dplyr)
data %>% 
  ggplot() +
  geom_line(aes(x = date, y = FaF, col="red")) + 
  geom_line(aes(x = date, y = LF, col="yellow")) +
  geom_line(aes(x = date, y = 한섬, col="green")) +
  geom_line(aes(x = date, y = 한세실업, col="blue")) +
  geom_line(aes(x = date, y = 신세계인터내셔날, col ="purple")) +
  xlab("Date") + ylab("Stock Price") + theme_bw() +
  ggtitle("섬유/의류/신발/호화품")+
  scale_color_discrete(name = "종목", 
                       labels = c("FaF", "LF","한섬","한세실업","신세계인터내셔날"))

#########################
date = as.Date(KOR_price[,1])
대한항공 = KOR_price[,"X003490"]
아시아나항공 = KOR_price[,"X020560"]
한진칼 = KOR_price[,"X180640"]
한국항공우주 = KOR_price[,"X047810"]
한화에어로스페이스 = KOR_price[,"X012450"]
data = data.frame(date,대한항공,아시아나항공,한진칼,
                  한국항공우주,한화에어로스페이스)
head(data)

library(ggplot2)
library(dplyr)
data %>% 
  ggplot() +
  geom_line(aes(x = date, y = 대한항공, col="red")) + 
  geom_line(aes(x = date, y = 아시아나항공, col="yellow")) +
  geom_line(aes(x = date, y = 한진칼, col="green")) +
  geom_line(aes(x = date, y = 한국항공우주, col="blue")) +
  geom_line(aes(x = date, y = 한화에어로스페이스, col ="purple")) +
  xlab("Date") + ylab("Stock Price") + theme_bw() +
  ggtitle("항공사 및 우주항공과 국방_주가")+
  scale_color_discrete(name = "종목", 
                       labels = c("대한항공","아시아나항공","한진칼",
                  "한국항공우주","한화에어로스페이스"))

###############################분기별 profit,stock 정리
library(stringr)
library(dplyr)
library(ggplot2)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1, stringsAsFactors = FALSE)
KOR_ticker$'종목코드' = as.factor(str_pad(KOR_ticker$'종목코드', 6,'left', 0))
KOR_price = read.csv('data/KOR_price_week.csv',stringsAsFactors = FALSE)

profit = read.csv('C:\\Users\\eunju\\Desktop\\3학년 2학기\\학부연구생\\분기순이익.csv',stringsAsFactors = FALSE)
profit 
profit$날짜 = as.Date(profit$날짜)
glimpse(profit)
colnames(profit) = c("날짜","KG동부제철_이익", "POSCO_이익","동국제강_이익","현대제철_이익","고려제강_이익",
                    "FaF_이익", "LF_이익","한섬_이익","한세실업_이익","신세계인터내셔날_이익",
                    "대한항공_이익","아시아나항공_이익","한진칼_이익","한국항공우주_이익","한화에어로스페이스_이익")
profit


KOR_price = KOR_price[195:480,]
head(KOR_price)

KG동부제철 = KOR_price[,"X016380"]
POSCO = KOR_price[,"X005490"]
동국제강 = KOR_price[,"X001230"]
현대제철 = KOR_price[,"X004020"]
고려제강 = KOR_price[,"X002240"]

FaF = KOR_price[,"X007700"]
LF = KOR_price[,"X093050"]
한섬 = KOR_price[,"X020000"]
한세실업 = KOR_price[,"X105630"]
신세계인터내셔날 = KOR_price[,"X031440"]

대한항공 = KOR_price[,"X003490"]
아시아나항공 = KOR_price[,"X020560"]
한진칼 = KOR_price[,"X180640"]
한국항공우주 = KOR_price[,"X047810"]
한화에어로스페이스 = KOR_price[,"X012450"]

data = data.frame(KG동부제철,POSCO,동국제강,현대제철,고려제강,
                   FaF,LF,한섬,한세실업,신세계인터내셔날,
                   대한항공,아시아나항공,한진칼,한국항공우주,한화에어로스페이스)

stock = data.frame()
for (i in c(0:21)){
  for (j in c(1:15)){
    stock[(i+1),j]=mean(data[(i*13+1):(i*13+13),j])
  }
}
stock_date = profit$날짜
stock = cbind(stock_date,stock)
colnames(stock) = c("날짜","KG동부제철_주가", "POSCO_주가","동국제강_주가","현대제철_주가","고려제강_주가",
                    "FaF_주가", "LF_주가","한섬_주가","한세실업_주가","신세계인터내셔날_주가",
                    "대한항공_주가","아시아나항공_주가","한진칼_주가","한국항공우주_주가","한화에어로스페이스_주가")
stock
glimpse(stock)

###########################################
library(ggplot2)
library(dplyr)
dd = merge(stock,profit)
ggplot(data=dd) +
  geom_line(aes(x=날짜, y=scale(대한항공_주가),color="black")) +
  geom_line(aes(x=날짜, y=scale(대한항공_이익), color="blue")) +
  xlab("Date") + ylab("scale") + theme_bw() +
  ggtitle("대한항공 주가,분기순이익 비교")+
  scale_color_discrete(name="label", 
                       labels = c("대한항공_주가", "대한항공_이익")) +
  theme(legend.position = 'bottom',
      legend.title = element_blank())

###########################################선형회귀모델
stock_ratio = data.frame()
for (i in 1:21){
  for (j in 1:15){
    stock_ratio[i,j] = (stock[(i+1),(j+1)]-stock[i,(j+1)])/stock[i,(j+1)]*100
  }
}
stock_ratio
ratio_date= dd[2:22,'날짜']
stock_ratio= cbind(ratio_date,stock_ratio)
colnames(stock_ratio) = c("날짜","KG동부제철_주가증감율", "POSCO_주가증감율","동국제강_주가증감율",
                          "현대제철_주가증감율","고려제강_주가증감율",
                          "FaF_주가증감율", "LF_주가증감율","한섬_주가증감율",
                          "한세실업_주가증감율","신세계인터내셔날_주가증감율",
                          "대한항공_주가증감율","아시아나항공_주가증감율","한진칼_주가증감율",
                          "한국항공우주_주가증감율","한화에어로스페이스_주가증감율")

profit_ratio = data.frame()
for (i in 1:21){
  for (j in 1:15){
    profit_ratio[i,j] = (profit[(i+1),(j+1)]-profit[i,(j+1)])/profit[i,(j+1)]*100
  }
}
profit_ratio
ratio_date= dd[2:22,'날짜']
profit_ratio= cbind(ratio_date,profit_ratio)
colnames(profit_ratio) = c("날짜","KG동부제철_이익증감율", "POSCO_이익증감율","동국제강_이익증감율",
                          "현대제철_이익증감율","고려제강_이익증감율",
                          "FaF_이익증감율", "LF_이익증감율","한섬_이익증감율",
                          "한세실업_이익증감율","신세계인터내셔날_이익증감율",
                          "대한항공_이익증감율","아시아나항공_이익증감율","한진칼_이익증감율",
                          "한국항공우주_이익증감율","한화에어로스페이스_이익증감율")

dd2 = merge(stock_ratio,profit_ratio)
ggplot(data=dd2) +
  geom_line(aes(x=날짜, y=scale(대한항공_주가증감율),color="black")) +
  geom_line(aes(x=날짜, y=scale(대한항공_이익증감율), color="blue")) +
  xlab("Date") + ylab("scale") + theme_bw() +
  ggtitle("대한항공 주가,분기순이익 비교")+
  scale_color_discrete(name="label", 
                       labels = c("대한항공_주가", "대한항공_이익"))
dd3 = dd2[-c(9,13,18),]
r = lm(대한항공_주가증감율~대한항공_이익증감율, data=dd3)
summary(r)
plot(r)
library(plotly)
ggplot(dd3, aes(x=(대한항공_이익증감율), y=(대한항공_주가증감율))) +
  geom_point() + stat_smooth(method = 'lm')

cor(dd2$대한항공_이익증감율,dd2$대한항공_주가증감율)
cor.test(dd2$대한항공_이익증감율,dd2$대한항공_주가증감율)


##################전종목
pro_ratio= data.frame()
for (j in 2:16){
  pro_ratio = rbind(pro_ratio,matrix(profit_ratio[,j]))
}

st_ratio= data.frame()
for (j in 2:16){
  st_ratio = rbind(st_ratio,matrix(stock_ratio[,j]))
}

da = cbind(pro_ratio,st_ratio)
colnames(da) = c("이익증감율","주가증감율")

cor(da$이익증감율,da$주가증감율)
r = lm(주가증감율~이익증감율, data=da)
summary(r)
plot(r)
da = da[-c(53,21,298),]
ggplot(da, aes(x=(이익증감율), y=(주가증감율))) +
  geom_point() + stat_smooth(method = 'lm')



###############################분기별 sale,stock 정리
library(stringr)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1, stringsAsFactors = FALSE)
KOR_ticker$'종목코드' = as.factor(str_pad(KOR_ticker$'종목코드', 6,'left', 0))
KOR_price = read.csv('data/KOR_price_week.csv',stringsAsFactors = FALSE)

sale = read.csv('C:\\Users\\eunju\\Desktop\\3학년 2학기\\학부연구생\\매출액.csv',stringsAsFactors = FALSE)
sale 
sale$날짜 = as.Date(sale$날짜)
glimpse(sale)
colnames(sale) = c("날짜","KG동부제철_매출액", "POSCO_매출액","동국제강_매출액","현대제철_매출액","고려제강_매출액",
                     "FaF_매출액", "LF_매출액","한섬_매출액","한세실업_매출액","신세계인터내셔날_매출액",
                     "대한항공_매출액","아시아나항공_매출액","한진칼_매출액","한국항공우주_매출액","한화에어로스페이스_매출액")
sale


KOR_price = KOR_price[195:480,]
head(KOR_price)

KG동부제철 = KOR_price[,"X016380"]
POSCO = KOR_price[,"X005490"]
동국제강 = KOR_price[,"X001230"]
현대제철 = KOR_price[,"X004020"]
고려제강 = KOR_price[,"X002240"]

FaF = KOR_price[,"X007700"]
LF = KOR_price[,"X093050"]
한섬 = KOR_price[,"X020000"]
한세실업 = KOR_price[,"X105630"]
신세계인터내셔날 = KOR_price[,"X031440"]

대한항공 = KOR_price[,"X003490"]
아시아나항공 = KOR_price[,"X020560"]
한진칼 = KOR_price[,"X180640"]
한국항공우주 = KOR_price[,"X047810"]
한화에어로스페이스 = KOR_price[,"X012450"]

data = data.frame(KG동부제철,POSCO,동국제강,현대제철,고려제강,
                  FaF,LF,한섬,한세실업,신세계인터내셔날,
                  대한항공,아시아나항공,한진칼,한국항공우주,한화에어로스페이스)

stock = data.frame()
for (i in c(0:21)){
  for (j in c(1:15)){
    stock[(i+1),j]=mean(data[(i*13+1):(i*13+13),j])
  }
}
stock_date = sale$날짜
stock = cbind(stock_date,stock)
colnames(stock) = c("날짜","KG동부제철_주가", "POSCO_주가","동국제강_주가","현대제철_주가","고려제강_주가",
                    "FaF_주가", "LF_주가","한섬_주가","한세실업_주가","신세계인터내셔날_주가",
                    "대한항공_주가","아시아나항공_주가","한진칼_주가","한국항공우주_주가","한화에어로스페이스_주가")
stock
glimpse(stock)

###########################################
library(ggplot2)
library(dplyr)
dd = merge(stock,sale)
ggplot(data=dd) +
  geom_line(aes(x=날짜, y=scale(대한항공_주가),color="black")) +
  geom_line(aes(x=날짜, y=scale(대한항공_매출액), color="blue")) +
  xlab("Date") + ylab("scale") + theme_bw() +
  ggtitle("대한항공 주가,매출액 비교")+
  scale_color_discrete(name="label",
                       labels = c("대한항공_주가", "대한항공_매출액"))+
  theme(legend.position = 'bottom',
        legend.title = element_blank())

###########################################선형회귀모델
stock_ratio = data.frame()
for (i in 1:21){
  for (j in 1:15){
    stock_ratio[i,j] = (stock[(i+1),(j+1)]-stock[i,(j+1)])/stock[i,(j+1)]*100
  }
}
stock_ratio
ratio_date= dd[2:22,'날짜']
stock_ratio= cbind(ratio_date,stock_ratio)
colnames(stock_ratio) = c("날짜","KG동부제철_주가증감율", "POSCO_주가증감율","동국제강_주가증감율",
                          "현대제철_주가증감율","고려제강_주가증감율",
                          "FaF_주가증감율", "LF_주가증감율","한섬_주가증감율",
                          "한세실업_주가증감율","신세계인터내셔날_주가증감율",
                          "대한항공_주가증감율","아시아나항공_주가증감율","한진칼_주가증감율",
                          "한국항공우주_주가증감율","한화에어로스페이스_주가증감율")

sale_ratio = data.frame()
for (i in 1:21){
  for (j in 1:15){
    sale_ratio[i,j] = (sale[(i+1),(j+1)]-sale[i,(j+1)])/sale[i,(j+1)]*100
  }
}
sale_ratio
ratio_date= dd[2:22,'날짜']
sale_ratio= cbind(ratio_date,sale_ratio)
colnames(sale_ratio) = c("날짜","KG동부제철_매출액증감율", "POSCO_매출액증감율","동국제강_매출액증감율",
                           "현대제철_매출액증감율","고려제강_매출액증감율율",
                           "FaF_매출액증감율", "LF_매출액증감율","한섬_매출액증감율",
                           "한세실업_매출액증감율","신세계인터내셔날_매출액증감율",
                           "대한항공_매출액증감율","아시아나항공_매출액증감율","한진칼_매출액증감율",
                           "한국항공우주_매출액증감율","한화에어로스페이스_매출액증감율")
dd3 = dd2[-c(9,18,21),]
dd2 = merge(stock_ratio,sale_ratio)
r = lm(대한항공_주가증감율~대한항공_매출액증감율, data=dd3)
summary(r)
plot(r)
ggplot(dd3, aes(x=(대한항공_매출액증감율), y=(대한항공_주가증감율))) +
  geom_point() + stat_smooth(method = 'lm')

cor(dd2$대한항공_매출액증감율,dd2$대한항공_주가증감율)
##################전종목
sal_ratio= data.frame()
for (j in 2:16){
  sal_ratio = rbind(sal_ratio,matrix(sale_ratio[,j]))
}

st_ratio= data.frame()
for (j in 2:16){
  st_ratio = rbind(st_ratio,matrix(stock_ratio[,j]))
}

da = cbind(sal_ratio,st_ratio)
colnames(da) = c("매출액증감율","주가증감율")

r = lm(주가증감율~매출액증감율, data=da)
summary(r)
plot(r)
da = da[-c(120,116,273)]
ggplot(da, aes(x=(매출액증감율), y=(주가증감율))) +
  geom_point() + stat_smooth(method = 'lm')
cor(da$매출액증감율,da$주가증감율)
