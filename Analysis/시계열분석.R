
###통계학과 201811526 이은주
###시계열분석 - COVID19에 따른 주식 변화

#######################################################1.수정주가 크롤링(주당)
library(httr)
library(rvest)
library(stringr)
library(xts)
library(lubridate)
library(readr)
library(dplyr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
print(KOR_ticker$'종목코드'[1])
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

ifelse(dir.exists('data/KOR_price_week'), FALSE,
       dir.create('data/KOR_price_week'))

for(i in 1 : nrow(KOR_ticker) ) {
  
  price = xts(NA, order.by = Sys.Date()) # 빈 시계열 데이터 생성
  name = KOR_ticker$'종목코드'[i] # 티커 부분 선택
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  tryCatch({
    # url 생성
    url = paste0(
      'https://fchart.stock.naver.com/sise.nhn?symbol='
      ,name,'&timeframe=week&count=500&requestType=0')
    
    # 이 후 과정은 위와 동일함
    # 데이터 다운로드
    data = GET(url)
    data_html = read_html(data, encoding = 'EUC-KR') %>%
      html_nodes("item") %>%
      html_attr("data") 
    
    # 데이터 나누기
    price = read_delim(data_html, delim = '|')
    
    # 필요한 열만 선택 후 클렌징
    price = price[c(1, 5)] 
    price = data.frame(price)
    colnames(price) = c('Date', 'Price')
    price[, 1] = ymd(price[, 1])
    
    rownames(price) = price[, 1]
    price[, 1] = NULL
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 폴더 내 csv 파일로 저장
  write.csv(price, paste0('data/KOR_price_week/', name,
                          '_price_week.csv'))
  
  # 타임슬립 적용
  Sys.sleep(2)
}

# 주가 정리
library(stringr)
library(xts)
library(magrittr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0') #티커를 6자리로 맞춤

price_list = list()
for (i in 1 : nrow(KOR_ticker)) {
  
  name = KOR_ticker[i, '종목코드']
  price_list[[i]] =
    read.csv(paste0('data/KOR_price_week/', name,
                    '_price_week.csv'),row.names = 1) %>%
    as.xts() #시계열 형태로 변경
  
}

price_list = do.call(cbind, price_list) %>% #리스트를 열 형태로 묶기
  na.locf() #결측치에는 전일 데이터 사용
colnames(price_list) = KOR_ticker$'종목코드'

head(price_list[, 1:5])
tail(price_list[, 1:5])
write.csv(data.frame(price_list), 'data/KOR_price_week.csv')


####################################################2. 시계열분석 - 담배제조업
library(astsa)
library(fpp)
library(tidyverse)
library(stringr)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1, stringsAsFactors = FALSE)
KOR_ticker$'종목코드' = as.factor(str_pad(KOR_ticker$'종목코드', 6,'left', 0))
KOR_price = read.csv('data/KOR_price_week.csv',stringsAsFactors = FALSE)

codex = paste0("X","033780")
date <- KOR_price[,1]
price =KOR_price[,codex]
data <- data.frame( cbind(date,price) )
data$date = as.Date(data$date)
data$price = as.character(data$price)
data$price = as.numeric(data$price)
head(data); dim(data)
data = data[39:499,] #2012년부터 

library(highcharter) 
highchart(type = 'stock') %>%
  hc_add_series(price) %>%
  hc_scrollbar(enabled = FALSE)

##시계열 데이터
data_ts = ts(data$price,start=c(2012,1),frequency=52)
plot.ts(data_ts)
pr_ts_train = window(data_ts, start=c(2012,1), end=c(2018,52))
pr_ts_validation = window(data_ts ,start=c(2019,1), end=c(2019,52))
pr_ts_test = window(data_ts, start=c(2020,1),end=c(2020,45))

##EDA
par(mfrow=c(2,1))
par(mar=c(2, 3, 0, 2), xaxs='i', yaxs='i')

plot(pr_ts_train, ylab="주가", type="c", pch =20, xaxt='n', xlab="")
text(pr_ts_train, col=1:52, labels=1:52, cex=.7)
plot(pr_ts_train, ylab="주가", type="o", pch =20, xlab="")

# `forecast` 패키지 계절변동 시각화
par(mfrow=c(1,1))
library(forecast)
seasonplot(pr_ts_train, ylab="주가", xlab="", 
           main="",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=20)

# 기본 시계열 분해
pr_ts_decompM <- decompose(data_ts, type = "multiplicative")  
plot(pr_ts_decompM, xlab="")

#자기상관
ggAcf(pr_ts_train) #추세나 계절성이 있다.
ggPacf(pr_ts_train)

#차분
dif=diff(pr_ts_train,differences=1)
plot.ts(dif)
prdif_ts_decompM <- decompose(dif, type = "multiplicative")  
plot(prdif_ts_decompM, xlab="")
ggAcf(dif) #절단점을 찾아서 arima 모델
ggPacf(dif)

auto.arima(pr_ts_train)
fit=arima(pr_ts_train, order=c(2,1,0), seasonal=c(0,0,1))
checkresiduals(fit)
fit %>% residuals() %>% ggtsdisplay()

f_arima= forecast(fit, 52)
plot(f_arima, main="", xaxt="n")
lines(pr_ts_validation, col='red')
accuracy(f_arima,pr_ts_validation)[2,,drop=FALSE]

f_arima= forecast(fit, 97)
plot(f_arima, main="", xaxt="n")
lines(pr_ts_test, col='red')

##예측모델
models <- list (
  mod_arima = auto.arima(pr_ts_train, ic='aicc', stepwise=FALSE),
  # mod_exponential = ets(pr_ts_train, model="MAM",ic='aicc', restrict=FALSE),
  mod_neural = nnetar(pr_ts_train, size=25),
  mod_tbats = tbats(pr_ts_train, ic='aicc', seasonal.periods=52),
  mod_bats = bats(pr_ts_train, ic='aicc', seasonal.periods=52),
  mod_stl = stlm(pr_ts_train, s.window=52, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(pr_ts_train)
)

forecasts <- lapply(models, forecast, 52)
forecasts$naive <- naive(pr_ts_train, 52)

par(mfrow=c(4,2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')
for(f in forecasts){
  plot(f, main="", xaxt="n")
  lines(pr_ts_validation, col='red')
}

acc <- lapply(forecasts, function(f){
  accuracy(f, pr_ts_validation)[2,,drop=FALSE]
})

acc <- Reduce(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),]
round(acc, 2)

##뉴스 공시 - 
data[which(data$price==min(data$price)),]
library(rvest)
library(httr)
url = 'https://finance.naver.com/news/news_search.nhn?rcdate=&q=KT%26G&x=0&y=0&sm=all.basic&pd=4&stDateStart=2020-03-17&stDateEnd=2020-03-23'
data = GET(url)
print(data)

data_title = data %>%
  read_html(encoding = 'EUC-KR') %>% #해당 페이지의 HTML 내용
  html_nodes('dl') %>% #해당 태그를 추출하는 함수
  html_nodes('.articleSubject') %>%
  html_nodes('a') %>%
  html_text()
print(data_title)

##############################################2. 시계열분석 - 의료용기기제조업
library(astsa)
library(fpp)
library(tidyverse)
library(stringr)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1, stringsAsFactors = FALSE)
KOR_ticker$'종목코드' = as.factor(str_pad(KOR_ticker$'종목코드', 6,'left', 0))
KOR_price = read.csv('data/KOR_price_week.csv',stringsAsFactors = FALSE)

codex = paste0("X","145720")
date <- KOR_price[,1]
price =KOR_price[,codex]
data <- data.frame( cbind(date,price) )
data$date = as.Date(data$date)
data$price = as.character(data$price)
data$price = as.numeric(data$price)
head(data); dim(data)
data = data[311:499,] #2017년 3월 24일 상장-> 2017년~ 

library(highcharter) 
highchart(type = 'stock') %>%
  hc_add_series(price) %>%
  hc_scrollbar(enabled = FALSE)

##시계열 데이터
data_ts = ts(data$price,start=c(2017,12),frequency=52)
plot.ts(data_ts)
pr_ts_train = window(data_ts, start=c(2017,12), end=c(2019,12))
pr_ts_validation = window(data_ts, start=c(2019,13), end=c(2019,52))
pr_ts_test = window(data_ts, start=c(2020,1),end=c(2020,44))

##EDA
par(mfrow=c(2,1))
par(mar=c(2, 3, 0, 2), xaxs='i', yaxs='i')

plot(pr_ts_train, ylab="주가", type="c", pch =20, xaxt='n', xlab="")
text(pr_ts_train, col=1:52, labels=1:52, cex=.7)
plot(pr_ts_train, ylab="주가", type="o", pch =20, xlab="")

# `forecast` 패키지 계절변동 시각화
par(mfrow=c(1,1))
library(forecast)
seasonplot(pr_ts_train, ylab="주가", xlab="", 
           main="",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=20)

# 기본 시계열 분해
pr_ts_decompM <- decompose(pr_ts_train, type = "multiplicative")
plot(pr_ts_decompM, xlab="")

#자기상관
ggAcf(pr_ts_train) #추세나 계절성이 없다.
ggPacf(pr_ts_train)

auto.arima(pr_ts_train)
fit=arima(pr_ts_train, order=c(0,1,0))
checkresiduals(fit)
fit %>% residuals() %>% ggtsdisplay()

f_arima= forecast(fit, 40)
plot(f_arima, main="", xaxt="n")
lines(pr_ts_validation, col='red')
accuracy(f_arima,pr_ts_validation)[2,,drop=FALSE]

f_arima= forecast(fit, 84)
plot(f_arima, main="", xaxt="n")
lines(pr_ts_test, col='red')

##예측모델
models <- list (
  mod_arima = auto.arima(pr_ts_train, ic='aicc', stepwise=FALSE),
  # mod_exponential = ets(pr_ts_train, model="MAM",ic='aicc', restrict=FALSE),
  mod_neural = nnetar(pr_ts_train, size=25),
  mod_tbats = tbats(pr_ts_train, ic='aicc', seasonal.periods=52),
  mod_bats = bats(pr_ts_train, ic='aicc', seasonal.periods=52),
  mod_stl = stlm(pr_ts_train, s.window=52, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(pr_ts_train)
)

forecasts <- lapply(models, forecast, 40)
forecasts$naive <- naive(pr_ts_train, 40)

par(mfrow=c(4,2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')
for(f in forecasts){
  plot(f, main="", xaxt="n")
  lines(pr_ts_validation, col='red')
}

acc <- lapply(forecasts, function(f){
  accuracy(f, pr_ts_validation)[2,,drop=FALSE]
})

acc <- Reduce(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),]
round(acc, 2)

#2020년 예측
mod_stl = stlm(pr_ts_train, s.window=52, ic='aicc', robust=TRUE, method='ets')
fit_stl = forecast(mod_stl,84)
par(mfrow=c(1,1))
plot(fit_stl, main="", xaxt="n")
lines(pr_ts_test, col='red')

##뉴스 공시
library(rvest)
library(httr)
url = 'https://finance.naver.com/news/news_search.nhn?rcdate=1&q=%B5%A7%C6%BC%BF%F2&x=22&y=16&sm=all.basic&pd=2&stDateStart=2020-10-04&stDateEnd=2020-11-03'
data = GET(url)
print(data)

data_title = data %>%
  read_html(encoding = 'EUC-KR') %>% #해당 페이지의 HTML 내용
  html_nodes('dl') %>% #해당 태그를 추출하는 함수
  html_nodes('.articleSubject') %>%
  html_nodes('a') %>%
  html_text()
print(data_title)


##############################################2. 시계열분석 - 의료용기기제조업
library(astsa)
library(fpp)
library(tidyverse)
library(stringr)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1, stringsAsFactors = FALSE)
KOR_ticker$'종목코드' = as.factor(str_pad(KOR_ticker$'종목코드', 6,'left', 0))
KOR_price = read.csv('data/KOR_price_week.csv',stringsAsFactors = FALSE)

codex = paste0("X","145720")
date <- KOR_price[,1]
price =KOR_price[,codex]
data <- data.frame( cbind(date,price) )
data$date = as.Date(data$date)
data$price = as.character(data$price)
data$price = as.numeric(data$price)
head(data); dim(data)
data = data[311:499,] #2017년 3월 24일 상장-> 2017년~ 

library(highcharter) 
highchart(type = 'stock') %>%
  hc_add_series(price) %>%
  hc_scrollbar(enabled = FALSE)

##시계열 데이터
data_ts = ts(data$price,start=c(2017,12),frequency=52)
plot.ts(data_ts)
pr_ts_train = window(data_ts, start=c(2017,12), end=c(2019,12))
pr_ts_validation = window(data_ts, start=c(2019,13), end=c(2019,52))
pr_ts_test = window(data_ts, start=c(2020,1),end=c(2020,44))

##EDA
par(mfrow=c(2,1))
par(mar=c(2, 3, 0, 2), xaxs='i', yaxs='i')

plot(pr_ts_train, ylab="주가", type="c", pch =20, xaxt='n', xlab="")
text(pr_ts_train, col=1:52, labels=1:52, cex=.7)
plot(pr_ts_train, ylab="주가", type="o", pch =20, xlab="")

# `forecast` 패키지 계절변동 시각화
par(mfrow=c(1,1))
library(forecast)
seasonplot(pr_ts_train, ylab="주가", xlab="", 
           main="",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=20)

# 기본 시계열 분해
pr_ts_decompM <- decompose(pr_ts_train, type = "multiplicative")
plot(pr_ts_decompM, xlab="")

#자기상관
ggAcf(pr_ts_train) #추세나 계절성이 없다.
ggPacf(pr_ts_train)

auto.arima(pr_ts_train)
fit=arima(pr_ts_train, order=c(0,1,0))
checkresiduals(fit)
fit %>% residuals() %>% ggtsdisplay()

f_arima= forecast(fit, 40)
plot(f_arima, main="", xaxt="n")
lines(pr_ts_validation, col='red')
accuracy(f_arima,pr_ts_validation)[2,,drop=FALSE]

f_arima= forecast(fit, 84)
plot(f_arima, main="", xaxt="n")
lines(pr_ts_test, col='red')

##예측모델
models <- list (
  mod_arima = auto.arima(pr_ts_train, ic='aicc', stepwise=FALSE),
  # mod_exponential = ets(pr_ts_train, model="MAM",ic='aicc', restrict=FALSE),
  mod_neural = nnetar(pr_ts_train, size=25),
  mod_tbats = tbats(pr_ts_train, ic='aicc', seasonal.periods=52),
  mod_bats = bats(pr_ts_train, ic='aicc', seasonal.periods=52),
  mod_stl = stlm(pr_ts_train, s.window=52, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(pr_ts_train)
)

forecasts <- lapply(models, forecast, 40)
forecasts$naive <- naive(pr_ts_train, 40)

par(mfrow=c(4,2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')
for(f in forecasts){
  plot(f, main="", xaxt="n")
  lines(pr_ts_validation, col='red')
}

acc <- lapply(forecasts, function(f){
  accuracy(f, pr_ts_validation)[2,,drop=FALSE]
})

acc <- Reduce(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),]
round(acc, 2)

#2020년 예측
mod_stl = stlm(pr_ts_train, s.window=52, ic='aicc', robust=TRUE, method='ets')
fit_stl = forecast(mod_stl,84)
par(mfrow=c(1,1))
plot(fit_stl, main="", xaxt="n")
lines(pr_ts_test, col='red')

##뉴스 공시
library(rvest)
library(httr)
url = 'https://finance.naver.com/news/news_search.nhn?rcdate=1&q=%B5%A7%C6%BC%BF%F2&x=22&y=16&sm=all.basic&pd=2&stDateStart=2020-10-04&stDateEnd=2020-11-03'
data = GET(url)
print(data)

data_title = data %>%
  read_html(encoding = 'EUC-KR') %>% #해당 페이지의 HTML 내용
  html_nodes('dl') %>% #해당 태그를 추출하는 함수
  html_nodes('.articleSubject') %>%
  html_nodes('a') %>%
  html_text()
print(data_title)

##############################################2. 시계열분석 - 의약품제조업
library(astsa)
library(fpp)
library(tidyverse)
library(stringr)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1, stringsAsFactors = FALSE)
KOR_ticker$'종목코드' = as.factor(str_pad(KOR_ticker$'종목코드', 6,'left', 0))
KOR_price = read.csv('data/KOR_price_week.csv',stringsAsFactors = FALSE)

codex = paste0("X","000100")
date <- KOR_price[,1]
price_유한양행 =KOR_price[,codex]
data1 <- data.frame( cbind(date,price_유한양행) )
data1$date = as.Date(data1$date)
data1$price_유한양행 = as.character(data1$price_유한양행)
data1$price_유한양행 = as.numeric(data1$price_유한양행)
head(data1); dim(data1)

codex = paste0("X","207940")
date <- KOR_price[,1]
price_삼성 =KOR_price[,codex]
data2 <- data.frame( cbind(date,price_삼성) )
data2$date = as.Date(data2$date)
data2$price_삼성 = as.character(data2$price_삼성)
data2$price_삼성 = as.numeric(data2$price_삼성)
head(data2); dim(data2)

codex = paste0("X","068270")
date <- KOR_price[,1]
price_셀트리온 =KOR_price[,codex]
data3 <- data.frame( cbind(date,price_셀트리온) )
data3$date = as.Date(data3$date)
data3$price_셀트리온 = as.character(data3$price_셀트리온)
data3$price_셀트리온 = as.numeric(data3$price_셀트리온)
head(data3); dim(data3)

data = merge(data1, data2, by='date', all=TRUE) 
head(data)
data = merge(data, data3, by='date', all=TRUE) 
head(data)

library(highcharter) 
highchart(type = 'stock') %>%
  hc_add_series(data$price_삼성) %>%
  #hc_add_series(data$price_셀트리온) %>%
  #hc_add_series(data$price_유한양행) %>%
  hc_scrollbar(enabled = FALSE) 



##############################################2. 시계열분석 - 조선
##대우조선해양
library(astsa)
library(fpp)
library(tidyverse)
library(stringr)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1, stringsAsFactors = FALSE)
KOR_ticker$'종목코드' = as.factor(str_pad(KOR_ticker$'종목코드', 6,'left', 0))
KOR_price = read.csv('data/KOR_price_week.csv',stringsAsFactors = FALSE)

codex = paste0("X","042660")
date <- KOR_price[,1]
price_대우 =KOR_price[,codex]
data1 <- data.frame( cbind(date,price_대우) )
data1$date = as.Date(data1$date)
data1$price_대우 = as.character(data1$price_대우)
data1$price_대우 = as.numeric(data1$price_대우)
head(data1); dim(data1)

codex = paste0("X","010140")
date <- KOR_price[,1]
price_삼성 =KOR_price[,codex]
data2 <- data.frame( cbind(date,price_삼성) )
data2$date = as.Date(data2$date)
data2$price_삼성 = as.character(data2$price_삼성)
data2$price_삼성 = as.numeric(data2$price_삼성)
head(data2); dim(data2)

codex = paste0("X","010620")
date <- KOR_price[,1]
price_현대 =KOR_price[,codex]
data3 <- data.frame( cbind(date,price_현대) )
data3$date = as.Date(data3$date)
data3$price_현대 = as.character(data3$price_현대)
data3$price_현대 = as.numeric(data3$price_현대)
head(data3); dim(data3)

codex = paste0("X","009540")
date <- KOR_price[,1]
price_한국 =KOR_price[,codex]
data4 <- data.frame( cbind(date,price_한국) )
data4$date = as.Date(data4$date)
data4$price_한국 = as.character(data4$price_한국)
data4$price_한국 = as.numeric(data4$price_한국)
head(data4); dim(data4)

codex = paste0("X","075580")
date <- KOR_price[,1]
price_세진 =KOR_price[,codex]
data5 <- data.frame( cbind(date,price_세진) )
data5$date = as.Date(data5$date)
data5$price_세진 = as.character(data5$price_세진)
data5$price_세진 = as.numeric(data5$price_세진)
head(data5); dim(data5)

data = merge(data1, data2,by='date', all=TRUE) 
data = merge(data, data3, by='date', all=TRUE) 
data = merge(data, data4, by='date', all=TRUE)
data = merge(data, data5, by='date', all=TRUE)
head(data)

library(highcharter) 
highchart(type = 'stock') %>%
  hc_add_series(data$price_삼성) %>%
  hc_add_series(data$price_대우) %>%
  hc_add_series(data$price_현대) %>%
  hc_add_series(data$price_세진) %>%
  hc_add_series(data$price_한국) %>%
  hc_scrollbar(enabled = FALSE) 

##############################################2. 시계열분석 - 항공
codex = paste0("X","003490") #대한항공
date <- KOR_price[,1]
price_대우 =KOR_price[,codex]
data1 <- data.frame( cbind(date,price_대우) )
data1$date = as.Date(data1$date)
data1$price_대우 = as.character(data1$price_대우)
data1$price_대우 = as.numeric(data1$price_대우)
head(data1); dim(data1)

codex = paste0("X","020560") #아시아나항공
date <- KOR_price[,1]
price_삼성 =KOR_price[,codex]
data2 <- data.frame( cbind(date,price_삼성) )
data2$date = as.Date(data2$date)
data2$price_삼성 = as.character(data2$price_삼성)
data2$price_삼성 = as.numeric(data2$price_삼성)
head(data2); dim(data2)

codex = paste0("X","272450") #진에어
date <- KOR_price[,1]
price_현대 =KOR_price[,codex]
data3 <- data.frame( cbind(date,price_현대) )
data3$date = as.Date(data3$date)
data3$price_현대 = as.character(data3$price_현대)
data3$price_현대 = as.numeric(data3$price_현대)
head(data3); dim(data3)

data = merge(data1, data2,by='date', all=TRUE) 
data = merge(data, data3, by='date', all=TRUE)

library(highcharter) 
highchart(type = 'stock') %>%
  hc_add_series(data$price_삼성) %>% #대한
  hc_add_series(data$price_대우) %>% #아시아나
  hc_add_series(data$price_현대) %>% #진에어
  hc_scrollbar(enabled = FALSE) 


##대한항공
library(astsa)
library(fpp)
library(tidyverse)
library(stringr)
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1, stringsAsFactors = FALSE)
KOR_ticker$'종목코드' = as.factor(str_pad(KOR_ticker$'종목코드', 6,'left', 0))
KOR_price = read.csv('data/KOR_price_week.csv',stringsAsFactors = FALSE)

codex = paste0("X","003490")
date <- KOR_price[,1]
price =KOR_price[,codex]
data <- data.frame( cbind(date,price) )
data$date = as.Date(data$date)
data$price = as.character(data$price)
data$price = as.numeric(data$price)
head(data); dim(data)
data = data[39:499,] #2012년부터 

library(highcharter) 
highchart(type = 'stock') %>%
  hc_add_series(price) %>%
  hc_scrollbar(enabled = FALSE)

##시계열 데이터
par(mfrow=c(1,1))
data_ts = ts(data$price,start=c(2012,1),frequency=52)
plot.ts(data_ts)
pr_ts_train = window(data_ts, start=c(2012,1), end=c(2018,52))
pr_ts_validation = window(data_ts ,start=c(2019,1), end=c(2019,52))
pr_ts_test = window(data_ts, start=c(2020,1),end=c(2020,45))

##EDA
par(mfrow=c(2,1))
par(mar=c(2, 3, 0, 2), xaxs='i', yaxs='i')

plot(pr_ts_train, ylab="주가", type="c", pch =20, xaxt='n', xlab="")
text(pr_ts_train, col=1:52, labels=1:52, cex=.7)
plot(pr_ts_train, ylab="주가", type="o", pch =20, xlab="")

# `forecast` 패키지 계절변동 시각화
par(mfrow=c(1,1))
library(forecast)
seasonplot(pr_ts_train, ylab="주가", xlab="", 
           main="",
           year.labels=TRUE, year.labels.left=TRUE, col=1:20, pch=20)

# 기본 시계열 분해
pr_ts_decompM <- decompose(pr_ts_train, type = "multiplicative")  
plot(pr_ts_decompM, xlab="")

#자기상관
par(mfrow=c(1,1))
plot(pr_ts_train)
#season = pr_ts_train %>% stl(s.window='periodic') %>% seasadj 
#autoplot(season) #계절성 조정

ggAcf(pr_ts_train) #정확히 있다, 없다 하기 어려움
ggPacf(pr_ts_train)
#library(TSA) eacf보고 모수 정하는것
#dif = diff(log(pr_ts_train))
#eacf(dif^2)
Box.test(pr_ts_train,type="Ljung-Box",lag=52) #자기상관성 있다.

dif = diff(log(pr_ts_train))
ggAcf(dif) 
ggPacf(dif)
Box.test(dif,type="Ljung-Box",lag=52)

#arma 모델
model_ar = arma(dif, order=c(1,2))
summary(model_ar)
checkresiduals(model_ar)
model_ar %>% residuals() %>% ggtsdisplay()
plot(model_ar)

f_ar= forecast(model_ar, 52) #오류
class(dif)
dif = as.double(dif)
plot(f_ar, main="", xaxt="n")
lines(pr_ts_validation, col='red')
accuracy(f_arima,pr_ts_validation)[2,,drop=FALSE]

arma_yt <- arima.sim(list(order = c(1, 0, 2), ar=0.7, ma = 0.7), n = 250)
astsa::acf2(arma_yt)

#arima모델1
model_arima1=arima(pr_ts_train, order=c(1,1,2))
checkresiduals(model_arima1)
model_arima1 %>% residuals() %>% ggtsdisplay()

f_arima1= forecast(model_arima1, 52)
plot(f_arima1, main="", xaxt="n")
lines(pr_ts_validation, col='red')
accuracy(f_arima1,pr_ts_validation)[2,,drop=FALSE]

#arima모델2
auto.arima(pr_ts_train)
model_arima2=arima(pr_ts_train, order=c(1,1,0), seasonal = c(2,0,0))
checkresiduals(model_arima2)
model_arima2 %>% residuals() %>% ggtsdisplay()

par(mfrow=c(1,1))
f_arima2= forecast(model_arima2, 52)
plot(f_arima2, main="", xaxt="n")
lines(pr_ts_validation, col='red')
accuracy(f_arima2,pr_ts_validation)[2,,drop=FALSE]

##garch모델
#arima(1,1,2) or #arima(1,1,0)
library(rugarch)
spec <- ugarchspec(variance.model = list( garchOrder = c(1, 1),
                                          model = "fGARCH", submodel="GARCH"), 
                   mean.model     = list(armaOrder = c(1, 0)))
garch <- ugarchfit(spec = spec, data =pr_ts_train, 
                   out.sample=52,solver.control = list(trace=0))
modelfor=ugarchforecast(garch, n.ahead = 52, n.roll=52, out.sample=52)

fore = as.character(modelfor@forecast$seriesFor[1,])
fore = as.numeric(fore)
forecast = ts(c(pr_ts_train,fore),start=c(2012,1),frequency=52)
tx =window(data_ts, start=c(2012,1), end=c(2019,52))

plot(tx)
par(new=TRUE)
plot(forecast, col="blue")
accuracy(modelfor@forecast$seriesFor[1,], pr_ts_validation)

##그외 예측모델
models <- list (
  mod_arima = auto.arima(pr_ts_train, ic='aicc', stepwise=FALSE),
  #mod_exponential = ets(pr_ts_train, model="MAM",ic='aicc', restrict=FALSE),
  mod_neural = nnetar(pr_ts_train, size=25),
  mod_tbats = tbats(pr_ts_train, ic='aicc', seasonal.periods=52),
  mod_bats = bats(pr_ts_train, ic='aicc', seasonal.periods=52),
  mod_stl = stlm(pr_ts_train, s.window=52, ic='aicc', robust=TRUE, method='ets'),
  mod_sts = StructTS(pr_ts_train)
)

forecasts <- lapply(models, forecast, 40)
forecasts$naive <- naive(pr_ts_train, 40)

par(mfrow=c(4,2))
par(mar=c(2, 2, 1.5, 2), xaxs='i', yaxs='i')
for(f in forecasts){
  plot(f, main="", xaxt="n")
  lines(pr_ts_validation, col='red')
}

acc <- lapply(forecasts, function(f){
  accuracy(f, pr_ts_validation)[2,,drop=FALSE]
})

acc <- Reduce(rbind, acc)
row.names(acc) <- names(forecasts)
acc <- acc[order(acc[,'MASE']),]
round(acc, 2)

#??????????????????????????2020년 예측
library(rugarch)
spec <- ugarchspec(variance.model = list( garchOrder = c(1, 1),
                                          model = "fGARCH", submodel="GARCH"), 
                   mean.model     = list(armaOrder = c(1, 0)))
garch <- ugarchfit(spec = spec, data =data_ts, 
                   out.sample=52,solver.control = list(trace=0))
modelfor=ugarchforecast(garch, n.ahead = 52, n.roll=52, out.sample=52)

fore = as.character(modelfor@forecast$seriesFor[1,])
fore = as.numeric(fore)
forecast = ts(c(data_ts,fore),start=c(2012,1),frequency=52)

a = rep(rgb(0.2,0.2,0.2,0.2),461)
b = rep(rgb(0.2,0.8,0.5,0.5),53)
ab = as.character(rbind(data.matrix(a),data.matrix(b)))
plot(forecast, col=ab)

mod_stl = stlm(pr_ts_train, s.window=52, ic='aicc', robust=TRUE, method='ets')
fit_stl = forecast(mod_stl,97 )
par(mfrow=c(1,1))
plot(fit_stl, main="", xaxt="n")
lines(pr_ts_test, col='red')

#2020 예측
predict = function(name){
  library(stringr)
  library(rugarch)
  KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1, stringsAsFactors = FALSE)
  KOR_ticker$'종목코드' = as.factor(str_pad(KOR_ticker$'종목코드', 6,'left', 0))
  KOR_price = read.csv('data/KOR_price_week.csv',stringsAsFactors = FALSE)
  code = KOR_ticker[which(KOR_ticker$종목명==name),'종목코드']
  
  codex = paste0("X",code)
  date = as.Date(KOR_price[,1])
  price = KOR_price[,codex]
  data = data.frame(date,price)
  data = data[39:499,]
  
  data_ts = ts(data$price,start=c(2012,1),frequency=52)
  pr_ts_train = window(data_ts, start=c(2012,1), end=c(2019,52))
  pr_ts_test = window(data_ts, start=c(2020,1),end=c(2020,45))
  
  a = auto.arima(pr_ts_train)
  spec <- ugarchspec(variance.model = list( garchOrder = c(1, 1),
                                            model = "fGARCH", submodel="GARCH"), 
                     mean.model     = list(armaOrder = c(a$arma[1], a$arma[3])))
  garch <- ugarchfit(spec = spec, data =pr_ts_train, 
                     out.sample=45,solver.control = list(trace=0))
  modelfor=ugarchforecast(garch, n.ahead = 45, n.roll=45, out.sample=45)
  
  tx = window(data_ts, start=c(2012,1),end=c(2020,45))
  fore = as.character(modelfor@forecast$seriesFor[1,])
  fore = as.numeric(fore)
  forecast = ts(c(pr_ts_train,fore),start=c(2012,1),frequency=52)
  
  plot(tx,ylim=c(min(pr_ts_train),max(pr_ts_train)))
  par(new=TRUE)
  plot(forecast, col="blue",ylim=c(min(pr_ts_train),max(pr_ts_train)))
  accuracy(modelfor@forecast$seriesFor[1,], pr_ts_test)
}

predict = function(code){
  library(stringr)
  library(rugarch)
  KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1, stringsAsFactors = FALSE)
  KOR_ticker$'종목코드' = as.factor(str_pad(KOR_ticker$'종목코드', 6,'left', 0))
  KOR_price = read.csv('data/KOR_price_week.csv',stringsAsFactors = FALSE)
  
  codex = paste0("X",code)
  date = as.Date(KOR_price[,1])
  price = KOR_price[, codex]
  data = data.frame(date,price)
  data = data[39:499,]
  
  data_ts = ts(data$price,start=c(2012,1),frequency=52)
  pr_ts_train = window(data_ts, start=c(2012,1), end=c(2019,52))
  pr_ts_test = window(data_ts, start=c(2020,1),end=c(2020,45))
  
  a = auto.arima(pr_ts_train)
  spec <- ugarchspec(variance.model = list( garchOrder = c(1, 1),
                                            model = "fGARCH", submodel="GARCH"), 
                     mean.model     = list(armaOrder = c(a$arma[1], a$arma[3])))
  garch <- ugarchfit(spec = spec, data =pr_ts_train, 
                     out.sample=45,solver.control = list(trace=0))
  modelfor=ugarchforecast(garch, n.ahead = 45, n.roll=45, out.sample=45)
  
  tx = window(data_ts, start=c(2012,1),end=c(2020,45))
  fore = as.character(modelfor@forecast$seriesFor[1,])
  fore = as.numeric(fore)
  forecast = ts(c(pr_ts_train,fore),start=c(2012,1),frequency=52)
  
  plot(tx,ylim=c(min(pr_ts_train),max(pr_ts_train)))
  par(new=TRUE)
  plot(forecast, col="blue",ylim=c(min(pr_ts_train),max(pr_ts_train)))
  accuracy(modelfor@forecast$seriesFor[1,], pr_ts_test)
}

par(mfrow=c(1,1))

predict("005490") #포스코
predict("016380") #KG동부제철 
predict("001230") #동국제강
predict("004020") #현대제철
predict("002240") #고려제강

predict("007700") #F&F
predict("093050") #LF
predict("020000") #한섬
predict("105630") #한세실업
predict("031430") #신세계인터내셔날
 
predict("003490") #대한항공
predict("020560") #아시아나항공
predict("180640") #한진칼####
predict("047810") #한국항공우주
predict("012450") #한화에어로스페이스

date = as.Date(KOR_price[,1])
price = KOR_price[, "X180640"]
data = data.frame(date,price)
data = data[143:499,]

data_ts = ts(data$price,start=c(2014,1),frequency=52)
pr_ts_train = window(data_ts, start=c(2014,1), end=c(2019,52))
pr_ts_test = window(data_ts, start=c(2020,1),end=c(2020,45))

a = auto.arima(pr_ts_train)
spec <- ugarchspec(variance.model = list( garchOrder = c(1, 1),
                                          model = "fGARCH", submodel="GARCH"), 
                   mean.model     = list(armaOrder = c(a$arma[1], a$arma[3])))
garch <- ugarchfit(spec = spec, data =pr_ts_train, 
                   out.sample=45,solver.control = list(trace=0))
modelfor=ugarchforecast(garch, n.ahead = 45, n.roll=45, out.sample=45)

tx = window(data_ts, start=c(2014,1),end=c(2020,45))
fore = as.character(modelfor@forecast$seriesFor[1,])
fore = as.numeric(fore)
forecast = ts(c(pr_ts_train,fore),start=c(2014,1),frequency=52)

plot(tx,ylim=c(10000,120000))
par(new=TRUE)
plot(forecast, col="blue",ylim=c(10000,120000))
accuracy(modelfor@forecast$seriesFor[1,], pr_ts_test)

