
###R을 이용한 퀀트 투자 포트폴리오 만들기
###통계학과 201811526 이은주

#############################################################
###Chapter 1 퀀트 투자의 심장: 데이터와 프로그래밍
pkg = c('magrittr', 'quantmod', 'rvest', 'httr', 'jsonlite',
        'readr', 'readxl', 'stringr', 'lubridate', 'dplyr',
        'tidyr', 'ggplot2', 'corrplot', 'dygraphs',
        'highcharter', 'plotly', 'PerformanceAnalytics',
        'nloptr', 'quadprog', 'RiskPortfolios', 'cccp',
        'timetk', 'broom', 'stargazer', 'timeSeries')

new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) {
  install.packages(new.pkg, dependencies = TRUE)}

#############################################################
###Chapter 2 크롤링을 위한 기본 지식
##2.5 오류에 대한 예외처리
result = tryCatch({
  expr #실행하고자 하는 코드
}, warning = function(w) { #경고
  warning-handler-code #경고 발생 시 실행할 구문
}, error = function(e) { #오류
  error-handler-code #오류 발생 시 실행할 구문
}, finally = { 
  cleanup-code #오류의 여부와 관계없이 무조건 수행할 구문
})

##############################################################
###chapter 3 API를 이용한 데이터 수집
#3.1 API를 이용한 Quandl 데이터 다운로드
#단점 : 원하는 항목에 대한 API를 일일이 얻기가 힘듦.
#       무료로 얻을 수 있는 정보에 제한이 있으며, 다운로드 양에도 제한 
#       (전 종목의 데이터를 구하기는 사실상 불가능)
url.aapl = "https://www.quandl.com/api/v3/datasets/WIKI/AAPL/data.csv?api_key=xw3NU3xLUZ7vZgrz5QnG"
data.aapl = read.csv(url.aapl)
head(data.aapl)

#3.2 getSymbols() 함수를 이용한 API 다운로드
library(quantmod)
getSymbols('AAPL') #괄호 안에 다운로드하려는 종목의 티커를 입력
head(AAPL)

chart_Series(Ad(AAPL))
head(Ad(AAPL)) #Ad; AAPL.Adjusted 변수만 추출
head(Vo(AAPL)) #Vo; AAPL.Volume 변수만 추출

data = getSymbols('AAPL', #다운로드한 데이터가 자동으로 티커와 동일한 변수명에 저장
                  from = '2000-01-01', to = '2018-12-31',
                  auto.assign = FALSE) #다운로드한 데이터가 원하는 변수에 저장
head(data)
ticker = c('FB', 'NVDA') 
getSymbols(ticker)
head(FB); head(NVDA)

#국내 종목 주가
getSymbols('005930.KS',
           from = '2000-01-01', to = '2018-12-31')
tail(Ad(`005930.KS`))
tail(Cl(`005930.KS`))

getSymbols("068760.KQ",
           from = '2000-01-01', to = '2018-12-31')
tail(Cl(`068760.KQ`))

#FRED 데이터; 미국 연방준비은행에서 관리하는 데이터
getSymbols('DGS10', src='FRED')
chart_Series(DGS10)

getSymbols('DEXKOUS', src='FRED')
tail(DEXKOUS)

#################################################################
###Chapter 4 크롤링 이해하기
#GET방식 - 금융 실시간 속보 제목 추출
library(rvest)
library(httr)

url = 'https://finance.naver.com/news/news_list.nhn?mode=LSS2D&section_id=101&section_id2=258'
data = GET(url)
print(data)

data_title = data %>%
  read_html(encoding = 'EUC-KR') %>% #해당 페이지의 HTML 내용
  html_nodes('dl') %>% #해당 태그를 추출하는 함수
  html_nodes('.articleSubject') %>%
  html_nodes('a') %>%
  html_attr('title') #속성을 추출하는 함수(title에 해당하는 부분만 추출)
print(data_title)

#POST방식 - 기업공시채널에서 오늘의 공시 추출
library(httr)
library(rvest)
Sys.setlocale("LC_ALL", "English") #한글로 작성된 페이지 오류 
                                   #로케일 언어를 영어로 설정

url = 'https://kind.krx.co.kr/disclosure/todaydisclosure.do'
data = POST(url, body = 
              list(
                method = 'searchTodayDisclosureSub',
                currentPageSize = '15',
                pageIndex = '1',
                orderMode = '0',
                orderStat = 'D',
                forward = 'todaydisclosure_sub',
                chose = 'S',
                todayFlag = 'Y',
                selDate = '2020-09-18'
              ))

data = read_html(data) %>%
  html_table(fill = TRUE) %>%
  .[[1]]
Sys.setlocale("LC_ALL", "Korean") #로케일 언어를 다시 한국어로 설정
print(head(data))

#네이버 금융에서 주식티커 크롤링
library(httr)
library(rvest)
library(stringr)

data = list()

# i = 0 은 코스피, i = 1 은 코스닥 종목
for (i in 0:1) {
  
  ticker = list()
  url =
    paste0('https://finance.naver.com/sise/',
           'sise_market_sum.nhn?sosok=',i,'&page=1')
  down_table = GET(url)
  
  # 최종 페이지 번호 찾아주기
  navi.final = read_html(down_table, encoding = "EUC-KR") %>%
    html_nodes(., ".pgRR") %>%
    html_nodes(., "a") %>%
    html_attr(.,"href") %>%
    strsplit(., "=") %>% #특정 글자 기준으로 나눔
    unlist() %>% #벡터 형태로 변환
    tail(., 1) %>% #뒤에서 첫번째 데이터만 선택
    as.numeric()
  
  # 첫번째 부터 마지막 페이지까지 for loop를 이용하여 테이블 추출하기
  for (j in 1:navi.final) {
    
    # 각 페이지에 해당하는 url 생성
    url = paste0(
      'https://finance.naver.com/sise/',
      'sise_market_sum.nhn?sosok=',i,"&page=",j)
    down_table = GET(url)
    
    Sys.setlocale("LC_ALL", "English")
    # 한글 오류 방지를 위해 영어로 로케일 언어 변경
    
    table = read_html(down_table, encoding = "EUC-KR") %>%
      html_table(fill = TRUE)
    table = table[[2]] # 원하는 테이블 추출
    
    Sys.setlocale("LC_ALL", "Korean")
    # 한글을 읽기위해 로케일 언어 재변경
    
    table[, ncol(table)] = NULL # 토론식 부분 삭제
    table = na.omit(table) # 빈 행 삭제
    
    # 6자리 티커만 추출
    symbol = read_html(down_table, encoding = "EUC-KR") %>%
      html_nodes(., "tbody") %>%
      html_nodes(., "td") %>%
      html_nodes(., "a") %>%
      html_attr(., "href")
    
    symbol = sapply(symbol, function(x) { #링크 주소 저장후, 마지막 6글자만 추출
      str_sub(x, -6, -1) 
    })
    
    symbol = unique(symbol)
    
    # 테이블에 티커 넣어준 후, 테이블 정리
    table$N = symbol
    colnames(table)[1] = "종목코드"
    
    rownames(table) = NULL
    ticker[[j]] = table
    
    Sys.sleep(0.5) # 페이지 당 0.5초의 슬립 적용
  }
  
  # do.call을 통해 리스트를 데이터 프레임으로 묶기
  ticker = do.call(rbind, ticker)
  data[[i + 1]] = ticker
}

# 코스피와 코스닥 테이블 묶기
data = do.call(rbind, data)
head(data)

