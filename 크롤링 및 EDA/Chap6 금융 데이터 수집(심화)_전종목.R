###R을 이용한 퀀트 투자 포트폴리오 만들기
###통계학과 201811526 이은주

###Chapter 6 금융 데이터 수집하기 (심화) _ 전종목
library(httr)
library(rvest)
library(stringr)
library(xts)
library(lubridate)
library(readr)
library(dplyr)
#############################################################
##6.1 수정주가 크롤링
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
print(KOR_ticker$'종목코드'[1])
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

ifelse(dir.exists('data/KOR_price'), FALSE,
       dir.create('data/KOR_price'))

for(i in 1 : nrow(KOR_ticker) ) {
  
  price = xts(NA, order.by = Sys.Date()) # 빈 시계열 데이터 생성
  name = KOR_ticker$'종목코드'[i] # 티커 부분 선택
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  tryCatch({
    # url 생성
    url = paste0(
      'https://fchart.stock.naver.com/sise.nhn?symbol='
      ,name,'&timeframe=day&count=500&requestType=0')
    
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
  write.csv(price, paste0('data/KOR_price/', name,
                          '_price.csv'))
  
  # 타임슬립 적용
  Sys.sleep(2)
}

############################################################
##6.2 재무제표 및 가치지표 크롤링
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6,side = c('left'), pad = '0')

ifelse(dir.exists('data/KOR_fs'), FALSE,
       dir.create('data/KOR_fs'))
ifelse(dir.exists('data/KOR_value'), FALSE,
       dir.create('data/KOR_value'))

for(i in 1 : nrow(KOR_ticker) ) {
  
  data_fs = c()
  data_value = c()
  name = KOR_ticker$'종목코드'[i]
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  tryCatch({
    
    Sys.setlocale('LC_ALL', 'English')
    
    # url 생성
    url = paste0(
      'http://comp.fnguide.com/SVO2/ASP/'
      ,'SVD_Finance.asp?pGB=1&gicode=A',
      name)
    
    # 이 후 과정은 위와 동일함
    
    # 데이터 다운로드 후 테이블 추출
    data = GET(url,
               user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                          AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36')) %>%
      read_html() %>%
      html_table()
    
    Sys.setlocale('LC_ALL', 'Korean')
    
    # 3개 재무제표를 하나로 합치기
    data_IS = data[[1]]
    data_BS = data[[3]]
    data_CF = data[[5]]
    
    data_IS = data_IS[, 1:(ncol(data_IS)-2)]
    data_fs = rbind(data_IS, data_BS, data_CF)
    
    # 데이터 클랜징
    data_fs[, 1] = gsub('계산에 참여한 계정 펼치기',
                        '', data_fs[, 1])
    data_fs = data_fs[!duplicated(data_fs[, 1]), ]
    
    rownames(data_fs) = NULL
    rownames(data_fs) = data_fs[, 1]
    data_fs[, 1] = NULL
    
    # 12월 재무제표만 선택
    data_fs =
      data_fs[, substr(colnames(data_fs), 6,7) == "12"]
    
    data_fs = sapply(data_fs, function(x) {
      str_replace_all(x, ',', '') %>%
        as.numeric()
    }) %>%
      data.frame(., row.names = rownames(data_fs))
    
    
    # 가치지표 분모부분
    value_type = c('지배주주순이익', 
                   '자본', 
                   '영업활동으로인한현금흐름', 
                   '매출액') 
    
    # 해당 재무데이터만 선택
    value_index = data_fs[match(value_type, rownames(data_fs)),
                          ncol(data_fs)]
    
    # Snapshot 페이지 불러오기
    url =
      paste0(
        'http://comp.fnguide.com/SVO2/ASP/SVD_Main.asp',
        '?pGB=1&gicode=A',name)
    data = GET(url,
               user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
    
    # 현재 주가 크롤링
    price = read_html(data) %>%
      html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>%
      html_text() %>%
      parse_number()
    
    # 보통주 발행주식수 크롤링
    share = read_html(data) %>%
      html_node(
        xpath =
          '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
      html_text() %>%
      strsplit('/') %>%
      unlist() %>%
      .[1] %>%
      parse_number()
    
    # 가치지표 계산
    data_value = price / (value_index * 100000000/ share)
    names(data_value) = c('PER', 'PBR', 'PCR', 'PSR')
    data_value[data_value < 0] = NA
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    data_fs <<- NA
    data_value <<- NA
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 각각의 폴더 내 csv 파일로 저장
  
  # 재무제표 저장
  write.csv(data_fs, paste0('data/KOR_fs/', name, '_fs.csv'))
  
  # 가치지표 저장
  write.csv(data_value, paste0('data/KOR_value/', name,
                               '_value.csv'))
  
  # 2초간 타임슬립 적용
  Sys.sleep(2)
}

##############################################################
##6.3 DART의 Open API를 이용한 데이터 수집하기
library(stringr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
corp_list =  read.csv('data/corp_list.csv', row.names = 1)

KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

corp_list$'code' =
  str_pad(corp_list$'code', 8, side = c('left'), pad = '0')

corp_list$'stock' =
  str_pad(corp_list$'stock', 6, side = c('left'), pad = '0')

ticker_list = KOR_ticker %>% left_join(corp_list, by = c('종목코드' = 'stock')) %>%
  select('종목코드', '종목명', 'code')

ifelse(dir.exists('data/dart_fs'), FALSE, dir.create('data/dart_fs'))

bsns_year = 2019
reprt_code = '11011'


for(i in 1 : nrow(ticker_list) ) {
  
  data_fs = c()
  name = ticker_list$code[i]
  
  # 오류 발생 시 이를 무시하고 다음 루프로 진행
  
  tryCatch({
    
    # url 생성
    url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=',
                 dart_api, 
                 '&corp_code=', name,
                 '&bsns_year=', bsns_year,
                 '&reprt_code=', reprt_code,'&fs_div=CFS'
    )
    
    # JSON 다운로드
    fs_data_all = fromJSON(url) 
    fs_data_all = fs_data_all[['list']]
    
    # 만일 연결재무제표 없어서 NULL 반환시
    # reprt_code를 OFS 즉 재무제표 다운로드
    if (is.null(fs_data_all)) {
      
      url = paste0('https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=',
                   dart_api, 
                   '&corp_code=', name,
                   '&bsns_year=', bsns_year,
                   '&reprt_code=', reprt_code,'&fs_div=OFS'
      )
      
      fs_data_all = fromJSON(url) 
      fs_data_all = fs_data_all[['list']]
      
    }
    
    
    # 데이터 선택 후 열이름을 연도로 변경
    yr_count = str_detect(colnames(fs_data_all), 'trm_amount') %>% sum()
    yr_name = seq(bsns_year, (bsns_year - yr_count + 1))
    
    fs_data_all = fs_data_all[, c('corp_code', 'sj_nm', 'account_nm', 'account_detail')] %>%
      cbind(fs_data_all[, str_which(colnames(fs_data_all), 'trm_amount')])
    
    colnames(fs_data_all)[str_which(colnames(fs_data_all), 'amount')] = yr_name
    
  }, error = function(e) {
    
    # 오류 발생시 해당 종목명을 출력하고 다음 루프로 이동
    data_fs <<- NA
    warning(paste0("Error in Ticker: ", name))
  })
  
  # 다운로드 받은 파일을 생성한 각각의 폴더 내 csv 파일로 저장
  
  # 재무제표 저장
  write.csv(fs_data_all, paste0('data/dart_fs/', ticker_list$종목코드[i], '_fs_dart.csv'))
  
  # 2초간 타임슬립 적용
  Sys.sleep(2)
}
