
###R을 이용한 퀀트 투자 포트폴리오 만들기
###통계학과 201811526 이은주

###Chapter 6 금융 데이터 수집하기 (심화)
library(httr)
library(rvest)
library(stringr)
library(lubridate)
library(readr)
library(timetk)
library(xts)

#############################################################
##6.1 수정주가 크롤링
KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
print(KOR_ticker$'종목코드'[1])

#종목코드가 6자리이므로 6자리가 되지 않는 문자 왼쪽에 0을 추가
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

ifelse(dir.exists('data/KOR_price'), FALSE,
       dir.create('data/KOR_price'))

i=1
price = xts(NA, order.by = Sys.Date()) # 빈 시계열 데이터 생성(인덱스; 현재 날짜=Sys.Date)
name = KOR_ticker$'종목코드'[1] 

#url 생성 및 데이터 다운로드
url = paste0( 
      'https://fchart.stock.naver.com/sise.nhn?symbol='
      ,name,'&timeframe=day&count=500&requestType=0') 
    
data = GET(url) #데이터 다운로드
data_html = read_html(data, encoding = 'EUC-KR') %>%
  html_nodes("item") %>% #item 태그
  html_attr("data") #data 속성
    
price = read_delim(data_html, delim = '|') #데이터 나누기
    
price = price[c(1, 5)] # 필요한 열, 날짜와 종가만 선택 후 클렌징
price = data.frame(price)
colnames(price) = c('Date', 'Price')
price[, 1] = ymd(price[, 1]) #ymd함수():yyyymmdd-> yyyy-mm-dd
price = tk_xts(price, date_var = Date) ###오류
print(tail(price))

write.csv(price, paste0('data/KOR_price/', name,
                        '_price.csv'))

############################################################
##6.2 재무제표 및 가치지표 크롤링
##재무제표
ifelse(dir.exists('data/KOR_fs'), FALSE,
       dir.create('data/KOR_fs'))

Sys.setlocale("LC_ALL", "English") #로케일 언어 영어로

url = paste0('http://comp.fnguide.com/SVO2/ASP/SVD_Finance.asp?pGB=1&gicode=A005930')

#user agent; 웹브라우저 구별(정체가 불분명한 웹브라우저를 통한 접속 막혀 있어)
#                             크롬을 통해 접속한 것처럼 데이터 요청)
data = GET(url,
           user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))
data = data %>%
  read_html() %>%
  html_table() #테이블 내용만

Sys.setlocale("LC_ALL", "Korean") #로케일 언어 한국어로

lapply(data, function(x) {
  head(x, 3)})

data_IS = data[[1]] #포괄손익계산서 연간
data_BS = data[[3]] #재무상태표 연간
data_CF = data[[5]] #현금흐름표 연간

print(names(data_IS)) 
data_IS = data_IS[, 1:(ncol(data_IS)-2)] #전년동기, 전년동기(%) 삭제

data_fs = rbind(data_IS, data_BS, data_CF)
data_fs[, 1] = gsub('계산에 참여한 계정 펼치기', #해당 글자 삭제
                    '', data_fs[, 1])
data_fs = data_fs[!duplicated(data_fs[, 1]), ] #중복되지 않는 계정명만 선택

rownames(data_fs) = NULL #행이름 초기화
rownames(data_fs) = data_fs[, 1] 
data_fs[, 1] = NULL #첫번째 열 삭제

#12월 결산 데이터만 선택
data_fs = data_fs[, substr(colnames(data_fs), 6,7) == '12'] #끝글자가 12인 열만 선택
print(head(data_fs))

sapply(data_fs, typeof) #문자형데이터->숫자형으로 변경

library(stringr)
data_fs = sapply(data_fs, function(x) {
  str_replace_all(x, ',', '') %>%
    as.numeric()
}) %>%
  data.frame(., row.names = rownames(data_fs))
print(head(data_fs))

sapply(data_fs, typeof)
write.csv(data_fs, 'data/KOR_fs/005930_fs.csv')

##가치지표
#PER=주가/순이익, PBR=주가/순자산, 
#PCR=주가/영업활동현금흐름, PSR=주가/매출액

#재무제표 항목에서 분모 부분에 해당하는 데이터만 선택
ifelse(dir.exists('data/KOR_value'), FALSE,
       dir.create('data/KOR_value'))

value_type = c('지배주주순이익',
               '자본',
               '영업활동으로인한현금흐름',
               '매출액')

value_index = data_fs[match(value_type, rownames(data_fs)),
                      ncol(data_fs)] #맨 오름쪽 최근년도 데이터 선택
print(value_index)

#분자 부분에 해당하는 주가 수집 -> Xpath 이용
library(readr)

url = 'http://comp.fnguide.com/SVO2/ASP/SVD_main.asp?pGB=1&gicode=A005930'
data = GET(url,
           user_agent('Mozilla/5.0 (Windows NT 10.0; Win64; x64)
                      AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'))

price = read_html(data) %>%
  html_node(xpath = '//*[@id="svdMainChartTxt11"]') %>% #해당 지점의 데이터
  html_text() %>% #텍스트만 추출
  parse_number() #readr패키지 함수로 콤마와 같은 불필요한 문자 제거후 숫자형 데이터로 변경

print(price)

#PER=주가/주당순이익 -> 주당순이익=순이익/전체 주식수
#전체 주식수 수집 -> Xpath 이용
share = read_html(data) %>%
  html_node(
    xpath =
      '//*[@id="svdMainGrid1"]/table/tbody/tr[7]/td[1]') %>%
  html_text()

print(share) #보통주/우선주

share = share %>% #보통주 발생 주식수 얻기
  strsplit('/') %>%
  unlist() %>% #리스트를 벡터화
  .[1] %>%
  parse_number() #문자형 데이터를 숫자형으로

print(share)

#재무데이터, 현재주가, 발행주식수 이용해 가치지표 계산
data_value = price / (value_index * 100000000 / share)
names(data_value) = c('PER', 'PBR', 'PCR', 'PSR')
data_value[data_value < 0] = NA

print(data_value)
write.csv(data_value, 'data/KOR_value/005930_value.csv')

############################################################
##6.3 DART의 OpenAPI를 이용한 데이터 수집하기
#1 API Key 발급 및 추가
file.edit("~/.Renviron")
#dart_api_key = '8da38d480193b450f5f09bc6c857f748eb852048'
dart_api = Sys.getenv("dart_api_key") #재시작하여 key 불러오기

#2 고유번호 다운로드
library(httr)
library(rvest)

codezip_url = paste0(
  'https://opendart.fss.or.kr/api/corpCode.xml?crtfc_key=',dart_api)

codezip_data = GET(codezip_url)
print(codezip_data) #바이너리 형태의 데이터 첨부

codezip_data$headers[["content-disposition"]] #zip파일 압축 해제 필요
tf = tempfile(fileext = '.zip') #빈 zip 파일 만들기
writeBin( #바이너리 형태의 파일 저장
  content(codezip_data, as = "raw"), #raw형태로 저장
  file.path(tf) #파일명은 tf
)

nm = unzip(tf, list = TRUE) #zip내 파일리스트 확인
print(nm)
code_data = read_xml(unzip(tf, nm$Name)) #zip내 CORPCODE.xml 불러오기
print(code_data)

corp_code = code_data %>% html_nodes('corp_code') %>% html_text() #고유번호
corp_name = code_data %>% html_nodes('corp_name') %>% html_text() #종목명
corp_stock = code_data %>% html_nodes('stock_code') %>% html_text() #거래소 상장 티커

corp_list = data.frame(
  'code' = corp_code,
  'name' = corp_name,
  'stock' = corp_stock,
  stringsAsFactors = FALSE
)
nrow(corp_list)
head(corp_list)

corp_list = corp_list[corp_list$stock != " ", ] #stock열이 빈 종목(거래소에 상장되지 않은 종목) 제거
write.csv(corp_list, 'data/corp_list.csv')

#3 공시 검색
library(lubridate)
library(stringr)
library(jsonlite)

bgn_date = (Sys.Date() - days(7)) %>% str_remove_all('-') #일주일전
end_date = (Sys.Date() ) %>% str_remove_all('-') #오늘 날짜
notice_url = paste0('https://opendart.fss.or.kr/api/list.json?crtfc_key=',dart_api,'&bgn_de=',
                    bgn_date,'&end_de=',end_date,'&page_no=1&page_count=100') #최근공시100건

notice_data = fromJSON(notice_url) #JSON형식
notice_data = notice_data[['list']]

head(notice_data)

#고유번호를 통해 특정 기업의 공시 검색
bgn_date = (Sys.Date() - days(30)) %>% str_remove_all('-')
end_date = (Sys.Date() ) %>% str_remove_all('-')
corp_code = '00126380'

notice_url_ss = paste0(
  'https://opendart.fss.or.kr/api/list.json?crtfc_key=',dart_api,
  '&corp_code=', corp_code, 
  '&bgn_de=', bgn_date,'&end_de=',
  end_date,'&page_no=1&page_count=100')

notice_data_ss = fromJSON(notice_url_ss) 
notice_data_ss = notice_data_ss[['list']]

head(notice_data_ss)

notice_url_exam = notice_data_ss[1, 'rcept_no'] #공시번호로 공시에 해당하는 url 접속
notice_dart_url = paste0(
  'http://dart.fss.or.kr/dsaf001/main.do?rcpNo=',notice_url_exam)

print(notice_dart_url)

#4 사업보고서 주요 정보
corp_code = '00126380' #고유번호
bsns_year = '2019' #사업년도
reprt_code = '11011' #보고서코드

url_div = paste0('https://opendart.fss.or.kr/api/alotMatter.json?crtfc_key=',
                 dart_api, 
                 '&corp_code=', corp_code,
                 '&bsns_year=', bsns_year,
                 '&reprt_code=', reprt_code
)

div_data_ss = fromJSON(url_div) 
div_data_ss = div_data_ss[['list']]
head(div_data_ss)

#5 상장기업 재무정보
corp_code = '00126380'
bsns_year = '2019'
reprt_code = '11011'

url_single = paste0(
  'https://opendart.fss.or.kr/api/fnlttSinglAcnt.json?crtfc_key=',
  dart_api, 
  '&corp_code=', corp_code,
  '&bsns_year=', bsns_year,
  '&reprt_code=', reprt_code
)

fs_data_single = fromJSON(url_single) 
fs_data_single = fs_data_single[['list']]

head(fs_data_single)

#다중회사 주요계정 한번에 다운로드
corp_code = c('00126380,00413046,00190321')
bsns_year = '2019'
reprt_code = '11011'

url_multiple = paste0(
  'https://opendart.fss.or.kr/api/fnlttMultiAcnt.json?crtfc_key=',
  dart_api, 
  '&corp_code=', corp_code,
  '&bsns_year=', bsns_year,
  '&reprt_code=', reprt_code
)

fs_data_multiple = fromJSON(url_multiple) 
fs_data_multiple = fs_data_multiple[['list']]

fs_data_list = fs_data_multiple %>% split(f = .$corp_code) #고유번호 단위로 각각의 리스트에 데이터 저장
lapply(fs_data_list, head, 2)

#6 단일회사 전체 재무제표
corp_code = '00126380'
bsns_year = 2019
reprt_code = '11011'

url_fs_all = paste0(
  'https://opendart.fss.or.kr/api/fnlttSinglAcntAll.json?crtfc_key=',
  dart_api, 
  '&corp_code=', corp_code,
  '&bsns_year=', bsns_year,
  '&reprt_code=', reprt_code,'&fs_div=CFS' #fs_div;연결재무제표와 일반재무제표 구분
                                           #CFS; 연결재무제표 의미
)

fs_data_all = fromJSON(url_fs_all) 
fs_data_all = fs_data_all[['list']]

head(fs_data_all)

#최근 3년 재무제표만을 선택
#thstrm_nm와 thstrm_amount는 당기(금년)
#frmtrm_nm과 frmtrm_amount는 전기
#bfefrmtrm_nm과 bfefrmtrm_amount는 전전기
yr_count = str_detect(colnames(fs_data_all), 'trm_amount') %>% sum() 
          #열이름에 trm_amount 들어간 갯수 확인
          #3이 아닌 경우 3개년 데이터 없는 경우이다.
yr_name = seq(bsns_year, (bsns_year - yr_count + 1))

fs_data_all = fs_data_all[, c('corp_code', 'sj_nm', 'account_nm', 'account_detail')] %>%
                              #고유번호   #재무제표명  #계정명     #계정상세
  cbind(fs_data_all[, str_which(colnames(fs_data_all), 'trm_amount')])

colnames(fs_data_all)[str_which(colnames(fs_data_all), 'amount')] = yr_name #열이름을 각 연도로 변경
head(fs_data_all)
