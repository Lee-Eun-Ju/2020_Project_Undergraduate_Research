
###R을 이용한 퀀트 투자 포트폴리오 만들기
###통계학과 201811526 이은주

#############################################################
###Chapter 5 금융 데이터 수집하기 (기본)
###5.1 한국거래소의 산업별 현황 및 개별지표 크롤링
##5.1.1산업별 현황 크롤링
library(httr)
library(rvest)
library(readr)

#1) 원하는 항목을 쿼리로 발송하여 해당 OTP 받음
gen_otp_url = "http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx"
gen_otp_data = list(
  name = " fileDown",
  filetype = "csv", #xls이지만 csv로 변경
  url = "MKD/03/0303/03030103/mkd03030103",
  tp_cd = "ALL",
  date = "20200928",
  lang = "ko",
  pagePath = "/contents/MKD/03/0303/03030103/MKD03030103.jsp")
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

#2) 부여받은 OTP를 http://file.krx.co.kr/download.jspx(Network-download.jspx-General;Request URL)
#   에 제출하여 해당 데이터 다운
down_url = 'http://file.krx.co.kr/download.jspx'
down_sector = POST(down_url, query = list(code = otp),
                   add_headers(referer = gen_otp_url)) %>%
                  #첫번째URL에 OTP를 부여받고 두번째URL에 제출하는 과정 지정 필요
  read_html() %>%
  html_text() %>%
  read_csv()

print(down_sector)

ifelse(dir.exists('data'), FALSE, dir.create('data'))
write.csv(down_sector, 'data/krx_sector.csv')

##5.1.2개별종목 지표 크롤링
library(httr)
library(rvest)
library(readr)

gen_otp_url =
  'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
gen_otp_data = list(
  name = 'fileDown',
  filetype = 'csv',
  url = "MKD/13/1302/13020401/mkd13020401",
  market_gubun = 'ALL',
  gubun = '1',
  schdate = '20190607',
  pagePath = "/contents/MKD/13/1302/13020401/MKD13020401.jsp")

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

down_url = 'http://file.krx.co.kr/download.jspx'
down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
  read_csv()
print(down_ind)

##5.1.3최근 영업일 기준 데이터 받기
library(httr)
library(rvest)
library(stringr)
library(readr)

#최근 영업일 구하기
url = 'https://finance.naver.com/sise/sise_deposit.nhn'

biz_day = GET(url) %>%
  read_html(encoding = 'EUC-KR') %>%
  html_nodes(xpath =
               '//*[@id="type_0"]/div/ul[2]/li/span') %>% #해당 지점의 데이터 추출
  html_text() %>% #텍스트 데이터만 추출
  str_match(('[0-9]+.[0-9]+.[0-9]+') ) %>% #정규표현식
  str_replace_all('\\.', '') #마침표를 모두 없애줌

print(biz_day)

#산업별 현황 OTP 발급
gen_otp_url =
  'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
gen_otp_data = list(
  name = 'fileDown',
  filetype = 'csv',
  url = 'MKD/03/0303/03030103/mkd03030103',
  tp_cd = 'ALL',
  date = biz_day, # 최근영업일로 변경
  lang = 'ko',
  pagePath = '/contents/MKD/03/0303/03030103/MKD03030103.jsp')
otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 산업별 현황 데이터 다운로드
down_url = 'http://file.krx.co.kr/download.jspx'
down_sector = POST(down_url, query = list(code = otp),
                   add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
  read_csv()

ifelse(dir.exists('data'), FALSE, dir.create('data'))
write.csv(down_sector, 'data/krx_sector.csv')

# 개별종목 지표 OTP 발급
gen_otp_url =
  'http://marketdata.krx.co.kr/contents/COM/GenerateOTP.jspx'
gen_otp_data = list(
  name = 'fileDown',
  filetype = 'csv',
  url = "MKD/13/1302/13020401/mkd13020401",
  market_gubun = 'ALL',
  gubun = '1',
  schdate = biz_day, # 최근영업일로 변경
  pagePath = "/contents/MKD/13/1302/13020401/MKD13020401.jsp")

otp = POST(gen_otp_url, query = gen_otp_data) %>%
  read_html() %>%
  html_text()

# 개별종목 지표 데이터 다운로드
down_url = 'http://file.krx.co.kr/download.jspx'
down_ind = POST(down_url, query = list(code = otp),
                add_headers(referer = gen_otp_url)) %>%
  read_html() %>%
  html_text() %>%
  read_csv()

write.csv(down_ind, 'data/krx_ind.csv')

##5.1.4 거래소 데이터 정리하기
#중복된 열, 불필요한 데이터 등 하나의 테이블로 합친 후 정리
down_sector = read.csv('data/krx_sector.csv', row.names = 1, #첫번째 열을 행이름으로 지정
                       stringsAsFactors = FALSE) #문자열 데이터가 팩터 형태로 변형되지 않게 함
down_ind = read.csv('data/krx_ind.csv',  row.names = 1,
                    stringsAsFactors = FALSE)

intersect(names(down_sector), names(down_ind)) #중복되는 열 확인
setdiff(down_sector[, '종목명'], down_ind[ ,'종목명']) #하나의 데이터에만 있는 종목

KOR_ticker = merge(down_sector, down_ind,
                   by = intersect(names(down_sector),
                                  names(down_ind)), 
                   #공통으로 존재하는 종목코드,종목명 기준으로 합치기
                   all = FALSE #FALSE;교집합, TRUE;합집합
                    )
KOR_ticker = KOR_ticker[order(-KOR_ticker['시가총액.원.']), ] #시가총액 기준 내림차순(-)
print(head(KOR_ticker))

#종목명에 스팩이 들어가는 종목, 종목코드 끝이 0이 아닌 우선주 종목 제외
library(stringr)
KOR_ticker[grepl('스팩', KOR_ticker[, '종목명']), '종목명']  
KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) != 0, '종목명']

KOR_ticker = KOR_ticker[!grepl('스팩', KOR_ticker[, '종목명']), ]  
KOR_ticker = KOR_ticker[str_sub(KOR_ticker[, '종목코드'], -1, -1) == 0, ]

rownames(KOR_ticker) = NULL
write.csv(KOR_ticker, 'data/KOR_ticker.csv')

###5.2 WICS 기준 섹터정보 크롤링
library(jsonlite)

url = 'http://www.wiseindex.com/Index/GetIndexComponets?ceil_yn=0&dt=20190607&sec_cd=G10'
data = fromJSON(url) #json형식의 데이터 크롤링
lapply(data, head)

sector_code = c('G25', 'G35', 'G50', 'G40', 'G10',
                'G20', 'G55', 'G30', 'G15', 'G45')
data_sector = list()

for (i in sector_code) {
  
  url = paste0(
    'http://www.wiseindex.com/Index/GetIndexComponets',
    '?ceil_yn=0&dt=',biz_day,'&sec_cd=',i)
  data = fromJSON(url)
  data = data$list
  
  data_sector[[i]] = data
  
  Sys.sleep(1)
}

data_sector = do.call(rbind, data_sector)
write.csv(data_sector, 'data/KOR_sector.csv')


