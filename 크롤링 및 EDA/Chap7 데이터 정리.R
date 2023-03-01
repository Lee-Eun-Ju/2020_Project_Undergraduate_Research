
###R을 이용한 퀀트 투자 포트폴리오 만들기
###통계학과 201811526 이은주

###Chapter 7 데이터 정리하기
###########################################################
##7.1 주가 정리하기
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
    read.csv(paste0('data/KOR_price/', name,
                    '_price.csv'),row.names = 1) %>%
    as.xts() #시계열 형태로 변경
  
}

price_list = do.call(cbind, price_list) %>% #리스트를 열 형태로 묶기
             na.locf() #결측치에는 전일 데이터 사용
colnames(price_list) = KOR_ticker$'종목코드'

head(price_list[, 1:5])
tail(price_list[, 1:5])
write.csv(data.frame(price_list), 'data/KOR_price.csv')

###########################################################
##7.2 재무제표 정리하기
library(stringr)
library(magrittr)
library(dplyr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

data_fs = list()

for (i in 1 : nrow(KOR_ticker)){
  
  name = KOR_ticker[i, '종목코드']
  data_fs[[i]] = read.csv(paste0('data/KOR_fs/', name,
                                 '_fs.csv'), row.names = 1)
}

fs_item = data_fs[[1]] %>% rownames()
length(fs_item)
print(head(fs_item))

# 매출액 기준으로 데이터 정리
select_fs = lapply(data_fs, function(x) {
  
  if ( '매출액' %in% rownames(x) ) { # 해당 항목이 있을시 데이터를 선택
    x[which(rownames(x) == '매출액'), ]
    
  } else { # 해당 항목이 존재하지 않을 시, NA로 된 데이터프레임 생성
    data.frame(NA)
  }
})

select_fs = bind_rows(select_fs) #데이터를 행으로 묶어줌
                                 #열개수 다를 경우 나머지부분을 NA로 처리
print(head(select_fs))

#NA; 매출액 항목이 없는 종목이거나 연도가 순서대로 저장되지 않은 경우 
#데이터 클렌징
select_fs = select_fs[!colnames(select_fs) %in%
                        c('.', 'NA.')] #.혹은 NA가 들어가지 않은 열만 선택
select_fs = select_fs[, order(names(select_fs))] #연도별 순서로 정리
rownames(select_fs) = KOR_ticker[, '종목코드']
print(head(select_fs))

######
#모든 재무 항목에 대한 데이터를 정리
fs_list = list()

for (i in 1 : length(fs_item)) {
  select_fs = lapply(data_fs, function(x) {
    # 해당 항목이 있을시 데이터를 선택
    if ( fs_item[i] %in% rownames(x) ) {
      x[which(rownames(x) == fs_item[i]), ]
      
      # 해당 항목이 존재하지 않을 시, NA로 된 데이터프레임 생성
    } else {
      data.frame(NA)
    }
  })
  
  # 리스트 데이터를 행으로 묶어줌 
  select_fs = bind_rows(select_fs)
  
  # 열이름이 '.' 혹은 'NA.'인 지점은 삭제 (NA 데이터)
  select_fs = select_fs[!colnames(select_fs) %in%
                          c('.', 'NA.')]
  
  # 연도 순별로 정리
  select_fs = select_fs[, order(names(select_fs))]
  
  # 행이름을 티커로 변경
  rownames(select_fs) = KOR_ticker[, '종목코드']
  
  # 리스트에 최종 저장
  fs_list[[i]] = select_fs
  
}

# 리스트 이름을 재무 항목으로 변경
names(fs_list) = fs_item

# KOR_fs.Rds 파일로 저장
saveRDS(fs_list, 'data/KOR_fs.Rds') 

###########################################################
##7.3 가치지표 정리하기
library(stringr)
library(magrittr)
library(dplyr)

KOR_ticker = read.csv('data/KOR_ticker.csv', row.names = 1)
KOR_ticker$'종목코드' =
  str_pad(KOR_ticker$'종목코드', 6, side = c('left'), pad = '0')

data_value = list()

for (i in 1 : nrow(KOR_ticker)){
  
  name = KOR_ticker[i, '종목코드']
  data_value[[i]] =
    read.csv(paste0('data/KOR_value/', name,
                    '_value.csv'), row.names = 1) %>%
    t() %>% data.frame() #t(); 열의 형태로 변경
  
}

data_value = bind_rows(data_value)
print(head(data_value))

data_value = data_value[colnames(data_value) %in%
                          c('PER', 'PBR', 'PCR', 'PSR')]

data_value = data_value %>%
  mutate_all(list(~na_if(., Inf))) #Inf 데이터를 NA로 계산

rownames(data_value) = KOR_ticker[, '종목코드']
print(head(data_value))
write.csv(data_value, 'data/KOR_value.csv')
