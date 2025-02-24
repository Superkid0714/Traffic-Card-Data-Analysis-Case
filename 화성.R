# 라이브러리
library(sp)
library(geojsonio)

# 작업 폴더 설정
dir.create("01_save")

# 행정동 geojson 파일 불러오기
admin <- geojsonio::geojson_read("SBJ_1910_001/tl_scco_emd.geojson",what="sp")
save(admin, file="01_001_admin.rdata") # 저장

# 플로팅
plot(admin)

# 라이브러리 불러오기
library(sp)        # 공간 데이터 처리 (Spatial Objects)
library(raster)   # 래스터 및 벡터 데이터 조작
library(leaflet)   # 웹 기반 지도 시각화


# 외곽 경계 만들기: xmin, xmax, ymin, ymax
# 이 코드를 통해서 특정 지역을 나타낸다
ext <- raster::extent(126.50625, 127.42245, 36.99653, 37.483419)

# 좌표를 폴리곤(벡터 데이터)으로 변환
# extent 객체를 SpatialPolygons 객체로 변환하여 사각형(경계박스 형성)
fishnet <- as(ext, "SpatialPolygons")

# 좌표계 설정 
# crs()는 좌표계를 설정하는 함수 -> 공간 데이터셋과 정렬할 수 있도록 보장
crs(fishnet) <- CRS("+proj=longlat +datum=WGS84 +no_defs")

# 플로팅
plot(fishnet, border="red") # 위에서 만든 사각형을 붉은색 테두리 표시
plot(admin, add=T)  # admin 객체(= 화성시 행정동 경계 데이터)

## fishnet(집계구) 만들기

# 래스터 변환
fishnet <- raster(fishnet)

# 0.1도 단위로 분할
res(fishnet) <- .01

# 좌표계 투영
crs(fishnet) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# 폴리곤으로 변환
fishnet <- rasterToPolygons(fishnet)

# 지도 시각화
leaflet() %>% addTiles() %>% 
  addPolygons(data=fishnet, weight=0.4, fillOpacity = 0) %>% 
  addPolygons(data=admin, color="red")


## 셀별 일렬번호 부여하기

# 현재 번호 없음
fishnet$id

# 일련번호 부여
fishnet@data <- data.frame(id=1:nrow(fishnet))

# 저장
save(fishnet, file="01_002_fishnet.rdata")

# 일련번호 확인
head(fishnet$id,10)


## 정류장 정보 불러오기 

# csv 파일 불러오기
sta_table <- read.csv("SBJ_1910_001/stations_table.csv",fileEncoding = "UTF-8")

# 필요한 칼럼만 추출
keeps <- c("표준정류장ID","이비카드정류장ID","WGS84위도","WGS84경도","시군명","정류소명")
sta_table <- sta_table[keeps]

# NA 제거
sta_table <-na.omit(sta_table)

# 저장
save(sta_table,file="01_003_sta_table.rdata")

# 불필요 변수 지우기
rm("keeps")
head(sta_table,2)

## 버스노선(route)별 정류장 정보 불러오기

# csv 불러오기
route_sta <- read.csv("SBJ_1910_001/routestationinfo.csv",fileEncoding = "UTF-8")

# 필요 칼럼 추출
keeps <- c("bus_line_no","bus_line_no_seq","station_id","station_nm")
route_sta <- route_sta[keeps]
head(route_sta,2)

## 매핑 테이블 생성 : 정류장 - 버스노선

# 정류장 id(station_id) + 좌푯값 (위도, 경도) 결합
route_sta <- merge(route_sta,sta_table, by.x="station_id",by.y="표준정류장ID")

# 결측치 확인
sum(is.na(route_sta$정류소명))

# 저장
save(route_sta, file="01_004_route_sta.rdata"); rm("keeps")
head(route_sta,2)

## 변수명 한글로 변경

# 개별 이동 데이터(trip_chain) 불러오기
trip_chain <- read.csv("SBJ_1910_001/TripChain.csv",fileEncoding = "UTF-8")

# 변수명 한글로 변경
colnames(trip_chain) <-  
  c("암호화카드번호", "트랜잭션ID", "환승횟수", "교통 카드발행사ID", "총이용객수", 
    "사용자구분", "교통수단CD1", "교통수단CD2", "교통수단CD3", "교통수단CD4", 
    "교통수단CD5", "버스노선ID1", "버스노선ID2", "버스노선ID3", "버스노선ID4", 
    "버스노선ID5", "차량ID1", "차량ID2", "차량ID3", "차량ID4", "차량ID5", 
    "총통행거리", "총탑승시간", "총소요시간", "승차일시1", "승차일시2", 
    "승차일시3",  "승차일시4", "승차일시5", "하차일시1", "하차일시2",  
    "하차일시3", "하차일시4", "하차일시5", "최초승차일시", "최종하차일시",
    "승차역ID1", "승차역ID2", "승차역ID3",  "승차역ID4", "승차역ID5", 
    "하차역ID1", "하차역ID2",  "하차역ID3", "하차역ID4", "하차역ID5", 
    "최초승차역ID", "최종하차역ID", "총이용금액", "수집건수", "트립체인완료코드")
colnames(trip_chain)


## 이동 시작 일시

# 옵션 변경 (지수=>숫자) : R의 숫자 출력 방식을 조정하는 코드, 숫자를 일반표기법으로 강제
options("scipen"=100)

# 이동 시작 날짜(start_day) => 1일~4일 중 하나

trip_chain$start_day <- as.factor(as.numeric(substr(trip_chain[,35],7,8)))
# trip_chian 데이터의 35번째 열에서 저장된 데이터를 선택
# substr(문자열,start,stop)을 사용하여 날짜부분 7~8번째 문자 추출
# as.numeric을 통해서 문자열을 숫자로 변환하여 저장

trip_chain$start_day <- as.numeric(trip_chain$start_day)
# as.factor()->as.numeric()을 다시하는 이유는 문자열 "01"이 1이 되도록 보장하기 위해서

# 이동 시작 시간(start_time) => 새벽4시 ~ 밤23시 중 하나
trip_chain$start_hour <- as.factor(as.numeric(substr(trip_chain[,35],9,10)))
trip_chain$start_hour <- as.numeric(trip_chain$start_hour)
head(trip_chain[,c(1,52,53)],2)

## 이동 종료 일시

# 이동 종료 날짜(end_day) => 1일~4일 중 하나
trip_chain$end_day <- as.factor(as.numeric(substr(trip_chain[,35],7,8)))
trip_chain$end_day <- as.numeric(trip_chain$end_day)

# 이동 종료 시간(start_time) => 새벽4시~밤23시 중 하나
trip_chain$end_hour <- as.factor(as.numeric(substr(trip_chain[,35],9,10)))
trip_chain$end_hour <- as.numeric(trip_chain$end_hour)

# 저장
save(trip_chain, file="02_001_trip_chain_full.rdata")
head(trip_chain[,c(1,52,53,54,55)],2)

## 출발-도착 정류장 정보에 좌푯값 결합
library(dplyr)
load("01_003_sta_table.rdata")

# 버스 정류장 정보(sta_table) 칼럼 이름 변경
colnames(sta_table) <- c("표준정류장ID","이비카드정류장ID","S_WGS84위도","S_WGS84경도",
                         "S_시군명","S_정류소명")
# 출발점 기준 => 이비카드정류장ID + 승차역 ID 결합
trip_chain <- merge(trip_chain,sta_table,by.x="승차역ID1","이비카드정류장ID")

# 결측치 확인
sum(is.na(trip_chain$S_정류소명))

# 버스 정류장 정보 (sta_table) 칼럼 이름 변경
colnames(sta_table) <- c("표준정류장ID","이비카드정류장ID","E_WGS84위도","E_WGS84경도",
                         "E_시군명","E_정류소명")
# 도착점 기준 => 이비카드정류장ID + 하차역 ID 결합
trip_chain <- merge(trip_chain,sta_table,by.x="최종하차역ID",by.y="이비카드정류장ID")

# 저장
save(trip_chain, file="02_002_trip_chian_full.rdata")

# 결측치 확인 
sum(is.na(trip_chain$E_정류소명))

## 승하차가 많은 지역은 어디인가?

# 피벗테이블 만들기
as.matrix(table(trip_chain$S_시군명,trip_chain$E_시군명))

# 어느 지역에서 가장 많이 승차하였는가?
sort(colSums(as.matrix(table(trip_chain$S_시군명,trip_chain$E_시군명))),decreasing = TRUE)

# 어느 지역에서 가장 많이 하차하였는가?
sort(rowSums(as.matrix(table(trip_chain$S_시군명, trip_chain$E_시군명))),decreasing = TRUE)


## (1) 데이터 필터링
# 출발 또는 도착지가 화성시인 경우만 필터링
trip_chain <- trip_chain[(trip_chain$S_시군명=="화성시"| trip_chain$E_시군명 =="화성시"),]
# 저장
save(trip_chain, file="02_003_trip_chain.rdata")
nrow(trip_chain)

## OD 행렬 생성
# OD 행렬 만드릭
OD <- as.matrix(table(trip_chain$S_시군명, trip_chain$E_시군명))
# OD 행렬을 데이터프레임으로 변환
OD <- as.data.frame(OD)
# 통행 발생 순서대로 정렬
OD <- OD[order(OD$Freq,decreasing = TRUE),]
# 일련번호 재정렬
rownames(OD) <- 1 : length(rownames(OD))

head(OD)


## 지역별 OD 발생량 누적 비율 보기

# 누적 비율 보기
OD <- OD %>% mutate(cum=round(((cumsum((Freq))/sum(Freq))*100),1))
# 칼럼명 변경
colnames(OD) <- c("From","To","Freq","Cum")
# OD 매트릭스 저장
save(OD,file="02_003_OD.rdata")

head(OD)

## 통행량 집중도 확인: 지니계수와 로렌츠 곡선
library(ineq)
# 지니계수
ineq(OD$Freq,type="Gini")
# 로렌츠 곡선 그리기
plot((OD$Freq),col="red",type='l',lwd=2)

## 대상 지역 교통량 데이터만 추출
# 출발 또는 도착 지역 지정
patterns <- c("수원시","화성시","용인시","오산시")
# 필요 변수 지정
colnames(sta_table) <- c("표준정류장ID","이비카드정류장ID","WGS84위도","WGS84경도",
                         "시군명","정류소명")
# 필요한 지역 - 변수만 필터링
sta_table <- filter(sta_table,grepl(paste(patterns,collapse = "|"),시군명))
# 일련번호 다시 부여하기
rownames(sta_table) <- seq(1:nrow(sta_table))
# 저장
save(sta_table,file="02_003_sta_table.rdata")
# 대상자 필터링 결과 확인
unique(sta_table$시군명)


## 이용자가 몇 번 버스를 타고 어디에서 어디로 이동했는지 

# 개별 이동 데이터 불러오기
load("02_003_trip_chain.rdata")
# 버스노선 정보 불러오기
route_map <- read.csv("SBJ_1910_001/routestationmapping.csv",fileEncoding = "UTF-8")
# 노선 ID와 버스 번호(노선명) 추출
route_map <- route_map[,5:6]
# 개별 이동 데이터와 버스 번호(노선명) 결합
route_sta <- merge(trip_chain,route_map,by.x="버스노선ID1",by.y="표준노선ID")
# 개별 이동 정보 확인
head(route_sta[1:2,c(4,66,60,65)],)


## 이용자가 많은 버스노선 추출

# 노선명을 문자형 변수로 속성 변환
route_sta$노선명 <- as.character(route_sta$노선명)
# 노선별 이용 건수를 피벗 테이블로 작성하기
bus_usr <- as.data.frame(table(route_sta$노선명))
# 노선별 이용 건수 sorting => 빈도수가 많은 것부터
bus_usr <- bus_usr[order(-bus_usr$Freq),]
# 칼럼명 변경
colnames(bus_usr) <- c("line","Freq")
# 일련번호 재정렬
rownames(bus_usr) <- 1:length(rownames(bus_usr))
# 비율 보기
bus_usr$pcnt = round(bus_usr$Freq/sum(bus_usr$Freq),3)*100

# 노선별 이용 건수 1~10위까지 불러오기 
head(bus_usr[1:10,],5)

## 노선별 누적 이용자 비율 분석
# 노선별 누적 비율 계산
bus_usr <- bus_usr %>% mutate(cum=round(((cumsum((Freq))/sum(Freq))*100),1))
# 저장
save(bus_usr, file="03_001_bus_usr.rdata")
# 상위 42개 노선이 전체 이동량의 87.3%를 차지하고 있음
head(bus_usr[39:42,],5)
# 누적 비율 차트
plot(bus_usr$cum, type="l", xlim=c(0,100))
abline(v=42,col="red",lwd=3,lty=2)

## 이동 발생 시간대 분석
hist <- hist(trip_chain$start_hour,plot=FALSE)
plot(hist,xaxt="n",xlab="시간",ylab="건수",
     main = "시간대별 trip 발생량", col ="blue")
axis(1,hist$mids,labels=c(5:23))

## 정류장 정보 추출
# 버스노선 번호 추출
bus_line <- as.character(bus_usr[1:42,1])
# 노선 - 정류장 매핑 테이블 불러오기 
load("01_004_route_sta.rdata")
# 이용량 많은 42개 버스노선의 정류장만 추출
bus_line <- filter(route_sta,grepl(paste(as.character(bus_line),collapse = "|"),
                                   route_sta$bus_line_no))
# 버스 노선 중 대상 지역에  위치하는 정류장 정보만 추출
patterns <- c("수원시","화성시","용인시","오산시") # 대상시 지정
bus_line <- filter(bus_line, grepl(paste(patterns,collapse = "|"),시군명)) # 필터링
# 중복되는 노선 지우기
bus_line <- bus_line[!duplicated(bus_line[c(2,4)]),]
bus_line <- bus_line[with(bus_line,order(bus_line_no,bus_line_no_seq)),]
# 순서대로 정렬하기
bus_line <- merge(bus_line, bus_usr[1:42,1:3],by.x="bus_line_no",by.y="line",
                  all.x=TRUE)
bus_line <- na.omit(bus_line)
save(bus_line, file="03_002_bus_line.rdata") # 저장

head(bus_line[,c(1,4,9)],2)


## 정류장 정보를 공간 포인트로 만들기

# 좌푯값 추출
library(tidyr)
library(sp)
coords <- bus_line %>% dplyr::select(WGS84경도,WGS84위도)
data <- bus_line[,1:11]

# 투영
crs <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
sta_pnt <- SpatialPointsDataFrame(coords=coords,data=data,proj4string = crs)

# 저장
save(sta_pnt, file = "03_003_sta_pnt.rdata")

# 시각화
library(tmap)
qtm("Hwaseong")
qtm("Hwaseong") + 
  # 이용자 1% 이상 노선
  qtm(subset(sta_pnt,sta_pnt@data$pcnt > 1), symbols.col="cadetblue2",symbols.size=.05) +
  # 이용자 2% 이상 노선
  qtm(subset(sta_pnt,sta_pnt@data$pcnt > 2), symbols.col="burlywood3",symbols.size=.1) +
  # 이용자 3% 이상 노선
  qtm(subset(sta_pnt,sta_pnt@data$pcnt > 3),symbols.col="orange",symbols.size=.3) +
  # 이용자 4% 이상 노선
  qtm(subset(sta_pnt,sta_pnt@data$pcnt > 4),symbols.col="red",sybols.size=.5)


## 정류장 데이터와 집계구의 공간 조인
# 파일 불러오기
load("02_003_trip_chain.rdata")
load("01_002_fishnet.rdata")
load("03_003_sta_pnt.rdata")
load("01_001_admin.rdata")

library(sp)
library(sf)

# sp 객체를 sf 객체로 변환
sta_pnt_sf <- st_as_sf(sta_pnt)
fishnet_sf <- st_as_sf(fishnet)

# 좌표계 확인 후 동일하게 설정
st_crs(sta_pnt_sf) <- st_crs(fishnet_sf)

# 공간 조인 실행
sta_pnt <- st_join(sta_pnt_sf, fishnet_sf)

# 올바른 객체 저장
save(sta_pnt, file="04_001_sta_pnt.rdata") 

# 저장한 객체 불러오기
load("04_001_sta_pnt.rdata")

# 데이터 확인
head(sta_pnt[, c(1, 4, 10, 12)], 2)

## 승하차 정류장 데이터에 집계구 아이디 추가
# trip_chain 번호 다시 매기기
rownames(trip_chain) <- seq(1:nrow(trip_chain))
trip_chain <- merge(trip_chain,sta_pnt[,c(5,12)],by.x="승차역ID1",by.y="이비카드정류장ID")
trip_chain <- merge(trip_chain,sta_pnt[,c(5,12)],by.x="최종하차역ID",by.y="이비카드정류장ID")

save(trip_chain,file="04_002_trip_chain.rdata")
tail(trip_chain[,c(3,60,65,66,67)],2)

## trip_chain에서 필요한 정보만 추출
keeps <- c("id.x","id.y","승차역ID1","최종하차역ID","총이용객수","환승횟수")
grid_chain <- trip_chain[keeps]
rm("trip_chain"); rm("keeps")
save(grid_chain, file="04_003_grid_chain.rdata")

head(grid_chain,2)


# 라이브러리 불러오기
library(dplyr) # install.packages("dplyr")    
library(sp)    # install.packages("sp")
library(sf)    # install.packages("sf")
# 출발지 기준 집계구 분석 
grid_in <- grid_chain %>%             
  group_by(id.x) %>%                  # 집계구에서 id.x별로 총 이용객 수, 
  summarize_if(is.numeric, sum) %>%   # 환승 횟수 데이터를 합하기 
  dplyr::select(id.x, 총이용객수, 환승횟수) 
# 평균 환승 횟수 계산 
grid_in$평균환승 <- round((grid_in$환승횟수 / grid_in$총이용객수),1)  
# 칼럼 이름 정리하기
colnames(grid_in) <- c("id", "총이용객수", "환승횟수", "평균환승")    
# s3(spatial polygon dataframe) => s4 포맷으로 변환
fishnet_2 <- as(fishnet,'sf')  
# fishnet_2 + "총이용객수", "환승횟수", "평균환승" 결합
fishnet_2 <- full_join(fishnet_2, grid_in, by = "id")  
# 저장
save(fishnet_2, file="04_004_fishnet_2.rdata")
head(fishnet_2, 2)

# 총 이용객 수
library(tmap)
tm_shape(fishnet_2) + tm_polygons("총이용객수", alpha = 0.6, border.col = "gray50", 
                                  border.alpha = .2, colorNA = NULL) + 
  tm_shape(admin,  alpha = 0.1) + tm_borders()  
# 평균 환승
tm_shape(fishnet_2) + tm_polygons("평균환승", alpha = 0.6, border.col = "gray50", bor-
                                  der.alpha = .2, colorNA = NULL) + 
  tm_shape(admin,  alpha = 0.1) + tm_borders()


# 도착지 기준 fishnet 분석
grid_out <- grid_chain %>% 
  group_by(id.y) %>% # 그리드에서 id.xy별로 총 이용객 수,
  summarise_if(is.numeric,sum) %>%  # 환승 횟수 데이터를 합치기
  dplyr::select(id.y,총이용객수, 환승횟수)
# 평균 환승 횟수 계산
grid_out$평균환승 <- round((grid_out$환승횟수 / grid_out$총이용객수),1)
# 칼럼 이름 정리하기
colnames(grid_out) <- c("id","총이용객수","환승횟수","평균환승")
# s3=>s4 포맷으로 변환
fishnet_2 <- as(fishnet,'sf')
# fishnet_2 + "총이용객수" ,"환승횟수","평균환승" 결합
fishnet_2 <- full_join(fishnet_2,grid_out,by="id")
# 저장
save(fishnet_2, file = "04_005_fishnet_2.rdata")
head(fishnet_2,2)

# 총 이용객 수
library(tmap)
tm_shape(fishnet_2) + tm_polygons("총이용객수",  alpha = 0.6, border.col = "gray50", 
                                  border.alpha = .2, colorNA = NULL) + 
  tm_shape(admin) + tm_borders()    
# 평균 환승
tm_shape(fishnet_2) + tm_polygons("평균환승", alpha = 0.6, border.col = "gray50",
                                  border.alpha = .2, colorNA = NULL) + 
  tm_shape(admin) + tm_borders()


## 통근 시간대 데이터 필터링

















































































