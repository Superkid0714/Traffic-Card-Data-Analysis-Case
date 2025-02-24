# 파일 불러오기
load("04_002_trip_chain.rdata")
load("04_001_sta_pnt.rdata")
load("01_002_fishnet.rdata")
load("01_001_admin.rdata")

# 통근 통행 필터링 (오전:7,8,9) (오후:17,18,19)
library(dplyr)
trip_cmt <-  trip_chain[grep("6|7|8|9|17|18|19", trip_chain$start_hour),]  

# trip_chain 번호 다시 매기기
rownames(trip_cmt) <- seq(1:nrow(trip_cmt))
save(trip_cmt, file="06_001_trip_cmt.rdata")

# trip_chain에서 필요한 정보만 추출하기
keeps <- c("id.x","id.y","승차역ID1","최종하차역ID","총이용객수","환승횟수")
grid_chain <- trip_cmt[keeps]
save(grid_chain, file="05_003_grid_chain.rdata")

rm("trip_chian"); rm("keeps")
head(grid_chain,2)


library(stplanr)  # install.packages("stplanr")
od_intra <- filter(grid_chain, id.x != id.y)  

# 그리드 간(intra) 이동별 총 이용객 수, 환승 횟수 집계하기 
library(dplyr)
od_intra2 <- od_intra %>%   
  group_by(id.x, id.y) %>% 
  summarise_each(funs(sum)) %>% 
  dplyr::select(id.x, id.y, 총이용객수, 환승횟수)

# 평균 환승 횟수 계산 
od_intra2$평균환승 <- round((od_intra2$환승횟수 / od_intra2$총이용객수),1) 

# 칼럼 이름 정리하기
colnames(od_intra2) <- c("id.x", "id.y", "총이용객수", "환승횟수", "평균환승") 
head(od_intra2, 2)

# 공간 데이터 형식(OD2LINE) 만들기 
od_line <- od2line(od_intra2, fishnet)   
# 저장
save(od_line, file="05_003_od_line.rdata")  

# 총 이용객 수 시각화

library(tmap)
qtm("Hwaseong") +
  qtm(subset(od_line, od_line$총이용객수 > 30), lines.col = "grey", lines.lwd = .3) +
  qtm(subset(od_line, od_line$총이용객수 > 100), lines.col = "blue", lines.alpha =.4, 
      lines.lwd = 1) +
  qtm(subset(od_line, od_line$총이용객수 > 400), lines.col = "orange", lines.alpha =.6, 
      lines.lwd = 2) +
  qtm(subset(od_line, od_line$총이용객수 > 1000), lines.col = "red", lines.alpha =.8, 
      lines.lwd = 4) 
# 평균 환승 시각화
qtm("Hwaseong") +
  qtm(subset(od_line, od_line$평균환승 >= 2 & od_line$평균환승 < 3), lines.col = "grey", 
      lines.lwd = .3) +
  qtm(subset(od_line, od_line$평균환승 >= 3 & od_line$평균환승 < 4), lines.col = "blue", 
      lines.alpha =.4, lines.lwd = 1) + 
  qtm(subset(od_line, od_line$평균환승 >= 4 & od_line$평균환승 < 5), lines.col = "red", 
      lines.alpha =.4, lines.lwd = 2)





