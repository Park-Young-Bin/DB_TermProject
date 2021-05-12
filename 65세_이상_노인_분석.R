# 주석 단축키: ctrl + shift + c

#### 1장(사전 준비) ------------------------------------------

# 1. 필요한 패키지와 데이터 불러오기 및 사전 데이터 전처리
# install.packages("dplyr") # 새로 설치되는 것을 막기 위해 주석 처리
# install.packages("haven")
# install.packages("ggplot2")
# install.packages('car')
# install.packages("corrplot")
# install.packages("lm.beta")

library(dplyr) # 전처리 패키지
library(haven) # spss, sas 파일 불러오기 패키지
library(ggplot2) # 시각화 패키지
library(car) # 회귀분석과 관련된 패키지
library(corrplot) # 상관 행렬 패키지
library(lm.beta) # 표준화계수 산출 패키지

ind <- read_spss(file = "T17IND.sav")
hh <- read_spss(file = "T17HH.sav")
appen <- read_spss(file = "T17APPEN.sav")
cd <- read_spss(file = "T17CD.sav")
er <- read_spss(file = "T17ER.sav")
In <- read_spss(file = "T17IN.sav")
ou <- read_spss(file = "T17OU.sav")

# 1.1 cd, er, In, ou에서 필요한 변수만 추출하기
new_cd <- cd %>% select(PIDWON, cd1_1, CD2)
new_er <- er %>% select(PIDWON, ERCOUNT)
new_In <- In %>% select(PIDWON, INCOUNT)
new_ou <- ou %>% select(PIDWON, OUCOUNT)

# 1.1.1 중복된 행을 제외한 순수한 행 개수
# which(duplicated(new_cd)) # 중복된 행 찾기
# nrow(unique(new_cd)) # 139,208개

# which(duplicated(new_er)) # 중복된 행 찾기
# nrow(unique(new_er)) # 1,554개
# 
# which(duplicated(new_In)) # 중복된 행 찾기
# nrow(unique(new_In)) # 2,145개
# 
# which(duplicated(new_ou)) # 중복된 행 찾기
# nrow(unique(new_ou)) # 14,968개

# 1.1.2 중복 행 제거
re_cd <- distinct(new_cd) # 전처리 과정 더 필요
re_er <- distinct(new_er) # 이 데이터들을 사용할 것이다.
re_In <- distinct(new_In) # 이 데이터들을 사용할 것이다.
re_ou <- distinct(new_ou) # 이 데이터들을 사용할 것이다.

# 1.1.3 re_cd 전처리
# 전처리 이전 re_cd 모습: 한 사람에 대한 여러 질병 유무 데이터가 존재한다.
# 전처리 방법1: 단 하나의 질병이라도 갖고 있으면 갖고 있다고 바꾼다.(값이 2 or 4이면 0(없음)으로, 나머지는 1(있음)로 처리)
# 전처리 방법2: 개인(가구원)별로 그룹화하여 개인이 갖고 있는 만성질환의 수를 구한다.
re_cd <- re_cd %>% mutate(chg_cd = ifelse(re_cd$CD2 == 2 | re_cd$CD2 == 4, 0, 1))
re_cd <- re_cd %>% group_by(PIDWON) %>% summarise(count = sum(chg_cd)) # 가구원별 앓고 있는 만정진환 수(최종)

# 1.2 ind: 출생년도 변수 -> 나이/연령대 변수 생성
ind$나이 <- 2017 - ind$C4_0 + 1
ind$연령대 <- ifelse(ind$나이 >= 65, '65세 이상', '65세 미만')

#### 2장(노인의 의료서비스 이용 비율/그래프) --------------------------

# 2.1 ind와 er/In/ou 파일 병합 후 필요한 값들만 추출
ind_er_65 <- merge(ind, re_er, by = c("PIDWON")) %>% filter(연령대 == '65세 이상') %>% select(PIDWON, ERCOUNT)
ind_In_65 <- merge(ind, re_In, by = c("PIDWON")) %>% filter(연령대 == '65세 이상') %>% select(PIDWON, INCOUNT)
ind_ou_65 <- merge(ind, re_ou, by = c("PIDWON")) %>% filter(연령대 == '65세 이상') %>% select(PIDWON, OUCOUNT)

# 2.2 각각의 의료이용 횟수에 대한 비율 구하기
## 응급 의료이용 비율과 빈도
ind_er_65_pct <- ind_er_65 %>% 
  group_by(ERCOUNT) %>% 
  summarise(n = n()) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  select(ERCOUNT, pct)

# ind_er_65 %>% 
#   group_by(ERCOUNT) %>% 
#   summarise(n = n()) %>% 
#   select(ERCOUNT, n)

## 입원 의료이용 비율과 빈도
ind_In_65_pct <- ind_In_65%>% 
  group_by(INCOUNT) %>% 
  summarise(n = n()) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  select(INCOUNT, pct)

# ind_In_65%>% 
#   group_by(INCOUNT) %>% 
#   summarise(n = n()) %>% 
#   select(INCOUNT, n)

## 외래 의료이용 비율과 빈도
ind_ou_65_pct <- ind_ou_65 %>% 
  group_by(OUCOUNT) %>% 
  summarise(n = n()) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  select(OUCOUNT, pct)

# 2.3 그래프 작성
## 65세 이상 노인의 응급 의료이용 그래프(비율과 빈도)
ggplot(data = ind_er_65_pct, aes(x = ERCOUNT, y = pct)) +
  geom_col() +
  theme_classic() + 
  ggtitle("65세 이상 노인의 응급 의료이용 비율") + 
  xlab('건수') + 
  ylab('비율(%)') +
  theme(plot.title = element_text(face = 'bold',
                                  size =15,
                                  hjust=0.5))

# ggplot(data = g1, aes(x = ERCOUNT, y = n)) + geom_col() + theme_classic()
  
## 65세 이상 노인의 입원 의료이용 그래프(비율과 빈도)
ggplot(data = ind_In_65_pct, aes(x = INCOUNT, y = pct)) +
  geom_col() +
  theme_classic() + 
  ggtitle("65세 이상 노인의 입원 의료이용 비율") + 
  xlab('건수') + 
  ylab('비율(%)') +
  theme(plot.title = element_text(face = 'bold',
                                  size =15,
                                  hjust=0.5))

# ggplot(data = g2, aes(x = INCOUNT, y = n)) + geom_col() + theme_classic()

## 65세 이상 노인의 외래 외래이용 그래프(빈도와 비율)
ggplot(data = ind_ou_65_pct, aes(x = OUCOUNT, y = pct)) +
  geom_col() +
  theme_classic() + 
  ggtitle("65세 이상 노인의 외래 의료이용 비율") + 
  xlab('건수') + 
  ylab('비율(%)') +
  theme(plot.title = element_text(face = 'bold',
                                  size =15,
                                  hjust=0.5))

# ggplot(data = g3, aes(x = OUCOUNT, y = n)) + geom_col() + theme_classic()

#### 3장(노인의 여러 요인에 따른 의료비 지출 현황_빈도/%) ---------------------------

#### 3장-(1)(데이터 결합_merge) ------------------------------------------

a1 <- merge(ind, hh, by = c("HHID"))
# nrow(unique(a1)) # 결과: 중복 행 없음
a2 <- merge(a1, appen, by = c("PIDWON"))
# nrow(unique(a2)) # 결과: 중복 행 없음
a3 <- merge(a2, re_cd, by = c("PIDWON"))
# nrow(unique(a3)) # 결과: 중복 행 없음(최종 -> 레코드: 13,460개, 칼럼: 254개)

raw_data <- a3 # 원본데이터 생성

#### 3장-(2)(데이터 전처리) --------------------------------------

# 필요한 변수의 변수명 변경(영 -> 한) "dt"
dt <- rename(raw_data, 
             성별 = C3,
             세대구성 = B3,
             의료보장형태 = C11, 
             경제활동여부 = C24,
             주택소유여부 = B7,
             만성질환개수 = count,
             흡연여부 = S2,
             음주여부 = S22,
             장애여부 = C13_1,
             신체활동제한 = SH117,
             우울경험 = S44)

# 필요한 변수만 추출 "b1"
b1 <- dt %>% 
  select(PIDWON, 성별, 나이, 연령대, 세대구성, 의료보장형태,경제활동여부, 주택소유여부,
         만성질환개수, 흡연여부, 음주여부,장애여부, 신체활동제한, 우울경험)

# 변수 전처리
##(1) 성별 변수 전처리(1: 남자, 0: 여자)
b1$성별 <- ifelse(b1$성별 == 1, 1, 0)

##(2) 세대구성 변수 전처리(1: 단독가구, 2: 부부가구, 3: 부부+자녀, 4: 기타)
b1$세대구성 <- ifelse(b1$세대구성 == 11, 1,
                  ifelse(b1$세대구성 == 12, 2,
                         ifelse(b1$세대구성 == 21, 3, 4)))

##(3) 의료보장형태 변수 전처리(1: 건강보험가입자, 0: 의료급여수급자, NA: 급여정지/미가입)
b1$의료보장형태 <- ifelse(b1$의료보장형태 %in% c(1,2,3,6,10), 1,
                    ifelse(b1$의료보장형태 %in% c(4,5), 0, NA))
table(b1$의료보장형태)
table(is.na(b1$의료보장형태)) # NA: 23개

##(4) 경제활동여부 변수 전처리(1: 경제활동함, 0: 경제활동안함)
b1$경제활동여부 <- ifelse(b1$경제활동여부 %in% c(2, -6), 0, 1)

##(5) 주택소유여부 변수 전처리(1: 자가, 0: 전세, 월세 등)
b1$주택소유여부 <- ifelse(b1$주택소유여부 == 1, 1, 0)

##(6) 만성질환개수 변수 전처리(0개, 1개, 2개, 3개, 4개, 5개이상)
b1$만성질환개수 <- ifelse(b1$만성질환개수 == 0, '0개', 
                        ifelse(b1$만성질환개수 == 1, '1개', 
                           ifelse(b1$만성질환개수 == 2, '2개',
                                  ifelse(b1$만성질환개수 == 3, '3개',
                                         ifelse(b1$만성질환개수 == 4, '4개', '5개 이상')))))

##(7) 흡연여부 변수 전처리(1: 흡연, 0: 흡연안함)
b1$흡연여부 <- ifelse(b1$흡연여부 %in% c(3, 4, -9), 0, 1)

##(8) 음주여부 변수 전처리(1: 월 1회 이상, 0: 월 1회 미만)
b1$음주여부 <- ifelse(b1$음주여부 %in% c(1, 2, -1), 0 ,1)

##(9) 장애여부 변수 전처리(1: 장애있음, 0: 장애없음)
b1$장애여부 <- ifelse(b1$장애여부 == 2, 0, 1)

##(10) 신체활동제한 변수 전처리(1: 제한있음, 0: 제한없음)
b1$신체활동제한 <- ifelse(b1$신체활동제한 == 1, 1, 0)

##(11) 우울경험 변수 전처리(1: 우울감 경험, 0: 우울감 경험 안함)
b1$우울경험 <- ifelse(b1$우울경험 %in% 1, 1, 0)


#### 3장-(3)(응급/입원/외래 진료비 파악) ----------------------

# 아래 3장-(3)-1), 2), 3)에 계속...

#### 3장-(3)-1) 응급 의료비(총진료비 + 처방약값) ----------------------

## 총진료비(ER26_5): 해당사항 없음(-1) -> 결측값, 모름/무응답(-9) -> 수납금액
## 처방약값(ER33): 무료(91)와 해당없음(-1) -> 0

mof_er <- er # 복사본 만들기

## 처방약값 전처리
mof_er$ER33 <- ifelse(mof_er$ER33 %in% c(91, -1), 0, mof_er$ER33) # 91, -1 -> 0으로 치환

## 총진료비 전처리
mof_er$ER26_5 <- ifelse(mof_er$ER26_5 == -1, NA, # -1 -> NA로 치환
                        ifelse(mof_er$ER26_5 == -9, mof_er$ER26_1, mof_er$ER26_5)) # -9 -> 수납금액으로 치환
table(is.na(mof_er$ER26_5)) # NA: 587개

## 총진료비의 요약통계량 및 분포 확인
summary(mof_er$ER26_5)
boxplot(mof_er$ER26_5)$stats
  
## (상자그림)
ggplot(data=mof_er, aes(y = ER26_5)) + 
  geom_boxplot() + 
  theme_classic() + 
  ggtitle("응급 총진료비 상자그림") +
  ylab('응급 총진료비') + 
  theme(plot.title = element_text(face = 'bold',
                                  size = 17,
                                  hjust = 0.5)) + 
  scale_y_continuous(labels = scales::comma)

## 총진료비의 NA를 중앙값(68102)으로 대체
mof_er$ER26_5 <- ifelse(is.na(mof_er$ER26_5), 113684, mof_er$ER26_5)

## (1) 가구원별 연간 총 응급 의료비(총진료비 + 처방약값)
dt_er <- mof_er %>%
  group_by(PIDWON) %>% 
  summarise(er_exp = sum(ER26_5 + ER33))

## (2) 응급 의료비 통계량
mean(dt_er$er_exp)      # 244,405.1원
median(dt_er$er_exp)    # 113,684원
max(dt_er$er_exp)       # 14,019,720원

#### 3장-(3)-2) 입원 의료비(총진료비 + 처방약값) ----------------------

## 총진료비(ER26_5): 해당사항 없음(-1) -> 결측값, 모름/무응답(-9) -> 수납금액
## 처방약값(ER33): 무료(91)와 해당없음(-1) -> 0

mof_In <- In # 복사본 만들기

## 처방약값 전처리(처방약값 91, -1을 0으로 치환)
mof_In$IN37 <- ifelse(mof_In$IN37 %in% c(91, -1), 0, mof_In$IN37)

## 총진료비 전처리
mof_In$IN35_6 <- ifelse(mof_In$IN35_6 == -1, NA, # -1 -> NA로 치환
                        ifelse(mof_In$IN35_6 == -9, mof_In$IN35_2, mof_In$IN35_6)) # -9 -> 수납금액으로 치환
table(is.na(mof_In$IN35_6)) # NA: 221개

## 총진료비의 요약통계량 및 분포 확인
summary(mof_In$IN35_6)
boxplot(mof_In$IN35_6)$stats

## (상자그림)
ggplot(data=mof_In, aes(y = IN35_6)) + 
  geom_boxplot() + 
  theme_classic() + 
  ggtitle("입원 총진료비 상자그림") +
  ylab('입원 총진료비') + 
  theme(plot.title = element_text(face = 'bold',
                                  size = 17,
                                  hjust = 0.5)) + 
  scale_y_continuous(labels = scales::comma)

## 총진료비의 NA를 중앙값(1501562)으로 대체
mof_In$IN35_6 <- ifelse(is.na(mof_In$IN35_6), 1501562, mof_In$IN35_6)

## (1) 가구원별 연간 총 입원 의료비(총진료비 + 처방약값)
dt_in <- mof_In %>%
  group_by(PIDWON) %>% 
  summarise(in_exp = sum(IN35_6 + IN37))

## (2) 입원 의료비의 평균과 중앙값
mean(dt_in$in_exp)      # 4,487,366원
median(dt_in$in_exp)    # 2,079,769원
max(dt_in$in_exp)       # 81,148,886원

#### 3장-(3)-3) 외래 의료비(총진료비 + 처방약값) #########################

## 총진료비(OU29_7): 해당사항 없음(-1) -> 결측값, 모름/무응답(-9) -> 수납금액
## 처방약값(OU35): 무료(91)와 해당없음(-1) -> 0, 모름/무응답(-9)은 NA 처리 -> 결측값 대체

mof_ou <- ou # 복사본 만들기

## 1) 처방약값 전처리
mof_ou$OU35 <- ifelse(mof_ou$OU35 %in% c(91, -1), 0, # 91, -1 -> 0으로 치환
                      ifelse(mof_ou$OU35 == -9, NA, mof_ou$OU35)) # -9 -> 결측값(NA)으로 치환
table(is.na(mof_ou$OU35)) # NA: 10개

### 처방약값의 요약통계량 및 분포 확인
summary(mof_ou$OU35)
boxplot(mof_ou$OU35)

### 처방약값의 NA를 중앙값(1200)으로 대체
mof_ou$OU35 <- ifelse(is.na(mof_ou$OU35), 1200, mof_ou$OU35)

## 2) 총진료비 전처리
mof_ou$OU29_7 <- ifelse(mof_ou$OU29_7 == -1, NA, # -1 -> 결측값(NA)으로 치환
                        ifelse(mof_ou$OU29_7 == -9, mof_ou$OU29_2, mof_ou$OU29_7)) # -9 -> 수납금액으로 치환
table(is.na(mof_ou$OU29_7)) # NA: 35,499개

## 총진료비의 요약통계량 및 분포 확인
summary(mof_ou$OU29_7)
boxplot(mof_ou$OU29_7)

## (상자그림)
ggplot(data=mof_ou, aes(y = OU29_7)) + 
  geom_boxplot() + 
  theme_classic() + 
  ggtitle("외래 총진료비 상자그림") +
  ylab('외래 총진료비') + 
  theme(plot.title = element_text(face = 'bold',
                                  size = 17,
                                  hjust = 0.5)) + 
  scale_y_continuous(labels = scales::comma)

## 총진료비의 NA를 중앙값(15100)으로 대체
mof_ou$OU29_7 <- ifelse(is.na(mof_ou$OU29_7), 15100, mof_ou$OU29_7)

## (1) 가구원별 연간 총 외래 의료비(총진료비 + 처방약값)
dt_ou <- mof_ou %>%
  group_by(PIDWON) %>% 
  summarise(ou_exp = sum(OU29_7 + OU35))

## (2) 외래 의료비의 평균과 중앙값
mean(dt_ou$ou_exp)      # 932,146.7원
median(dt_ou$ou_exp)    # 471,307원
max(dt_ou$ou_exp)       # 30,074,284원

#### 3장-(4) EDA --------
#### 3장-(4)-1)(65세 이상 응급 의료이용자 현황) ----------------------

b1_er <- merge(b1, dt_er, by = c("PIDWON")) %>% filter(연령대 == '65세 이상'& !is.na(의료보장형태))

## 1) 성별에 따른 응급이용 현황과 개인지출의료비
b1_er %>% 
  group_by(성별) %>% 
  summarise(n = n(),
            avg_expense = mean(er_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(성별))

## 2) 세대구성에 따른 응급이용 현황과 개인지출의료비
b1_er %>% 
  group_by(세대구성) %>% 
  summarise(n = n(),
            avg_expense = mean(er_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2))

## 3) 의료보장형태에 따른 응급이용 현황과 개인지출의료비
b1_er %>% 
  group_by(의료보장형태) %>% 
  summarise(n = n(),
            avg_expense = mean(er_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(의료보장형태))

## 4) 경제활동여부에 따른 응급이용 현황과 개인지출의료비
b1_er %>% 
  group_by(경제활동여부) %>% 
  summarise(n = n(),
            avg_expense = mean(er_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(경제활동여부))

## 5) 주택소유여부에 따른 응급이용 현황과 개인지출의료비
b1_er %>% 
  group_by(주택소유여부) %>% 
  summarise(n = n(),
            avg_expense = mean(er_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(주택소유여부))

## 6) 만성질환 개수에 따른 응급이용 현황과 개인지출의료비
b1_er %>% 
  group_by(만성질환개수) %>% 
  summarise(n = n(),
            avg_expense = mean(er_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2))

## 7) 흡연여부에 따른 응급이용 현황과 개인지출의료비
b1_er %>% 
  group_by(흡연여부) %>% 
  summarise(n = n(),
            avg_expense = mean(er_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(흡연여부))

## 8) 음주여부에 따른 응급이용 현황과 개인지출의료비
b1_er %>% 
  group_by(음주여부) %>% 
  summarise(n = n(),
            avg_expense = mean(er_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(음주여부))

## 9) 장애여부에 따른 응급이용 현황과 개인지출의료비
b1_er %>% 
  group_by(장애여부) %>% 
  summarise(n = n(),
            avg_expense = mean(er_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(장애여부))

## 10) 신체활동제한에 따른 응급이용 현황과 개인지출의료비
b1_er %>% 
  group_by(신체활동제한) %>% 
  summarise(n = n(),
            avg_expense = mean(er_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(신체활동제한))

## 11) 우울경험에 따른 응급이용 현황과 개인지출의료비
b1_er %>% 
  group_by(우울경험) %>% 
  summarise(n = n(),
            avg_expense = mean(er_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(우울경험))

## 12) 응급이용 개인지출의료비
summary(b1_er$er_exp)

#(상자그림)
ggplot(data=b1_er, aes(y = er_exp)) + 
  geom_boxplot() + 
  theme_classic() + 
  ggtitle("응급의료비 상자그림") +
  theme(plot.title = element_text(face = 'bold',
                                  size = 17,
                                  hjust = 0.5)) + 
  scale_y_continuous(labels = scales::comma)

#(히스토그램)
ggplot(data = b1_er, aes(x = er_exp)) + 
  geom_histogram() + 
  theme_classic() + 
  ylab('빈도') +
  ggtitle("응급의료비 히스토그램") + 
  theme(plot.title = element_text(face = 'bold',
                                  size = 17,
                                  hjust = 0.5)) + 
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma)


#### 3장-(4)-2)(65세 이상 입원 의료이용자 현황) ----------------------

b1_in <- merge(b1, dt_in, by = c("PIDWON")) %>% filter(연령대 == '65세 이상'& !is.na(의료보장형태))

## 1) 성별에 따른 입원이용 현황과 개인지출의료비
b1_in %>% 
  group_by(성별) %>% 
  summarise(n = n(),
            avg_expense = mean(in_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(성별))

## 2) 세대구성에 따른 입원이용 현황과 개인지출의료비
b1_in %>% 
  group_by(세대구성) %>% 
  summarise(n = n(),
            avg_expense = mean(in_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2))

## 3) 의료보장형태에 따른 입원이용 현황과 개인지출의료비
b1_in %>% 
  group_by(의료보장형태) %>% 
  summarise(n = n(),
            avg_expense = mean(in_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(의료보장형태))

## 4) 경제활동여부에 따른 입원이용 현황과 개인지출의료비
b1_in %>% 
  group_by(경제활동여부) %>% 
  summarise(n = n(),
            avg_expense = mean(in_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(경제활동여부))

## 5) 주택소유여부에 따른 입원이용 현황과 개인지출의료비
b1_in %>% 
  group_by(주택소유여부) %>% 
  summarise(n = n(),
            avg_expense = mean(in_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(주택소유여부))

## 6) 만성질환 개수에 따른 입원이용 현황과 개인지출의료비
b1_in %>% 
  group_by(만성질환개수) %>% 
  summarise(n = n(),
            avg_expense = mean(in_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2))

## 7) 흡연여부에 따른 입원이용 현황과 개인지출의료비
b1_in %>% 
  group_by(흡연여부) %>% 
  summarise(n = n(),
            avg_expense = mean(in_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(흡연여부))

## 8) 음주여부에 따른 입원이용 현황과 개인지출의료비
b1_in %>% 
  group_by(음주여부) %>% 
  summarise(n = n(),
            avg_expense = mean(in_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(음주여부))

## 9) 장애여부에 따른 입원이용 현황과 개인지출의료비
b1_in %>% 
  group_by(장애여부) %>% 
  summarise(n = n(),
            avg_expense = mean(in_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(장애여부))

## 10) 신체활동제한에 따른 입원이용 현황과 개인지출의료비
b1_in %>% 
  group_by(신체활동제한) %>% 
  summarise(n = n(),
            avg_expense = mean(in_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(신체활동제한))

## 11) 우울경험에 따른 입원이용 현황과 개인지출의료비
b1_in %>% 
  group_by(우울경험) %>% 
  summarise(n = n(),
            avg_expense = mean(in_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(우울경험))

## 12) 입원이용 개인지출의료비
summary(b1_in$in_exp)

#(상자그림)
ggplot(data = b1_in, aes(y = in_exp)) + 
  geom_boxplot() + 
  theme_classic() + 
  ggtitle("입원의료비 상자그림") +
  theme(plot.title = element_text(face = 'bold',
                                  size = 17,
                                  hjust = 0.5)) + 
  scale_y_continuous(labels = scales::comma)

#(히스토그램)
ggplot(data = b1_in, aes(x = in_exp)) + 
  geom_histogram() + 
  theme_classic() + 
  ylab('빈도') +
  ggtitle("입원의료비 히스토그램") + 
  theme(plot.title = element_text(face = 'bold',
                                  size = 17,
                                  hjust = 0.5)) + 
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma)



#### 3장-(4)-3)(65세 이상 외래 의료이용자 현황) ----------------------

b1_ou <- merge(b1, dt_ou, by = c("PIDWON")) %>% filter(연령대 == '65세 이상'& !is.na(의료보장형태))

## 1) 성별에 따른 외래이용 현황과 개인지출의료비
b1_ou %>% 
  group_by(성별) %>% 
  summarise(n = n(),
            avg_expense = mean(ou_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(성별))

## 2) 세대구성에 따른 외래이용 현황과 개인지출의료비
b1_ou %>% 
  group_by(세대구성) %>% 
  summarise(n = n(),
            avg_expense = mean(ou_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2))

## 3) 의료보장형태에 따른 외래이용 현황과 개인지출의료비
b1_ou %>% 
  group_by(의료보장형태) %>% 
  summarise(n = n(),
            avg_expense = mean(ou_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(의료보장형태))

## 4) 경제활동여부에 따른 외래이용 현황과 개인지출의료비
b1_ou %>% 
  group_by(경제활동여부) %>% 
  summarise(n = n(),
            avg_expense = mean(ou_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(경제활동여부))

## 5) 주택소유여부에 따른 외래이용 현황과 개인지출의료비
b1_ou %>% 
  group_by(주택소유여부) %>% 
  summarise(n = n(),
            avg_expense = mean(ou_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(주택소유여부))

## 6) 만성질환 개수에 따른 외래이용 현황과 개인지출의료비
b1_ou %>% 
  group_by(만성질환개수) %>% 
  summarise(n = n(),
            avg_expense = mean(ou_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2))

## 7) 흡연여부에 따른 외래이용 현황과 개인지출의료비
b1_ou %>% 
  group_by(흡연여부) %>% 
  summarise(n = n(),
            avg_expense = mean(ou_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(흡연여부))

## 8) 음주여부에 따른 외래이용 현황과 개인지출의료비
b1_ou %>% 
  group_by(음주여부) %>% 
  summarise(n = n(),
            avg_expense = mean(ou_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(음주여부))

## 9) 장애여부에 따른 외래이용 현황과 개인지출의료비
b1_ou %>% 
  group_by(장애여부) %>% 
  summarise(n = n(),
            avg_expense = mean(ou_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(장애여부))

## 10) 신체활동제한에 따른 외래이용 현황과 개인지출의료비
b1_ou %>% 
  group_by(신체활동제한) %>% 
  summarise(n = n(),
            avg_expense = mean(ou_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(신체활동제한))

## 11) 우울경험에 따른 외래이용 현황과 개인지출의료비
b1_ou %>% 
  group_by(우울경험) %>% 
  summarise(n = n(),
            avg_expense = mean(ou_exp)) %>% 
  mutate(tot = sum(n)) %>% 
  mutate(pct = round(n/tot * 100, 2)) %>% 
  arrange(desc(우울경험))

## 12) 외래이용 개인지출의료비
summary(b1_ou$ou_exp)

#(상자그림)
ggplot(data = b1_ou, aes(y = ou_exp)) + 
  geom_boxplot() + 
  theme_classic() + 
  ggtitle("외래의료비 상자그림") +
  theme(plot.title = element_text(face = 'bold',
                                  size = 17,
                                  hjust = 0.5)) + 
  scale_y_continuous(labels = scales::comma)

#(히스토그램)
ggplot(data = b1_ou, aes(x = ou_exp)) + 
  geom_histogram() + 
  theme_classic() + 
  ylab('빈도') +
  ggtitle("외래의료비 히스토그램") + 
  theme(plot.title = element_text(face = 'bold',
                                  size = 17,
                                  hjust = 0.5)) + 
  scale_x_continuous(labels = scales::comma) + 
  scale_y_continuous(labels = scales::comma)

#### 4장(다중회귀분석) ----------------------
# 아래에 계속...

#### 4장-(1) 더미변수 생성(대상: 세대구성, 만성질환개수) ----------------------

# 1. 응급(b1_er)

## 1) 세대구성('기타' 제외)
b1_er$더미_단독 = ifelse(b1_er$세대구성 == 1, 1, 0)
b1_er$더미_부부 = ifelse(b1_er$세대구성 == 2, 1, 0)
b1_er$더미_부부자녀 = ifelse(b1_er$세대구성 == 3, 1, 0)

## 2) 만성질환개수('5개 이상' 제외)
b1_er$더미만성zero = ifelse(b1_er$만성질환개수 == '0개', 1, 0)
b1_er$더미만성one = ifelse(b1_er$만성질환개수 == '1개', 1, 0)
b1_er$더미만성two = ifelse(b1_er$만성질환개수 == '2개', 1, 0)
b1_er$더미만성three = ifelse(b1_er$만성질환개수 == '3개', 1, 0)
b1_er$더미만성four = ifelse(b1_er$만성질환개수 == '4개', 1, 0)

test_er <- b1_er %>% select(-PIDWON, -연령대, -세대구성, -만성질환개수)

# 2. 입원(b1_in)

## 1) 세대구성('기타' 제외)
b1_in$더미_단독 = ifelse(b1_in$세대구성 == 1, 1, 0)
b1_in$더미_부부 = ifelse(b1_in$세대구성 == 2, 1, 0)
b1_in$더미_부부자녀 = ifelse(b1_in$세대구성 == 3, 1, 0)

## 2) 만성질환개수('5개 이상' 제외)
b1_in$더미만성zero = ifelse(b1_in$만성질환개수 == '0개', 1, 0)
b1_in$더미만성one = ifelse(b1_in$만성질환개수 == '1개', 1, 0)
b1_in$더미만성two = ifelse(b1_in$만성질환개수 == '2개', 1, 0)
b1_in$더미만성three = ifelse(b1_in$만성질환개수 == '3개', 1, 0)
b1_in$더미만성four = ifelse(b1_in$만성질환개수 == '4개', 1, 0)

test_in <- b1_in %>% select(-PIDWON, -연령대, -세대구성, -만성질환개수)

# 3. 외래(b1_ou)

## 1) 세대구성('기타' 제외)
b1_ou$더미_단독 = ifelse(b1_ou$세대구성 == 1, 1, 0)
b1_ou$더미_부부 = ifelse(b1_ou$세대구성 == 2, 1, 0)
b1_ou$더미_부부자녀 = ifelse(b1_ou$세대구성 == 3, 1, 0)

## 2) 만성질환개수('5개 이상' 제외)
b1_ou$더미만성zero = ifelse(b1_ou$만성질환개수 == '0개', 1, 0)
b1_ou$더미만성one = ifelse(b1_ou$만성질환개수 == '1개', 1, 0)
b1_ou$더미만성two = ifelse(b1_ou$만성질환개수 == '2개', 1, 0)
b1_ou$더미만성three = ifelse(b1_ou$만성질환개수 == '3개', 1, 0)
b1_ou$더미만성four = ifelse(b1_ou$만성질환개수 == '4개', 1, 0)

test_ou <- b1_ou %>% select(-PIDWON, -연령대, -세대구성, -만성질환개수)

#### 4장-(2)(회귀분석 실시) --------------------------

model1 <- step(lm(er_exp ~., data=test_er))
model2 <- step(lm(in_exp ~., data=test_in))
model3 <- step(lm(ou_exp ~., data=test_ou))
par(mfrow = c(2, 2))
plot(model1)
plot(model2)
plot(model3)
shapiro.test(model1$residuals)
shapiro.test(model2$residuals)
shapiro.test(model3$residuals)

#### 4장-(2)-1)(응급 데이터 회귀분석 실시) ----------------------

# 1. 인구/경제학적 변수
im_er_1 <- step(lm(er_exp ~ 성별 + 나이 + 의료보장형태 + 경제활동여부 + 주택소유여부 + 
             더미_단독 + 더미_부부 + 더미_부부자녀, data = test_er)) # 단계적 회귀분석 사용
summary(im_er_1)
durbinWatsonTest(im_er_1) # 잔차의 독립성 검정
lm.beta(im_er_1) # 표준화계수 산출

## 1) 팽창계수 확인
vif(im_er_1)  # 다중공선성 없음

## 2) 상관계수 확인
### (1) 상관행렬 생성
cor_er_1 <- cor(test_er[c('성별', '나이', '의료보장형태', '경제활동여부', 
              '주택소유여부', '더미_단독', '더미_부부', '더미_부부자녀')])

### (2) 상관행렬 히트맵 생성
### colorRampPalette() 함수를 사용하여 색상 코드 목록 생성
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cor_er_1,
         method = "color", # 색깔로 표현
         col = col(200), # 색상 200개 설정
         type = "lower", # 왼쪽 아래 행렬만 표시
         addCoef.col = "black", # 상관계수 색깔
         tl.col = "black", # 변수명 색깔
         tl.srt = 45, # 변수명 45도 기울임
         diag = F) # 대각 행렬 제외

## 3) 회귀분석 가정 확인
## 1행1열: 선형성 / 1행2열: 정규성 / 2행1열: 잔차의 등분산성 / 2행2열: 극단값
par(mfrow = c(2, 2))
plot(im_er_1)
shapiro.test(im_er_1$residuals) # 샤피로 검정으로 잔차의 정규성 확인(p-value가 높을수록 좋다.)
par(mfrow = c(1, 1)) # 재분배

influencePlot(im_er_1, id.method="identity")

#------------------------------------------------#

# 2. 건강상태 및 건강행위에 따른 변수
im_er_2 <- step(lm(er_exp ~ 흡연여부 + 음주여부 + 장애여부 + 신체활동제한 + 우울경험 + 더미만성zero + 
                더미만성one + 더미만성two + 더미만성three + 더미만성four, data = test_er)) # 단계적 회귀분석 사용
summary(im_er_2)
durbinWatsonTest(im_er_2) # 잔차의 독립성 검정
lm.beta(im_er_2) # 표준화계수 산출

## 1) 팽창계수 확인
vif(im_er_2)  # 다중공선성 없음

## 2) 상관계수 확인
### (1) 상관행렬 생성
cor_er_2 <- cor(test_er[c('흡연여부', '음주여부', '장애여부', '신체활동제한', '우울경험',
                          '더미만성zero', '더미만성one', '더미만성two', '더미만성three', '더미만성four')])

### (2) 상관행렬 히트맵 생성
corrplot(cor_er_2,
         method = "color", # 색깔로 표현
         col = col(200), # 색상 200개 설정
         type = "lower", # 왼쪽 아래 행렬만 표시
         addCoef.col = "black", # 상관계수 색깔
         tl.col = "black", # 변수명 색깔
         tl.srt = 45, # 변수명 45도 기울임
         diag = F) # 대각 행렬 제외

## 3) 회귀분석 가정 확인
## 1행1열: 선형성 / 1행2열: 정규성 / 2행1열: 잔차의 등분산성 / 2행2열: 극단값
par(mfrow = c(2, 2))
plot(im_er_2)
shapiro.test(im_er_2$residuals) # 샤피로 검정으로 잔차의 정규성 확인(p-value가 높을수록 좋다.)
par(mfrow = c(1, 1)) # 재분배

influencePlot(im_er_2, id.method="identity")

#### 4장-(2)-2)(입원 데이터 회귀분석 실시) ----------------------

# 1. 인구/경제학적 변수
im_in_1 <- step(lm(in_exp ~ 성별 + 나이 + 의료보장형태 + 경제활동여부 + 주택소유여부 + 
                더미_단독 + 더미_부부 + 더미_부부자녀, data = test_in)) # 단계적 회귀분석 사용
summary(im_in_1)
durbinWatsonTest(im_in_1) # 잔차의 독립성 검정
lm.beta(im_in_1) # 표준화계수 산출

## 1) 팽창계수 확인
vif(im_in_1)  # 다중공선성 없음

## 2) 상관계수 확인
### (1) 상관행렬 생성
cor_in_1 <- cor(test_in[c('성별', '나이', '의료보장형태', '경제활동여부', 
              '주택소유여부', '더미_단독', '더미_부부', '더미_부부자녀')])

### (2) 상관행렬 히트맵 생성
corrplot(cor_in_1,
         method = "color", # 색깔로 표현
         col = col(200), # 색상 200개 설정
         type = "lower", # 왼쪽 아래 행렬만 표시
         addCoef.col = "black", # 상관계수 색깔
         tl.col = "black", # 변수명 색깔
         tl.srt = 45, # 변수명 45도 기울임
         diag = F) # 대각 행렬 제외

## 3) 회귀분석 가정 확인
## 1행1열: 선형성 / 1행2열: 정규성 / 2행1열: 잔차의 등분산성 / 2행2열: 극단값
par(mfrow = c(2, 2))
plot(im_in_1)
shapiro.test(im_in_1$residuals) # 샤피로 검정으로 잔차의 정규성 확인(p-value가 높을수록 좋다.)
par(mfrow = c(1, 1)) # 재분배

influencePlot(im_in_1, id.method="identity")

#------------------------------------------------#

# 2. 건강상태 및 건강행위에 따른 변수
im_in_2 <- step(lm(in_exp ~ 흡연여부 + 음주여부 + 장애여부 + 신체활동제한 + 우울경험 + 더미만성zero + 
                더미만성one + 더미만성two + 더미만성three + 더미만성four, data = test_in)) # 단계적 회귀분석 사용
summary(im_in_2)
durbinWatsonTest(im_in_2) # 잔차의 독립성 검정
lm.beta(im_in_2) # 표준화계수 산출

## 1) 팽창계수 확인
vif(im_in_2)  # 다중공선성 없음

## 2) 상관계수 확인
### (1) 상관행렬 생성
cor_in_2 <- cor(test_in[c('흡연여부', '음주여부', '장애여부', '신체활동제한', '우울경험',
                          '더미만성zero','더미만성one', '더미만성two', '더미만성three', '더미만성four')])

### (2) 상관행렬 히트맵 생성
corrplot(cor_in_2,
         method = "color", # 색깔로 표현
         col = col(200), # 색상 200개 설정
         type = "lower", # 왼쪽 아래 행렬만 표시
         addCoef.col = "black", # 상관계수 색깔
         tl.col = "black", # 변수명 색깔
         tl.srt = 45, # 변수명 45도 기울임
         diag = F) # 대각 행렬 제외

## 3) 회귀분석 가정 확인
## 1행1열: 선형성 / 1행2열: 정규성 / 2행1열: 잔차의 등분산성 / 2행2열: 극단값
par(mfrow = c(2, 2))
plot(im_in_2)
shapiro.test(im_in_2$residuals) # 샤피로 검정으로 잔차의 정규성 확인(p-value가 높을수록 좋다.)
par(mfrow = c(1, 1)) # 재분배

influencePlot(im_in_2, id.method="identity")

#### 4장-(2)-3)(외래 데이터 회귀분석 실시) ----------------------

# 1. 인구/경제학적 변수
im_ou_1 <- step(lm(ou_exp ~ 성별 + 나이 + 의료보장형태 + 경제활동여부 + 주택소유여부 + 
                더미_단독 + 더미_부부 + 더미_부부자녀, data = test_ou)) # 단계적 회귀분석 사용
summary(im_ou_1)
durbinWatsonTest(im_ou_1) # 잔차의 독립성 검정
lm.beta(im_ou_1) # 표준화계수 산출

## 1) 팽창계수 확인
vif(im_ou_1)  # 다중공선성 없음

## 2) 상관계수 확인
### (1) 상관행렬 생성
cor_ou_1 <- cor(test_ou[c('성별', '나이', '의료보장형태', '경제활동여부', 
              '주택소유여부', '더미_단독', '더미_부부', '더미_부부자녀')])

### (2) 상관행렬 히트맵 생성
corrplot(cor_ou_1,
         method = "color", # 색깔로 표현
         col = col(200), # 색상 200개 설정
         type = "lower", # 왼쪽 아래 행렬만 표시
         addCoef.col = "black", # 상관계수 색깔
         tl.col = "black", # 변수명 색깔
         tl.srt = 45, # 변수명 45도 기울임
         diag = F) # 대각 행렬 제외

## 3) 회귀분석 가정 확인
## 1행1열: 선형성 / 1행2열: 정규성 / 2행1열: 잔차의 등분산성 / 2행2열: 극단값
par(mfrow = c(2, 2))
plot(im_ou_1)
shapiro.test(im_ou_1$residuals) # 샤피로 검정으로 잔차의 정규성 확인(p-value가 높을수록 좋다.)
par(mfrow = c(1, 1)) # 재분배

influencePlot(im_ou_1, id.method="identity")

#------------------------------------------------#

# 2. 건강상태 및 건강행위에 따른 변수
im_ou_2 <- step(lm(ou_exp ~ 흡연여부 + 음주여부 + 장애여부 + 신체활동제한 + 우울경험 + 더미만성zero +
                더미만성one + 더미만성two + 더미만성three + 더미만성four, data = test_ou)) # 단계적 회귀분석 사용
summary(im_ou_2)
durbinWatsonTest(im_ou_2) # 잔차의 독립성 검정
lm.beta(im_ou_2) # 표준화계수 산출

## 1) 팽창계수 확인
vif(im_ou_2)  # 다중공선성 없음

## 2) 상관계수 확인
### (1) 상관행렬 생성
cor_ou_2 <- cor(test_ou[c('흡연여부', '음주여부', '장애여부', '신체활동제한', '우울경험',
              '더미만성one', '더미만성two', '더미만성three', '더미만성four')])

### (2) 상관행렬 히트맵 생성
corrplot(cor_ou_2,
         method = "color", # 색깔로 표현
         col = col(200), # 색상 200개 설정
         type = "lower", # 왼쪽 아래 행렬만 표시
         addCoef.col = "black", # 상관계수 색깔
         tl.col = "black", # 변수명 색깔
         tl.srt = 45, # 변수명 45도 기울임
         diag = F) # 대각 행렬 제외

## 3) 회귀분석 가정 확인
## 1행1열: 선형성 / 1행2열: 정규성 / 2행1열: 잔차의 등분산성 / 2행2열: 극단값
par(mfrow = c(2, 2))
plot(im_ou_2)
shapiro.test(im_ou_2$residuals) # 샤피로 검정으로 잔차의 정규성 확인(p-value가 높을수록 좋다.)
par(mfrow = c(1, 1)) # 재분배

influencePlot(im_ou_2, id.method="identity")
