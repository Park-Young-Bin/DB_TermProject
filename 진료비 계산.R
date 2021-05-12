# 진료비 계산 #

############################## 응급 진료비 ################################

# 응급 진료비(총진료비 + 처방약값)
# 총진료비 변수: 해당사항 없음(-1) 응답은 응급실 이용을 하지 않았다는 것으로 제외한다.
# 처방약값 변수: 무료(91)와 해당없음(-1)은 제외한다.

# (1) 가구원별 연간 총진료비
dt_er <- er %>% filter(ER26_5 != -1) %>% 
  group_by(PIDWON) %>% 
  summarise(er_exp = sum(ER26_5))

# (2) -9 결측값 갯수 구하기(총 1,193개 중에서 84개 존재)
dt_er %>% 
  filter(er_exp < 0) %>% 
  summarise(n = n()) 

# (3) 응급 진료비의 평균과 중앙값(결측값을 뭐로 대체할지 고민해보자)
mean(dt_er$er_exp)      # 251,249.6원
median(dt_er$er_exp)    # 129,670원
max(dt_er$er_exp)       # 14,019,720원

############################## 입원 진료비 ################################

# 입원 진료비(-1: 해당사항 없음, -9: 모름/무응답)
# 해당사항 없음(-1) 응답은 입원이용을 하지 않았다는 것으로 제외한다.

# (1) 가구원별 연간 총진료비
dt_in <- In %>% filter(IN35_6 != -1) %>% 
  group_by(PIDWON) %>% 
  summarise(in_exp = sum(IN35_6))

# (2) -9 결측값 갯수 구하기(총 2,061개 중에서 170개 존재)
dt_in %>% 
  filter(in_exp < 0) %>% 
  summarise(n = n())

# (3) 입원 진료비의 평균과 중앙값(결측값을 뭐로 대체할지 고민해보자)
mean(dt_in$in_exp)      # 4,331,855원
median(dt_in$in_exp)    # 2,034,570원
max(dt_in$in_exp)       # 79,647,324원

############################## 외래 진료비 ###############################

# 외래 진료비(-1: 해당사항 없음, -9: 모름/무응답)
# 해당사항 없음(-1) 응답은 외래이용을 하지 않았다는 것으로 제외한다.

# (1) 가구원별 연간 총진료비
dt_ou <- ou %>% filter(OU29_7 != -1) %>% 
  group_by(PIDWON) %>% 
  summarise(ou_exp = sum(OU29_7))

# (2) -9 결측값 갯수 구하기(총 14,673개 중에서 1,125개 존재)
dt_ou %>% 
  filter(ou_exp < 0) %>% 
  summarise(n = n())
  
# (3) 외래 진료비의 평균과 중앙값(결측값을 뭐로 대체할지 고민해보자)
mean(dt_ou$ou_exp)      # 723,035.6원
median(dt_ou$ou_exp)    # 301,270원
max(dt_ou$ou_exp)       # 29,963,139원

############################## b1과 dt_er/dt_in/dt_ou 결합 ###############################

b1_er <- merge(b1, dt_er, by = c("PIDWON")) %>% filter(연령대 == '65세 이상')

b1_in <- merge(b1, dt_in, by = c("PIDWON")) %>% filter(연령대 == '65세 이상')

b1_ou <- merge(b1, dt_ou, by = c("PIDWON")) %>% filter(연령대 == '65세 이상')

############################## 응급 의료이용자 현황 ###############################

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

############################## 입원 의료이용자 현황 ###############################

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

############################## 외래 의료이용자 현황 ###############################

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



