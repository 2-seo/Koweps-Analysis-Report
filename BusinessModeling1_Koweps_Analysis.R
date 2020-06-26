library(foreign)
library(dplyr)
library(ggplot2)

# 1) 저소득 가구의 가구 형태

originalData <- read.spss("Koweps_hpc10_2015_beta1.sav", to.data.frame = T)

Data1 <- originalData
# h10_hc : 소득에 따른 가구 구분
# 1. 일반가구 2. 저소득층 가구

# h1001_110 : 가구 형태
# 1.단독, 2.모자, 3.부자, 4. 조손 가구 or 소년/소녀 가장, 5. 기타
# 이번 분석에서는 '기타'가구 형태는 제외하겠다.

Data1 <- Data1 %>%
  select(h10_hc, h1001_110)

Data1 <- Data1 %>%
  rename('classification_income' = h10_hc) %>%
  rename('form' = h1001_110)

Data1$form <- ifelse(Data1$form == 5, NA, Data1$form)

Result1 <- Data1 %>%
  filter(!is.na(form) & classification_income == 2) %>%
  group_by(classification_income , form) %>%
  summarise(count = n())

ggplot(Result1, aes(reorder(form, -count), count)) +
  geom_col() +
  xlab('Family Form') + ylab('Count') +
  ggtitle('Forms of Low-income Household')
  


# 2) 담배와 숙면의 관계

# p1005_3aq5 : 현재 흡연 여부
# 1. 피움 2. 피우지 않음
# p1005_3aq6  하루 평균 흡연량(개비)
# p1005_13 : 잠을 설치는 정도
#   1. 극히 드물다.(일주일에 1일 미만)
#   2. 가끔 있었다.(일주일에 1~2일간)
#   3. 종종 있었다.(일주일에 3~4일간)
#   4. 대부분 그랬다.(일주일에 5일 이상)

# 불면증이 심한 사람을 비교
# 4번을 선택한 사람과 담배량 비교

Data2 <- originalData

Data2 <- Data2 %>%
  select(p1005_3aq5, p1005_3aq6, p1005_13) %>%
  filter(p1005_3aq5 %in% c(1,2) & p1005_13 %in% c(1,2,3,4)) %>%
  rename(avg_daily_smoking = p1005_3aq6,
         sleeplessness = p1005_13,
         smoking = p1005_3aq5)

Data2$smoking <- ifelse(Data2$smoking == 1, 'yes', 'no')

table(Data2$smoking)

# 흡연자 Data
smoker_data <- Data2 %>%
  filter(smoking == 'yes')

# 비흡연자 Sampleing
non_smoker <- Data2 %>%
  filter(smoking == 'no')

non_smoker_row <- sample(rownames(non_smoker), dim(non_smoker)[1]*(2110/10728))
non_smoker_data <- non_smoker[non_smoker_row,]

View(non_smoker_data)

# 비흡연자 불면증 정도
ggplot(non_smoker_data) +
  geom_bar(aes(sleeplessness))

# 흡연자 불면증 정도
ggplot(smoker_data) +
  geom_bar(aes(sleeplessness))

# 흡연자 비흡연자 그래프 같이 보기
smoker_nonsmoker_data <- bind_rows(smoker_data, non_smoker_data)

smoker_nonsmoker_data %>%
  group_by(sleeplessness, smoking) %>%
  summarise(count = n()) %>%
  ggplot() +
  geom_col(aes(sleeplessness, count, fill = smoking), position = 'dodge') +
  ggtitle('Smoking and Insomnia')

# 흡연자 담배량에 따른 불면증 정도
smoker_data %>%
  group_by(sleeplessness) %>%
  summarise(mean_smoking = mean(avg_daily_smoking)) %>%
  ggplot() +
  geom_col(aes(sleeplessness, mean_smoking, fill = sleeplessness))



# 3) 최종 학력과 임금과의 상관 관계

# p1002_8aq1 : 일한달의 월 평균 임금(만원)
# p1007_3aq1 : 최종 학력
#   1. 중학교 졸업 이하                      
#   2. 고등학교 중퇴, 졸업  
#   3. 전문대학 재학, 중퇴, 졸업                                          
#   4. 대학교(4년제) 재학, 중퇴, 졸업               
#   5. 대학원 이상"
# p1003_9 : 직업 만족도
#   1. 매우 불만족 2. 대체로 만족 3. 그저그렇다. 4. 대체로 만족. 5. 매우만족

Data3 <- originalData

Data3 <- Data3 %>%
  rename(avgMonthIncome = p1002_8aq1,
         finalEdu= p1007_3aq1) %>%
  filter(!is.na(finalEdu), !is.na(avgMonthIncome)) %>%
  select(avgMonthIncome, finalEdu) %>%
  group_by(finalEdu) %>%
  summarise(avgIncome = mean(avgMonthIncome),
            maxIncome = max(avgMonthIncome),
            minIncome = min(avgMonthIncome))

# 최종학력과 평균 임금
ggplot(Data3) +
  geom_col(aes(finalEdu, avgIncome)) +
  ggtitle('Final education and average wage')

# 최종학력과 최고 임금
ggplot(Data3) +
  geom_line(aes(finalEdu, maxIncome)) +
  ggtitle('Final education and maximum wage')

# 최종학력과 최저 임금
ggplot(Data3) +
  geom_line(aes(finalEdu, minIncome)) +
  ggtitle('Final education and minimum wage')










