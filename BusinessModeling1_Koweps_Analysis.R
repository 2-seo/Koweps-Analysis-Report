library(foreign)
library(dplyr)
library(ggplot2)

###############################################################################
###############################################################################

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

# 저소득 가구의 형태 그래프
ggplot(Result1, aes(reorder(form, -count), count, fill = form)) +
  geom_col() +
  xlab('Family Form') + ylab('Count') +
  ggtitle('Forms of Low-income Household')
  
###############################################################################
###############################################################################

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

smoker_data$sleeplessness <- ifelse(smoker_data$sleeplessness == 1, 'A',
                                    ifelse(smoker_data$sleeplessness == 2, 'B',
                                           ifelse(smoker_data$sleeplessness == 3, 'C', 'D')))


# 비흡연자 Sampling
non_smoker <- Data2 %>%
  filter(smoking == 'no')

non_smoker_row <- sample(rownames(non_smoker), dim(non_smoker)[1]*(2110/10728))
non_smoker_data <- non_smoker[non_smoker_row,]

non_smoker_data$sleeplessness <- ifelse(non_smoker_data$sleeplessness == 1, 'A',
                                        ifelse(non_smoker_data$sleeplessness == 2, 'B',
                                               ifelse(non_smoker_data$sleeplessness == 3, 'C', 'D')))

View(non_smoker_data)

# 비흡연자 불면증 정도
non_smoker_data %>%
  group_by(sleeplessness) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = sleeplessness, y = Count, fill = sleeplessness, label = Count)) +
  geom_col() +
  geom_text(vjust = -0.2, color = 'black') +
  ggtitle('The degree to which non-smokers sleep') +
  xlab('Degree of insomnia') + ylab('Count') +
  scale_fill_manual(values = c( "#FFCC00", "#FF9900", 
                                "#FF6600", "#FF3300")) +
  theme(plot.title = element_text(size=18), legend.position = 'bottom')

# 흡연자 불면증 정도
smoker_data %>%
  group_by(sleeplessness) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = sleeplessness, y = Count, fill = sleeplessness, label = Count)) +
  geom_col() +
  geom_text(vjust = -0.2, color = 'black') +
  ggtitle('The degree to which smokers sleep') +
  xlab('Degree of insomnia') + ylab('Count') +
  scale_fill_manual(values = c( "#FFCC00", "#FF9900", 
                                "#FF6600", "#FF3300")) +
  theme(plot.title = element_text(size=18), legend.position = 'bottom')

# 흡연자 비흡연자 그래프 같이 보기
smoker_nonsmoker_data <- bind_rows(smoker_data, non_smoker_data)

smoker_nonsmoker_data %>%
  group_by(sleeplessness, smoking) %>%
  summarise(count = n()) %>%
  ggplot(aes(sleeplessness, count, fill = smoking, label = count)) +
  geom_col(position = 'dodge') +
  geom_text(vjust = 1.5, color = 'black', position = position_dodge(width = 1)) +
  ggtitle('Smoker and Non-smoker') +
  xlab('Degree of insomnia') + ylab('Count') +
  theme(plot.title = element_text(size=18), legend.position = 'bottom')

# 흡연자 담배량에 따른 불면증 정도
smoker_data %>%
  group_by(sleeplessness) %>%
  summarise(mean_smoking = round(mean(avg_daily_smoking), 2)) %>%
  ggplot(aes(sleeplessness, mean_smoking, fill = sleeplessness, label = mean_smoking)) +
  geom_col() +
  geom_text(vjust = -0.2, color = 'black') +
  ggtitle('Average amount of smoking and insomnia') +
  scale_fill_manual(values = c( "#FFCC00", "#FF9900", 
                                "#FF6600", "#FF3300")) +
  theme(plot.title = element_text(size=18), legend.position = 'bottom')


###############################################################################
###############################################################################

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

Data3_Income <- Data3 %>%
  rename(avgMonthIncome = p1002_8aq1,
         finalEdu= p1007_3aq1) %>%
  filter(!is.na(finalEdu), !is.na(avgMonthIncome)) %>%
  select(avgMonthIncome, finalEdu) %>%
  group_by(finalEdu) %>%
  summarise(avgIncome = round(mean(avgMonthIncome), 2),
            maxIncome = max(avgMonthIncome),
            minIncome = min(avgMonthIncome))

Data3_Income$finalEdu <- ifelse(Data3_Income$finalEdu == 5, 'Very High',
                                ifelse(Data3_Income$finalEdu == 4, 'High',
                                       ifelse(Data3_Income$finalEdu == 3, 'Middle',
                                              ifelse(Data3_Income$finalEdu == 2, 'Low', 'Very Low'))))

View(Data3_Income)

# 최종학력과 평균 임금

Data3_Income %>%
ggplot(aes(reorder(finalEdu, avgIncome), avgIncome, label = avgIncome)) +
  geom_col(aes(fill = finalEdu)) +
  geom_text(vjust = -0.2, color = 'black') +
  ggtitle('Final education and average wage') +
  xlab('Final Education') + ylab('Average Income(Unit: 10,000won)') +
  theme(plot.title = element_text(size=18), legend.position = 'bottom')

# 최종학력과 최고 임금
Data3_Income %>%
ggplot(aes(reorder(finalEdu, maxIncome), maxIncome, label = maxIncome)) +
  geom_col(aes(fill = finalEdu)) +
  geom_text(vjust = -0.2, color = 'black') +
  ggtitle('Final education and maximum wage') +
  xlab('Final Education') + ylab('Highest Wage(Unit: 10,000won)') +
  theme(plot.title = element_text(size=18), legend.position = 'bottom')

# 최종학력과 최저 임금
Data3_Income %>%
ggplot(aes(reorder(finalEdu, minIncome), minIncome, label = minIncome)) +
  geom_col(aes(fill = finalEdu)) +
  geom_text(vjust = -0.2, color = 'black') +
  ggtitle('Final education and minimum wage') +
  xlab('Final Education') + ylab('Lowest Wage(Unit: 10,000won)') +
  theme(plot.title = element_text(size=18), legend.position = 'bottom')

# 직업 만족도
job_Satisfaction <- Data3 %>%
  select(p1007_3aq1, p1003_9) %>%
  rename(finalEdu = p1007_3aq1,
         Satisfaction = p1003_9) %>%
  filter(!is.na(finalEdu) & !is.na(Satisfaction)) %>%
  group_by(finalEdu, Satisfaction) %>%
  summarise(Count = n())

View(job_Satisfaction)

table(job_Satisfaction$Satisfaction)

job_Satisfaction$finalEdu <- ifelse(job_Satisfaction$finalEdu == 5, 'Very High',
                                    ifelse(job_Satisfaction$finalEdu == 4, 'High',
                                           ifelse(job_Satisfaction$finalEdu == 3, 'Middle',
                                                  ifelse(job_Satisfaction$finalEdu == 2, 'Low', 'Very Low'))))

job_Satisfaction$Satisfaction <- ifelse(job_Satisfaction$Satisfaction == 5, 'A',
                                    ifelse(job_Satisfaction$Satisfaction == 4, 'B',
                                           ifelse(job_Satisfaction$Satisfaction == 3, 'C',
                                                  ifelse(job_Satisfaction$Satisfaction == 2, 'D', 'E'))))


job_Satisfaction %>%
  ggplot(aes(finalEdu, Count, fill = Satisfaction, label = Count)) +
  geom_col(position = 'dodge2') +
  geom_text(vjust = -0.2, color = 'black', position = position_dodge(width = 1)) +
  xlab('Final Education') + ylab('Count') +
  ggtitle('Job Satisfaction by Final Education') +
  theme(plot.title = element_text(size=18), legend.position = 'bottom') +
  scale_fill_manual(values = c( "#18bfe9", "#7595eb", "#c371ed", 
                                "#db62a9", "#f4505e"))

job_Satisfaction <- job_Satisfaction %>%
  group_by(finalEdu) %>%
  mutate(Total_People = sum(Count))

job_Satisfaction <- job_Satisfaction %>%
  mutate(Rate = round(Count/Total_People, 2))

job_Satisfaction  
  
# 만족도 비율 보기
install.packages('ggrepel')
library('ggrepel')

job_Satisfaction %>%
  ggplot(aes(reorder(finalEdu, -Rate), Rate, fill = Satisfaction, label = Rate)) +
  geom_col(position = 'dodge2') +
  geom_text_repel(hjust = 0.5, vjust = -0.5, position = position_dodge(width = 1)) +
  xlab('Final Education') + ylab('Rate') +
  ggtitle('Percentage of job satisfaction according to final education') +
  theme(plot.title = element_text(size=13), legend.position = 'bottom') +
  scale_fill_manual(values = c( "#18bfe9", "#7595eb", "#c371ed", 
                                "#db62a9", "#f4505e"))

  
  









