# 安装并加载必要的R包
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

# 数据
experiment_data <- data.frame(
  Participant = rep(1:15, each=3),
  Environment = rep(c('Simple', 'Medium', 'Complex'), 15),
  ResponseTime = c(10, 15, 30, 12, 18, 35, 8, 20, 25, 9, 16, 28, 11, 19, 32, 
                   10, 17, 29, 12, 18, 35, 9, 16, 28, 11, 20, 30, 10, 15, 28, 
                   12, 19, 32, 8, 17, 29, 9, 18, 35, 11, 16, 30, 10, 20, 28),
  Success = rep(c('Success', 'Success', 'Failure'), 15),
  ControlTime = c(35, 38, 42, 36, 40, 45, 34, 42, 48, 35, 39, 44, 35, 41, 46, 36, 39, 45, 
                  36, 40, 45, 34, 37, 42, 35, 41, 44, 36, 38, 43, 35, 40, 45, 34, 39, 44, 
                  35, 38, 43, 36, 40, 45, 36, 41, 43),
  ControlEfficiency = c(8, 7, 5, 8, 6, 4, 9, 5, 6, 8, 7, 5, 8, 6, 4, 9, 7, 5, 
                        8, 6, 4, 9, 7, 5, 8, 6, 4, 9, 7, 5, 8, 6, 4, 9, 7, 5, 
                        8, 6, 4, 9, 7, 5, 8, 6, 4)
)

# 问卷调查数据
survey_data <- data.frame(
  Participant = 1:15,
  InterfaceUsability = c(8, 7, 9, 8, 7, 9, 8, 9, 7, 8, 7, 9, 8, 7, 9),
  ControlDifficulty = c(7, 6, 5, 7, 6, 7, 6, 7, 6, 7, 5, 6, 7, 6, 7),
  OverallSatisfaction = c(8, 7, 8, 8, 7, 9, 8, 8, 7, 8, 7, 8, 8, 7, 9)
)

# 数据预处理
experiment_data$Environment <- factor(experiment_data$Environment)
experiment_data$Success <- factor(experiment_data$Success)
survey_data$Participant <- factor(survey_data$Participant)

# 描述性统计分析
cat("数据的描述性统计分析结果：\n")
print(summary(experiment_data))
print(summary(survey_data))

# 反应时间和成功率的柱状图
ggplot(experiment_data, aes(x=Environment, y=ResponseTime, fill=Success)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="反应时间和成功率", x="环境", y="反应时间(秒)") +
  theme_minimal()

# 生成反应时间的箱线图
ggplot(experiment_data, aes(x=Environment, y=ResponseTime, fill=Environment)) +
  geom_boxplot() +
  labs(title="不同环境下反应时间的箱线图", x="环境", y="反应时间(秒)") +
  theme_minimal()

# 成功和失败的频数统计
cat("成功和失败的频数统计结果：\n")
print(table(experiment_data$Success))

# 成功率和环境的关系
success_rate <- prop.table(table(experiment_data$Environment, experiment_data$Success), margin=1)
cat("不同环境下的成功率：\n")
print(success_rate)

# 成功率的柱状图
ggplot(experiment_data, aes(x=Environment, fill=Success)) +
  geom_bar(position="fill") +
  labs(title="不同环境下的成功率", x="环境", y="比例") +
  scale_y_continuous(labels=scales::percent) +
  theme_minimal()

# 问卷调查结果的柱状图
ggplot(survey_data, aes(x=Participant, y=InterfaceUsability)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="界面易用性评分", x="参与者", y="评分") +
  theme_minimal()

ggplot(survey_data, aes(x=Participant, y=ControlDifficulty)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="控制难度评分", x="参与者", y="评分") +
  theme_minimal()

ggplot(survey_data, aes(x=Participant, y=OverallSatisfaction)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(title="总体满意度评分", x="参与者", y="评分") +
  theme_minimal()

# 卡方检验
response_table <- xtabs(~ Success + Environment, data=experiment_data)
cat("卡方检验结果：\n")
print(chisq.test(response_table))

