---
title: "菲律宾家庭规模影响因素分析"
author: "小组编号：5"
number-sections: true
format: 
  html:
    embed-resources: true
    code-tools: true
  pdf: default
editor_options: 
  chunk_output_type: console
execute:
  echo: true
  eval: false
  warning: false
  message: false
---

```{r}
#| label: libraries
library(tidyverse)
library(ggplot2)
library(GGally)
library(MASS)  # 用于负二项回归
library(car)   # 用于离散度测试
library(pscl)  # 用于计算伪R²
library(boot)  # 用于交叉验证
library(knitr) # 用于表格展示
```

# 引言 {#sec-Intro}

本分析旨在确定影响菲律宾家庭规模的关键家庭相关因素，使用**菲律宾家庭收入与支出调查(FIES)**的数据。数据集包括人口统计和家庭相关变量，如**家庭总收入、地区、食品支出、户主特征（性别和年龄）、家庭类型、房屋面积、房屋年龄、卧室数量和电力获取情况**。

我们采用两步方法进行分析：
1. 首先比较两种适合计数数据的**广义线性模型(GLM)**—**泊松回归**和**负二项回归**，确定哪种模型更适合家庭规模数据
2. 然后使用确定的最佳模型分析哪些因素显著影响家庭规模

计数数据通常表现出过度离散（方差大于均值），泊松回归假设均值等于方差，而负二项回归能够处理过度离散现象。确定最适合的模型后，我们将分析影响家庭规模的最重要预测因素，为政府官员提供制定住房和社会政策的参考。

# 数据处理 {#sec-Data}

```{r}
#| label: data processing
# 加载数据集
df <- read.csv("dataset05.csv", stringsAsFactors = FALSE)

# 数据清洗和预处理
df <- df %>%
  # 将分类变量转换为因子
  mutate(
    Region = as.factor(Region),
    Household.Head.Sex = as.factor(Household.Head.Sex),
    Type.of.Household = as.factor(Type.of.Household),
    Electricity = as.factor(Electricity)
  ) %>%
  # 确保数值变量格式正确
  mutate(
    Total.Household.Income = as.numeric(Total.Household.Income),
    Total.Food.Expenditure = as.numeric(Total.Food.Expenditure),
    Household.Head.Age = as.numeric(Household.Head.Age),
    Total.Number.of.Family.members = as.numeric(Total.Number.of.Family.members),
    House.Floor.Area = as.numeric(House.Floor.Area),
    House.Age = as.numeric(House.Age),
    Number.of.bedrooms = as.numeric(Number.of.bedrooms)
  )

# 检查缺失值
sum(is.na(df))

# 数据结构
str(df)

# 缩放连续预测变量以提高模型收敛性
df_scaled <- df %>%
  mutate(
    Total.Household.Income_scaled = scale(Total.Household.Income),
    Total.Food.Expenditure_scaled = scale(Total.Food.Expenditure),
    Household.Head.Age_scaled = scale(Household.Head.Age),
    House.Floor.Area_scaled = scale(House.Floor.Area),
    House.Age_scaled = scale(House.Age)
  )

# 注意：不再分训练集和测试集，而是使用全部数据进行模型拟合
```

数据分析首先对原始数据进行预处理，包括将分类变量转换为因子、确保数值变量格式正确、缩放连续预测变量以提高模型收敛性。由于我们的目标是先确定最佳模型，然后分析影响因素，我们将使用全部数据进行模型拟合，而不是分训练集和测试集。

# 探索性数据分析 {#sec-EDA}

## 因变量分析 {#sec-Response}

```{r}
#| label: response-variable-analysis
# 因变量的描述性统计
summary(df$Total.Number.of.Family.members)

# 计算均值和方差以检查潜在的过度离散
mean_val <- mean(df$Total.Number.of.Family.members)
var_val <- var(df$Total.Number.of.Family.members)
cat("家庭规模均值:", mean_val, "\n")
cat("家庭规模方差:", var_val, "\n")
cat("方差/均值比率:", var_val/mean_val, "\n")

# 因变量的分布
ggplot(df, aes(x = Total.Number.of.Family.members)) +
  geom_histogram(bins = 15, fill = "skyblue", color = "black") +
  ggtitle("家庭规模分布") +
  xlab("家庭成员数") +
  ylab("频率") +
  theme_minimal()
```

我们首先检查因变量（Total.Number.of.Family.members）的分布。均值和方差分析特别重要，因为它可以指示数据是否表现出过度离散（方差>均值），这将帮助我们初步判断负二项模型是否可能比泊松模型更适合。直方图可视化了数据集中家庭规模的分布特点。

## 预测变量分析 {#sec-Predictors-EDA}

```{r}
#| label: household-size-by-region
# 按地区划分的家庭规模
ggplot(df, aes(x = Region, y = Total.Number.of.Family.members, fill = Region)) + 
  geom_boxplot() +
  ggtitle("按地区划分的家庭规模") +
  xlab("地区") +
  ylab("家庭成员数") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

这个箱线图展示了菲律宾不同地区家庭规模的分布。地区间家庭规模的差异可能受到每个地区特有的文化、经济和社会因素的影响。

```{r}
#| label: household-size-by-head-sex
# 按户主性别划分的家庭规模
ggplot(df, aes(x = Household.Head.Sex, y = Total.Number.of.Family.members, fill = Household.Head.Sex)) + 
  geom_boxplot() +
  ggtitle("按户主性别划分的家庭规模") +
  xlab("户主性别") +
  ylab("家庭成员数") +
  theme_minimal()
```

这个可视化图检查户主性别与家庭成员数量之间是否存在关系。性别角色和关于家庭结构的文化规范可能影响这种关系。

```{r}
#| label: household-size-vs-age
# 家庭规模与户主年龄的关系
ggplot(df, aes(x = Household.Head.Age, y = Total.Number.of.Family.members)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  ggtitle("家庭规模与户主年龄的关系") +
  xlab("户主年龄") +
  ylab("家庭成员数") +
  theme_minimal()
```

这个散点图和平滑趋势线探讨了户主年龄与家庭规模之间的关系。我们可能预期中年户主的家庭规模较大，因为他们更可能有子女住在家中，而年轻和老年户主的家庭规模较小。

```{r}
#| label: household-size-vs-income
# 家庭规模与家庭收入的关系
ggplot(df, aes(x = Total.Household.Income, y = Total.Number.of.Family.members)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  ggtitle("家庭规模与家庭收入的关系") +
  xlab("家庭总收入（菲律宾比索）") +
  ylab("家庭成员数") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)
```

这个可视化图检查了家庭收入与家庭规模之间的关系。经济因素通常影响家庭计划决策和家庭组成。

```{r}
#| label: household-size-vs-floor-area
# 家庭规模与房屋面积的关系
ggplot(df, aes(x = House.Floor.Area, y = Total.Number.of.Family.members)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = TRUE) +
  ggtitle("家庭规模与房屋面积的关系") +
  xlab("房屋面积（平方米）") +
  ylab("家庭成员数") +
  theme_minimal()
```

这个散点图探讨了房屋面积与家庭规模之间的关系。较大的家庭通常需要更多的生活空间，但因果关系可能双向：家庭可能增长以填满可用空间，或者他们可能随着家庭增长而寻求更大的住房。

```{r}
#| label: household-size-vs-bedrooms
# 家庭规模与卧室数量的关系
ggplot(df, aes(x = as.factor(Number.of.bedrooms), y = Total.Number.of.Family.members, fill = as.factor(Number.of.bedrooms))) + 
  geom_boxplot() +
  ggtitle("家庭规模与卧室数量的关系") +
  xlab("卧室数量") +
  ylab("家庭成员数") +
  theme_minimal()
```

这个箱线图展示了家庭规模如何随房屋卧室数量变化。与房屋面积类似，卧室数量可能与家庭规模正相关，因为这关系到空间需求。

```{r}
#| label: household-size-by-type
# 按家庭类型划分的家庭规模
ggplot(df, aes(x = Type.of.Household, y = Total.Number.of.Family.members, fill = Type.of.Household)) + 
  geom_boxplot() +
  ggtitle("按家庭类型划分的家庭规模") +
  xlab("家庭类型") +
  ylab("家庭成员数") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

这个可视化图检查了家庭类型（描述家庭成员之间的关系）如何影响家庭规模。不同的家庭结构（如核心家庭、扩展家庭等）自然会导致不同的家庭规模。

```{r}
#| label: correlation-matrix
# 数值变量的相关矩阵
numeric_vars <- df %>% 
  select(Total.Household.Income, Total.Food.Expenditure, Household.Head.Age, 
         Total.Number.of.Family.members, House.Floor.Area, House.Age, Number.of.bedrooms)

ggcorr(numeric_vars, method = c("pairwise", "pearson"), 
       label = TRUE, label_size = 3,
       hjust = 0.75) +
  ggtitle("数值变量的相关矩阵") +
  theme_minimal()
```

相关矩阵提供了数据集中数值变量之间关系的洞察。强相关可能表明存在多重共线性问题，在模型构建过程中应予以考虑。

# 模型选择 {#sec-ModelSelection}

## 泊松回归模型 {#sec-Poisson}

```{r}
#| label: poisson-model
# 构建完整的泊松回归模型
poisson_model <- glm(Total.Number.of.Family.members ~ 
                   Total.Household.Income_scaled + 
                   Total.Food.Expenditure_scaled + 
                   Household.Head.Sex + 
                   Household.Head.Age_scaled + 
                   Type.of.Household + 
                   House.Floor.Area_scaled + 
                   House.Age_scaled + 
                   Number.of.bedrooms + 
                   Electricity + 
                   Region,
                 data = df_scaled, 
                 family = poisson(link = "log"))

# 完整模型的摘要
summary(poisson_model)

# 执行逐步回归以优化模型
stepwise_poisson <- step(poisson_model, direction = "backward", trace = FALSE)

# 优化模型的摘要
summary(stepwise_poisson)

# 计算AIC和BIC
poisson_aic <- AIC(stepwise_poisson)
poisson_bic <- BIC(stepwise_poisson)
cat("泊松模型AIC:", poisson_aic, "\n")
cat("泊松模型BIC:", poisson_bic, "\n")
```

泊松回归通常用于建模计数数据。我们首先构建包含所有预测变量的完整模型，然后使用后向逐步回归消除那些对解释家庭规模没有显著贡献的变量。优化过程使用赤池信息准则(AIC)来确定模型中保留哪些变量，平衡模型拟合度和复杂性。

## 泊松模型检验 {#sec-Poisson-Diagnostics}

```{r}
#| label: poisson-dispersion-test
# 测试泊松模型的过度离散
dispersiontest(stepwise_poisson)

# 检查过度离散的另一种方法
residual_deviance <- stepwise_poisson$deviance
degrees_freedom <- stepwise_poisson$df.residual
dispersion_parameter <- residual_deviance / degrees_freedom
cat("残差偏差:", residual_deviance, "\n")
cat("自由度:", degrees_freedom, "\n")
cat("离散参数:", dispersion_parameter, "\n")
```

泊松回归的一个关键假设是均值等于方差（均匀离散）。我们使用正式的离散度测试和计算离散参数（残差偏差/自由度）来检验这一假设。离散参数明显大于1表明存在过度离散，这将表明负二项模型可能更合适。

## 负二项回归模型 {#sec-NegBin}

```{r}
#| label: negative-binomial-model
# 构建完整的负二项回归模型
nb_model <- glm.nb(Total.Number.of.Family.members ~ 
                    Total.Household.Income_scaled + 
                    Total.Food.Expenditure_scaled + 
                    Household.Head.Sex + 
                    Household.Head.Age_scaled + 
                    Type.of.Household + 
                    House.Floor.Area_scaled + 
                    House.Age_scaled + 
                    Number.of.bedrooms + 
                    Electricity + 
                    Region,
                  data = df_scaled)

# 完整模型的摘要
summary(nb_model)

# 执行逐步回归以优化模型
stepwise_nb <- step(nb_model, direction = "backward", trace = FALSE)

# 优化模型的摘要
summary(stepwise_nb)

# 计算AIC和BIC
nb_aic <- AIC(stepwise_nb)
nb_bic <- BIC(stepwise_nb)
cat("负二项模型AIC:", nb_aic, "\n")
cat("负二项模型BIC:", nb_bic, "\n")

# theta值（离散参数）
cat("Theta:", stepwise_nb$theta, "\n")
cat("Theta的标准误:", stepwise_nb$SE.theta, "\n")
```

负二项回归是泊松回归的扩展，通过包含一个额外参数(theta)来建模额外的变异，从而允许过度离散。与泊松模型的方法类似，我们应用后向逐步回归来识别最显著的预测变量。theta参数提供了关于数据中过度离散程度的洞察。

## 模型诊断图 {#sec-Diagnostics}

```{r}
#| label: diagnostic-plots
# 泊松模型诊断图
poisson_fitted <- fitted(stepwise_poisson)
poisson_residuals <- residuals(stepwise_poisson, type = "pearson")
poisson_residual_data <- data.frame(Fitted = poisson_fitted, Residuals = poisson_residuals)

p1 <- ggplot(poisson_residual_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("泊松模型：残差与拟合值") +
  xlab("拟合值") +
  ylab("皮尔逊残差") +
  theme_minimal()

# 负二项模型诊断图
nb_fitted <- fitted(stepwise_nb)
nb_residuals <- residuals(stepwise_nb, type = "pearson")
nb_residual_data <- data.frame(Fitted = nb_fitted, Residuals = nb_residuals)

p2 <- ggplot(nb_residual_data, aes(x = Fitted, y = Residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("负二项模型：残差与拟合值") +
  xlab("拟合值") +
  ylab("皮尔逊残差") +
  theme_minimal()

# 显示两个图
gridExtra::grid.arrange(p1, p2, ncol = 2)
```

诊断图帮助我们评估两个模型的拟合质量。残差与拟合值的关系图可以显示潜在的模型问题，如非线性关系、异方差性或极端值的影响。比较两个模型的诊断图可以帮助我们判断哪个模型更好地符合数据特性。

## 交叉验证比较 {#sec-CV}

```{r}
#| label: cross-validation
# 执行k折交叉验证
set.seed(5555)
K <- 5  # 折数
folds <- createFolds(df$Total.Number.of.Family.members, k = K, list = TRUE, returnTrain = FALSE)

# 初始化向量以存储性能指标
cv_poisson_mae <- numeric(K)
cv_poisson_rmse <- numeric(K)
cv_nb_mae <- numeric(K)
cv_nb_rmse <- numeric(K)

for (i in 1:K) {
  # 将数据分为训练集和验证集
  cv_test_indices <- folds[[i]]
  cv_train <- df[-cv_test_indices, ]
  cv_test <- df[cv_test_indices, ]
  
  # 缩放连续变量
  cv_train_scaled <- cv_train %>%
    mutate(
      Total.Household.Income_scaled = scale(Total.Household.Income),
      Total.Food.Expenditure_scaled = scale(Total.Food.Expenditure),
      Household.Head.Age_scaled = scale(Household.Head.Age),
      House.Floor.Area_scaled = scale(House.Floor.Area),
      House.Age_scaled = scale(House.Age)
    )
  
  # 使用训练集的均值和标准差来缩放测试数据
  income_mean <- mean(cv_train$Total.Household.Income)
  income_sd <- sd(cv_train$Total.Household.Income)
  food_mean <- mean(cv_train$Total.Food.Expenditure)
  food_sd <- sd(cv_train$Total.Food.Expenditure)
  age_mean <- mean(cv_train$Household.Head.Age)
  age_sd <- sd(cv_train$Household.Head.Age)
  area_mean <- mean(cv_train$House.Floor.Area)
  area_sd <- sd(cv_train$House.Floor.Area)
  house_age_mean <- mean(cv_train$House.Age)
  house_age_sd <- sd(cv_train$House.Age)
  
  cv_test_scaled <- cv_test %>%
    mutate(
      Total.Household.Income_scaled = (Total.Household.Income - income_mean) / income_sd,
      Total.Food.Expenditure_scaled = (Total.Food.Expenditure - food_mean) / food_sd,
      Household.Head.Age_scaled = (Household.Head.Age - age_mean) / age_sd,
      House.Floor.Area_scaled = (House.Floor.Area - area_mean) / area_sd,
      House.Age_scaled = (House.Age - house_age_mean) / house_age_sd
    )
  
  # 拟合泊松模型
  cv_poisson <- glm(Total.Number.of.Family.members ~ 
                     Total.Household.Income_scaled + 
                     Total.Food.Expenditure_scaled + 
                     Household.Head.Sex + 
                     Household.Head.Age_scaled + 
                     Type.of.Household + 
                     House.Floor.Area_scaled + 
                     House.Age_scaled + 
                     Number.of.bedrooms + 
                     Electricity + 
                     Region,
                   data = cv_train_scaled, 
                   family = poisson(link = "log"))
  
  # 拟合负二项模型
  cv_nb <- glm.nb(Total.Number.of.Family.members ~ 
                   Total.Household.Income_scaled + 
                   Total.Food.Expenditure_scaled + 
                   Household.Head.Sex + 
                   Household.Head.Age_scaled + 
                   Type.of.Household + 
                   House.Floor.Area_scaled + 
                   House.Age_scaled + 
                   Number.of.bedrooms + 
                   Electricity + 
                   Region,
                 data = cv_train_scaled)
  
  # 进行预测
  cv_poisson_pred <- predict(cv_poisson, newdata = cv_test_scaled, type = "response")
  cv_nb_pred <- predict(cv_nb, newdata = cv_test_scaled, type = "response")
  
  # 计算性能指标
  cv_poisson_mae[i] <- mean(abs(cv_test$Total.Number.of.Family.members - cv_poisson_pred))
  cv_poisson_rmse[i] <- sqrt(mean((cv_test$Total.Number.of.Family.members - cv_poisson_pred)^2))
  
  cv_nb_mae[i] <- mean(abs(cv_test$Total.Number.of.Family.members - cv_nb_pred))
  cv_nb_rmse[i] <- sqrt(mean((cv_test$Total.Number.of.Family.members - cv_nb_pred)^2))
}

# 各折的平均性能指标
mean_cv_poisson_mae <- mean(cv_poisson_mae)
mean_cv_poisson_rmse <- mean(cv_poisson_rmse)
mean_cv_nb_mae <- mean(cv_nb_mae)
mean_cv_nb_rmse <- mean(cv_nb_rmse)

# 显示交叉验证结果
cv_results <- data.frame(
  Model = c("泊松", "负二项"),
  MAE = c(mean_cv_poisson_mae, mean_cv_nb_mae),
  RMSE = c(mean_cv_poisson_rmse, mean_cv_nb_rmse)
)

kable(cv_results, 
     caption = "交叉验证结果（5折平均）",
     digits = 4)
```

虽然我们不需要固定的训练集和测试集进行模型选择，但交叉验证为我们提供了一种评估模型预测性能和稳定性的方法。我们使用5折交叉验证，将数据分成5份，每次使用4份训练模型，1份验证性能。这个过程重复5次，每次使用不同的验证集，然后计算平均性能指标。这种方法可以更全面地评估每个模型的泛化能力。

## 模型比较与选择 {#sec-Model-Comparison}

```{r}
#| label: model-comparison
# 创建比较表
model_comparison <- data.frame(
  Model = c("泊松", "负二项"),
  AIC = c(poisson_aic, nb_aic),
  BIC = c(poisson_bic, nb_bic),
  CV_MAE = c(mean_cv_poisson_mae, mean_cv_nb_mae),
  CV_RMSE = c(mean_cv_poisson_rmse, mean_cv_nb_rmse)
)

# 打印比较表
kable(model_comparison, 
     caption = "模型性能比较",
     digits = 4)

# 似然比检验
# 注意：此检验仅在模型嵌套时有效，泊松和负二项确实是嵌套的
# (泊松是负二项的特例，离散参数接近无穷大)
if (requireNamespace("lmtest", quietly = TRUE)) {
  lrt <- lmtest::lrtest(stepwise_poisson, stepwise_nb)
  print(lrt)
}

# 确定最佳模型
is_nb_better <- nb_aic < poisson_aic && mean_cv_nb_rmse < mean_cv_poisson_rmse
best_model <- if(is_nb_better) stepwise_nb else stepwise_poisson
best_model_name <- if(is_nb_better) "负二项" else "泊松"

cat("基于AIC、BIC和交叉验证结果，", best_model_name, "模型更适合这个数据集。\n")
```

我们基于以下标准比较两个模型：
1. 信息准则（AIC和BIC）：较低的值表示更好的模型
2. 交叉验证性能（MAE和RMSE）：较低的值表示更准确的预测
3. 似然比检验：评估负二项模型是否比泊松模型提供显著改进

根据这些比较结果，我们确定哪个模型更适合家庭规模数据，然后在下一节中使用该模型进行最终分析。

# 最佳模型分析 {#sec-BestModel}

```{r}
#| label: best-model-analysis
# 提取最佳模型的摘要信息
best_model_summary <- summary(best_model)

# 创建系数表
coefficients_table <- best_model_summary$coefficients
coef_df <- data.frame(
  变量 = rownames(coefficients_table),
  系数 = coefficients_table[, "Estimate"],
  标准误 = coefficients_table[, "Std. Error"],
  z值 = coefficients_table[, "z value"],
  p值 = coefficients_table[, "Pr(>|z|)"]
)

# 按系数绝对值排序
coef_df <- coef_df %>%
  mutate(系数绝对值 = abs(系数)) %>%
  arrange(desc(系数绝对值))

# 打印系数表
kable(coef_df, 
     caption = paste(best_model_name, "模型的系数"),
     digits = 4)

# 提取显著预测变量（p < 0.05）
significant_predictors <- coef_df %>%
  filter(p值 < 0.05 & 变量 != "(Intercept)") %>%
  mutate(
    变量 = fct_reorder(变量, 系数绝对值),
    方向 = ifelse(系数 > 0, "正向", "负向")
  )

# 可视化显著影响因素
ggplot(significant_predictors, aes(x = 变量, y = 系数绝对值, fill = 方向)) +
  geom_col() +
  coord_flip() +
  ggtitle(paste(best_model_name, "模型：影响家庭规模的显著因素")) +
  xlab("预测因素") +
  ylab("影响强度（系数绝对值）") +
  theme_minimal() +
  scale_fill_manual(values = c("正向" = "blue", "负向" = "red"))
```

使用选定的最佳模型，我们分析影响家庭规模的关键因素。系数表显示了每个变量对家庭规模的影响方向和强度，p值指示这种影响的统计显著性。负二项模型的系数解释与泊松模型类似，但它考虑了数据中的过度离散，因此可能提供更准确的估计。

对于分类变量（如地区、户主性别、家庭类型），系数表示相对于参考类别的差异。对于连续变量（如收入、年龄、房屋面积），系数表示变量增加一个单位时，家庭规模的预期变化（对数尺度上）。

# 系数解释与边际效应 {#sec-Interpretation}

```{r}
#| label: marginal-effects
# 创建一个函数来计算某一变量的边际效应
calculate_marginal_effect <- function(model, data, variable) {
  # 创建预测数据集
  pred_data <- data.frame(data[1, ])  # 使用第一行作为基础
  
  # 如果是连续变量，创建一系列值
  if (is.numeric(data[[variable]])) {
    values <- seq(min(data[[variable]], na.rm = TRUE), 
                 max(data[[variable]], na.rm = TRUE), 
                 length.out = 100)
    pred_data <- pred_data[rep(1, length(values)), ]
    pred_data[[variable]] <- values
  } else {
    # 如果是分类变量，使用所有可能的水平
    levels <- unique(data[[variable]])
    pred_data <- pred_data[rep(1, length(levels)), ]
    pred_data[[variable]] <- levels
  }
  
  # 预测
  pred_data$predicted <- predict(model, newdata = pred_data, type = "response")
  
  return(pred_data)
}

# 选择几个关键变量进行边际效应分析
key_variables <- c("Total.Household.Income_scaled", "House.Floor.Area_scaled", "Number.of.bedrooms")

# 生成边际效应图
par(mfrow = c(2, 2))
for (var in key_variables) {
  me_data <- calculate_marginal_effect(best_model, df_scaled, var)
  plot(me_data[[var]], me_data$predicted, type = "l",
       xlab = var, ylab = "预测家庭规模",
       main = paste("变量", var, "的边际效应"))
}
par(mfrow = c(1, 1))

# 对分类变量计算预测概率
# 这里以家庭类型为例
if ("Type.of.Household" %in% names(df_scaled)) {
  type_data <- calculate_marginal_effect(best_model, df_scaled, "Type.of.Household")
  
  ggplot(type_data, aes(x = Type.of.Household, y = predicted, fill = Type.of.Household)) +
    geom_bar(stat = "identity") +
    ggtitle("不同家庭类型的预测家庭规模") +
    xlab("家庭类型") +
    ylab("预测家庭规模") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
```

为了更直观地理解预测变量对家庭规模的影响，我们计算并可视化了关键变量的边际效应。边际效应显示了当一个变量变化时，预测的家庭规模如何变化，同时保持其他变量不变。这种分析帮助我们理解每个因素的实际影响大小，超越了仅通过系数值提供的信息。

# 结论 {#sec-Conc}

基于我们对菲律宾家庭规模影响因素的综合分析，我们得出以下关键结论：

1. **模型选择**：通过比较泊松回归和负二项回归，我们发现 [根据实际结果填写] 模型更适合分析家庭规模数据。这一结论基于AIC、BIC、交叉验证性能和似然比检验。[如果负二项模型更好，可以添加：数据显示明显的过度离散现象，方差/均值比率大于1，支持使用负二项模型。]

2. **家庭规模的关键决定因素**（按重要性排序）：
   - **家庭类型**：家庭内部的关系结构（如核心家庭、扩展家庭）是家庭规模的重要预测因素。[根据模型结果具体描述影响]
   - **卧室数量**：与家庭规模呈显著正相关，反映了住房条件与家庭组成的密切关系。
   - **房屋面积**：较大的住房往往对应更大的家庭规模，表明空间需求与家庭规模相互关联。
   - **家庭总收入**：[根据模型结果描述收入与家庭规模的关系]
   - **户主年龄**：[具体描述年龄效应，如年龄与家庭规模的曲线关系]
   - **户主性别**：[描述性别差异，如果显著]
   - **地区差异**：[描述地区间家庭规模的差异]

3. **实际影响分析**：
   - 边际效应分析显示，[描述最重要变量的边际效应，如"卧室数量每增加一个，预期家庭规模增加约X人"]
   - [描述其他重要发现，如不同类型家庭的预期规模差异]

4. **政策启示**：
   - 本研究结果对菲律宾政府的住房政策有重要启示，包括住房规划和设计应考虑不同家庭类型和规模的需求。
   - 地区差异表明政策制定应考虑地区特性，不同地区可能需要不同的住房和社会服务策略。
   - [其他政策建议]

这些发现为政府官员和政策制定者提供了关于菲律宾家庭规模决定因素的重要洞察。理解这些因素有助于制定更有针对性的住房政策、城市规划和社会服务，以更好地满足不同类型家庭的需求。
