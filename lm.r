data_alfa <- read.csv("alfa_BPT_ERD.csv")
data_beta <- read.csv("beta_BPT_ERD.csv")
data_alfa <- read.csv("EVK_alfa_BPT_ERD.csv")
data_beta <- read.csv("EVK_beta_BPT_ERD.csv")
colnames(data_alfa)
colnames(data_beta)
library(lme4)
library(car)
library(lmerTest)


M1_a <- lmer(Mean_ERD ~ Condition + (1|Subject), data = data_alfa,REML = FALSE)

M2_a <- lmer(
  formula = Mean_ERD ~ Mean_traits + Condition + (1|Subject),
  data = data_alfa,
  REML = FALSE
)

M3_a <- lmer(
  formula = Mean_ERD ~ Mean_traits* Condition + (1|Subject),
  data = data_alfa,
  REML = FALSE
)



M1_b <- lmer(Mean_ERD ~ Condition + (1|Subject), data=data_beta, REML = FALSE)

M2_b <- lmer(
  formula = Mean_ERD ~ Mean_traits + Condition + (1|Subject),
  data = data_beta,
  REML = FALSE
)

M3_b <- lmer(
  formula = Mean_ERD ~ Mean_traits* Condition + (1|Subject),
  data = data_beta,
  REML = FALSE
)




summary(M2)
summary(M3)
summary(M4)



anova(M1_a,M2_a)
anova(M1_a,M3_a)
anova(M2_a,M3_a)

anova(M1_b,M2_b)
anova(M1_b,M3_b)
anova(M2_b,M3_b)


summary(M1_a)
summary(M1_b)
anova(M1_a)
anova(M1_b)


summary(M2)
summary(M3)
summary(M4)

