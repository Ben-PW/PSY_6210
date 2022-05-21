################################Advanced Stats Assessment Code ##########################################

library(here)
load(here("Data", "Assessment_Data.RData"))
#################################### Question 1
###### Multivariate normality
require(MVN)
test <- mvn(GamingAptitude[,c('ScreenTime','Extraversion','Interest')], 
            mvnTest = 'royston')
test$multivariateNormality

###### Check for multicolinearity 

cor(GamingAptitude[,c(1,4,5)], use="complete.obs")
# Nothing particularly egregious standing out

###### Check for linearity of relationship

plot(GamingAptitude$ScreenTime, GamingAptitude$Interest)
plot(GamingAptitude$Extraversion, GamingAptitude$Interest)
# Again nothing standing out

###### Specify models according to brief

lmfunc <- function(formula){
  a <- lm(data = GamingAptitude, formula = formula)
  return(a)
}

q1lm1 <- lmfunc(formula = Interest ~ ScreenTime)
q1lm2 <- lmfunc(formula = Interest ~ ScreenTime + Sex)
q1lm3 <- lmfunc(formula = Interest ~ ScreenTime + Sex + SES)
q1lm4 <- lmfunc(formula = Interest ~ ScreenTime + Sex + SES + Extraversion)

###### Inspect output of models

summary(q1lm1)
summary(q1lm2)
summary(q1lm3)
summary(q1lm4)

###### Determine difference between models, check with ANOVA

anova(q1lm1, q1lm2, q1lm3, q1lm4)
# 4 is confirmed best in this case

rm(q1lm1, q1lm2, q1lm3)

###### Lets check each level of SES is significantly higher than the other
emmeans::emmeans(q1lm4, pairwise~SES)

###### Inspect lm4 for issues

car::qqPlot(lm(data = GamingAptitude, formula = Interest ~ ScreenTime + Sex + SES + Extraversion))
car::residualPlot(q1lm4, type = "rstudent")
car::influenceIndexPlot(q1lm4, vars = c("Cook", "studentized"))
car::influenceIndexPlot(q1lm4, vars = c("Bonf", "hat"))
stats::acf(resid(q1lm4))
# 366 and 582 are problematic on multiple metrics

###### Rerun lm4 without outliers
q1lm5 <- lm(data = GamingAptitude,
            formula = Interest ~ ScreenTime + Sex + SES + Extraversion,
            subset = -c(366))
summary(q1lm4)
summary(q1lm5)
# Seems to improve model but not by much

#Re run residuals checks
car::qqPlot(lm(data = GamingAptitude, formula = Interest ~ ScreenTime + Sex + SES + Extraversion, subset = -366))
car::residualPlot(q1lm5, type = "rstudent")
car::influenceIndexPlot(q1lm5, vars = c("Cook", "studentized"))
car::influenceIndexPlot(q1lm5, vars = c("Bonf", "hat"))
stats::acf(resid(q1lm5))


################################# Question 2

###### Check assumptions of CFA
# MVN
require(MVN)
test <- mvn(GamingAptitude[,c('ScreenTime','Extraversion','Interest','Task','GlobalMovement','Strategy',
                              'TaskSwitch','StroopTask','UpdatingTask','Knowledge','TimeSpent')], 
            mvnTest = 'royston')
test$multivariateNormality

# UVN
p1 <- plot(density(GamingAptitude$Strategy), main='')
p2 <- plot(density(GamingAptitude$Task), main='')
p3 <- plot(density(GamingAptitude$Character), main='')
p4 <- plot(density(GamingAptitude$GlobalMovement), main='')
p5 <- plot(density(GamingAptitude$Knowledge), main='')
p6 <- plot(density(GamingAptitude$TaskSwitch), main='')
p7 <- plot(density(GamingAptitude$StroopTask), main='')
p8 <- plot(density(GamingAptitude$UpdatingTask), main='')
p9 <- plot(density(GamingAptitude$TimeSpent), main='')


###### Specify 3 factor Confirmatory Factor Analysis model
## Model is 3 factor standardised variable scaling

library(lavaan)

# NA*: disables marker variables (TRUE by default)
# var ~~ 1*var: z-scores latent variables (standardised)
# item ~~ 1: adds intercept

threef1 <- '
  metacog =~ NA*Strategy + Task + Character + GlobalMovement
  workmem =~ NA*TaskSwitch + StroopTask + UpdatingTask
  crystalab =~ NA*Knowledge + TimeSpent 

  metacog ~~ 1*metacog
  workmem ~~ 1*workmem
  crystalab ~~ 1*crystalab

  Strategy ~ 1
  Task ~ 1
  Character ~ 1
  GlobalMovement ~ 1
  TaskSwitch ~ 1
  StroopTask ~ 1
  UpdatingTask ~ 1
  Knowledge ~ 1
  TimeSpent ~ 1
'

# categorise threef1 as cfa object named fit3f
fit3f <- cfa(threef1, data = GamingAptitude)

summary(fit3f, 
        fit.measures = T, 
        standardized = TRUE)

###### Specify 2 factor model

# NA* disables marker variables (TRUE by default)
# var ~~ 1*var z-scores latent variables (standardised)
# item ~~ 1 adds intercept

twof1 <- '
metacog =~ NA*Strategy + Task + Character + GlobalMovement + Knowledge
workmem =~ NA*TaskSwitch + StroopTask + UpdatingTask + TimeSpent

metacog ~~ 1*metacog
workmem ~~ 1*workmem

Strategy ~ 1
Task ~ 1
Character ~ 1
GlobalMovement ~ 1
Knowledge ~ 1
TaskSwitch ~ 1
StroopTask ~ 1
UpdatingTask ~ 1
TimeSpent ~ 1
'

fit2f <- cfa(twof1, data = GamingAptitude)

summary(fit2f, 
        fit.measures = T, 
        standardized = TRUE)

###### Do modification indices suggest different options?

library(dplyr)

modificationindices(fit3f) %>%
  as_data_frame() %>%
  arrange(-mi) %>%
  filter(mi > 11) %>%
  select(lhs, op, rhs, mi, epc)
# Adding GlobalMovement to crystalab seems advisable

###### Respecify fit3f

threef2 <- '
  metacog =~ NA*Strategy + Task + Character 
  workmem =~ NA*TaskSwitch + StroopTask + UpdatingTask
  crystalab =~ NA*Knowledge + TimeSpent + GlobalMovement

  metacog ~~ 1*metacog
  workmem ~~ 1*workmem
  crystalab ~~ 1*crystalab

  Strategy ~ 1
  Task ~ 1
  Character ~ 1
  GlobalMovement ~ 1
  TaskSwitch ~ 1
  StroopTask ~ 1
  UpdatingTask ~ 1
  Knowledge ~ 1
  TimeSpent ~ 1
'

fit3frespec <- cfa(threef2, data = GamingAptitude)

summary(fit3frespec, 
        fit.measures = T, 
        standardized = TRUE)
# Fit indices seem a lot happier and change is theoretically justifiable
# Would not recommend further change as don't want to overfit/deviate from theory

rm(fit3f, threef1)

################################## Question 3

###### Specify Structural Equation Model based off two factor solution in question 2

###### Additional variables being included so need to run checks again
cor(GamingAptitude[,c(6:17)])

# Univariate density plots
plot(density(GamingAptitude$Strategy), main='')
plot(density(GamingAptitude$Task), main='')
plot(density(GamingAptitude$Character), main='')
plot(density(GamingAptitude$GlobalMovement), main='')
plot(density(GamingAptitude$Knowledge), main='')
plot(density(GamingAptitude$TaskSwitch), main='')
plot(density(GamingAptitude$StroopTask), main='')
plot(density(GamingAptitude$UpdatingTask), main='')
plot(density(GamingAptitude$TimeSpent), main='')
plot(density(GamingAptitude$ExploreExploit), main='')
plot(density(GamingAptitude$BreakTime), main='')
plot(density(GamingAptitude$EloScore), main='')

# Multivariate normality
require(MVN)
test <- mvn(GamingAptitude[,c('Strategy','Task','Character','GlobalMovement','Knowledge','TaskSwitch','StroopTask',
                            'UpdatingTask','TimeSpent','ExploreExploit','BreakTime','EloScore')], 
          mvnTest = 'royston')
test$multivariateNormality

# Check linearity of relationship between observed variables
plot(GamingAptitude$BreakTime, GamingAptitude$ExploreExploit)
plot(GamingAptitude$BreakTime, GamingAptitude$EloScore)
plot(GamingAptitude$ExploreExploit, GamingAptitude$EloScore)

###### Specify model structure
q3model <- '
metacog =~ NA*Strategy + Task + Character + GlobalMovement + Knowledge
workmem =~ NA*TaskSwitch + StroopTask + UpdatingTask + TimeSpent
ExploreExploit ~ a*BreakTime + a1*metacog
EloScore ~ c*BreakTime + c1*metacog + workmem + b*ExploreExploit

metacog ~~ 1*metacog
workmem ~~ 1*workmem

indirectbreak := a*b
indirectmeta := a1*b
directbreak := c
directmeta := c1
totalbreak := c + (a*b)
totalmeta := c1 + (a1*b)
'

sem1 <- sem(q3model, data = GamingAptitude, meanstructure = T)

summary(sem1,
        fit.measures = T,
        standardized = T
        )

inspect(sem1, "r2")

semPlot::semPaths(sem1, what = "std", residuals = F)

###### Test for metric invariance

# Configural invariance model
semconfig <- sem(q3model, data = GamingAptitude, group='Sex')

#Metric invariance model
semmetric <- sem(q3model, data = GamingAptitude, group = 'Sex', group.equal = 'loadings',
                 meanstructure = T)

#Compare models
require(semTools)
summary(compareFit(semconfig, semmetric))
#Very little difference, metric invariance in "Sex" assumed to be little-none


################################## Question 4

###### Constructing mixed effects linear regression with partial pooling to estimate effect of SceneType

###### Run prelim checks
# Density plots
plot(density(SceneDec$RT), main='')
plot(density(SceneDec$Age), main='')
plot(density(SceneDec$Consc), main='')

# Check continuous variables for multicollinearity 
cor(SceneDec$EloScore, SceneDec$Consc)
cor(SceneDec$EloScore, SceneDec$Age)
cor(SceneDec$Consc, SceneDec$Age)

# Linearity of relationship
plot(SceneDec$RT, SceneDec$Age)
plot(SceneDec$RT, SceneDec$Consc)
plot(SceneDec$RT, SceneDec$EloScore)

###### Build model options
require(lme4)
require(lmerTest)

mem1a <- lmer(data = SceneDec, formula = RT ~ SceneType + (1 + SceneType|Participant_ID))
mem1b <- lmer(data = SceneDec, formula = RT ~ SceneType + Age + (1 + SceneType|Participant_ID))
mem1c <- lmer(data = SceneDec, formula = RT ~ SceneType + Age + Consc + (1 + SceneType|Participant_ID))
mem1d <- lmer(data = SceneDec, formula = RT ~ SceneType + Age + Consc + EloScore + (1 + SceneType|Participant_ID))

# Compare model outcomes

anova(mem1a, mem1b, mem1c, mem1d) 
summary(mem1d, cor = F)

# Plot stuff
require(sjPlot)

plot_model(mem1d, type='re', sort.est='sort.all', grid=FALSE, auto.label = TRUE)[1]
plot_model(mem1d, type='re', sort.est='sort.all', grid=FALSE)[2]
plot_model(mem1d, type='eff')

SceneDec$predicted = predict(mem1d)
plot(SceneDec$predicted, SceneDec$RT)

###### How does model adjust slope for first participant
ranef(mem1d)
fixef(mem1d)

###### Which variables influence variability explained by random adjustments
# Construct baseline model
Basecomp <- lmer(data = SceneDec, formula = RT ~ (1 + SceneType|Participant_ID))

# Construct model alternatives for comparison
STcomp <- lmer(data = SceneDec, formula = RT ~ SceneType + (1 + SceneType|Participant_ID))
Agecomp <- lmer(data = SceneDec, formula = RT ~ Age + (1 + SceneType|Participant_ID))
Concomp <- lmer(data = SceneDec, formula = RT ~ Consc + (1 + SceneType|Participant_ID))
Elocomp <- lmer(data = SceneDec, formula = RT ~ EloScore + (1 + SceneType|Participant_ID))

# Compare models
require(MuMIn)
r.squaredGLMM(Basecomp)
r.squaredGLMM(STcomp)
r.squaredGLMM(Agecomp)
r.squaredGLMM(Concomp)
r.squaredGLMM(Elocomp)
r.squaredGLMM(mem1d)
