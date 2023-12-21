# PEPITE_CE_Analysis

## Required packages

```{r}

library(see)
library(dplyr)
library(ggplot2) 
library(gmnl) 
library(mlogit)
library(tibble)
library(tidyr)
library(AER)
library(poLCA)
library(DescTools)
source('lc_helpers.r')

```

## Three step LC

### 0. Prepare DCE data with covariables

```{r}
# load DCE data processed for the analysis

data_DCE <- readRDS("output/data_DCE_publi.rds")

# transform data format for latent class analysis

data_DCE_mlogit <- mlogit.data(data_DCE,
                               choice = "choice",
                               shape = "long",
                               alt.var = "Scenario",
                               id.var = "survey_person",
                               chid.var = "chid")

# load socio-economic and environmental data

data_clean_com_nat_analysis <- readRDS("output/data_indiv_publi.rds")
```

### 1. Get the best LC model without covariates

#### 1.1 Models

```{r}
# model with 2 latent classes

lc_3step_2 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 2,
            panel = TRUE)
summary(lc_3step_2) 
AIC_lc_3step_2 <- 2 * length(coef(lc_3step_2)) - 2 * lc_3step_2$logLik$maximum
BIC_lc_3step_2 <- length(coef(lc_3step_2)) * log(nrow(lc_3step_2$residuals)) - 2 * lc_3step_2$logLik$maximum
CAIC_lc_3step_2 <- length(coef(lc_3step_2)) * (log(nrow(lc_3step_2$residuals)) + 1 ) - 2 * lc_3step_2$logLik$maximum
AWE_lc_3step_2 <-  2* length(coef(lc_3step_2)) * (log(nrow(lc_3step_2$residuals)) + 1.5 ) - 2 * lc_3step_2$logLik$maximum

shares(lc_3step_2)

# model with 3 latent classes

lc_3step_3 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 3,
            panel = TRUE)
summary(lc_3step_3)
AIC_lc_3step_3 <- 2 * length(coef(lc_3step_3)) - 2 * lc_3step_3$logLik$maximum
BIC_lc_3step_3 <- length(coef(lc_3step_3)) * log(nrow(lc_3step_3$residuals)) - 2 * lc_3step_3$logLik$maximum
CAIC_lc_3step_3 <- length(coef(lc_3step_3)) * (log(nrow(lc_3step_3$residuals)) + 1 ) - 2 * lc_3step_3$logLik$maximum
AWE_lc_3step_3 <-  2* length(coef(lc_3step_3)) * (log(nrow(lc_3step_3$residuals)) + 1.5 ) - 2 * lc_3step_3$logLik$maximum

shares(lc_3step_3)

# model with 4 latent classes

lc_3step_4 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 4,
            panel = TRUE)
summary(lc_3step_4)
AIC_lc_3step_4 <- 2 * length(coef(lc_3step_4)) - 2 * lc_3step_4$logLik$maximum
BIC_lc_3step_4 <- length(coef(lc_3step_4)) * log(nrow(lc_3step_4$residuals)) - 2 * lc_3step_4$logLik$maximum
CAIC_lc_3step_4 <- length(coef(lc_3step_4)) * (log(nrow(lc_3step_4$residuals)) + 1 ) - 2 * lc_3step_4$logLik$maximum
AWE_lc_3step_4 <-  2* length(coef(lc_3step_4)) * (log(nrow(lc_3step_4$residuals)) + 1.5 ) - 2 * lc_3step_4$logLik$maximum

shares(lc_3step_4)

# model with 5 latent classes

lc_3step_5 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 5,
            panel = TRUE)
summary(lc_3step_5)
AIC_lc_3step_5 <- 2 * length(coef(lc_3step_5)) - 2 * lc_3step_5$logLik$maximum
BIC_lc_3step_5 <- length(coef(lc_3step_5)) * log(nrow(lc_3step_5$residuals)) - 2 * lc_3step_5$logLik$maximum
CAIC_lc_3step_5 <- length(coef(lc_3step_5)) * (log(nrow(lc_3step_5$residuals)) + 1 ) - 2 * lc_3step_5$logLik$maximum
AWE_lc_3step_5 <-  2* length(coef(lc_3step_5)) * (log(nrow(lc_3step_5$residuals)) + 1.5 ) - 2 * lc_3step_5$logLik$maximum

shares(lc_3step_5)

# model with 6 latent classes

lc_3step_6 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 6,
            panel = TRUE)
summary(lc_3step_6)
AIC_lc_3step_6 <- 2 * length(coef(lc_3step_6)) - 2 * lc_3step_6$logLik$maximum
BIC_lc_3step_6 <- length(coef(lc_3step_6)) * log(nrow(lc_3step_6$residuals)) - 2 * lc_3step_6$logLik$maximum
CAIC_lc_3step_6 <- length(coef(lc_3step_6)) * (log(nrow(lc_3step_6$residuals)) + 1 ) - 2 * lc_3step_6$logLik$maximum
AWE_lc_3step_6 <-  2* length(coef(lc_3step_6)) * (log(nrow(lc_3step_6$residuals)) + 1.5 ) - 2 * lc_3step_6$logLik$maximum

shares(lc_3step_6)

# model with 7 latent classes

lc_3step_7 <- gmnl(choice ~ Time + Landscape + Acces + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 7,
            panel = TRUE)
summary(lc_3step_7)
AIC_lc_3step_7 <- 2 * length(coef(lc_3step_7)) - 2 * lc_3step_7$logLik$maximum
BIC_lc_3step_7 <- length(coef(lc_3step_7)) * log(nrow(lc_3step_7$residuals)) - 2 * lc_3step_7$logLik$maximum
CAIC_lc_3step_7 <- length(coef(lc_3step_7)) * (log(nrow(lc_3step_7$residuals)) + 1 ) - 2 * lc_3step_7$logLik$maximum
AWE_lc_3step_7 <-  2* length(coef(lc_3step_7)) * (log(nrow(lc_3step_7$residuals)) + 1.5 ) - 2 * lc_3step_7$logLik$maximum

shares(lc_3step_7)

```


#### 1.2 Model fit criteria

```{r}

IC_mod <- data.frame(group = c(2:6),
                     AIC = c(AIC_lc_3step_2,AIC_lc_3step_3,AIC_lc_3step_4,AIC_lc_3step_5,AIC_lc_3step_6),
                     BIC = c(BIC_lc_3step_2,BIC_lc_3step_3,BIC_lc_3step_4,BIC_lc_3step_5,BIC_lc_3step_6),
                     CAIC = c(CAIC_lc_3step_2,CAIC_lc_3step_3,CAIC_lc_3step_4,CAIC_lc_3step_5,CAIC_lc_3step_6),
                     AWE = c(AWE_lc_3step_2,AWE_lc_3step_3,AWE_lc_3step_4,AWE_lc_3step_5,AWE_lc_3step_6),
                     log_likelihood = c(summary(lc_3step_2)$logLik$maximum,summary(lc_3step_3)$logLik$maximum,summary(lc_3step_4)$logLik$maximum,summary(lc_3step_5)$logLik$maximum,summary(lc_3step_6)$logLik$maximum))

IC_mod_long <- melt(IC_mod, id.vars = "group")

ggplot(IC_mod_long) + 
  geom_line(data=IC_mod_long[which(IC_mod_long$variable!="log_likelihood"),], aes(x = group, y=value, col=variable)) +
  theme_modern()
  
```


#### 1.3 Diagnotic criteria

```{r}

pi_hat <- lc_3step_2$Qir
predictions <- apply(pi_hat,1,  which.max)
m2 <- matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),
                     mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2])),3), nrow=2)

m2
mean(diag(m2))

pi_hat <- lc_3step_3$Qir
predictions <- apply(pi_hat,1,  which.max)
m3 <- matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),mean(pi_hat[which(predictions==1),3]),
                     mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2]),mean(pi_hat[which(predictions==2),3]),
                     mean(pi_hat[which(predictions==3),1]),mean(pi_hat[which(predictions==3),2]),mean(pi_hat[which(predictions==3),3])),3), nrow=3)

m3
mean(diag(m3))

pi_hat <- lc_3step_4$Qir
predictions <- apply(pi_hat,1,  which.max)
m4 <- matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),mean(pi_hat[which(predictions==1),3]),mean(pi_hat[which(predictions==1),4]),
                     mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2]),mean(pi_hat[which(predictions==2),3]),mean(pi_hat[which(predictions==2),4]),
                     mean(pi_hat[which(predictions==3),1]),mean(pi_hat[which(predictions==3),2]),mean(pi_hat[which(predictions==3),3]),mean(pi_hat[which(predictions==3),4]),
                     mean(pi_hat[which(predictions==4),1]),mean(pi_hat[which(predictions==4),2]),mean(pi_hat[which(predictions==4),3]),mean(pi_hat[which(predictions==4),4])),3), nrow=4)
m4
mean(diag(m4))

pi_hat <- lc_3step_5$Qir
predictions <- apply(pi_hat,1,  which.max)
m5 <- matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),mean(pi_hat[which(predictions==1),3]),mean(pi_hat[which(predictions==1),4]),mean(pi_hat[which(predictions==1),5]),
                     mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2]),mean(pi_hat[which(predictions==2),3]),mean(pi_hat[which(predictions==2),4]),mean(pi_hat[which(predictions==2),5]),
                     mean(pi_hat[which(predictions==3),1]),mean(pi_hat[which(predictions==3),2]),mean(pi_hat[which(predictions==3),3]),mean(pi_hat[which(predictions==3),4]),mean(pi_hat[which(predictions==3),5]),
                     mean(pi_hat[which(predictions==4),1]),mean(pi_hat[which(predictions==4),2]),mean(pi_hat[which(predictions==4),3]),mean(pi_hat[which(predictions==4),4]),mean(pi_hat[which(predictions==4),5]),
                     mean(pi_hat[which(predictions==5),1]),mean(pi_hat[which(predictions==5),2]),mean(pi_hat[which(predictions==5),3]),mean(pi_hat[which(predictions==5),4]),mean(pi_hat[which(predictions==5),5])),3), nrow=5)

m5

mean(diag(m5))

pi_hat <- lc_3step_6$Qir
predictions <- apply(pi_hat,1,  which.max)
m6 <- matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),mean(pi_hat[which(predictions==1),3]),mean(pi_hat[which(predictions==1),4]),mean(pi_hat[which(predictions==1),5]),mean(pi_hat[which(predictions==1),6]),
                     mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2]),mean(pi_hat[which(predictions==2),3]),mean(pi_hat[which(predictions==2),4]),mean(pi_hat[which(predictions==2),5]),mean(pi_hat[which(predictions==2),6]),
                     mean(pi_hat[which(predictions==3),1]),mean(pi_hat[which(predictions==3),2]),mean(pi_hat[which(predictions==3),3]),mean(pi_hat[which(predictions==3),4]),mean(pi_hat[which(predictions==3),5]),mean(pi_hat[which(predictions==3),6]),
                     mean(pi_hat[which(predictions==4),1]),mean(pi_hat[which(predictions==4),2]),mean(pi_hat[which(predictions==4),3]),mean(pi_hat[which(predictions==4),4]),mean(pi_hat[which(predictions==4),5]),mean(pi_hat[which(predictions==4),6]),
                     mean(pi_hat[which(predictions==5),1]),mean(pi_hat[which(predictions==5),2]),mean(pi_hat[which(predictions==5),3]),mean(pi_hat[which(predictions==5),4]),mean(pi_hat[which(predictions==5),5]),mean(pi_hat[which(predictions==5),6]),
                     mean(pi_hat[which(predictions==6),1]),mean(pi_hat[which(predictions==6),2]),mean(pi_hat[which(predictions==6),3]),mean(pi_hat[which(predictions==6),4]),mean(pi_hat[which(predictions==6),5]),mean(pi_hat[which(predictions==6),6])),3), nrow=6)

m6

mean(diag(m6))


c(mean(diag(m2)),mean(diag(m3)),mean(diag(m4)),mean(diag(m5)),mean(diag(m6)))

c(min(shares(lc_3step_2)),min(shares(lc_3step_3)),min(shares(lc_3step_4)),min(shares(lc_3step_5)),min(shares(lc_3step_6)))
round(c(min(shares(lc_3step_2)),min(shares(lc_3step_3)),min(shares(lc_3step_4)),min(shares(lc_3step_5)),min(shares(lc_3step_6)))*1094)

plot_ci_lc_ggplot(lc_3step_4, var = c("Time","Landscape","Acces","Biodiversity","Biome1","Biome2"))

```

### 1.4 Conditional proba

```{r}

# Get individuals' estimates for Landscape
bi_Landscape <- effect.gmnl(lc_3step_4, par = "Landscape", effect = "ce")$mean
summary(bi_Landscape)
# Plotting the distribution of the individuals' estimates
plot(lc_3step_4, par = "Landscape", effect = "ce", type = "density", col = "blue")

# Get individuals' estimates for Acces
bi_Acces <- effect.gmnl(lc_3step_4, par = "Acces", effect = "ce")$mean
summary(bi_Acces)
# Plotting the distribution of the individuals' estimates
plot(lc_3step_4, par = "Acces", effect = "ce", type = "density", col = "blue")

# Get individuals' estimates for Biodiversity
bi_Biodiversity <- effect.gmnl(lc_3step_4, par = "Biodiversity", effect = "ce")$mean
summary(bi_Biodiversity)
# Plotting the distribution of the individuals' estimates
plot(lc_3step_4, par = "Biodiversity", effect = "ce", type = "density", col = "blue")

# Get individuals' estimates for Biome
bi_Biome <- effect.gmnl(lc_3step_4, par = "Biome1", effect = "ce")$mean
summary(bi_Biome)
# Plotting the distribution of the individuals' estimates
plot(lc_3step_4, par = "Biome1", effect = "ce", type = "density", col = "blue")

# Get individuals' estimates for Biome
bi_Biome <- effect.gmnl(lc_3step_4, par = "Biome2", effect = "ce")$mean
summary(bi_Biome)
# Plotting the distribution of the individuals' estimates
plot(lc_3step_4, par = "Biome2", effect = "ce", type = "density", col = "blue")

```

#### 1.5 WTA

```{r}

wtp <- -coef(lc_3step_4)["class.3.Landscape"] / coef(lc_3step_4)["class.3.Time"]
sig <- -coef(lc_3step_4)["class.3.Landscape"] / coef(lc_3step_4)["class.3.Time"]*sqrt(((summary(lc_3step_4)$CoefTable["class.3.Landscape","Std. Error"]/coef(lc_3step_4)["class.3.Landscape"])^2 + (summary(lc_3step_4)$CoefTable["class.3.Time","Std. Error"]/coef(lc_3step_4)["class.3.Time"])^2 ))
1.96*sig

wtp <- -coef(lc_3step_4)["class.3.Acces"] / coef(lc_3step_4)["class.3.Time"]
sig <- -coef(lc_3step_4)["class.3.Acces"] / coef(lc_3step_4)["class.3.Time"]*sqrt(((summary(lc_3step_4)$CoefTable["class.3.Acces","Std. Error"]/coef(lc_3step_4)["class.3.Acces"])^2 + (summary(lc_3step_4)$CoefTable["class.3.Time","Std. Error"]/coef(lc_3step_4)["class.3.Time"])^2 ))
1.96*sig

wtp <- -coef(lc_3step_4)["class.3.Biodiversity"] / coef(lc_3step_4)["class.3.Time"]
sig <- -coef(lc_3step_4)["class.3.Biodiversity"] / coef(lc_3step_4)["class.3.Time"]*sqrt(((summary(lc_3step_4)$CoefTable["class.3.Biodiversity","Std. Error"]/coef(lc_3step_4)["class.3.Biodiversity"])^2 + (summary(lc_3step_4)$CoefTable["class.3.Time","Std. Error"]/coef(lc_3step_4)["class.3.Time"])^2 ))
1.96*sig

```


### 2. Get latent variables and merge with covariate

```{r}

res_lc4 <- data.frame(survey_person=unique(data_DCE_mlogit$survey_person),class=apply(lc_3step_4$Qir,1,  which.max),lc_3step_4$Qir)

names(res_lc4)[3:ncol(res_lc4)] <- paste0("q",1:4)

res_lc4 <- merge(res_lc4,data_clean_com_nat_analysis, by="survey_person",all.x=TRUE)

res_lc4 <- na.omit(res_lc4[,c("class","q1","q2","q3","q4","Gender",
                      "Age","Education","Income","CSPgroup",
                      "class_nat","survey_id","journey_duration2","journey_duration3",
                      "main_vehicule","Perso_relation_nature")])

```

### 3. Analyse covariate effects

```{r}

lm_lc4_q1 <- glm(q1 ~ Gender + Age + factor(Education) + Income + 
                   CSPgroup + factor(class_nat) + survey_id + journey_duration2 + main_vehicule + Perso_relation_nature,
                 family="binomial",data=res_lc4)
summary(lm_lc4_q1)
M1.step <- step(lm_lc4_q1)
#step(lm_lc4_q1,k=log(nrow(res_lc4))) BIC 

#calculate McFadden's R-squared for model
with(summary(M1.step), 1 - deviance/null.deviance)

glm.diag.plots(M1.step,glm.diag(M1.step)) # ou

# Residual vs. fitted
E2 <- resid(M1.step, type="pearson")
F2 <- fitted(M1.step, type="response")
plot(x=F2, y=E2, xlab="fitted values", ylab="Pearson residuals")
abline(h=0, lty=2)
# Cook's distance
plot(cooks.distance(M1.step), ylim=c(0,1), ylab="Cook distance values", type="h")
# Pearson residuals vs. continous covariate
plot(x=res_lc4$Income, y=E2, xlab="Income", ylab="Pearson residuals")
abline(h=0, lty=2)
plot(x=res_lc4$Age, y=E2, xlab="Age", ylab="Pearson residuals")
abline(h=0, lty=2)
plot(x=res_lc4$journey_duration2, y=E2, xlab="Daily transportation time", ylab="Pearson residuals")
abline(h=0, lty=2)
plot(x=res_lc4$Perso_relation_nature, y=E2, xlab="INS scale", ylab="Pearson residuals")
abline(h=0, lty=2)

lm_lc4_q2 <- glm(q2 ~ Gender + Age + factor(Education) + Income + 
                   CSPgroup + factor(class_nat) + survey_id + journey_duration2 + main_vehicule + Perso_relation_nature, 
                 family="binomial",data=res_lc4)
summary(lm_lc4_q2)
M2.step <- step(lm_lc4_q2)
#step(lm_lc4_q2,k=log(nrow(res_lc4)))

glm.diag.plots(M2.step,glm.diag(M2.step))
with(summary(M2.step), 1 - deviance/null.deviance)


lm_lc4_q3 <- glm(q3 ~ Gender + Age + factor(Education) + Income + 
                   CSPgroup + factor(class_nat) + survey_id + journey_duration2 + main_vehicule + Perso_relation_nature, 
                 family="binomial",data=res_lc4)
summary(lm_lc4_q3)
M3.step <- step(lm_lc4_q3)
#step(lm_lc4_q3,k=log(nrow(res_lc4)))

glm.diag.plots(M3.step,glm.diag(M3.step))
with(summary(M3.step), 1 - deviance/null.deviance)

lm_lc4_q2 <- glm(q2 ~ Gender + Age + factor(Education) + Income + 
                   CSPgroup + factor(class_nat) + survey_id + journey_duration2 + main_vehicule + Perso_relation_nature, 
                 family="binomial",data=res_lc4)
summary(lm_lc4_q2)
M2.step <- step(lm_lc4_q2)
#step(lm_lc4_q2,k=log(nrow(res_lc4)))

glm.diag.plots(M2.step,glm.diag(M2.step))
with(summary(M2.step), 1 - deviance/null.deviance)


lm_lc4_q4 <- glm(q4 ~ Gender + Age + factor(Education) + Income + 
                   CSPgroup + factor(class_nat) + survey_id + journey_duration2 + main_vehicule + Perso_relation_nature, 
                 family="binomial",data=res_lc4)
summary(lm_lc4_q4)
M4.step <- step(lm_lc4_q4)
#step(lm_lc4_q4,k=log(nrow(res_lc4)))

glm.diag.plots(M4.step,glm.diag(M4.step))
with(summary(M4.step), 1 - deviance/null.deviance)


# plot odds


boxLabels = c("Income","Naturalness -","Naturalness +","INS")
df <- data.frame(yAxis = length(boxLabels):1, 
                 boxOdds = exp(coef(M1.step)[-1]), 
                 boxCILow = exp(coef(M1.step)[-1]-1.96*summary(M1.step)$coefficients[2:5,2]), 
                 boxCIHigh = exp(coef(M1.step)[-1]+1.96*summary(M1.step)$coefficients[2:5,2])
)


ggplot(df, aes(x = boxOdds, y = boxLabels)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = 
                     .2, color = "gray50") +
    geom_point(size = 3.5, color = "#00bfc4ff") +
    theme_modern()+
    ylab("") +
    xlab("Odds ratio") +
    annotate(geom = "text", y =1.1, x = 1, 
             label = paste0("McFadden R² = ", round(with(summary(M1.step), 1 - deviance/null.deviance),2)), size = 3.5, hjust = 0)


boxLabels = c("Gender","Age","Framing","INS")
df <- data.frame(yAxis = length(boxLabels):1, 
                 boxOdds = exp(coef(M2.step)[-1]), 
                 boxCILow = exp(coef(M2.step)[-1]-1.96*summary(M2.step)$coefficients[2:5,2]), 
                 boxCIHigh = exp(coef(M2.step)[-1]+1.96*summary(M2.step)$coefficients[2:5,2])
)


ggplot(df, aes(x = boxOdds, y = boxLabels)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 3.5, color = "#7cae00ff") +
  theme_modern()+
  ylab("") +
  xlab("Odds ratio") +
  annotate(geom = "text", y =1.5, x = 1, 
           label = paste0("McFadden R² = ", round(with(summary(M2.step), 1 - deviance/null.deviance),2)), size = 3.5, hjust = 0)

boxLabels = c("Gender","Age","Naturalness -","Naturalness +","INS")
df <- data.frame(yAxis = length(boxLabels):1, 
                 boxOdds = exp(coef(M3.step)[-1]), 
                 boxCILow = exp(coef(M3.step)[-1]-1.96*summary(M3.step)$coefficients[2:6,2]), 
                 boxCIHigh = exp(coef(M3.step)[-1]+1.96*summary(M3.step)$coefficients[2:6,2])
)


ggplot(df, aes(x = boxOdds, y = boxLabels)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 3.5, color = "#f8766dff") +
  theme_modern()+
  ylab("") +
  xlab("Odds ratio") +
  annotate(geom = "text", y =0.6, x = 1, 
           label = paste0("McFadden R² = ", round(with(summary(M3.step), 1 - deviance/null.deviance),2)), size = 3.5, hjust = 0)


boxLabels = c("Framing","INS")
df <- data.frame(yAxis = length(boxLabels):1, 
                 boxOdds = exp(coef(M4.step)[-1]), 
                 boxCILow = exp(coef(M4.step)[-1]-1.96*summary(M4.step)$coefficients[2:3,2]), 
                 boxCIHigh = exp(coef(M4.step)[-1]+1.96*summary(M4.step)$coefficients[2:3,2])
)


ggplot(df, aes(x = boxOdds, y = boxLabels)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 3.5, color = "#c77cffff") +
  theme_modern()+
  ylab("") +
  xlab("Odds ratio") +
  annotate(geom = "text", y =0.5, x = 1, 
           label = paste0("McFadden R² = ", round(with(summary(M4.step), 1 - deviance/null.deviance),2)), size = 3.5, hjust = 0)

```

