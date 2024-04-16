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

data_DCE <- read.csv("data_DCE_publi.csv")
data_DCE$asc <- ifelse(data_DCE$Scenario=="Reference scenario",1,0)
data_DCE$Biome <- as.factor(data_DCE$Biome)

# transform data format for latent class analysis

data_DCE_mlogit <- mlogit.data(data_DCE,
                               choice = "choice",
                               shape = "long",
                               alt.var = "Scenario",
                               id.var = "survey_person",
                               chid.var = "chid")

# load socio-economic and environmental data

data_clean_com_nat_analysis <- read.csv("data_indiv_publi.csv")
```

### 1. Get the best LC model without covariates

#### 1.1 Models

```{r}
# model with 2 latent classes

lc_3step_2 <- gmnl(choice ~ Time + Landscape + Access + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
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

lc_3step_3 <- gmnl(choice ~ Time + Landscape + Access + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
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

lc_3step_4 <- gmnl(choice ~ Time + Landscape + Access + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
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

lc_3step_5 <- gmnl(choice ~ Time + Landscape + Access + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
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

lc_3step_6 <- gmnl(choice ~ Time + Landscape + Access + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
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

lc_3step_7 <- gmnl(choice ~ Time + Landscape + Access + Biodiversity + Biome + asc | 0 | 0 | 0 | 1,
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

plot_ci_lc_ggplot(lc_3step_4, var = c("Time","Landscape","Access","Biodiversity","Biome1","Biome2"))

```

### 1.4 Conditional proba

```{r}

# Get individuals' estimates for Landscape
bi_Landscape <- effect.gmnl(lc_3step_4, par = "Landscape", effect = "ce")$mean
summary(bi_Landscape)
# Plotting the distribution of the individuals' estimates
plot(lc_3step_4, par = "Landscape", effect = "ce", type = "density", col = "blue")

# Get individuals' estimates for Access
bi_Access <- effect.gmnl(lc_3step_4, par = "Access", effect = "ce")$mean
summary(bi_Access)
# Plotting the distribution of the individuals' estimates
plot(lc_3step_4, par = "Access", effect = "ce", type = "density", col = "blue")

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

#### 1.5 WTT

```{r}

# For group 3

wtt_g3_landscape <- -coef(lc_3step_4)["class.3.Landscape"] / coef(lc_3step_4)["class.3.Time"]
sig_g3_landscape <- -coef(lc_3step_4)["class.3.Landscape"] / coef(lc_3step_4)["class.3.Time"]*sqrt(((summary(lc_3step_4)$CoefTable["class.3.Landscape","Std. Error"]/coef(lc_3step_4)["class.3.Landscape"])^2 + (summary(lc_3step_4)$CoefTable["class.3.Time","Std. Error"]/coef(lc_3step_4)["class.3.Time"])^2 ))

wtt_g3_access <- -coef(lc_3step_4)["class.3.Access"] / coef(lc_3step_4)["class.3.Time"]
sig_g3_access <- -coef(lc_3step_4)["class.3.Access"] / coef(lc_3step_4)["class.3.Time"]*sqrt(((summary(lc_3step_4)$CoefTable["class.3.Access","Std. Error"]/coef(lc_3step_4)["class.3.Access"])^2 + (summary(lc_3step_4)$CoefTable["class.3.Time","Std. Error"]/coef(lc_3step_4)["class.3.Time"])^2 ))

wtt_g3_biodiv <- -coef(lc_3step_4)["class.3.Biodiversity"] / coef(lc_3step_4)["class.3.Time"]
sig_g3_biodiv <- -coef(lc_3step_4)["class.3.Biodiversity"] / coef(lc_3step_4)["class.3.Time"]*sqrt(((summary(lc_3step_4)$CoefTable["class.3.Biodiversity","Std. Error"]/coef(lc_3step_4)["class.3.Biodiversity"])^2 + (summary(lc_3step_4)$CoefTable["class.3.Time","Std. Error"]/coef(lc_3step_4)["class.3.Time"])^2 ))

# For group 4

wtt_g4_access <- -coef(lc_3step_4)["class.4.Access"] / coef(lc_3step_4)["class.4.Time"]
sig_g4_access <- -coef(lc_3step_4)["class.4.Access"] / coef(lc_3step_4)["class.4.Time"]*sqrt(((summary(lc_3step_4)$CoefTable["class.4.Access","Std. Error"]/coef(lc_3step_4)["class.4.Access"])^2 + (summary(lc_3step_4)$CoefTable["class.4.Time","Std. Error"]/coef(lc_3step_4)["class.4.Time"])^2 ))

wtt_g4_biodiv <- -coef(lc_3step_4)["class.4.Biodiversity"] / coef(lc_3step_4)["class.4.Time"]
sig_g4_biodiv <- -coef(lc_3step_4)["class.4.Biodiversity"] / coef(lc_3step_4)["class.4.Time"]*sqrt(((summary(lc_3step_4)$CoefTable["class.4.Biodiversity","Std. Error"]/coef(lc_3step_4)["class.4.Biodiversity"])^2 + (summary(lc_3step_4)$CoefTable["class.4.Time","Std. Error"]/coef(lc_3step_4)["class.4.Time"])^2 ))

wtt_g4_biome <- -coef(lc_3step_4)["class.4.Biome1"] / coef(lc_3step_4)["class.4.Time"]
sig_g4_biome <- -coef(lc_3step_4)["class.4.Biome1"] / coef(lc_3step_4)["class.4.Time"]*sqrt(((summary(lc_3step_4)$CoefTable["class.4.Biome1","Std. Error"]/coef(lc_3step_4)["class.4.Biome1"])^2 + (summary(lc_3step_4)$CoefTable["class.4.Time","Std. Error"]/coef(lc_3step_4)["class.4.Time"])^2 ))


```


### 2. Get latent variables and merge with covariate

```{r}

res_lc4 <- data.frame(survey_person=unique(data_DCE_mlogit$survey_person),class=apply(lc_3step_4$Qir,1,  which.max),lc_3step_4$Qir)

names(res_lc4)[3:ncol(res_lc4)] <- paste0("q",1:4)

res_lc4 <- merge(res_lc4,data_clean_com_nat_analysis, by="survey_person",all.x=TRUE)

res_lc4$wtt_landscape <- effect.gmnl(lc_3step_4,par = "Landscape",effect = "wtp", wrt="Time")$mean
res_lc4$wtt_access <- effect.gmnl(lc_3step_4,par = "Access",effect = "wtp", wrt="Time")$mean
res_lc4$wtt_biodiv <- effect.gmnl(lc_3step_4,par = "Biodiversity",effect = "wtp", wrt="Time")$mean
res_lc4$wtt_biome <- effect.gmnl(lc_3step_4,par = "Biome1",effect = "wtp", wrt="Time")$mean

res_lc4 <- na.omit(res_lc4[,c("class","q1","q2","q3","q4","Gender",
                      "Age","Education","Income","SPC",
                      "class_nat","survey_id","journey_duration2","journey_duration3",
                      "main_vehicule","INS_scale","Nb_adult",
                      "wtt_landscape","wtt_access","wtt_biodiv","wtt_biome")])

res_lc4$Nb_adult[which(res_lc4$Nb_adult==22)] <- 2
res_lc4$Nb_adult[which(res_lc4$Nb_adult==0)] <- 1

```

### 3. Analyse covariate effects

```{r}

lm_lc4_q1 <- glm(q1 ~ Gender + Age + factor(Education) + Income + 
                   SPC + factor(class_nat) + survey_id + journey_duration2 + main_vehicule + INS_scale,
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
plot(x=res_lc4$INS_scale, y=E2, xlab="INS scale", ylab="Pearson residuals")
abline(h=0, lty=2)

lm_lc4_q2 <- glm(q2 ~ Gender + Age + factor(Education) + Income + 
                   SPC + factor(class_nat) + survey_id + journey_duration2 + main_vehicule + INS_scale, 
                 family="binomial",data=res_lc4)
summary(lm_lc4_q2)
M2.step <- step(lm_lc4_q2)
#step(lm_lc4_q2,k=log(nrow(res_lc4)))

glm.diag.plots(M2.step,glm.diag(M2.step))
with(summary(M2.step), 1 - deviance/null.deviance)


lm_lc4_q3 <- glm(q3 ~ Gender + Age + factor(Education) + Income + 
                   SPC + factor(class_nat) + survey_id + journey_duration2 + main_vehicule + INS_scale, 
                 family="binomial",data=res_lc4)
summary(lm_lc4_q3)
M3.step <- step(lm_lc4_q3)
#step(lm_lc4_q3,k=log(nrow(res_lc4)))

glm.diag.plots(M3.step,glm.diag(M3.step))
with(summary(M3.step), 1 - deviance/null.deviance)

lm_lc4_q2 <- glm(q2 ~ Gender + Age + factor(Education) + Income + 
                   SPC + factor(class_nat) + survey_id + journey_duration2 + main_vehicule + INS_scale, 
                 family="binomial",data=res_lc4)
summary(lm_lc4_q2)
M2.step <- step(lm_lc4_q2)
#step(lm_lc4_q2,k=log(nrow(res_lc4)))

glm.diag.plots(M2.step,glm.diag(M2.step))
with(summary(M2.step), 1 - deviance/null.deviance)


lm_lc4_q4 <- glm(q4 ~ Gender + Age + factor(Education) + Income + 
                   SPC + factor(class_nat) + survey_id + journey_duration2 + main_vehicule + INS_scale, 
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


### 4. Benefit lack analysis

#### 4.1 Prepare data

```{r}

data_soc_dem <- res_lc4[,c("class","Gender","Age", "Education", "Income", "SPC", "class_nat","INS_scale","Nb_adult",
                           "wtt_landscape","wtt_access","wtt_biodiv","wtt_biome")]
data_soc_dem$Gender <- as.character(data_soc_dem$Gender)
data_soc_dem$Gender[which(data_soc_dem$Gender=="Femme")] <- "Women"
data_soc_dem$Gender[which(data_soc_dem$Gender=="Homme")] <- "Men"
data_soc_dem$Age <- as.character(data_soc_dem$Age)
data_soc_dem$Age[which(data_soc_dem$Age=="1")] <- "18-29 yo"
data_soc_dem$Age[which(data_soc_dem$Age=="2")] <- "30-44 yo"
data_soc_dem$Age[which(data_soc_dem$Age=="3")] <- "45-59 yo"
data_soc_dem$Age[which(data_soc_dem$Age=="4")] <- "60 yo and above"
data_soc_dem$Education <- as.character(data_soc_dem$Education)
data_soc_dem$Education[which(data_soc_dem$Education=="1")] <- "Below secondary or short"
data_soc_dem$Education[which(data_soc_dem$Education=="2")] <- "Secondary long"
data_soc_dem$Education[which(data_soc_dem$Education=="3")] <- "Superior"
data_soc_dem$Income <- as.character(data_soc_dem$Income)
data_soc_dem$Income2 <- NA
data_soc_dem$Income2[which(data_soc_dem$Income=="1")] <- 1200
data_soc_dem$Income2[which(data_soc_dem$Income=="2")] <- 1350
data_soc_dem$Income2[which(data_soc_dem$Income=="3")] <- 1650
data_soc_dem$Income2[which(data_soc_dem$Income=="4")] <- 1950
data_soc_dem$Income2[which(data_soc_dem$Income=="5")] <- 2350
data_soc_dem$Income2[which(data_soc_dem$Income=="6")] <- 2850
data_soc_dem$Income2[which(data_soc_dem$Income=="7")] <- 3300
data_soc_dem$Income2[which(data_soc_dem$Income=="8")] <- 3850
data_soc_dem$Income2[which(data_soc_dem$Income=="9")] <- 4800
data_soc_dem$Income2[which(data_soc_dem$Income=="10")] <- 5400
data_soc_dem$Income[which(data_soc_dem$Income=="1")] <- "1200 € and below"
data_soc_dem$Income[which(data_soc_dem$Income=="2")] <- "1201-1500 €"
data_soc_dem$Income[which(data_soc_dem$Income=="3")] <- "1501-1800 €"
data_soc_dem$Income[which(data_soc_dem$Income=="4")] <- "1801-2100 €"
data_soc_dem$Income[which(data_soc_dem$Income=="5")] <- "2101-2600 €"
data_soc_dem$Income[which(data_soc_dem$Income=="6")] <- "2601-3100 €"
data_soc_dem$Income[which(data_soc_dem$Income=="7")] <- "3101-3500 €"
data_soc_dem$Income[which(data_soc_dem$Income=="8")] <- "3501-4200 €"
data_soc_dem$Income[which(data_soc_dem$Income=="9")] <- "4201-5400 €"
data_soc_dem$Income[which(data_soc_dem$Income=="10")] <- "5401 € and above"
data_soc_dem$SPC[which(data_soc_dem$SPC=="moins")] <- "Lower SPC"
data_soc_dem$SPC[which(data_soc_dem$SPC=="plus")] <- "Higher SPC"
data_soc_dem$SPC[which(data_soc_dem$SPC=="Retraités")] <- "Retired"
data_soc_dem$SPC[which(data_soc_dem$SPC=="Inactifs")] <- "Not in employment"
data_soc_dem$class_nat <- as.character(data_soc_dem$class_nat)
data_soc_dem$class_nat[which(data_soc_dem$class_nat=="1")] <- "Very low naturalness"
data_soc_dem$class_nat[which(data_soc_dem$class_nat=="2")] <- "Low naturalness"
data_soc_dem$class_nat[which(data_soc_dem$class_nat=="3")] <- "Above average naturalness"

```

#### 4.2 Benefit lack from classical approach

```{r}
# Translate WTT to WTP using income

# Income per capita

data_soc_dem$Income_cap <- data_soc_dem$Income2/data_soc_dem$Nb_adult

# Monthly income divided by the working hour per month 151,67 h par mois 

income_cap_g1 <- mean(data_soc_dem$Income_cap[which(data_soc_dem$class==1)])/151.67
sd_income_cap_g1 <- sd(data_soc_dem$Income_cap[which(data_soc_dem$class==1)]/151.67)

income_cap_g2 <- mean(data_soc_dem$Income_cap[which(data_soc_dem$class==2)])/151.67
sd_income_cap_g2 <- sd(data_soc_dem$Income_cap[which(data_soc_dem$class==2)]/151.67)

income_cap_g3 <- mean(data_soc_dem$Income_cap[which(data_soc_dem$class==3)])/151.67
sd_income_cap_g3 <- sd(data_soc_dem$Income_cap[which(data_soc_dem$class==3)]/151.67)

income_cap_g4 <- mean(data_soc_dem$Income_cap[which(data_soc_dem$class==4)])/151.67
sd_income_cap_g4 <- sd(data_soc_dem$Income_cap[which(data_soc_dem$class==4)]/151.67)


# Value of travel time expressed as 1/3 of income for each class

value_travel_time_g1 <- 1/3*income_cap_g1
value_travel_time_g1_low <- 1/3*(income_cap_g1-1.96*sd_income_cap_g1)
value_travel_time_g1_upp <- 1/3*(income_cap_g1+1.96*sd_income_cap_g1)

value_travel_time_g2 <- 1/3*income_cap_g2
value_travel_time_g2_low <- 1/3*(income_cap_g2-1.96*sd_income_cap_g2)
value_travel_time_g2_upp <- 1/3*(income_cap_g2+1.96*sd_income_cap_g2)

value_travel_time_g3 <- 1/3*income_cap_g3
value_travel_time_g3_low <- 1/3*(income_cap_g3-1.96*sd_income_cap_g3)
value_travel_time_g3_upp <- 1/3*(income_cap_g3+1.96*sd_income_cap_g3)

value_travel_time_g4 <- 1/3*income_cap_g4
value_travel_time_g4_low <- 1/3*(income_cap_g4-1.96*sd_income_cap_g4)
value_travel_time_g4_upp <- 1/3*(income_cap_g4+1.96*sd_income_cap_g4)

# Average value for the population

average_time_value_per_capita <- sum(data_soc_dem$Income_cap/151.67*1/3)/nrow(data_soc_dem)
sd_time_value_per_capita <- sd(data_soc_dem$Income_cap/151.67*1/3)

# Monetary value of travel time increase (above, value is in hour, converted to minute)

benefit_lack_2min <- average_time_value_per_capita*2/60
benefit_lack_2min_low <- (average_time_value_per_capita-1.96*sd_time_value_per_capita)*2/60
benefit_lack_2min_upp <- (average_time_value_per_capita+1.96*sd_time_value_per_capita)*2/60
benefit_lack_4min <- average_time_value_per_capita*4/60
benefit_lack_4min_low <- (average_time_value_per_capita-1.96*sd_time_value_per_capita)*4/60
benefit_lack_4min_upp <- (average_time_value_per_capita+1.96*sd_time_value_per_capita)*4/60
benefit_lack_6min <- average_time_value_per_capita*6/60
benefit_lack_6min_low <- (average_time_value_per_capita-1.96*sd_time_value_per_capita)*6/60
benefit_lack_6min_upp <- (average_time_value_per_capita+1.96*sd_time_value_per_capita)*6/60
benefit_lack_8min <- average_time_value_per_capita*8/60
benefit_lack_8min_low <- (average_time_value_per_capita-1.96*sd_time_value_per_capita)*8/60
benefit_lack_8min_upp <- (average_time_value_per_capita+1.96*sd_time_value_per_capita)*8/60

# Benefit lack taking all project into account

proportion_user_public_transport <- 0.106 # https://www.statistiques.developpement-durable.gouv.fr/resultats-detailles-de-lenquete-mobilite-des-personnes-de-2019?rubrique=60&dossier=1345 and https://www.statistiques.developpement-durable.gouv.fr/media/5041/download?inline

impact_tram_freq_public_transport <- 0.208-0.025 # effet tram periode recente moins temoin https://journals.openedition.org/rge/3508 https://doi.org/10.4000/rge.3508

com_isochrone <- readRDS("com_isochrone.rds")

proportion_user_tram <- proportion_user_public_transport * impact_tram_freq_public_transport

nb_person_per_area <- sum(com_isochrone$pop)

benefit_lack_2min_infra_monetary <- benefit_lack_2min * nb_person_per_area * proportion_user_tram
benefit_lack_2min_infra_monetary_low <- benefit_lack_2min_low * nb_person_per_area * proportion_user_tram
benefit_lack_2min_infra_monetary_upp <- benefit_lack_2min_upp * nb_person_per_area * proportion_user_tram
benefit_lack_2min_infra_time <- 2 * nb_person_per_area * proportion_user_tram
benefit_lack_4min_infra_monetary <- benefit_lack_4min * nb_person_per_area * proportion_user_tram
benefit_lack_4min_infra_monetary_low <- benefit_lack_4min_low * nb_person_per_area * proportion_user_tram
benefit_lack_4min_infra_monetary_upp <- benefit_lack_4min_upp * nb_person_per_area * proportion_user_tram
benefit_lack_4min_infra_time <- 4 * nb_person_per_area * proportion_user_tram
benefit_lack_6min_infra_monetary <- benefit_lack_6min * nb_person_per_area * proportion_user_tram
benefit_lack_6min_infra_monetary_low <- benefit_lack_6min_low * nb_person_per_area * proportion_user_tram
benefit_lack_6min_infra_monetary_upp <- benefit_lack_6min_upp * nb_person_per_area * proportion_user_tram
benefit_lack_6min_infra_time <- 6 * nb_person_per_area * proportion_user_tram
benefit_lack_8min_infra_monetary <- benefit_lack_8min * nb_person_per_area * proportion_user_tram
benefit_lack_8min_infra_monetary_low <- benefit_lack_8min_low * nb_person_per_area * proportion_user_tram
benefit_lack_8min_infra_monetary_upp <- benefit_lack_8min_upp * nb_person_per_area * proportion_user_tram
benefit_lack_8min_infra_time <- 8 * nb_person_per_area * proportion_user_tram

```

#### 4.3 Benefit lack taking heterogeneity into account

```{r}

# Proportion of each group

proba_g1 <- nrow(data_soc_dem[which(data_soc_dem$class==1),])/nrow(data_soc_dem)
proba_g2 <- nrow(data_soc_dem[which(data_soc_dem$class==2),])/nrow(data_soc_dem)
proba_g3 <- nrow(data_soc_dem[which(data_soc_dem$class==3),])/nrow(data_soc_dem)
proba_g4 <- nrow(data_soc_dem[which(data_soc_dem$class==4),])/nrow(data_soc_dem)


benefit_lack_2min_landscape <- 2/60*value_travel_time_g1*proba_g1 + 2/60*value_travel_time_g2*proba_g2 +
  (2-wtt_g3_landscape)/60*value_travel_time_g3*proba_g3 + 2/60*value_travel_time_g4*proba_g4

sd_benefit_lack_2min_landscape <- 2/60*sqrt((1/3*sd_income_cap_g1*proba_g1)^2+(1/3*sd_income_cap_g2*proba_g2)^2+(1/3*sd_income_cap_g3*proba_g3)^2+(1/3*sd_income_cap_g4*proba_g4)^2 - 2*cov(data_soc_dem$Income_cap[which(data_soc_dem$class==3)]/151.67*1/3 ,data_soc_dem$wtt_landscape[which(data_soc_dem$class==3)]/60)*sqrt(var(data_soc_dem$Income_cap[which(data_soc_dem$class==3)]/151.67*1/3 ))+(cov(data_soc_dem$Income_cap[which(data_soc_dem$class==3)]/151.67*1/3 ,data_soc_dem$wtt_landscape[which(data_soc_dem$class==3)]/60))^2)

benefit_lack_2min_landscape_low <- benefit_lack_2min_landscape-1.96*sd_benefit_lack_2min_landscape
benefit_lack_2min_landscape_upp <- benefit_lack_2min_landscape+1.96*sd_benefit_lack_2min_landscape

benefit_lack_4min_landscape <- 4/60*value_travel_time_g1*proba_g1 + 4/60*value_travel_time_g2*proba_g2 +
  (4-wtt_g3_landscape)/60*value_travel_time_g3*proba_g3 + 4/60*value_travel_time_g4*proba_g4
benefit_lack_4min_landscape_low <- benefit_lack_4min_landscape-1.96*2*sd_benefit_lack_2min_landscape
benefit_lack_4min_landscape_upp <- benefit_lack_4min_landscape+1.96*2*sd_benefit_lack_2min_landscape

benefit_lack_6min_landscape <- 6/60*value_travel_time_g1*proba_g1 + 6/60*value_travel_time_g2*proba_g2 +
  (6-wtt_g3_landscape)/60*value_travel_time_g3*proba_g3 + 6/60*value_travel_time_g4*proba_g4
benefit_lack_6min_landscape_low <- benefit_lack_6min_landscape-1.96*3*sd_benefit_lack_2min_landscape
benefit_lack_6min_landscape_upp <- benefit_lack_6min_landscape+1.96*3*sd_benefit_lack_2min_landscape

benefit_lack_8min_landscape <- 8/60*value_travel_time_g1*proba_g1 + 8/60*value_travel_time_g2*proba_g2 +
  (8-wtt_g3_landscape)/60*value_travel_time_g3*proba_g3 + 8/60*value_travel_time_g4*proba_g4
benefit_lack_8min_landscape_low <- benefit_lack_8min_landscape-1.96*4*sd_benefit_lack_2min_landscape
benefit_lack_8min_landscape_upp <- benefit_lack_8min_landscape+1.96*4*sd_benefit_lack_2min_landscape


benefit_lack_2min_access <- 2/60*value_travel_time_g1*proba_g1 + 2/60*value_travel_time_g2*proba_g2 +
  (2-wtt_g3_access)/60*value_travel_time_g3*proba_g3 + (2-wtt_g4_access)/60*value_travel_time_g4*proba_g4

sd_benefit_lack_2min_access <- 2/60*sqrt((1/3*sd_income_cap_g1*proba_g1)^2+(1/3*sd_income_cap_g2*proba_g2)^2+(1/3*sd_income_cap_g3*proba_g3)^2+(1/3*sd_income_cap_g4*proba_g4)^2 - 2*cov(data_soc_dem$Income_cap[which(data_soc_dem$class==3)]/151.67*1/3 ,data_soc_dem$wtt_access[which(data_soc_dem$class==3)]/60)*sqrt(var(data_soc_dem$Income_cap[which(data_soc_dem$class==3)]/151.67*1/3 ))+(cov(data_soc_dem$Income_cap[which(data_soc_dem$class==3)]/151.67*1/3 ,data_soc_dem$wtt_access[which(data_soc_dem$class==3)]/60))^2 - 2*cov(data_soc_dem$Income_cap[which(data_soc_dem$class==4)]/151.67*1/3 ,data_soc_dem$wtt_access[which(data_soc_dem$class==4)]/60)*sqrt(var(data_soc_dem$Income_cap[which(data_soc_dem$class==4)]/151.67*1/3 ))+(cov(data_soc_dem$Income_cap[which(data_soc_dem$class==4)]/151.67*1/3 ,data_soc_dem$wtt_access[which(data_soc_dem$class==4)]/60))^2)

benefit_lack_2min_access_low <- benefit_lack_2min_access-1.96*sd_benefit_lack_2min_access
benefit_lack_2min_access_upp <- benefit_lack_2min_access+1.96*sd_benefit_lack_2min_access

benefit_lack_4min_access <- 4/60*value_travel_time_g1*proba_g1 + 4/60*value_travel_time_g2*proba_g2 +
  (4-wtt_g3_access)/60*value_travel_time_g3*proba_g3 + (4-wtt_g4_access)/60*value_travel_time_g4*proba_g4
benefit_lack_4min_access_low <- benefit_lack_4min_access-1.96*2*sd_benefit_lack_2min_access
benefit_lack_4min_access_upp <- benefit_lack_4min_access+1.96*2*sd_benefit_lack_2min_access

benefit_lack_6min_access <- 6/60*value_travel_time_g1*proba_g1 + 6/60*value_travel_time_g2*proba_g2 +
  (6-wtt_g3_access)/60*value_travel_time_g3*proba_g3 + (6-wtt_g4_access)/60*value_travel_time_g4*proba_g4
benefit_lack_6min_access_low <- benefit_lack_6min_access-1.96*3*sd_benefit_lack_2min_access
benefit_lack_6min_access_upp <- benefit_lack_6min_access+1.96*3*sd_benefit_lack_2min_access

benefit_lack_8min_access <- 8/60*value_travel_time_g1*proba_g1 + 8/60*value_travel_time_g2*proba_g2 +
  (8-wtt_g3_access)/60*value_travel_time_g3*proba_g3 + (8-wtt_g4_access)/60*value_travel_time_g4*proba_g4
benefit_lack_8min_access_low <- benefit_lack_8min_access-1.96*4*sd_benefit_lack_2min_access
benefit_lack_8min_access_upp <- benefit_lack_8min_access+1.96*4*sd_benefit_lack_2min_access


benefit_lack_2min_biodiv <- 2/60*value_travel_time_g1*proba_g1 + 2/60*value_travel_time_g2*proba_g2 +
  (2-wtt_g3_biodiv)/60*value_travel_time_g3*proba_g3 + (2-wtt_g4_biodiv)/60*value_travel_time_g4*proba_g4

sd_benefit_lack_2min_biodiv <- 2/60*sqrt((1/3*sd_income_cap_g1*proba_g1)^2+(1/3*sd_income_cap_g2*proba_g2)^2+(1/3*sd_income_cap_g3*proba_g3)^2+(1/3*sd_income_cap_g4*proba_g4)^2 - 2*cov(data_soc_dem$Income_cap[which(data_soc_dem$class==3)]/151.67*1/3 ,data_soc_dem$wtt_biodiv[which(data_soc_dem$class==3)]/60)*sqrt(var(data_soc_dem$Income_cap[which(data_soc_dem$class==3)]/151.67*1/3 ))+(cov(data_soc_dem$Income_cap[which(data_soc_dem$class==3)]/151.67*1/3 ,data_soc_dem$wtt_biodiv[which(data_soc_dem$class==3)]/60))^2 - 2*cov(data_soc_dem$Income_cap[which(data_soc_dem$class==4)]/151.67*1/3 ,data_soc_dem$wtt_biodiv[which(data_soc_dem$class==4)]/60)*sqrt(var(data_soc_dem$Income_cap[which(data_soc_dem$class==4)]/151.67*1/3 ))+(cov(data_soc_dem$Income_cap[which(data_soc_dem$class==4)]/151.67*1/3 ,data_soc_dem$wtt_biodiv[which(data_soc_dem$class==4)]/60))^2)

benefit_lack_2min_biodiv_low <- benefit_lack_2min_biodiv-1.96*sd_benefit_lack_2min_biodiv
benefit_lack_2min_biodiv_upp <- benefit_lack_2min_biodiv+1.96*sd_benefit_lack_2min_biodiv

benefit_lack_4min_biodiv <- 4/60*value_travel_time_g1*proba_g1 + 4/60*value_travel_time_g2*proba_g2 +
  (4-wtt_g3_biodiv)/60*value_travel_time_g3*proba_g3 + (4-wtt_g4_biodiv)/60*value_travel_time_g4*proba_g4
benefit_lack_4min_biodiv_low <- benefit_lack_4min_biodiv-1.96*2*sd_benefit_lack_2min_biodiv
benefit_lack_4min_biodiv_upp <- benefit_lack_4min_biodiv+1.96*2*sd_benefit_lack_2min_biodiv

benefit_lack_6min_biodiv <- 6/60*value_travel_time_g1*proba_g1 + 6/60*value_travel_time_g2*proba_g2 +
  (6-wtt_g3_biodiv)/60*value_travel_time_g3*proba_g3 + (6-wtt_g4_biodiv)/60*value_travel_time_g4*proba_g4
benefit_lack_6min_biodiv_low <- benefit_lack_6min_biodiv-1.96*3*sd_benefit_lack_2min_biodiv
benefit_lack_6min_biodiv_upp <- benefit_lack_6min_biodiv+1.96*3*sd_benefit_lack_2min_biodiv

benefit_lack_8min_biodiv <- 8/60*value_travel_time_g1*proba_g1 + 8/60*value_travel_time_g2*proba_g2 +
  (8-wtt_g3_biodiv)/60*value_travel_time_g3*proba_g3 + (8-wtt_g4_biodiv)/60*value_travel_time_g4*proba_g4
benefit_lack_8min_biodiv_low <- benefit_lack_8min_biodiv-1.96*4*sd_benefit_lack_2min_biodiv
benefit_lack_8min_biodiv_upp <- benefit_lack_8min_biodiv+1.96*4*sd_benefit_lack_2min_biodiv


benefit_lack_2min_biome <- 2/60*value_travel_time_g1*proba_g1 + 2/60*value_travel_time_g2*proba_g2 +
  2/60*value_travel_time_g3*proba_g3 + (2-wtt_g4_biome)/60*value_travel_time_g4*proba_g4
  
sd_benefit_lack_2min_biome <- 2/60*sqrt((1/3*sd_income_cap_g1*proba_g1)^2+(1/3*sd_income_cap_g2*proba_g2)^2+(1/3*sd_income_cap_g3*proba_g3)^2+(1/3*sd_income_cap_g4*proba_g4)^2 - 2*cov(data_soc_dem$Income_cap[which(data_soc_dem$class==4)]/151.67*1/3 ,data_soc_dem$wtt_biome[which(data_soc_dem$class==4)]/60)*sqrt(var(data_soc_dem$Income_cap[which(data_soc_dem$class==4)]/151.67*1/3 ))+(cov(data_soc_dem$Income_cap[which(data_soc_dem$class==4)]/151.67*1/3 ,data_soc_dem$wtt_biome[which(data_soc_dem$class==4)]/60))^2)

benefit_lack_2min_biome_low <- benefit_lack_2min_biome-1.96*sd_benefit_lack_2min_biome
benefit_lack_2min_biome_upp <- benefit_lack_2min_biome+1.96*sd_benefit_lack_2min_biome

benefit_lack_4min_biome <- 4/60*value_travel_time_g1*proba_g1 + 4/60*value_travel_time_g2*proba_g2 +
  4/60*value_travel_time_g3*proba_g3 + (4-wtt_g4_biome)/60*value_travel_time_g4*proba_g4
benefit_lack_4min_biome_low <- benefit_lack_4min_biome-1.96*2*sd_benefit_lack_2min_biome
benefit_lack_4min_biome_upp <- benefit_lack_4min_biome+1.96*2*sd_benefit_lack_2min_biome

benefit_lack_6min_biome <- 6/60*value_travel_time_g1*proba_g1 + 6/60*value_travel_time_g2*proba_g2 +
  6/60*value_travel_time_g3*proba_g3 + (6-wtt_g4_biome)/60*value_travel_time_g4*proba_g4
benefit_lack_6min_biome_low <- benefit_lack_6min_biome-1.96*3*sd_benefit_lack_2min_biome
benefit_lack_6min_biome_upp <- benefit_lack_6min_biome+1.96*3*sd_benefit_lack_2min_biome

benefit_lack_8min_biome <- 8/60*value_travel_time_g1*proba_g1 + 8/60*value_travel_time_g2*proba_g2 +
  8/60*value_travel_time_g3*proba_g3 + (8-wtt_g4_biome)/60*value_travel_time_g4*proba_g4
benefit_lack_8min_biome_low <- benefit_lack_8min_biome-1.96*4*sd_benefit_lack_2min_biome
benefit_lack_8min_biome_upp <- benefit_lack_8min_biome+1.96*4*sd_benefit_lack_2min_biome

```

#### 4.4 Plot for comparison

```{r}
df_benefit_lack <- data.frame(time_increase=rep(c(2,4,6,8),5),
                                 group = c(rep("1_classic_L_benefit_percapita",4),rep("2_L_benefit_landscape",4),rep("3_L_benefit_nature_use",4),rep("4_L_benefit_biodiversity",4),rep("5_L_benefit_biome",4)),
                                 value = c(benefit_lack_2min,benefit_lack_4min,benefit_lack_6min,benefit_lack_8min,
                                           benefit_lack_2min_landscape,benefit_lack_4min_landscape,benefit_lack_6min_landscape,benefit_lack_8min_landscape,
                                           benefit_lack_2min_access,benefit_lack_4min_access,benefit_lack_6min_access,benefit_lack_8min_access,
                                           benefit_lack_2min_biodiv,benefit_lack_4min_biodiv,benefit_lack_6min_biodiv,benefit_lack_8min_biodiv,
                                           benefit_lack_2min_biome,benefit_lack_4min_biome,benefit_lack_6min_biome,benefit_lack_8min_biome),
                                 lower = c(benefit_lack_2min_low,benefit_lack_4min_low,benefit_lack_6min_low,benefit_lack_8min_low,
                                           benefit_lack_2min_landscape_low,benefit_lack_4min_landscape_low,benefit_lack_6min_landscape_low,benefit_lack_8min_landscape_low,
                                           benefit_lack_2min_access_low,benefit_lack_4min_access_low,benefit_lack_6min_access_low,benefit_lack_8min_access_low,
                                           benefit_lack_2min_biodiv_low,benefit_lack_4min_biodiv_low,benefit_lack_6min_biodiv_low,benefit_lack_8min_biodiv_low,
                                           benefit_lack_2min_biome_low,benefit_lack_4min_biome_low,benefit_lack_6min_biome_low,benefit_lack_8min_biome_low),
                                 upper = c(benefit_lack_2min_upp,benefit_lack_4min_upp,benefit_lack_6min_upp,benefit_lack_8min_upp,
                                           benefit_lack_2min_landscape_upp,benefit_lack_4min_landscape_upp,benefit_lack_6min_landscape_upp,benefit_lack_8min_landscape_upp,
                                           benefit_lack_2min_access_upp,benefit_lack_4min_access_upp,benefit_lack_6min_access_upp,benefit_lack_8min_access_upp,
                                           benefit_lack_2min_biodiv_upp,benefit_lack_4min_biodiv_upp,benefit_lack_6min_biodiv_upp,benefit_lack_8min_biodiv_upp,
                                           benefit_lack_2min_biome_upp,benefit_lack_4min_biome_upp,benefit_lack_6min_biome_upp,benefit_lack_8min_biome_upp))

df_benefit_lack$value_total <- df_benefit_lack$value * nb_person_per_area * proportion_user_tram

my_lab <- c(expression(L['benefit classical']),
            expression(L['benefit landscape']), 
            expression(L['benefit nature use']),
            expression(L['benefit biodiversity']),
            expression(L['benefit biome']))

ggplot(df_benefit_lack, aes(x=time_increase, group=group)) +
  geom_point(aes(y=value, shape=group), position=position_dodge(width=0.4), size=3) + 
  geom_errorbar(aes(ymin=lower,ymax=upper), position=position_dodge(width=0.4), alpha=0.2, width=0.5, linewidth=0.5) +
  geom_hline(yintercept = 0, linetype=2) +
  scale_shape_discrete(name="",labels=c(my_lab[1],my_lab[2],my_lab[3],my_lab[4],my_lab[5])) +
  xlab("Travel time increase") +
  scale_y_continuous(
    name = "Value per capita",
    sec.axis = sec_axis( trans=~.* nb_person_per_area * proportion_user_tram, name="Total value")) +
  theme_modern(legend.position = c(.2, .9))
```
