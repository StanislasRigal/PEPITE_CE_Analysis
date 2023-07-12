remotes::install_github("mauricio1986/gmnl")
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(gmnl) # Multinomial Logit Models with Random Parameters
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics
library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax
library(mlogit) # Multinomial Logit Models
library(stargazer) # Well-Formatted Regression and Summary Statistics Tables
library(tibble) # Simple Data Frames
library(tidyr) # Tidy Messy Data

# example from https://doi.org/10.1007/978-3-031-20719-8

data("RiskyTransport", package = "mlogit")

all_available <- RiskyTransport %>%
  group_by(chid) %>%
  summarise(no_rows = length(chid), .groups = 'drop') %>%
  filter(no_rows == 4) %>% select(chid)

RT <- inner_join(RiskyTransport,
                 all_available,
                 by = "chid") %>%
  drop_na()

df <- RT %>% group_by(mode) %>% summarise(proportion = sum(choice),
                                          `min (cost)` = min(cost),
                                          `mean (cost)` = mean(cost),
                                          `max(cost)` = max(cost),
                                          `min (risk)` = min(risk),
                                          `mean (risk)` = mean(risk),
                                          `max (risk)` = max(risk),
                                          .groups = 'drop') %>%
  mutate(proportion = proportion/sum(proportion)) %>%
  column_to_rownames(var = "mode")

RT <- RT %>%
  mutate(`cost:dwage` = cost * dwage,
         `risk:dwage` = risk * dwage,
         dwage2 = dwage^2)

RT <- mlogit.data(RT,
                  choice = "choice",
                  shape = "long",
                  alt.var = "mode",
                  id.var = "id",
                  chid.var = "chid")

mnl.rt0 <- mlogit(choice ~ cost + risk | 0,
                  data = RT)
2 * length(coef(mnl.rt0)) - 2 * mnl.rt0$logLik

lc2 <- gmnl(choice ~ cost + risk | 0 | 0 | 0 | 1,
            data = RT,
            model = 'lc',
            Q = 2,
            panel = TRUE,
            method = "bhhh")
2 * length(coef(lc2)) - 2 * lc2$logLik$maximum

lc3 <- gmnl(choice ~ cost + risk | 0 | 0 | 0 | 1,
            data = RT,
            model = 'lc',
            Q = 3,
            panel = TRUE,
            method = "bhhh")
2 * length(coef(lc3)) - 2 * lc3$logLik$maximum

as.numeric(exp(((2 * length(coef(lc3)) - 2 * lc3$logLik$maximum) -
                  (2 * length(coef(mnl.rt0)) - 2 * mnl.rt0$logLik))/2))

# apply to PEPITE data

data_DCE <- readRDS("output/data_DCE.rds")


df <- data_DCE[data_DCE$choice==1,] %>% group_by(survey_id) %>%summarise(proportion = sum(choice),
                                                                         `mean (Paysage)` = mean(Paysage),
                                                                         `mean (Temps)` = mean(Temps),
                                                                         `sum (Acces)` = sum(Acces),
                                                                         `mean (Biodiversite)` = mean(Biodiversite),
                                                                         `mean (Biome)` = mean(Biome),
                                                                         .groups = 'drop') %>%
  mutate(proportion = proportion/sum(proportion)) %>%
  column_to_rownames(var = "survey_id")

data_DCE_numeric <- data_DCE
data_DCE_numeric$Biome_rural <- ifelse(data_DCE_numeric$Biome == 2,1,0)
data_DCE_numeric$Biome_peri <- ifelse(data_DCE_numeric$Biome == 1,1,0)
data_DCE_numeric$Biome_urbain <- ifelse(data_DCE_numeric$Biome == 0,1,0)
data_DCE_numeric$Biome[which(data_DCE_numeric$Biome==-1)] <- 0
data_DCE_numeric$Biome <- as.factor(data_DCE_numeric$Biome)
#data_DCE_numeric$Biodiversite <- data_DCE_numeric$Biodiversite + 1
data_DCE_numeric$Biodiversite[which(data_DCE_numeric$Biodiversite == -1)] <- 0
data_DCE_numeric$Gender <- as.numeric(as.factor(data_DCE_numeric$Gender))
data_DCE_numeric <- droplevels(data_DCE_numeric[which(data_DCE_numeric$Gender != 3),])
data_DCE_numeric$Age <- as.numeric(as.factor(data_DCE_numeric$Age))
data_DCE_numeric$Income <- as.numeric(as.factor(data_DCE_numeric$Income))
data_DCE_numeric$Education <- as.numeric(as.factor(data_DCE_numeric$Education))
data_DCE_numeric$CSPgroup <- as.character(data_DCE_numeric$CSP)
data_DCE_numeric$CSPgroup[which(data_DCE_numeric$CSPgroup %in% c("Agriculteurs","Employés (employés administratifs de la fonction publique, agents de service et auxiliaires de santé, policiers, militaires, pompiers, agents de sécurité, employés administratifs, employés de commerce, personnels des services directs aux particuliers )","Ouvriers et conducteurs de transport"))] <- "moins"
data_DCE_numeric$CSPgroup[which(data_DCE_numeric$CSPgroup %in% c("Artisans, commerçants et chefs d'entreprise","Cadres et professions intellectuelles supérieures (professions libérales, cadres administratifs et techniques de la fonction publique, professions scientifiques supérieures, professions de l'information et de l'art, cadres des services administratifs et commerciaux d'entreprise, ingénieurs et cadres techniques d'entreprise)","Professions intermédiaires (professions de l'enseignement primaire et professionnel et du sport, professions intermédiaires de la santé, ministres du culte, professions intermédiaires de la fonction publique, professions intermédiaires administratives et commerciales des entreprises, techniciens, agents de maîtrise)"))] <- "plus"
data_DCE_numeric$CSPgroup[which(data_DCE_numeric$CSPgroup %in% c("Étudiants","Sans emploi"))] <- "Inactifs"
data_DCE_numeric$CSPgroup <- as.numeric(as.factor(data_DCE_numeric$CSPgroup))
data_DCE_numeric$main_vehicule <- as.character(data_DCE_numeric$vehicule_1)
data_DCE_numeric$main_vehicule[which(data_DCE_numeric$main_vehicule %in% c("bus","métro / RER métropolitain","train (TER / Intercité / TGV)","tramway"))] <- "commun"
data_DCE_numeric$main_vehicule[which(data_DCE_numeric$main_vehicule %in% c("moto, scooter","trottinette","vélo"))] <- "indiv_nv"
data_DCE_numeric$main_vehicule <- as.numeric(as.factor(data_DCE_numeric$main_vehicule))
data_DCE_numeric$asc <- ifelse(data_DCE_numeric$Scenario=="Scénario de référence",1,0)
data_DCE_numeric$Paysage <- as.numeric(as.factor(data_DCE_numeric$Paysage))-1
data_DCE_numeric$class_nat[which(data_DCE_numeric$class_nat == "Naturality --")] <- "a"
data_DCE_numeric$class_nat[which(data_DCE_numeric$class_nat == "Naturality -")] <- "b"
data_DCE_numeric$class_nat[which(data_DCE_numeric$class_nat == "Naturality +")] <- "c"
data_DCE_numeric$class_nat <- as.numeric(as.factor(data_DCE_numeric$class_nat))
data_DCE_numeric$survey_id <- as.numeric(as.factor(data_DCE_numeric$survey_id))
data_DCE_numeric$Perso_relation_nature <- as.numeric(data_DCE_numeric$Perso_relation_nature)
data_DCE_numeric$time_associative[which(is.na(data_DCE_numeric$time_associative))] <- 0
data_DCE_numeric$time_domestic[which(is.na(data_DCE_numeric$time_domestic))] <- 0
data_DCE_numeric$time_driver[which(is.na(data_DCE_numeric$time_driver))] <- 0
data_DCE_numeric$time_leisure[which(is.na(data_DCE_numeric$time_leisure))] <- 0
data_DCE_numeric$time_professionel[which(is.na(data_DCE_numeric$time_professionel))] <- 0
data_DCE_numeric$journey_duration <- data_DCE_numeric$time_associative + data_DCE_numeric$time_domestic + data_DCE_numeric$time_driver +data_DCE_numeric$time_leisure + data_DCE_numeric$time_professionel
data_DCE_numeric$journey_duration2 <- ifelse(data_DCE_numeric$journey_duration <= 180, data_DCE_numeric$journey_duration, NA)

saveRDS(data_DCE_numeric,"output/data_DCE_numeric.rds")

# both survey merged

data_DCE_mlogit <- mlogit.data(data_DCE_numeric,
                               choice = "choice",
                               shape = "long",
                               alt.var = "Scenario",
                               id.var = "survey_person",
                               chid.var = "chid")

mnl.rt0 <- mlogit(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0,
                  data = data_DCE_mlogit)
2 * length(coef(mnl.rt0)) - 2 * mnl.rt0$logLik


lc2 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat + survey_id + journey_duration2,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 2,
            panel = TRUE)
summary(lc2) # likelihood to maximize (0)
# AIC to minimize
AIC_lc2 <- 2 * length(coef(lc2)) - 2 * lc2$logLik$maximum
# BIC to minimize
BIC_lc2 <- length(coef(lc2)) * log(nrow(lc2$residuals)) - 2 * lc2$logLik$maximum
# CAIC
CAIC_lc2 <- length(coef(lc2)) * (log(nrow(lc2$residuals)) + 1 ) - 2 * lc2$logLik$maximum
# AWE
AWE_lc2 <-  2* length(coef(lc2)) * (log(nrow(lc2$residuals)) + 1.5 ) - 2 * lc2$logLik$maximum

apply(lc2$Wnq,2,mean)
freq_g2 <- exp(coef(lc2)["(class)2"]) / (exp(0) + exp(coef(lc2)["(class)2"]))
freq_g1 <- 1-freq_g2
shares(lc2)


lc3 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat + survey_id + journey_duration2,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 3,
            panel = TRUE)
summary(lc3)
AIC_lc3 <- 2 * length(coef(lc3)) - 2 * lc3$logLik$maximum
BIC_lc3 <- length(coef(lc3)) * log(nrow(lc3$residuals)) - 2 * lc3$logLik$maximum
CAIC_lc3 <- length(coef(lc3)) * (log(nrow(lc3$residuals)) + 1 ) - 2 * lc3$logLik$maximum
AWE_lc3 <-  2* length(coef(lc3)) * (log(nrow(lc3$residuals)) + 1.5 ) - 2 * lc3$logLik$maximum

apply(lc3$Wnq,2,mean)
freq_g2 <- exp(coef(lc3)["(class)2"]) / (exp(0) + exp(coef(lc3)["(class)2"]) + exp(coef(lc3)["(class)3"]))
freq_g3 <- exp(coef(lc3)["(class)3"]) / (exp(0) + exp(coef(lc3)["(class)2"]) + exp(coef(lc3)["(class)3"]))
freq_g1 <- 1-freq_g2-freq_g3
shares(lc3)

predictions <- apply(lc3$prob.alt,1,  which.max)
lc3_choice <- c()
for(i in 1:(length(lc3$mf$choice)/3)){
  index_choice <- seq(from = 1, to = length(lc3$mf$choice), by=3)[i]
  lc3_choice[i] <- which(lc3$mf$choice[index_choice:(index_choice+2)])
}
table(predictions, lc3_choice)
predictions <- apply(lc3$Wnq,1,  which.max)



lc4 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat + survey_id + journey_duration2,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 4,
            panel = TRUE)
summary(lc4)
AIC_lc4 <- 2 * length(coef(lc4)) - 2 * lc4$logLik$maximum
BIC_lc4 <- length(coef(lc4)) * log(nrow(lc4$residuals)) - 2 * lc4$logLik$maximum
CAIC_lc4 <- length(coef(lc4)) * (log(nrow(lc4$residuals)) + 1 ) - 2 * lc4$logLik$maximum
AWE_lc4 <-  2* length(coef(lc4)) * (log(nrow(lc4$residuals)) + 1.5 ) - 2 * lc4$logLik$maximum

apply(lc4$Wnq,2,mean)
freq_g2 <- exp(coef(lc4)["(class)2"]) / (exp(0) + exp(coef(lc4)["(class)2"]) + exp(coef(lc4)["(class)3"]) + exp(coef(lc4)["(class)4"]))
freq_g3 <- exp(coef(lc4)["(class)3"]) / (exp(0) + exp(coef(lc4)["(class)2"]) + exp(coef(lc4)["(class)3"]) + exp(coef(lc4)["(class)4"]))
freq_g4 <- exp(coef(lc4)["(class)4"]) / (exp(0) + exp(coef(lc4)["(class)2"]) + exp(coef(lc4)["(class)3"]) + exp(coef(lc4)["(class)4"]))
freq_g1 <- 1-freq_g2-freq_g3-freq_g4
shares(lc4)

lc5 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat + survey_id + journey_duration2,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 5,
            panel = TRUE)
summary(lc5)
AIC_lc5 <- 2 * length(coef(lc5)) - 2 * lc5$logLik$maximum
BIC_lc5 <- length(coef(lc5)) * log(nrow(lc5$residuals)) - 2 * lc5$logLik$maximum
CAIC_lc5 <- length(coef(lc5)) * (log(nrow(lc5$residuals)) + 1 ) - 2 * lc5$logLik$maximum
AWE_lc5 <-  2* length(coef(lc5)) * (log(nrow(lc5$residuals)) + 1.5 ) - 2 * lc5$logLik$maximum

apply(lc5$Wnq,2,mean)
freq_g2 <- exp(coef(lc5)["(class)2"]) / (exp(0) + exp(coef(lc5)["(class)2"]) + exp(coef(lc5)["(class)3"]) + exp(coef(lc5)["(class)4"]) + exp(coef(lc5)["(class)5"]))
freq_g3 <- exp(coef(lc5)["(class)3"]) / (exp(0) + exp(coef(lc5)["(class)2"]) + exp(coef(lc5)["(class)3"]) + exp(coef(lc5)["(class)4"]) + exp(coef(lc5)["(class)5"]))
freq_g4 <- exp(coef(lc5)["(class)4"]) / (exp(0) + exp(coef(lc5)["(class)2"]) + exp(coef(lc5)["(class)3"]) + exp(coef(lc5)["(class)4"]) + exp(coef(lc5)["(class)5"]))
freq_g5 <- exp(coef(lc5)["(class)5"]) / (exp(0) + exp(coef(lc5)["(class)2"]) + exp(coef(lc5)["(class)3"]) + exp(coef(lc5)["(class)4"]) + exp(coef(lc5)["(class)5"]))
freq_g1 <- 1-freq_g2-freq_g3-freq_g4-freq_g5
shares(lc5)

lc6 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat + survey_id + journey_duration2,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 6,
            panel = TRUE)
summary(lc6)
AIC_lc6 <- 2 * length(coef(lc6)) - 2 * lc6$logLik$maximum
BIC_lc6 <- length(coef(lc6)) * log(nrow(lc6$residuals)) - 2 * lc6$logLik$maximum
CAIC_lc6 <- length(coef(lc6)) * (log(nrow(lc6$residuals)) + 1 ) - 2 * lc6$logLik$maximum
AWE_lc6 <-  2* length(coef(lc6)) * (log(nrow(lc6$residuals)) + 1.5 ) - 2 * lc6$logLik$maximum

apply(lc6$Wnq,2,mean)
freq_g2 <- exp(coef(lc6)["(class)2"]) / (exp(0) + exp(coef(lc6)["(class)2"]) + exp(coef(lc6)["(class)3"]) + exp(coef(lc6)["(class)4"]) + exp(coef(lc6)["(class)5"]) + exp(coef(lc6)["(class)6"]))
freq_g3 <- exp(coef(lc6)["(class)3"]) / (exp(0) + exp(coef(lc6)["(class)2"]) + exp(coef(lc6)["(class)3"]) + exp(coef(lc6)["(class)4"]) + exp(coef(lc6)["(class)5"]) + exp(coef(lc6)["(class)6"]))
freq_g4 <- exp(coef(lc6)["(class)4"]) / (exp(0) + exp(coef(lc6)["(class)2"]) + exp(coef(lc6)["(class)3"]) + exp(coef(lc6)["(class)4"]) + exp(coef(lc6)["(class)5"]) + exp(coef(lc6)["(class)6"]))
freq_g5 <- exp(coef(lc6)["(class)5"]) / (exp(0) + exp(coef(lc6)["(class)2"]) + exp(coef(lc6)["(class)3"]) + exp(coef(lc6)["(class)4"]) + exp(coef(lc6)["(class)5"]) + exp(coef(lc6)["(class)6"]))
freq_g6 <- exp(coef(lc6)["(class)6"]) / (exp(0) + exp(coef(lc6)["(class)2"]) + exp(coef(lc6)["(class)3"]) + exp(coef(lc6)["(class)4"]) + exp(coef(lc6)["(class)5"]) + exp(coef(lc6)["(class)6"]))
freq_g1 <- 1-freq_g2-freq_g3-freq_g4-freq_g5-freq_g6
shares(lc6)

RL <- exp((BIC_lc5-BIC_lc4)/2)

# Model evaluation by fit quality

IC_mod <- data.frame(group = c(2:6),
                     AIC = c(AIC_lc2,AIC_lc3,AIC_lc4,AIC_lc5,AIC_lc6),
                     BIC = c(BIC_lc2,BIC_lc3,BIC_lc4,BIC_lc5,BIC_lc6),
                     CAIC = c(CAIC_lc2,CAIC_lc3,CAIC_lc4,CAIC_lc5,CAIC_lc6),
                     AWE = c(AWE_lc2,AWE_lc3,AWE_lc4,AWE_lc5,AWE_lc6),
                     log_likelihood = c(summary(lc2)$logLik$maximum,summary(lc3)$logLik$maximum,summary(lc4)$logLik$maximum,summary(lc5)$logLik$maximum,summary(lc6)$logLik$maximum))

IC_mod_long <- melt(IC_mod, id.vars = "group")

ggplot(IC_mod_long) + 
  geom_line(data=IC_mod_long[which(IC_mod_long$variable!="log_likelihood"),], aes(x = group, y=value, col=variable)) +
  theme_modern()

ggsave("output/ic_mod.png",
       width = 8,
       height = 6,
       dpi = 300)

plot_ci_lc(lc4)


# Model evaluation by classification diagnostics

# Conditional probabilitites
pi_hat <- lc4$Qir
colnames(pi_hat) <- c("q = 1", "q = 2", "q = 3", "q = 4")

# Posterior probability
predictions <- apply(pi_hat,1,  which.max)
matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),mean(pi_hat[which(predictions==1),3]),mean(pi_hat[which(predictions==1),4]),
               mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2]),mean(pi_hat[which(predictions==2),3]),mean(pi_hat[which(predictions==2),4]),
               mean(pi_hat[which(predictions==3),1]),mean(pi_hat[which(predictions==3),2]),mean(pi_hat[which(predictions==3),3]),mean(pi_hat[which(predictions==3),4]),
               mean(pi_hat[which(predictions==4),1]),mean(pi_hat[which(predictions==4),2]),mean(pi_hat[which(predictions==4),3]),mean(pi_hat[which(predictions==4),4])),2), nrow=4)


pi_hat <- lc5$Qir

predictions <- apply(pi_hat,1,  which.max)
matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),mean(pi_hat[which(predictions==1),3]),mean(pi_hat[which(predictions==1),4]),mean(pi_hat[which(predictions==1),5]),
               mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2]),mean(pi_hat[which(predictions==2),3]),mean(pi_hat[which(predictions==2),4]),mean(pi_hat[which(predictions==2),5]),
               mean(pi_hat[which(predictions==3),1]),mean(pi_hat[which(predictions==3),2]),mean(pi_hat[which(predictions==3),3]),mean(pi_hat[which(predictions==3),4]),mean(pi_hat[which(predictions==3),5]),
               mean(pi_hat[which(predictions==4),1]),mean(pi_hat[which(predictions==4),2]),mean(pi_hat[which(predictions==4),3]),mean(pi_hat[which(predictions==4),4]),mean(pi_hat[which(predictions==4),5]),
               mean(pi_hat[which(predictions==4),1]),mean(pi_hat[which(predictions==5),2]),mean(pi_hat[which(predictions==5),3]),mean(pi_hat[which(predictions==5),4]),mean(pi_hat[which(predictions==5),5])),2), nrow=5)


# Get individuals' estimates for Paysage
bi_Paysage <- effect.gmnl(lc4, par = "Paysage", effect = "ce")$mean
summary(bi_Paysage)
# Plotting the distribution of the individuals' estimates
plot(lc4, par = "Paysage", effect = "ce", type = "density", col = "blue")

# Get individuals' estimates for Acces
bi_Acces <- effect.gmnl(lc4, par = "Acces", effect = "ce")$mean
summary(bi_Acces)
# Plotting the distribution of the individuals' estimates
plot(lc4, par = "Acces", effect = "ce", type = "density", col = "blue")

# Get individuals' estimates for Biodiversite
bi_Biodiversite <- effect.gmnl(lc4, par = "Biodiversite", effect = "ce")$mean
summary(bi_Biodiversite)
# Plotting the distribution of the individuals' estimates
plot(lc4, par = "Biodiversite", effect = "ce", type = "density", col = "blue")

# Get individuals' estimates for Biome
bi_Biome <- effect.gmnl(lc4, par = "Biome", effect = "ce")$mean
summary(bi_Biome)
# Plotting the distribution of the individuals' estimates
plot(lc4, par = "Biome", effect = "ce", type = "density", col = "blue")


# classe 1 : saving time
# classe 2 : trade-off time-biodiv
# classe 3 : gain access and nice landscape whatever the cost
# classe 4 : saving biodiversity whatever the cost


#Create a data frame for plotting:
df <- data.frame(Age = seq(min(data_DCE_mlogit$Age),
                             to = max(data_DCE_mlogit$Age),
                             by = (max(data_DCE_mlogit$Age) - min(data_DCE_mlogit$Age))/100))
# Use the class-membership model to calculate the membership probabilities
df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Age"] * Age) +
            exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Age"] * Age) + 
            exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Age"] * Age))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Age"] * Age) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Age"] * Age) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Age"] * Age)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Age"] * Age)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Age"] * Age) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Age"] * Age) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Age"] * Age)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Age"] * Age)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Age"] * Age) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Age"] * Age) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Age"] * Age)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Age"] * Age)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Age"] * Age) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Age"] * Age) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Age"] * Age))
         ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Age,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = Age)) +
  geom_line(aes(x = Age,
                y = p,
                color = Class)) +
  theme_modern()

ggsave("output/lc4_age.png",
       width = 8,
       height = 6,
       dpi = 300)

#Create a data frame for plotting:
df <- data.frame(Income = seq(min(na.omit(data_DCE_mlogit$Income)),
                           to = max(na.omit(data_DCE_mlogit$Income)),
                           by = (max(na.omit(data_DCE_mlogit$Income)) - min(na.omit(data_DCE_mlogit$Income)))/100))
# Use the class-membership model to calculate the membership probabilities
df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Income"] * Income) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Income"] * Income) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Income"] * Income))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Income"] * Income) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Income"] * Income) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Income"] * Income)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Income"] * Income)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Income"] * Income) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Income"] * Income) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Income"] * Income)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Income"] * Income)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Income"] * Income) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Income"] * Income) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Income"] * Income)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Income"] * Income)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Income"] * Income) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Income"] * Income) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Income"] * Income))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Income,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = Income)) +
  geom_line(aes(x = Income,
                y = p,
                color = Class)) +
  theme_modern()

ggsave("output/lc4_income.png",
       width = 8,
       height = 6,
       dpi = 300)

#Create a data frame for plotting:
df <- data.frame(CSPgroup = seq(min(data_DCE_mlogit$CSPgroup),
                           to = max(data_DCE_mlogit$CSPgroup),
                           by = (max(data_DCE_mlogit$CSPgroup) - min(data_DCE_mlogit$CSPgroup))/100))
# Use the class-membership model to calculate the membership probabilities
df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup"] * CSPgroup) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup"] * CSPgroup) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup"] * CSPgroup))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup"] * CSPgroup) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup"] * CSPgroup) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup"] * CSPgroup)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup"] * CSPgroup)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup"] * CSPgroup) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup"] * CSPgroup) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup"] * CSPgroup)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup"] * CSPgroup)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup"] * CSPgroup) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup"] * CSPgroup) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup"] * CSPgroup)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup"] * CSPgroup)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup"] * CSPgroup) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup"] * CSPgroup) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup"] * CSPgroup))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -CSPgroup,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = CSPgroup)) +
  geom_line(aes(x = CSPgroup,
                y = p,
                color = Class))


#Create a data frame for plotting:
df <- data.frame(class_nat = seq(min(na.omit(data_DCE_mlogit$class_nat)),
                           to = max(na.omit(data_DCE_mlogit$class_nat)),
                           by = (max(na.omit(data_DCE_mlogit$class_nat)) - min(na.omit(data_DCE_mlogit$class_nat)))/100))
# Use the class-membership model to calculate the membership probabilities
df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:class_nat"] * class_nat) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class3:class_nat"] * class_nat) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class4:class_nat"] * class_nat))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:class_nat"] * class_nat) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:class_nat"] * class_nat) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:class_nat"] * class_nat)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:class_nat"] * class_nat)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:class_nat"] * class_nat) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:class_nat"] * class_nat) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:class_nat"] * class_nat)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:class_nat"] * class_nat)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:class_nat"] * class_nat) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:class_nat"] * class_nat) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:class_nat"] * class_nat)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:class_nat"] * class_nat)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:class_nat"] * class_nat) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:class_nat"] * class_nat) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:class_nat"] * class_nat))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -class_nat,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = class_nat)) +
  geom_line(aes(x = class_nat,
                y = p,
                color = Class)) +
  theme_modern()

ggsave("output/lc4_class_nat.png",
       width = 8,
       height = 6,
       dpi = 300)

#Create a data frame for plotting:
df <- data.frame(survey_id = seq(min(data_DCE_mlogit$survey_id),
                           to = max(data_DCE_mlogit$survey_id),
                           by = (max(data_DCE_mlogit$survey_id) - min(data_DCE_mlogit$survey_id))/100))
# Use the class-membership model to calculate the membership probabilities
df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:survey_id"] * survey_id) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class3:survey_id"] * survey_id) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class4:survey_id"] * survey_id))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:survey_id"] * survey_id) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:survey_id"] * survey_id) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:survey_id"] * survey_id)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:survey_id"] * survey_id)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:survey_id"] * survey_id) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:survey_id"] * survey_id) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:survey_id"] * survey_id)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:survey_id"] * survey_id)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:survey_id"] * survey_id) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:survey_id"] * survey_id) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:survey_id"] * survey_id)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:survey_id"] * survey_id)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:survey_id"] * survey_id) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:survey_id"] * survey_id) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:survey_id"] * survey_id))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -survey_id,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = survey_id)) +
  geom_line(aes(x = survey_id,
                y = p,
                color = Class)) +
  theme_modern()

ggsave("output/lc4_framing.png",
       width = 8,
       height = 6,
       dpi = 300)


#Create a data frame for plotting:
df <- data.frame(Perso_relation_nature = seq(min(na.omit(data_DCE_mlogit$Perso_relation_nature)),
                                 to = max(na.omit(data_DCE_mlogit$Perso_relation_nature)),
                                 by = (max(na.omit(data_DCE_mlogit$Perso_relation_nature)) - min(na.omit(data_DCE_mlogit$Perso_relation_nature)))/100))
# Use the class-membership model to calculate the membership probabilities
df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Perso_relation_nature"] * Perso_relation_nature) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Perso_relation_nature"] * Perso_relation_nature) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Perso_relation_nature"] * Perso_relation_nature))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Perso_relation_nature"] * Perso_relation_nature) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Perso_relation_nature"] * Perso_relation_nature) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Perso_relation_nature"] * Perso_relation_nature)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Perso_relation_nature"] * Perso_relation_nature)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Perso_relation_nature"] * Perso_relation_nature) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Perso_relation_nature"] * Perso_relation_nature) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Perso_relation_nature"] * Perso_relation_nature)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Perso_relation_nature"] * Perso_relation_nature)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Perso_relation_nature"] * Perso_relation_nature) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Perso_relation_nature"] * Perso_relation_nature) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Perso_relation_nature"] * Perso_relation_nature)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Perso_relation_nature"] * Perso_relation_nature)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:Perso_relation_nature"] * Perso_relation_nature) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:Perso_relation_nature"] * Perso_relation_nature) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:Perso_relation_nature"] * Perso_relation_nature))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Perso_relation_nature,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = Perso_relation_nature)) +
  geom_line(aes(x = Perso_relation_nature,
                y = p,
                color = Class)) +
  theme_modern()

ggsave("output/lc4_Perso_relation_nature.png",
       width = 8,
       height = 6,
       dpi = 300)

ggsave("output/lc4_sex.png",
       width = 8,
       height = 6,
       dpi = 300)


#Create a data frame for plotting:
df <- data.frame(Perso_relation_nature = seq(min(na.omit(data_DCE_mlogit$Perso_relation_nature)),
                              to = max(na.omit(data_DCE_mlogit$Perso_relation_nature)),
                              by = (max(na.omit(data_DCE_mlogit$Perso_relation_nature)) - min(na.omit(data_DCE_mlogit$Perso_relation_nature)))/100))
# Use the class-membership model to calculate the membership probabilities
df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["Perso_relation_nature:class2"] * Perso_relation_nature) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["Perso_relation_nature:class3"] * Perso_relation_nature) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["Perso_relation_nature:class4"] * Perso_relation_nature))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["Perso_relation_nature:class2"] * Perso_relation_nature) + exp(coef(lc4)["(class)3"] + coef(lc4)["Perso_relation_nature:class3"] * Perso_relation_nature) + exp(coef(lc4)["(class)4"] + coef(lc4)["Perso_relation_nature:class4"] * Perso_relation_nature)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["Perso_relation_nature:class2"] * Perso_relation_nature)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["Perso_relation_nature:class2"] * Perso_relation_nature) + exp(coef(lc4)["(class)3"] + coef(lc4)["Perso_relation_nature:class3"] * Perso_relation_nature) + exp(coef(lc4)["(class)4"] + coef(lc4)["Perso_relation_nature:class4"] * Perso_relation_nature)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["Perso_relation_nature:class3"] * Perso_relation_nature)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["Perso_relation_nature:class2"] * Perso_relation_nature) + exp(coef(lc4)["(class)3"] + coef(lc4)["Perso_relation_nature:class3"] * Perso_relation_nature) + exp(coef(lc4)["(class)4"] + coef(lc4)["Perso_relation_nature:class4"] * Perso_relation_nature)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["Perso_relation_nature:class4"] * Perso_relation_nature)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["Perso_relation_nature:class2"] * Perso_relation_nature) + exp(coef(lc4)["(class)3"] + coef(lc4)["Perso_relation_nature:class3"] * Perso_relation_nature) + exp(coef(lc4)["(class)4"] + coef(lc4)["Perso_relation_nature:class4"] * Perso_relation_nature))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Perso_relation_nature,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = Perso_relation_nature)) +
  geom_line(aes(x = Perso_relation_nature,
                y = p,
                color = Class)) +
  theme_modern()

ggsave("output/lc4_sex.png",
       width = 8,
       height = 6,
       dpi = 300)

# regarder INS et choix hétérodoxe dans chaque classe

membership_id <- data.frame(survey_person = unique(na.omit(data_DCE_mlogit[,c("choice","Temps","Paysage","Acces","Biodiversite","Biome","asc","Gender","Age","Income","CSPgroup","class_nat","survey_id","survey_person")])$survey_person),
                            class1=lc4$Wnq[,1]/apply(lc4$Wnq,2,mean)[1],
                            class2=lc4$Wnq[,2]/apply(lc4$Wnq,2,mean)[2],
                            class3=lc4$Wnq[,3]/apply(lc4$Wnq,2,mean)[3],
                            class4=lc4$Wnq[,4]/apply(lc4$Wnq,2,mean)[4])

membership_id$member <- 0
membership_id$member[which(lc4$Wnq[,1] >= quantile(lc4$Wnq[,1],0.95))] <- 1
membership_id$member[which(lc4$Wnq[,2] >= quantile(lc4$Wnq[,2],0.8))] <- 2
membership_id$member[which(lc4$Wnq[,3] >= quantile(lc4$Wnq[,3],0.52))] <- 3
membership_id$member[which(lc4$Wnq[,4] >= quantile(lc4$Wnq[,4],0.73))] <- 4


data_DCE_sub <- data_DCE_mlogit[which(!is.na(data_DCE_mlogit$choice) & !is.na(data_DCE_mlogit$Temps) & !is.na(data_DCE_mlogit$Paysage) & !is.na(data_DCE_mlogit$Acces) & !is.na(data_DCE_mlogit$Biodiversite) & !is.na(data_DCE_mlogit$Biome) &
                                        !is.na(data_DCE_mlogit$asc) & !is.na(data_DCE_mlogit$Gender) & !is.na(data_DCE_mlogit$Age) & !is.na(data_DCE_mlogit$Income) & !is.na(data_DCE_mlogit$CSPgroup) & !is.na(data_DCE_mlogit$class_nat) & !is.na(data_DCE_mlogit$survey_id) & !is.na(data_DCE_mlogit$survey_person)),c("choice","Temps","Paysage","Acces","Biodiversite","Biome","asc","Gender","Age","Income","CSPgroup","class_nat","survey_id","survey_person",
                                   "Perso_relation_nature","Alternative_tram_important","Alternative_pedestrian","Alternative_bicycle","Alternative_bus",
                                   "Alternative_carpool","Alternative_remote_work","Alternative_closer_work","Alternative_change_city")]
data_DCE_sub <- merge(data_DCE_sub,membership_id, by="survey_person",all.x=TRUE)

mean(as.numeric(data_DCE_sub$Perso_relation_nature)*data_DCE_sub$class1)

mean(data_DCE_sub$class1[which(data_DCE_sub$Alternative_tram_important=="Oui")])
mean(data_DCE_sub$class2[which(data_DCE_sub$Alternative_tram_important=="Oui")])
mean(data_DCE_sub$class3[which(data_DCE_sub$Alternative_tram_important=="Oui")])
mean(data_DCE_sub$class4[which(data_DCE_sub$Alternative_tram_important=="Oui")])

mean(data_DCE_sub$class1[which(data_DCE_sub$Alternative_pedestrian=="Oui")])
mean(data_DCE_sub$class2[which(data_DCE_sub$Alternative_pedestrian=="Oui")])
mean(data_DCE_sub$class3[which(data_DCE_sub$Alternative_pedestrian=="Oui")])
mean(data_DCE_sub$class4[which(data_DCE_sub$Alternative_pedestrian=="Oui")])

mean(data_DCE_sub$class1[which(data_DCE_sub$Alternative_bicycle=="Oui")])
mean(data_DCE_sub$class2[which(data_DCE_sub$Alternative_bicycle=="Oui")])
mean(data_DCE_sub$class3[which(data_DCE_sub$Alternative_bicycle=="Oui")])
mean(data_DCE_sub$class4[which(data_DCE_sub$Alternative_bicycle=="Oui")])

mean(data_DCE_sub$class1[which(data_DCE_sub$Alternative_bus=="Oui")])
mean(data_DCE_sub$class2[which(data_DCE_sub$Alternative_bus=="Oui")])
mean(data_DCE_sub$class3[which(data_DCE_sub$Alternative_bus=="Oui")])
mean(data_DCE_sub$class4[which(data_DCE_sub$Alternative_bus=="Oui")])

mean(data_DCE_sub$class1[which(data_DCE_sub$Alternative_carpool=="Oui")])
mean(data_DCE_sub$class2[which(data_DCE_sub$Alternative_carpool=="Oui")])
mean(data_DCE_sub$class3[which(data_DCE_sub$Alternative_carpool=="Oui")])
mean(data_DCE_sub$class4[which(data_DCE_sub$Alternative_carpool=="Oui")])

mean(data_DCE_sub$class1[which(data_DCE_sub$Alternative_remote_work=="Oui")])
mean(data_DCE_sub$class2[which(data_DCE_sub$Alternative_remote_work=="Oui")])
mean(data_DCE_sub$class3[which(data_DCE_sub$Alternative_remote_work=="Oui")])
mean(data_DCE_sub$class4[which(data_DCE_sub$Alternative_remote_work=="Oui")])

mean(data_DCE_sub$class1[which(data_DCE_sub$Alternative_closer_work=="Oui")])
mean(data_DCE_sub$class2[which(data_DCE_sub$Alternative_closer_work=="Oui")])
mean(data_DCE_sub$class3[which(data_DCE_sub$Alternative_closer_work=="Oui")])
mean(data_DCE_sub$class4[which(data_DCE_sub$Alternative_closer_work=="Oui")])

mean(data_DCE_sub$class1[which(data_DCE_sub$Alternative_change_city=="Oui")])
mean(data_DCE_sub$class2[which(data_DCE_sub$Alternative_change_city=="Oui")])
mean(data_DCE_sub$class3[which(data_DCE_sub$Alternative_change_city=="Oui")])
mean(data_DCE_sub$class4[which(data_DCE_sub$Alternative_change_city=="Oui")])

table(data_DCE_sub$Alternative_tram_important)/nrow(data_DCE_sub)
table(data_DCE_sub$Alternative_pedestrian)/nrow(data_DCE_sub)
table(data_DCE_sub$Alternative_bicycle)/nrow(data_DCE_sub)
table(data_DCE_sub$Alternative_bus)/nrow(data_DCE_sub)
table(data_DCE_sub$Alternative_carpool)/nrow(data_DCE_sub)
table(data_DCE_sub$Alternative_remote_work)/nrow(data_DCE_sub)
table(data_DCE_sub$Alternative_closer_work)/nrow(data_DCE_sub)
table(data_DCE_sub$Alternative_change_city)/nrow(data_DCE_sub)

data_DCE_sub2 <- data_DCE_mlogit[which(!is.na(data_DCE_mlogit$choice) & !is.na(data_DCE_mlogit$Temps) & !is.na(data_DCE_mlogit$Paysage) & !is.na(data_DCE_mlogit$Acces) & !is.na(data_DCE_mlogit$Biodiversite) & !is.na(data_DCE_mlogit$Biome) &
                                        !is.na(data_DCE_mlogit$asc) & !is.na(data_DCE_mlogit$Gender) & !is.na(data_DCE_mlogit$Age) & !is.na(data_DCE_mlogit$Income) & !is.na(data_DCE_mlogit$CSPgroup) & !is.na(data_DCE_mlogit$class_nat) & !is.na(data_DCE_mlogit$survey_id) & !is.na(data_DCE_mlogit$survey_person)),c("choice","Temps","Paysage","Acces","Biodiversite","Biome","asc","Gender","Age","Income","CSPgroup","class_nat","survey_id","survey_person",
                                                                                                                                                                                                                                                                                                                          "Perso_relation_nature","Alternative_tram_important","Alternative_pedestrian","Alternative_bicycle","Alternative_bus",
                                                                                                                                                                                                                                                                                                                          "Alternative_carpool","Alternative_remote_work","Alternative_closer_work","Alternative_change_city")]
data_DCE_sub2 <- merge(membership_id, data_DCE_sub,by="survey_person",all.x=TRUE)

ggplot(data_DCE_sub, aes(x=as.factor(member), y=as.numeric(Perso_relation_nature))) + 
  geom_boxplot(fill="slateblue", alpha=0.2)


# Willingness to spend time
## Paysage
-coef(lc4)["class.1.Paysage"] / coef(lc4)["class.1.Temps"]
as.numeric(lc4$coefficients[2]) / as.numeric(lc4$coefficients[1])
as.numeric(lc4$coefficients[8]) / as.numeric(lc4$coefficients[7])
as.numeric(lc4$coefficients[14]) / as.numeric(lc4$coefficients[13])
as.numeric(lc4$coefficients[20]) / as.numeric(lc4$coefficients[19])
## Acces
as.numeric(lc4$coefficients[3]) / as.numeric(lc4$coefficients[1])
as.numeric(lc4$coefficients[9]) / as.numeric(lc4$coefficients[7])
as.numeric(lc4$coefficients[15]) / as.numeric(lc4$coefficients[13])
as.numeric(lc4$coefficients[21]) / as.numeric(lc4$coefficients[19])
## Biodiversite
as.numeric(lc4$coefficients[4]) / as.numeric(lc4$coefficients[1])
as.numeric(lc4$coefficients[10]) / as.numeric(lc4$coefficients[7])
as.numeric(lc4$coefficients[16]) / as.numeric(lc4$coefficients[13])
as.numeric(lc4$coefficients[22]) / as.numeric(lc4$coefficients[19])
## Biome
as.numeric(lc4$coefficients[5]) / as.numeric(lc4$coefficients[1])
as.numeric(lc4$coefficients[11]) / as.numeric(lc4$coefficients[7])
as.numeric(lc4$coefficients[17]) / as.numeric(lc4$coefficients[13])
as.numeric(lc4$coefficients[23]) / as.numeric(lc4$coefficients[19])


# survey framing long


data_DCE_L_mlogit <- mlogit.data(data_DCE_numeric[which(data_DCE_numeric$survey_id==2),], # 672984
                                 choice = "choice",
                                 shape = "long",
                                 alt.var = "Scenario",
                                 id.var = "id",
                                 chid.var = "chid")

mnl.rt0_L <- mlogit(choice ~ Temps + Paysage + Acces + Biodiversite + Biome  | 0,
                    data = data_DCE_L_mlogit)
summary(mnl.rt0_L)
2 * length(coef(mnl.rt0_L)) - 2 * mnl.rt0_L$logLik


lc2_L <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat,
            data = data_DCE_L_mlogit,
            model = 'lc',
            Q = 2,
            panel = TRUE)
summary(lc2_L)
AIC_lc2_L <- 2 * length(coef(lc2_L)) - 2 * lc2_L$logLik$maximum
BIC_lc2_L <- length(coef(lc2_L)) * log(nrow(lc2_L$residuals)) - 2 * lc2_L$logLik$maximum
CAIC_lc2_L <- length(coef(lc2_L)) * (log(nrow(lc2_L$residuals)) + 1 ) - 2 * lc2_L$logLik$maximum
AWE_lc2_L <-  2* length(coef(lc2_L)) * (log(nrow(lc2_L$residuals)) + 1.5 ) - 2 * lc2_L$logLik$maximum
apply(lc2_L$Wnq,2,mean)

lc3_L <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat,
            data = data_DCE_L_mlogit,
            model = 'lc',
            Q = 3,
            panel = TRUE)
summary(lc3_L)
AIC_lc3_L <- 2 * length(coef(lc3_L)) - 2 * lc3_L$logLik$maximum
BIC_lc3_L <- length(coef(lc3_L)) * log(nrow(lc3_L$residuals)) - 2 * lc3_L$logLik$maximum
CAIC_lc3_L <- length(coef(lc3_L)) * (log(nrow(lc3_L$residuals)) + 1 ) - 2 * lc3_L$logLik$maximum
AWE_lc3_L <-  2* length(coef(lc3_L)) * (log(nrow(lc3_L$residuals)) + 1.5 ) - 2 * lc3_L$logLik$maximum
apply(lc3_L$Wnq,2,mean)

lc4_L <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat,
            data = data_DCE_L_mlogit,
            model = 'lc',
            Q = 4,
            panel = TRUE)
summary(lc4_L)
AIC_lc4_L <- 2 * length(coef(lc4_L)) - 2 * lc4_L$logLik$maximum
BIC_lc4_L <- length(coef(lc4_L)) * log(nrow(lc4_L$residuals)) - 2 * lc4_L$logLik$maximum
CAIC_lc4_L <- length(coef(lc4_L)) * (log(nrow(lc4_L$residuals)) + 1 ) - 2 * lc4_L$logLik$maximum
AWE_lc4_L <-  2* length(coef(lc4_L)) * (log(nrow(lc4_L$residuals)) + 1.5 ) - 2 * lc4_L$logLik$maximum
apply(lc4_L$Wnq,2,mean)

lc5_L <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat,
            data = data_DCE_L_mlogit,
            model = 'lc',
            Q = 5,
            panel = TRUE)
summary(lc5_L)
AIC_lc5_L <- 2 * length(coef(lc5_L)) - 2 * lc5_L$logLik$maximum
BIC_lc5_L <- length(coef(lc5_L)) * log(nrow(lc5_L$residuals)) - 2 * lc5_L$logLik$maximum
CAIC_lc5_L <- length(coef(lc5_L)) * (log(nrow(lc5_L$residuals)) + 1 ) - 2 * lc5_L$logLik$maximum
AWE_lc5_L <-  2* length(coef(lc5_L)) * (log(nrow(lc5_L$residuals)) + 1.5 ) - 2 * lc5_L$logLik$maximum
apply(lc5_L$Wnq,2,mean)

lc6_L <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat,
              data = data_DCE_L_mlogit,
              model = 'lc',
              Q = 6,
              panel = TRUE)
summary(lc6_L)
AIC_lc6_L <- 2 * length(coef(lc6_L)) - 2 * lc6_L$logLik$maximum
BIC_lc6_L <- length(coef(lc6_L)) * log(nrow(lc6_L$residuals)) - 2 * lc6_L$logLik$maximum
CAIC_lc6_L <- length(coef(lc6_L)) * (log(nrow(lc6_L$residuals)) + 1 ) - 2 * lc6_L$logLik$maximum
AWE_lc6_L <-  2* length(coef(lc6_L)) * (log(nrow(lc6_L$residuals)) + 1.5 ) - 2 * lc6_L$logLik$maximum
apply(lc6_L$Wnq,2,mean)

IC_mod_L <- data.frame(group = c(2:6),
                     AIC = c(AIC_lc2_L,AIC_lc3_L,AIC_lc4_L,AIC_lc5_L,AIC_lc6_L),
                     BIC = c(BIC_lc2_L,BIC_lc3_L,BIC_lc4_L,BIC_lc5_L,BIC_lc6_L),
                     CAIC = c(CAIC_lc2_L,CAIC_lc3_L,CAIC_lc4_L,CAIC_lc5_L,CAIC_lc6_L),
                     AWE = c(AWE_lc2_L,AWE_lc3_L,AWE_lc4_L,AWE_lc5_L,AWE_lc6_L),
                     log_likelihood = c(summary(lc2_L)$logLik$maximum,summary(lc3_L)$logLik$maximum,summary(lc4_L)$logLik$maximum,summary(lc5_L)$logLik$maximum,summary(lc6_L)$logLik$maximum))

IC_mod_long_L <- melt(IC_mod_L, id.vars = "group")

ggplot(IC_mod_long_L) + 
  geom_line(data=IC_mod_long_L[which(IC_mod_long_L$variable!="log_likelihood"),], aes(x = group, y=value, col=variable)) +
  theme_modern()

ggsave("output/ic_mod_l.png",
       width = 8,
       height = 6,
       dpi = 300)

# classe 1 : saving time
# classe 2 : time and biodiv
# classe 3 : time and landscape
# classe 4 : biodiv
# classe 4 : access

# survey framing short


data_DCE_S_mlogit <- mlogit.data(data_DCE_numeric[which(data_DCE_numeric$survey_id==2),], # 672984
                                 choice = "choice",
                                 shape = "long",
                                 alt.var = "Scenario",
                                 id.var = "id",
                                 chid.var = "chid")

mnl.rt0_S <- mlogit(choice ~ Temps + Paysage + Acces + Biodiversite + Biome  | 0,
                    data = data_DCE_S_mlogit)
summary(mnl.rt0_S)
2 * length(coef(mnl.rt0_S)) - 2 * mnl.rt0_S$logLik


lc2_S <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat,
              data = data_DCE_S_mlogit,
              model = 'lc',
              Q = 2,
              panel = TRUE)
summary(lc2_S)
AIC_lc2_S <- 2 * length(coef(lc2_S)) - 2 * lc2_S$logLik$maximum
BIC_lc2_S <- length(coef(lc2_S)) * log(nrow(lc2_S$residuals)) - 2 * lc2_S$logLik$maximum
CAIC_lc2_S <- length(coef(lc2_S)) * (log(nrow(lc2_S$residuals)) + 1 ) - 2 * lc2_S$logLik$maximum
AWE_lc2_S <-  2* length(coef(lc2_S)) * (log(nrow(lc2_S$residuals)) + 1.5 ) - 2 * lc2_S$logLik$maximum
apply(lc2_S$Wnq,2,mean)

lc3_S <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat,
              data = data_DCE_S_mlogit,
              model = 'lc',
              Q = 3,
              panel = TRUE)
summary(lc3_S)
AIC_lc3_S <- 2 * length(coef(lc3_S)) - 2 * lc3_S$logLik$maximum
BIC_lc3_S <- length(coef(lc3_S)) * log(nrow(lc3_S$residuals)) - 2 * lc3_S$logLik$maximum
CAIC_lc3_S <- length(coef(lc3_S)) * (log(nrow(lc3_S$residuals)) + 1 ) - 2 * lc3_S$logLik$maximum
AWE_lc3_S <-  2* length(coef(lc3_S)) * (log(nrow(lc3_S$residuals)) + 1.5 ) - 2 * lc3_S$logLik$maximum
apply(lc3_S$Wnq,2,mean)

lc4_S <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat,
              data = data_DCE_S_mlogit,
              model = 'lc',
              Q = 4,
              panel = TRUE)
summary(lc4_S)
AIC_lc4_S <- 2 * length(coef(lc4_S)) - 2 * lc4_S$logLik$maximum
BIC_lc4_S <- length(coef(lc4_S)) * log(nrow(lc4_S$residuals)) - 2 * lc4_S$logLik$maximum
CAIC_lc4_S <- length(coef(lc4_S)) * (log(nrow(lc4_S$residuals)) + 1 ) - 2 * lc4_S$logLik$maximum
AWE_lc4_S <-  2* length(coef(lc4_S)) * (log(nrow(lc4_S$residuals)) + 1.5 ) - 2 * lc4_S$logLik$maximum
apply(lc4_S$Wnq,2,mean)

lc5_S <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat,
              data = data_DCE_S_mlogit,
              model = 'lc',
              Q = 5,
              panel = TRUE)
summary(lc5_S)
AIC_lc5_S <- 2 * length(coef(lc5_S)) - 2 * lc5_S$logLik$maximum
BIC_lc5_S <- length(coef(lc5_S)) * log(nrow(lc5_S$residuals)) - 2 * lc5_S$logLik$maximum
CAIC_lc5_S <- length(coef(lc5_S)) * (log(nrow(lc5_S$residuals)) + 1 ) - 2 * lc5_S$logLik$maximum
AWE_lc5_S <-  2* length(coef(lc5_S)) * (log(nrow(lc5_S$residuals)) + 1.5 ) - 2 * lc5_S$logLik$maximum
apply(lc5_S$Wnq,2,mean)

lc6_S <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup + class_nat,
              data = data_DCE_S_mlogit,
              model = 'lc',
              Q = 6,
              panel = TRUE)
summary(lc6_S)
AIC_lc6_S <- 2 * length(coef(lc6_S)) - 2 * lc6_S$logLik$maximum
BIC_lc6_S <- length(coef(lc6_S)) * log(nrow(lc6_S$residuals)) - 2 * lc6_S$logLik$maximum
CAIC_lc6_S <- length(coef(lc6_S)) * (log(nrow(lc6_S$residuals)) + 1 ) - 2 * lc6_S$logLik$maximum
AWE_lc6_S <-  2* length(coef(lc6_S)) * (log(nrow(lc6_S$residuals)) + 1.5 ) - 2 * lc6_S$logLik$maximum
apply(lc6_S$Wnq,2,mean)

IC_mod_S <- data.frame(group = c(2:6),
                       AIC = c(AIC_lc2_S,AIC_lc3_S,AIC_lc4_S,AIC_lc5_S,AIC_lc6_S),
                       BIC = c(BIC_lc2_S,BIC_lc3_S,BIC_lc4_S,BIC_lc5_S,BIC_lc6_S),
                       CAIC = c(CAIC_lc2_S,CAIC_lc3_S,CAIC_lc4_S,CAIC_lc5_S,CAIC_lc6_S),
                       AWE = c(AWE_lc2_S,AWE_lc3_S,AWE_lc4_S,AWE_lc5_S,AWE_lc6_S),
                       log_likelihood = c(summary(lc2_S)$logLik$maximum,summary(lc3_S)$logLik$maximum,summary(lc4_S)$logLik$maximum,summary(lc5_S)$logLik$maximum,summary(lc6_S)$logLik$maximum))

IC_mod_long_S <- melt(IC_mod_S, id.vars = "group")

ggplot(IC_mod_long_S) + 
  geom_line(data=IC_mod_long_S[which(IC_mod_long_S$variable!="log_likelihood"),], aes(x = group, y=value, col=variable)) +
  theme_modern()

ggsave("output/ic_mod_S.png",
       width = 8,
       height = 6,
       dpi = 300)

# classe 1 : saving time
# classe 2 : saving biodiversity whatever the cost
# classe 3 : gain access whatever the cost
# classe 4 : rural landscape