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
data_DCE_numeric$CSPgroup_inactif <- data_DCE_numeric$CSPgroup_plus <- data_DCE_numeric$CSPgroup_moins <- data_DCE_numeric$CSPgroup_retraite <- 0 
data_DCE_numeric$CSPgroup_inactif[which(data_DCE_numeric$CSPgroup == "Inactifs")] <- 1
data_DCE_numeric$CSPgroup_moins[which(data_DCE_numeric$CSPgroup == "moins")] <- 1
data_DCE_numeric$CSPgroup_plus[which(data_DCE_numeric$CSPgroup == "plus")] <- 1
data_DCE_numeric$CSPgroup_retraite[which(data_DCE_numeric$CSPgroup == "Retraités")] <- 1
data_DCE_numeric$CSPgroup <- as.numeric(as.factor(data_DCE_numeric$CSPgroup))
data_DCE_numeric$main_vehicule <- as.character(data_DCE_numeric$vehicule_1)
data_DCE_numeric$main_vehicule[which(data_DCE_numeric$main_vehicule %in% c("bus","métro / RER métropolitain","train (TER / Intercité / TGV)","tramway"))] <- "commun"
data_DCE_numeric$main_vehicule[which(data_DCE_numeric$main_vehicule %in% c("moto, scooter","voiture"))] <- "indiv_motor"
data_DCE_numeric$main_vehicule[which(data_DCE_numeric$main_vehicule %in% c("trottinette","vélo","à pied"))] <- "indiv_no_motor"
data_DCE_numeric$main_vehicule_commun <- ifelse(data_DCE_numeric$main_vehicule == "commun",1,0)
data_DCE_numeric$main_vehicule_indiv_no_motor <- ifelse(data_DCE_numeric$main_vehicule == "indiv_no_motor",1,0)
data_DCE_numeric$main_vehicule_indiv_motor <- ifelse(data_DCE_numeric$main_vehicule == "indiv_motor",1,0)
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
data_DCE_numeric$journey_duration3 <- data_DCE_numeric$journey_duration
data_DCE_numeric$journey_duration3[which(data_DCE_numeric$journey_duration3<21)] <- 1
data_DCE_numeric$journey_duration3[which(data_DCE_numeric$journey_duration3>20 & data_DCE_numeric$journey_duration3<31)] <- 2
data_DCE_numeric$journey_duration3[which(data_DCE_numeric$journey_duration3>30 & data_DCE_numeric$journey_duration3<41)] <- 3
data_DCE_numeric$journey_duration3[which(data_DCE_numeric$journey_duration3>40 & data_DCE_numeric$journey_duration3<61)] <- 4
data_DCE_numeric$journey_duration3[which(data_DCE_numeric$journey_duration3>60 & data_DCE_numeric$journey_duration3<91)] <- 5
data_DCE_numeric$journey_duration3[which(data_DCE_numeric$journey_duration3>90)] <- 6


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


lc2 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup_inactif + CSPgroup_plus + CSPgroup_moins + class_nat + survey_id + journey_duration3 + main_vehicule_indiv_motor + main_vehicule_commun,
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


lc3 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup_inactif + CSPgroup_plus + CSPgroup_moins + class_nat + survey_id + journey_duration3 + main_vehicule_indiv_motor + main_vehicule_commun,
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



lc4 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup_inactif + CSPgroup_plus + CSPgroup_moins + class_nat + survey_id + journey_duration3 + main_vehicule_indiv_motor + main_vehicule_commun,
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

lc5 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup_inactif + CSPgroup_plus + CSPgroup_moins + class_nat + survey_id + journey_duration3 + main_vehicule_indiv_motor + main_vehicule_commun,
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

lc6 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup_inactif + CSPgroup_plus + CSPgroup_moins + class_nat + survey_id + journey_duration3 + main_vehicule_indiv_motor + main_vehicule_commun,
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

lc7 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Gender + Age + Income + CSPgroup_inactif + CSPgroup_plus + CSPgroup_moins + class_nat + survey_id + journey_duration3 + main_vehicule_indiv_motor + main_vehicule_commun,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 7,
            panel = TRUE)
summary(lc7)
AIC_lc7 <- 2 * length(coef(lc7)) - 2 * lc7$logLik$maximum
BIC_lc7 <- length(coef(lc7)) * log(nrow(lc7$residuals)) - 2 * lc7$logLik$maximum
CAIC_lc7 <- length(coef(lc7)) * (log(nrow(lc7$residuals)) + 1 ) - 2 * lc7$logLik$maximum
AWE_lc7 <-  2* length(coef(lc7)) * (log(nrow(lc7$residuals)) + 1.5 ) - 2 * lc7$logLik$maximum

shares(lc7)

RL <- exp((BIC_lc5-BIC_lc4)/2)

# Model evaluation by fit quality

IC_mod <- data.frame(group = c(2:7),
                     AIC = c(AIC_lc2,AIC_lc3,AIC_lc4,AIC_lc5,AIC_lc6,AIC_lc7),
                     BIC = c(BIC_lc2,BIC_lc3,BIC_lc4,BIC_lc5,BIC_lc6,BIC_lc7),
                     CAIC = c(CAIC_lc2,CAIC_lc3,CAIC_lc4,CAIC_lc5,CAIC_lc6,CAIC_lc7),
                     AWE = c(AWE_lc2,AWE_lc3,AWE_lc4,AWE_lc5,AWE_lc6,AWE_lc7),
                     log_likelihood = c(summary(lc2)$logLik$maximum,summary(lc3)$logLik$maximum,summary(lc4)$logLik$maximum,summary(lc5)$logLik$maximum,summary(lc6)$logLik$maximum,summary(lc7)$logLik$maximum))

IC_mod_long <- melt(IC_mod, id.vars = "group")

ggplot(IC_mod_long) + 
  geom_line(data=IC_mod_long[which(IC_mod_long$variable!="log_likelihood"),], aes(x = group, y=value, col=variable)) +
  theme_modern()

ggsave("output/ic_mod.png",
       width = 8,
       height = 6,
       dpi = 300)

plot_ci_lc(lc4)
plot_ci_lc(lc4, var = c("Temps","Paysage","Acces","Biodiversite","Biome1","Biome2"))
# modifier pour ggplot :
plot_ci_lc_ggplot

# Model evaluation by classification diagnostics

# Conditional probabilitites
pi_hat <- lc4$Qir
colnames(pi_hat) <- c("q = 1", "q = 2", "q = 3", "q = 4")

# Posterior probability
predictions <- apply(pi_hat,1,  which.max)
m4 <- matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),mean(pi_hat[which(predictions==1),3]),mean(pi_hat[which(predictions==1),4]),
               mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2]),mean(pi_hat[which(predictions==2),3]),mean(pi_hat[which(predictions==2),4]),
               mean(pi_hat[which(predictions==3),1]),mean(pi_hat[which(predictions==3),2]),mean(pi_hat[which(predictions==3),3]),mean(pi_hat[which(predictions==3),4]),
               mean(pi_hat[which(predictions==4),1]),mean(pi_hat[which(predictions==4),2]),mean(pi_hat[which(predictions==4),3]),mean(pi_hat[which(predictions==4),4])),3), nrow=4)
m4

mean(diag(m4))

pi_hat <- lc2$Qir
predictions <- apply(pi_hat,1,  which.max)
m2 <- matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),
                     mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2])),3), nrow=2)

m2
mean(diag(m2))

pi_hat <- lc3$Qir
predictions <- apply(pi_hat,1,  which.max)
m3 <- matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),mean(pi_hat[which(predictions==1),3]),
                     mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2]),mean(pi_hat[which(predictions==2),3]),
                     mean(pi_hat[which(predictions==3),1]),mean(pi_hat[which(predictions==3),2]),mean(pi_hat[which(predictions==3),3])),3), nrow=3)

m3
mean(diag(m3))

pi_hat <- lc5$Qir
predictions <- apply(pi_hat,1,  which.max)
m5 <- matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),mean(pi_hat[which(predictions==1),3]),mean(pi_hat[which(predictions==1),4]),mean(pi_hat[which(predictions==1),5]),
               mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2]),mean(pi_hat[which(predictions==2),3]),mean(pi_hat[which(predictions==2),4]),mean(pi_hat[which(predictions==2),5]),
               mean(pi_hat[which(predictions==3),1]),mean(pi_hat[which(predictions==3),2]),mean(pi_hat[which(predictions==3),3]),mean(pi_hat[which(predictions==3),4]),mean(pi_hat[which(predictions==3),5]),
               mean(pi_hat[which(predictions==4),1]),mean(pi_hat[which(predictions==4),2]),mean(pi_hat[which(predictions==4),3]),mean(pi_hat[which(predictions==4),4]),mean(pi_hat[which(predictions==4),5]),
               mean(pi_hat[which(predictions==5),1]),mean(pi_hat[which(predictions==5),2]),mean(pi_hat[which(predictions==5),3]),mean(pi_hat[which(predictions==5),4]),mean(pi_hat[which(predictions==5),5])),3), nrow=5)

m5

mean(diag(m5))

pi_hat <- lc6$Qir
predictions <- apply(pi_hat,1,  which.max)
m6 <- matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),mean(pi_hat[which(predictions==1),3]),mean(pi_hat[which(predictions==1),4]),mean(pi_hat[which(predictions==1),5]),mean(pi_hat[which(predictions==1),6]),
                     mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2]),mean(pi_hat[which(predictions==2),3]),mean(pi_hat[which(predictions==2),4]),mean(pi_hat[which(predictions==2),5]),mean(pi_hat[which(predictions==2),6]),
                     mean(pi_hat[which(predictions==3),1]),mean(pi_hat[which(predictions==3),2]),mean(pi_hat[which(predictions==3),3]),mean(pi_hat[which(predictions==3),4]),mean(pi_hat[which(predictions==3),5]),mean(pi_hat[which(predictions==3),6]),
                     mean(pi_hat[which(predictions==4),1]),mean(pi_hat[which(predictions==4),2]),mean(pi_hat[which(predictions==4),3]),mean(pi_hat[which(predictions==4),4]),mean(pi_hat[which(predictions==4),5]),mean(pi_hat[which(predictions==4),6]),
                     mean(pi_hat[which(predictions==5),1]),mean(pi_hat[which(predictions==5),2]),mean(pi_hat[which(predictions==5),3]),mean(pi_hat[which(predictions==5),4]),mean(pi_hat[which(predictions==5),5]),mean(pi_hat[which(predictions==5),6]),
                     mean(pi_hat[which(predictions==6),1]),mean(pi_hat[which(predictions==6),2]),mean(pi_hat[which(predictions==6),3]),mean(pi_hat[which(predictions==6),4]),mean(pi_hat[which(predictions==6),5]),mean(pi_hat[which(predictions==6),6])),3), nrow=6)

m6

mean(diag(m6))

pi_hat <- lc7$Qir
predictions <- apply(pi_hat,1,  which.max)
m7 <- matrix(round(c(mean(pi_hat[which(predictions==1),1]),mean(pi_hat[which(predictions==1),2]),mean(pi_hat[which(predictions==1),3]),mean(pi_hat[which(predictions==1),4]),mean(pi_hat[which(predictions==1),5]),mean(pi_hat[which(predictions==1),6]),mean(pi_hat[which(predictions==1),7]),
                     mean(pi_hat[which(predictions==2),1]),mean(pi_hat[which(predictions==2),2]),mean(pi_hat[which(predictions==2),3]),mean(pi_hat[which(predictions==2),4]),mean(pi_hat[which(predictions==2),5]),mean(pi_hat[which(predictions==2),6]),mean(pi_hat[which(predictions==2),7]),
                     mean(pi_hat[which(predictions==3),1]),mean(pi_hat[which(predictions==3),2]),mean(pi_hat[which(predictions==3),3]),mean(pi_hat[which(predictions==3),4]),mean(pi_hat[which(predictions==3),5]),mean(pi_hat[which(predictions==3),6]),mean(pi_hat[which(predictions==3),7]),
                     mean(pi_hat[which(predictions==4),1]),mean(pi_hat[which(predictions==4),2]),mean(pi_hat[which(predictions==4),3]),mean(pi_hat[which(predictions==4),4]),mean(pi_hat[which(predictions==4),5]),mean(pi_hat[which(predictions==4),6]),mean(pi_hat[which(predictions==4),7]),
                     mean(pi_hat[which(predictions==5),1]),mean(pi_hat[which(predictions==5),2]),mean(pi_hat[which(predictions==5),3]),mean(pi_hat[which(predictions==5),4]),mean(pi_hat[which(predictions==5),5]),mean(pi_hat[which(predictions==5),6]),mean(pi_hat[which(predictions==5),7]),
                     mean(pi_hat[which(predictions==6),1]),mean(pi_hat[which(predictions==6),2]),mean(pi_hat[which(predictions==6),3]),mean(pi_hat[which(predictions==6),4]),mean(pi_hat[which(predictions==6),5]),mean(pi_hat[which(predictions==6),6]),mean(pi_hat[which(predictions==6),7]),
                     mean(pi_hat[which(predictions==7),1]),mean(pi_hat[which(predictions==7),2]),mean(pi_hat[which(predictions==7),3]),mean(pi_hat[which(predictions==7),4]),mean(pi_hat[which(predictions==7),5]),mean(pi_hat[which(predictions==7),6]),mean(pi_hat[which(predictions==7),7])),3), nrow=7)

m7

mean(diag(m7))

c(mean(diag(m2)),mean(diag(m3)),mean(diag(m4)),mean(diag(m5)),mean(diag(m6)),mean(diag(m7)))

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
# classe 3 : nature for itself
# classe 4 : nature to use (landscape, access) whatever the cost


#Create a data frame for plotting:
df <- data.frame(Biodiversite = seq(min(data_DCE_mlogit$Biodiversite),
                           to = max(data_DCE_mlogit$Biodiversite),
                           by = (max(data_DCE_mlogit$Biodiversite) - min(data_DCE_mlogit$Biodiversite))/100))
# Use the class-membership model to calculate the membership probabilities
df_biodiversite <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biodiversite"] * Biodiversite) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biodiversite"] * Biodiversite) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biodiversite"] * Biodiversite))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biodiversite"] * Biodiversite) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biodiversite"] * Biodiversite) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biodiversite"] * Biodiversite)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biodiversite"] * Biodiversite)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biodiversite"] * Biodiversite) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biodiversite"] * Biodiversite) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biodiversite"] * Biodiversite)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biodiversite"] * Biodiversite)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biodiversite"] * Biodiversite) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biodiversite"] * Biodiversite) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biodiversite"] * Biodiversite)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biodiversite"] * Biodiversite)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biodiversite"] * Biodiversite) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biodiversite"] * Biodiversite) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biodiversite"] * Biodiversite))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Biodiversite,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = Biodiversite)) +
  geom_line(aes(x = Biodiversite,
                y = p,
                color = Class)) +
  theme_modern()


#Create a data frame for plotting:
df <- data.frame(Acces = seq(min(data_DCE_mlogit$Acces),
                                    to = max(data_DCE_mlogit$Acces),
                                    by = (max(data_DCE_mlogit$Acces) - min(data_DCE_mlogit$Acces))/100))
# Use the class-membership model to calculate the membership probabilities
df_acces <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Acces"] * Acces) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Acces"] * Acces) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Acces"] * Acces))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Acces"] * Acces) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Acces"] * Acces) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Acces"] * Acces)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Acces"] * Acces)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Acces"] * Acces) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Acces"] * Acces) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Acces"] * Acces)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Acces"] * Acces)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Acces"] * Acces) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Acces"] * Acces) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Acces"] * Acces)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Acces"] * Acces)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Acces"] * Acces) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Acces"] * Acces) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Acces"] * Acces))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Acces,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = Acces)) +
  geom_line(aes(x = Acces,
                y = p,
                color = Class)) +
  theme_modern()

#Create a data frame for plotting:
df <- data.frame(Paysage = seq(min(data_DCE_mlogit$Paysage),
                                    to = max(data_DCE_mlogit$Paysage),
                                    by = (max(data_DCE_mlogit$Paysage) - min(data_DCE_mlogit$Paysage))/100))
# Use the class-membership model to calculate the membership probabilities
df_paysage <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Paysage"] * Paysage) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Paysage"] * Paysage) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Paysage"] * Paysage))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Paysage"] * Paysage) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Paysage"] * Paysage) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Paysage"] * Paysage)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Paysage"] * Paysage)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Paysage"] * Paysage) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Paysage"] * Paysage) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Paysage"] * Paysage)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Paysage"] * Paysage)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Paysage"] * Paysage) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Paysage"] * Paysage) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Paysage"] * Paysage)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Paysage"] * Paysage)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Paysage"] * Paysage) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Paysage"] * Paysage) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Paysage"] * Paysage))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Paysage,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = Paysage)) +
  geom_line(aes(x = Paysage,
                y = p,
                color = Class)) +
  theme_modern()


#Create a data frame for plotting:
df <- data.frame(Temps = seq(min(data_DCE_mlogit$Temps),
                                    to = max(data_DCE_mlogit$Temps),
                                    by = (max(data_DCE_mlogit$Temps) - min(data_DCE_mlogit$Temps))/100))
# Use the class-membership model to calculate the membership probabilities
df_temps <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Temps"] * Temps) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Temps"] * Temps) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Temps"] * Temps))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Temps"] * Temps) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Temps"] * Temps) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Temps"] * Temps)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Temps"] * Temps)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Temps"] * Temps) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Temps"] * Temps) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Temps"] * Temps)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Temps"] * Temps)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Temps"] * Temps) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Temps"] * Temps) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Temps"] * Temps)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Temps"] * Temps)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Temps"] * Temps) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Temps"] * Temps) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Temps"] * Temps))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Temps,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = Temps)) +
  geom_line(aes(x = Temps,
                y = p,
                color = Class)) +
  theme_modern()

#Create a data frame for plotting:
df <- data.frame(Biome1 = seq(0,
                              to = 1,
                              by = 0.01))
# Use the class-membership model to calculate the membership probabilities
df_biome1 <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome1"] * Biome1) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome1"] * Biome1) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome1"] * Biome1))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome1"] * Biome1) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome1"] * Biome1) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome1"] * Biome1)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome1"] * Biome1)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome1"] * Biome1) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome1"] * Biome1) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome1"] * Biome1)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome1"] * Biome1)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome1"] * Biome1) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome1"] * Biome1) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome1"] * Biome1)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome1"] * Biome1)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome1"] * Biome1) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome1"] * Biome1) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome1"] * Biome1))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Biome1,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = Biome1)) +
  geom_line(aes(x = Biome1,
                y = p,
                color = Class)) +
  theme_modern()


#Create a data frame for plotting:
df <- data.frame(Biome2 = seq(0,
                              to = 1,
                              by = 0.01))
# Use the class-membership model to calculate the membership probabilities
df_biome2 <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome2"] * Biome2) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome2"] * Biome2) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome2"] * Biome2))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome2"] * Biome2) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome2"] * Biome2) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome2"] * Biome2)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome2"] * Biome2)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome2"] * Biome2) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome2"] * Biome2) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome2"] * Biome2)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome2"] * Biome2)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome2"] * Biome2) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome2"] * Biome2) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome2"] * Biome2)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome2"] * Biome2)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class.2.Biome2"] * Biome2) + exp(coef(lc4)["(class)3"] + coef(lc4)["class.3.Biome2"] * Biome2) + exp(coef(lc4)["(class)4"] + coef(lc4)["class.4.Biome2"] * Biome2))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Biome2,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = Biome2)) +
  geom_line(aes(x = Biome2,
                y = p,
                color = Class)) +
  theme_modern()


#Create a data frame for plotting:
df <- data.frame(Age = seq(min(data_DCE_mlogit$Age),
                             to = max(data_DCE_mlogit$Age),
                             by = (max(data_DCE_mlogit$Age) - min(data_DCE_mlogit$Age))/102))
# Use the class-membership model to calculate the membership probabilities
df_age <- df <- df %>%
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
                           by = (max(na.omit(data_DCE_mlogit$Income)) - min(na.omit(data_DCE_mlogit$Income)))/108))
# Use the class-membership model to calculate the membership probabilities
df_income <- df <- df %>%
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
df <- data.frame(CSPgroup_inactif = seq(min(data_DCE_mlogit$CSPgroup_inactif),
                           to = max(data_DCE_mlogit$CSPgroup_inactif),
                           by = (max(data_DCE_mlogit$CSPgroup_inactif) - min(data_DCE_mlogit$CSPgroup_inactif))/100))
# Use the class-membership model to calculate the membership probabilities
df_CSPgroup_inactif <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_inactif"] * CSPgroup_inactif) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_inactif"] * CSPgroup_inactif) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_inactif"] * CSPgroup_inactif))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_inactif"] * CSPgroup_inactif) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_inactif"] * CSPgroup_inactif) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_inactif"] * CSPgroup_inactif)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_inactif"] * CSPgroup_inactif)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_inactif"] * CSPgroup_inactif) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_inactif"] * CSPgroup_inactif) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_inactif"] * CSPgroup_inactif)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_inactif"] * CSPgroup_inactif)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_inactif"] * CSPgroup_inactif) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_inactif"] * CSPgroup_inactif) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_inactif"] * CSPgroup_inactif)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_inactif"] * CSPgroup_inactif)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_inactif"] * CSPgroup_inactif) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_inactif"] * CSPgroup_inactif) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_inactif"] * CSPgroup_inactif))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -CSPgroup_inactif,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = CSPgroup_inactif)) +
  geom_line(aes(x = CSPgroup_inactif,
                y = p,
                color = Class))


#Create a data frame for plotting:
df <- data.frame(CSPgroup_plus = seq(min(data_DCE_mlogit$CSPgroup_plus),
                                        to = max(data_DCE_mlogit$CSPgroup_plus),
                                        by = (max(data_DCE_mlogit$CSPgroup_plus) - min(data_DCE_mlogit$CSPgroup_plus))/100))
# Use the class-membership model to calculate the membership probabilities
df_CSPgroup_plus <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_plus"] * CSPgroup_plus) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_plus"] * CSPgroup_plus) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_plus"] * CSPgroup_plus))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_plus"] * CSPgroup_plus) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_plus"] * CSPgroup_plus) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_plus"] * CSPgroup_plus)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_plus"] * CSPgroup_plus)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_plus"] * CSPgroup_plus) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_plus"] * CSPgroup_plus) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_plus"] * CSPgroup_plus)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_plus"] * CSPgroup_plus)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_plus"] * CSPgroup_plus) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_plus"] * CSPgroup_plus) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_plus"] * CSPgroup_plus)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_plus"] * CSPgroup_plus)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_plus"] * CSPgroup_plus) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_plus"] * CSPgroup_plus) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_plus"] * CSPgroup_plus))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -CSPgroup_plus,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = CSPgroup_plus)) +
  geom_line(aes(x = CSPgroup_plus,
                y = p,
                color = Class))

#Create a data frame for plotting:
df <- data.frame(CSPgroup_moins = seq(min(data_DCE_mlogit$CSPgroup_moins),
                                        to = max(data_DCE_mlogit$CSPgroup_moins),
                                        by = (max(data_DCE_mlogit$CSPgroup_moins) - min(data_DCE_mlogit$CSPgroup_moins))/100))
# Use the class-membership model to calculate the membership probabilities
df_CSPgroup_moins <-  df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_moins"] * CSPgroup_moins) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_moins"] * CSPgroup_moins) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_moins"] * CSPgroup_moins))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_moins"] * CSPgroup_moins) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_moins"] * CSPgroup_moins) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_moins"] * CSPgroup_moins)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_moins"] * CSPgroup_moins)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_moins"] * CSPgroup_moins) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_moins"] * CSPgroup_moins) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_moins"] * CSPgroup_moins)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_moins"] * CSPgroup_moins)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_moins"] * CSPgroup_moins) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_moins"] * CSPgroup_moins) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_moins"] * CSPgroup_moins)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_moins"] * CSPgroup_moins)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:CSPgroup_moins"] * CSPgroup_moins) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:CSPgroup_moins"] * CSPgroup_moins) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:CSPgroup_moins"] * CSPgroup_moins))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -CSPgroup_moins,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = CSPgroup_moins)) +
  geom_line(aes(x = CSPgroup_moins,
                y = p,
                color = Class))

#Create a data frame for plotting:
df <- data.frame(class_nat = seq(min(na.omit(data_DCE_mlogit$class_nat)),
                           to = max(na.omit(data_DCE_mlogit$class_nat)),
                           by = (max(na.omit(data_DCE_mlogit$class_nat)) - min(na.omit(data_DCE_mlogit$class_nat)))/100))
# Use the class-membership model to calculate the membership probabilities
df_class_nat <- df <- df %>%
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
df_survey_id <- df <- df %>%
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
df <- data.frame(journey_duration3 = seq(min(data_DCE_mlogit$journey_duration3),
                                 to = max(data_DCE_mlogit$journey_duration3),
                                 by = (max(data_DCE_mlogit$journey_duration3) - min(data_DCE_mlogit$journey_duration3))/100))
# Use the class-membership model to calculate the membership probabilities
df_journey_duration3 <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:journey_duration3"] * journey_duration3) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class3:journey_duration3"] * journey_duration3) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class4:journey_duration3"] * journey_duration3))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:journey_duration3"] * journey_duration3) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:journey_duration3"] * journey_duration3) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:journey_duration3"] * journey_duration3)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:journey_duration3"] * journey_duration3)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:journey_duration3"] * journey_duration3) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:journey_duration3"] * journey_duration3) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:journey_duration3"] * journey_duration3)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:journey_duration3"] * journey_duration3)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:journey_duration3"] * journey_duration3) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:journey_duration3"] * journey_duration3) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:journey_duration3"] * journey_duration3)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:journey_duration3"] * journey_duration3)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:journey_duration3"] * journey_duration3) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:journey_duration3"] * journey_duration3) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:journey_duration3"] * journey_duration3))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -journey_duration3,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = journey_duration3)) +
  geom_line(aes(x = journey_duration3,
                y = p,
                color = Class)) +
  theme_modern()

#Create a data frame for plotting:
df <- data.frame(main_vehicule_indiv_motor = seq(min(data_DCE_mlogit$main_vehicule_indiv_motor),
                                 to = max(data_DCE_mlogit$main_vehicule_indiv_motor),
                                 by = (max(data_DCE_mlogit$main_vehicule_indiv_motor) - min(data_DCE_mlogit$main_vehicule_indiv_motor))/100))
# Use the class-membership model to calculate the membership probabilities
df_main_vehicule_indiv_motor <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_indiv_motor"] * main_vehicule_indiv_motor))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -main_vehicule_indiv_motor,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = main_vehicule_indiv_motor)) +
  geom_line(aes(x = main_vehicule_indiv_motor,
                y = p,
                color = Class)) +
  theme_modern()

#Create a data frame for plotting:
df <- data.frame(main_vehicule_commun = seq(min(data_DCE_mlogit$main_vehicule_commun),
                                 to = max(data_DCE_mlogit$main_vehicule_commun),
                                 by = (max(data_DCE_mlogit$main_vehicule_commun) - min(data_DCE_mlogit$main_vehicule_commun))/100))
# Use the class-membership model to calculate the membership probabilities
df_main_vehicule_commun <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_commun"] * main_vehicule_commun) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_commun"] * main_vehicule_commun) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_commun"] * main_vehicule_commun))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_commun"] * main_vehicule_commun) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_commun"] * main_vehicule_commun) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_commun"] * main_vehicule_commun)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_commun"] * main_vehicule_commun)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_commun"] * main_vehicule_commun) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_commun"] * main_vehicule_commun) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_commun"] * main_vehicule_commun)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_commun"] * main_vehicule_commun)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_commun"] * main_vehicule_commun) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_commun"] * main_vehicule_commun) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_commun"] * main_vehicule_commun)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_commun"] * main_vehicule_commun)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["class2:main_vehicule_commun"] * main_vehicule_commun) + exp(coef(lc4)["(class)3"] + coef(lc4)["class3:main_vehicule_commun"] * main_vehicule_commun) + exp(coef(lc4)["(class)4"] + coef(lc4)["class4:main_vehicule_commun"] * main_vehicule_commun))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -main_vehicule_commun,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = main_vehicule_commun)) +
  geom_line(aes(x = main_vehicule_commun,
                y = p,
                color = Class)) +
  theme_modern()

#Create a data frame for plotting:
df <- data.frame(Gender = seq(min(na.omit(data_DCE_mlogit$Gender)),
                                 to = max(na.omit(data_DCE_mlogit$Gender)),
                                 by = (max(na.omit(data_DCE_mlogit$Gender)) - min(na.omit(data_DCE_mlogit$Gender)))/100))
# Use the class-membership model to calculate the membership probabilities
df_gender <- df <- df %>%
  mutate(p_1 = 1 -
           (exp(coef(lc4)["(class)2"] + coef(lc4)["Gender:class2"] * Gender) +
              exp(coef(lc4)["(class)3"] + coef(lc4)["Gender:class3"] * Gender) + 
              exp(coef(lc4)["(class)4"] + coef(lc4)["Gender:class4"] * Gender))/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["Gender:class2"] * Gender) + exp(coef(lc4)["(class)3"] + coef(lc4)["Gender:class3"] * Gender) + exp(coef(lc4)["(class)4"] + coef(lc4)["Gender:class4"] * Gender)),
         p_2 = exp(coef(lc4)["(class)2"] + coef(lc4)["Gender:class2"] * Gender)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["Gender:class2"] * Gender) + exp(coef(lc4)["(class)3"] + coef(lc4)["Gender:class3"] * Gender) + exp(coef(lc4)["(class)4"] + coef(lc4)["Gender:class4"] * Gender)),
         p_3 = exp(coef(lc4)["(class)3"] + coef(lc4)["Gender:class3"] * Gender)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["Gender:class2"] * Gender) + exp(coef(lc4)["(class)3"] + coef(lc4)["Gender:class3"] * Gender) + exp(coef(lc4)["(class)4"] + coef(lc4)["Gender:class4"] * Gender)),
         p_4 = exp(coef(lc4)["(class)4"] + coef(lc4)["Gender:class4"] * Gender)/
           (1 + exp(coef(lc4)["(class)2"] + coef(lc4)["Gender:class2"] * Gender) + exp(coef(lc4)["(class)3"] + coef(lc4)["Gender:class3"] * Gender) + exp(coef(lc4)["(class)4"] + coef(lc4)["Gender:class4"] * Gender))
  ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Gender,
               names_to = "Class",
               values_to = "p") %>%
  mutate(Class = sub("p_","Class ", Class))

ggplot(df, aes(x = Gender)) +
  geom_line(aes(x = Gender,
                y = p,
                color = Class)) +
  theme_modern()

ggsave("output/lc4_Gender.png",
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

ggsave("output/lc4_Perso_relation_nature.png",
       width = 8,
       height = 6,
       dpi = 300)


# plot all results
summary_lc4 <- summary(lc4)$CoefTable
summary_lc4 <- as.data.frame(summary_lc4[which(!grepl(")",row.names(summary_lc4))),])
summary_lc4$class_name <- c(rep("class_1",7),rep("class_2",7),rep("class_3",7),rep("class_4",7)
                ,rep(c("class_2","class_3","class_4"),11))
summary_lc4$variable <- c(rep(c("Time","Landscape","Access","Biodiversity","Biome_Peri-urban","Biome_Rural","asc"),4),
                          rep("Gender_Male",3),rep("Age",3),rep("Income",3),
                          rep("CSPgroup_inactif",3),rep("CSPgroup_plus",3),rep("CSPgroup_moins",3),
                          rep("class_nat",3),rep("survey_id",3),rep("journey_duration3",3),
                          rep("main_vehicule_indiv_motor",3),rep("main_vehicule_commun",3))

summary_lc4_class1 <- summary_lc4[1:11,]
summary_lc4_class1[] <- 0
row.names(summary_lc4_class1) <- NULL
summary_lc4_class1$class_name <- "class_1"
summary_lc4_class1$variable <- c("Gender_Male","Age","Income","CSPgroup_inactif","CSPgroup_plus","CSPgroup_moins",
                                 "class_nat","survey_id","journey_duration3","main_vehicule_indiv_motor","main_vehicule_commun")


df_temps$Variable <- "Time"
df_paysage$Variable <- "Landscape"
df_acces$Variable <- "Access"
df_biodiversite$Variable <- "Biodiversity"
df_biome1$Variable <- "Periurban"
df_biome2$Variable <- "Rural"
df_gender$Variable <- "Gender"
df_age$Variable <- "Age"
df_income$Variable <- "Income"
df_CSPgroup_inactif$Variable <- "Not_in_employment"
df_CSPgroup_moins$Variable <- "Lower_SPC"
df_CSPgroup_plus$Variable <- "Higher_SPC"
df_class_nat$Variable <- "Naturalness"
df_survey_id$Variable <- "Framing"
df_journey_duration3$Variable <- "Daily_travel_time"
df_main_vehicule_commun$Variable <- "Public_transport"
df_main_vehicule_indiv_motor$Variable <- "Motorised_personal"

names(df_temps)[1] <- names(df_paysage)[1] <- names(df_acces)[1] <- names(df_biodiversite)[1] <-
names(df_biome1)[1] <- names(df_biome2)[1] <- names(df_gender)[1] <- names(df_age)[1] <-
names(df_income)[1] <- names(df_CSPgroup_inactif)[1] <- names(df_CSPgroup_moins)[1] <- names(df_CSPgroup_plus)[1] <-
names(df_class_nat)[1] <- names(df_survey_id)[1] <- names(df_journey_duration3)[1] <- names(df_main_vehicule_commun)[1] <- 
names(df_main_vehicule_indiv_motor)[1] <-"Value"

df_temps <- df_temps[which(df_temps$Value %in% c(0,2,4,6,8)),]
df_paysage <- df_paysage[which(df_paysage$Value %in% c(0,1,2)),]
df_acces <- df_acces[which(df_acces$Value %in% c(0,1)),]
df_biodiversite <- df_biodiversite[which(df_biodiversite$Value %in% c(0,1,2)),]
df_biome0 <- df_biome1[which(df_biome1$Value %in% c(0)),]
df_biome0$Variable <- "Urban"
df_biome1 <- df_biome1[which(df_biome1$Value %in% c(1)),]
df_biome2 <- df_biome2[which(df_biome2$Value %in% c(1)),]
df_biome2$Value <- 2
df_biome <- rbind(df_biome0,df_biome1,df_biome2)
df_gender <- df_gender[which(df_gender$Value %in% c(1,2)),]
df_age <- df_age[which(df_age$Value %in% c(1,2,3,4)),]
df_income <- df_income[which(df_income$Value %in% c(1,2,3,4,5,6,7,8,9,10)),]
df_CSPgroup_retraite <- df_CSPgroup_inactif[which(df_CSPgroup_inactif$Value %in% c(0)),]
df_CSPgroup_retraite$Variable <- "Retired"
df_CSPgroup_inactif <- df_CSPgroup_inactif[which(df_CSPgroup_inactif$Value %in% c(1)),]
df_CSPgroup_moins <- df_CSPgroup_moins[which(df_CSPgroup_moins$Value %in% c(1)),]
df_CSPgroup_moins$Value <- 2
df_CSPgroup_plus <- df_CSPgroup_plus[which(df_CSPgroup_plus$Value %in% c(1)),]
df_CSPgroup_plus$Value <- 3
df_CSPgroup <- rbind(df_CSPgroup_retraite,df_CSPgroup_inactif,df_CSPgroup_moins,df_CSPgroup_plus)
df_class_nat <- df_class_nat[which(df_class_nat$Value %in% c(1,2,3)),]
df_survey_id <- df_survey_id[which(df_survey_id$Value %in% c(1,2)),]
df_journey_duration3 <- df_journey_duration3[which(df_journey_duration3$Value %in% c(1,2,3,4,5,6)),]
df_main_vehicule_indiv_unmotor <- df_main_vehicule_indiv_motor[which(df_main_vehicule_indiv_motor$Value %in% c(0)),]
df_main_vehicule_indiv_unmotor$Variable <- "Unmotorised_personal"
df_main_vehicule_indiv_motor <- df_main_vehicule_indiv_motor[which(df_main_vehicule_indiv_motor$Value %in% c(1)),]
df_main_vehicule_commun <- df_main_vehicule_commun[which(df_main_vehicule_commun$Value %in% c(1)),]
df_main_vehicule_commun$Value <- 2
df_main_vehicule <- rbind(df_main_vehicule_indiv_unmotor,df_main_vehicule_indiv_motor,df_main_vehicule_commun)

df_biome$Variable <- "Biome"
df_main_vehicule$Variable <- "Main_transport"
df_CSPgroup$Variable <- "SPC"

df_all <- rbind(df_temps, df_paysage, df_acces, df_biodiversite,
                df_biome, df_gender, df_age, df_income,
                df_CSPgroup, df_class_nat, df_survey_id, df_journey_duration3,
                df_main_vehicule)

df_all$Variable2 <- paste0(df_all$Variable,sep="_",df_all$Value)
df_all$Variable2[which(df_all$Variable=="Age")] <- c(rep("a1 18-29 yo",4),rep("a2 30-44 yo",4),rep("a3 45-59 yo",4),rep("a4 ≥ 60 yo",4))
df_all$Variable2[which(df_all$Variable=="Daily_travel_time")] <- c(rep("b1 ≤ 20",4),rep("b2 21-30",4),rep("b3 31-40",4),rep("b4 41-60",4),rep("b5 61-90",4),rep("b6 > 90",4))
df_all$Variable2[which(df_all$Variable=="Gender")] <- c(rep("c1 Women",4),rep("c2 Men",4))
df_all$Variable2[which(df_all$Variable=="Income")] <- c(rep("d1 ≤ 1200 €",4),rep("d2 1201-1500 €",4),rep("d3 1501-1800 €",4),rep("d3 1801-2100 €",4),rep("d5 2101-2600 €",4),
                                                        rep("d6 2601-3100 €",4),rep("d7 3101-3500 €",4),rep("d8 3501-4200 €",4),rep("d9 4201-5400 €",4),rep("d91 > 5400 €",4))
df_all$Variable2[which(df_all$Variable=="Main_transport")] <- c(rep("e1 Unmotorised personal",4),rep("e2 Motorised personal",4),rep("e3 Public",4))
df_all$Variable2[which(df_all$Variable=="Naturalness")] <- c(rep("f1 Very low",4),rep("f2 Low ",4),rep("f3 Above average",4))
df_all$Variable2[which(df_all$Variable=="SPC")] <- c(rep("g1 Retired",4),rep("g2 Not in employment",4),rep("g3 Lower",4),rep("g4 Higher",4))
df_all$Variable2[which(df_all$Variable=="Framing")] <- c(rep("h1 Information",4),rep("h2 No information",4))

df_all$Class[which(df_all$Class=="Class 1")] <- "Saving time"
df_all$Class[which(df_all$Class=="Class 2")] <- "Protecting biodiversity on its own"
df_all$Class[which(df_all$Class=="Class 3")] <- "Landscape and nature use"
df_all$Class[which(df_all$Class=="Class 4")] <- "Trade-off time biodiversity"

ggplot(df_all[which(df_all$Variable %in% c("Gender", "Age", "Income","SPC",
                                           "Naturalness", "Framing", "Daily_travel_time","Main_transport")),],
       aes(Variable2, p, group = Class, color = Class)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  labs(x = NULL, y = "Class probability membership") +
  theme_modern() +
  geom_vline(xintercept = 4.5, size=1.5) +
  geom_vline(xintercept = 10.5, size=1.5) +
  geom_vline(xintercept = 12.5, size=1.5) +
  geom_vline(xintercept = 22.5, size=1.5) +
  geom_vline(xintercept = 25.5, size=1.5) +
  geom_vline(xintercept = 28.5, size=1.5) +
  geom_vline(xintercept = 32.5, size=1.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

ggsave("output/lc4_profile.png",
       width = 12,
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



# time from classical approaches

data_DCE_mlogit <- mutate(data_DCE_numeric,
             `Temps:Income` = Temps * Income)
data_DCE_mlogit$Income2 <- data_DCE_mlogit$Income
data_DCE_mlogit$Income2[which(data_DCE_mlogit$Income2 %in% c(1:4))] <- 1
data_DCE_mlogit$Income2[which(data_DCE_mlogit$Income2 %in% c(5:7))] <- 2
data_DCE_mlogit$Income2[which(data_DCE_mlogit$Income2 %in% c(7:8))] <- 3
data_DCE_mlogit$Income3 <- data_DCE_mlogit$Income*data_DCE_mlogit$Income


data_DCE_mlogit <- mlogit.data(data_DCE_mlogit,
                               choice = "choice",
                               shape = "long",
                               alt.levels = c("Scénario de référence",
                                              "Scénario 1",
                                              "Scénario 2" ),
                               chid.var = "chid")


mxl1 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | Gender + Age + Income + CSPgroup + class_nat + survey_id + journey_duration2,
            data = data_DCE_mlogit,
            model = 'mixl',
            ranp = c(Temps = "n"),
            R = 50)

mxl2 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | Income ,
            data = data_DCE_mlogit,
            model = 'mixl',
            ranp = c(Temps = "n"),
            R = 50)

mxl3 <- gmnl(choice ~ Temps + Temps:Income + Paysage + Acces + Biodiversite + Biome + asc | 1,
            data = data_DCE_mlogit,
            model = 'mixl',
            ranp = c(Temps = "n"),
            R = 50)

mxl4 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 1 | 0 | Income ,
            data = data_DCE_mlogit,
            model = 'mixl',
            mvar = list(Temps = c("Income")),
            ranp = c(Temps = "n"),
            R = 50)

mxl5 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 1 | 0 | Gender + Age + Income + CSPgroup + class_nat + survey_id + journey_duration2 ,
             data = data_DCE_mlogit,
             model = 'mixl',
             mvar = list(Temps = c("Gender", "Age", "Income", "CSPgroup", "class_nat", "survey_id", "journey_duration2")),
             ranp = c(Temps = "n"),
             R = 50)

mxl6 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 1 | 0 | Gender + Age + Income2 + CSPgroup + class_nat + survey_id + journey_duration2 ,
             data = data_DCE_mlogit,
             model = 'mixl',
             mvar = list(Temps = c("Gender", "Age", "Income2", "CSPgroup", "class_nat", "survey_id", "journey_duration2")),
             ranp = c(Temps = "n"),
             R = 50)

mxl7 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 1 | 0 | Gender + Age + Income3 + CSPgroup + class_nat + survey_id + journey_duration2 ,
             data = data_DCE_mlogit,
             model = 'mixl',
             mvar = list(Temps = c("Gender", "Age", "Income3", "CSPgroup", "class_nat", "survey_id", "journey_duration2")),
             ranp = c(Temps = "n"),
             R = 50)

# ajouter income en categorie (non lineaier cf Depalma)

mnl_cov <- mlogit(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | Income,
                  data = data_DCE_mlogit)
mnl_exp <- mlogit(choice ~ Temps + Temps:Income + Paysage + Acces + Biodiversite + Biome + asc | 1,
                  data = data_DCE_mlogit)


# plot map with number of respondent
# commune geo from https://public.opendatasoft.com/explore/dataset/georef-france-commune-arrondissement-municipal-millesime/export/?disjunctive.reg_name&disjunctive.dep_name&disjunctive.arrdep_name&disjunctive.ze2020_name&disjunctive.bv2012_name&disjunctive.epci_name&disjunctive.ept_name&disjunctive.com_name&disjunctive.com_arm_name&disjunctive.com_arm_is_mountain_area&sort=year&location=6,46.97276,3.93311&basemap=jawg.light
spdf <- geojson_read("raw_data/georef-france-commune-arrondissement-municipal-millesime.geojson",  what = "sp")

spdf_metro <- spdf[which(spdf@data$reg_name %in% c("Nouvelle-Aquitaine","Centre-Val de Loire","Bourgogne-Franche-Comté","Hauts-de-France",
                                                   "Normandie","Grand Est","Bretagne","Occitanie","Île-de-France","Provence-Alpes-Côte d'Azur",
                                                   "Pays de la Loire","Auvergne-Rhône-Alpes","Corse")),]

data_clean_com_nat <- readRDS("output/data_clean_com_nat.rds")
response_per_commune <- na.omit(data_clean_com_nat[,c("code_commune_insee" ,"post_code_home")] %>% group_by(code_commune_insee) %>% summarise(count=n()))
names(response_per_commune)[1] <- "com_code"

spdf_metro@data <- join(spdf_metro@data, response_per_commune, by="com_code")

spdf_metro_resp <- spdf_metro[which(!is.na(spdf_metro@data$count)),]

spdf_metro_resp_fort <- fortify(spdf_metro_resp, region='com_code')
spdf_metro_resp_fort$com_code <- spdf_metro_resp_fort$id
sf.df <- join(spdf_metro_resp_fort, spdf_metro_resp@data, by="com_code")

fr_boundary <- ne_states("france",returnclass = "sf")
fr_boundary <- fr_boundary[which(fr_boundary$type_en=="Metropolitan department"),]

fr_boundary %>% 
  group_by(admin) %>% 
  summarise() %>% 
  ggplot() +
  geom_sf() +
  theme(legend.position = 'none')

fr_boundary <- fr_boundary %>% 
  group_by(admin) %>% 
  summarise()

city_name <- c("Angers","Aubagne","Avignon","Bordeaux","Grenoble","Le Havre","Lille",
               "Lyon","Montpellier","Nantes","Nice","Rouen","Strasbourg","Toulouse","Tours",
               "Geneve","Caen","Paris")

spdf_city <- spdf[which(spdf@data$com_name %in% city_name),]
spdf_city_fort <- fortify(spdf_city, region='com_name')
spdf_city_fort$com_name <- spdf_city_fort$id
spdf_city_fort <- spdf_city_fort %>%
  group_by(com_name) %>%
  summarise(long = mean(long, na.rm = T), lat = mean(lat, na.rm = T), group = group)

ggplot(sf.df) + coord_fixed(ratio = 1.3) +
  geom_sf(data=fr_boundary, fill='white') +
  geom_polygon(aes(x = long, y = lat, group = com_code, fill=count)) + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_void() +
  geom_text(data = spdf_city_fort, aes(long, lat, label = com_name, group = group), size = 10)
  
ggsave("output/map_resp.png",
       width = 32,
       height = 24,
       dpi = 400)

spdf_metro_resp_city <- spdf_metro[which(spdf_metro@data$arrdep_name %in% city_name),]
spdf_metro_resp_city_fort <- fortify(spdf_metro_resp_city, region='com_code')
spdf_metro_resp_city_fort$com_code <- spdf_metro_resp_city_fort$id
sf.df2 <- join(spdf_metro_resp_city_fort, spdf_metro_resp_city@data, by="com_code")

ggplot(sf.df2[which(sf.df2$arrdep_name==city_name[1]),]) + coord_fixed(ratio = 1.3) +
  geom_polygon(aes(x = long, y = lat, group = com_code, fill=count), col="black") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_polygon(data=sf.df2[which(sf.df2$com_name==city_name[1]),], aes(x = long, y = lat), fill="white", col="black") + 
  theme_void() 

ggsave("output/map_resp1.png",
       width = 16,
       height = 12,
       dpi = 400)

spdf_metro_resp_city2 <- spdf_metro[which(spdf_metro@data$arrdep_name %in% c("Marseille", "Gex")),]
spdf_metro_resp_city2_fort <- fortify(spdf_metro_resp_city2, region='com_name')
spdf_metro_resp_city2_fort$com_name <- spdf_metro_resp_city2_fort$id
sf.df3 <- join(spdf_metro_resp_city2_fort, spdf_metro_resp_city2@data, by="com_name")

ggplot(sf.df3[which(sf.df3$arrdep_name=="Marseille"),]) + coord_fixed(ratio = 1.3) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=count), col="black") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_polygon(data=sf.df3[which(sf.df3$com_name=="Aubagne"),], aes(x = long, y = lat), fill="white", col="black") + 
  theme_void() 

ggsave("output/map_resp_marseille.png",
       width = 16,
       height = 12,
       dpi = 400)

ggplot(sf.df3[which(sf.df3$arrdep_name=="Gex"),]) + coord_fixed(ratio = 1.3) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=count), col="black") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_void() 

ggsave("output/map_resp_geneve.png",
       width = 16,
       height = 12,
       dpi = 400)
