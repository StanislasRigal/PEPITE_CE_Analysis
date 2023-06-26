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


lc2 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | 1,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 2,
            panel = TRUE,
            method = "bhhh")
summary(lc2)
2 * length(coef(lc2)) - 2 * lc2$logLik$maximum

exp(coef(lc2)["(class)2"]) / (exp(0) + exp(coef(lc2)["(class)2"]))

lc3 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome +asc | 0 | 0 | 0 | 1,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 3,
            panel = TRUE,
            method = "bhhh")
summary(lc3)
2 * length(coef(lc3)) - 2 * lc3$logLik$maximum

exp(coef(lc3)["(class)2"]) / (exp(0) + exp(coef(lc3)["(class)2"]) + exp(coef(lc3)["(class)3"]))
exp(coef(lc3)["(class)3"]) / (exp(0) + exp(coef(lc3)["(class)2"]) + exp(coef(lc3)["(class)3"]))

lc4 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc| 0 | 0 | 0 | 1,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 4,
            panel = TRUE,
            method = "bhhh")
summary(lc4)
2 * length(coef(lc4)) - 2 * lc4$logLik$maximum
exp(coef(lc4)["(class)2"]) / (exp(0) + exp(coef(lc4)["(class)2"]) + exp(coef(lc4)["(class)3"]) + exp(coef(lc4)["(class)4"]))
exp(coef(lc4)["(class)3"]) / (exp(0) + exp(coef(lc4)["(class)2"]) + exp(coef(lc4)["(class)3"]) + exp(coef(lc4)["(class)4"]))
exp(coef(lc4)["(class)4"]) / (exp(0) + exp(coef(lc4)["(class)2"]) + exp(coef(lc4)["(class)3"]) + exp(coef(lc4)["(class)4"]))


lc2 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | 0 | Age + CSPgroup + Income + main_vehicule + class_nat,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 2,
            panel = TRUE,
            method = "bhhh")
summary(lc2)
2 * length(coef(lc2)) - 2 * lc2$logLik$maximum

lc3 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc + Age + CSPgroup + Income + main_vehicule + class_nat  | 0 | 0 | 0 | 1,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 3,
            panel = TRUE,
            method = "bhhh")
summary(lc3)
2 * length(coef(lc3)) - 2 * lc3$logLik$maximum

lc4 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc| 0 | 0 | 0 | 1,
            data = data_DCE_mlogit,
            model = 'lc',
            Q = 4,
            panel = TRUE,
            method = "bhhh")
summary(lc4)
2 * length(coef(lc4)) - 2 * lc4$logLik$maximum

# classe 1 : saving time
# classe 2 : saving biodiversity whatever the cost
# classe 3 : gain access
# classe 4 : rural landscape

# Willingness to spend time
## Paysage
as.numeric(lc4$coefficients[2]) / as.numeric(lc4$coefficients[1])
as.numeric(lc4$coefficients[7]) / as.numeric(lc4$coefficients[6])
as.numeric(lc4$coefficients[12]) / as.numeric(lc4$coefficients[11])
as.numeric(lc4$coefficients[17]) / as.numeric(lc4$coefficients[16])
## Acces
as.numeric(lc4$coefficients[3]) / as.numeric(lc4$coefficients[1])
as.numeric(lc4$coefficients[8]) / as.numeric(lc4$coefficients[6])
as.numeric(lc4$coefficients[13]) / as.numeric(lc4$coefficients[11])
as.numeric(lc4$coefficients[18]) / as.numeric(lc4$coefficients[16])
## Biodiversite
as.numeric(lc4$coefficients[4]) / as.numeric(lc4$coefficients[1])
as.numeric(lc4$coefficients[9]) / as.numeric(lc4$coefficients[6])
as.numeric(lc4$coefficients[14]) / as.numeric(lc4$coefficients[11])
as.numeric(lc4$coefficients[19]) / as.numeric(lc4$coefficients[16])
## Biome
as.numeric(lc4$coefficients[5]) / as.numeric(lc4$coefficients[1])
as.numeric(lc4$coefficients[10]) / as.numeric(lc4$coefficients[6])
as.numeric(lc4$coefficients[15]) / as.numeric(lc4$coefficients[11])
as.numeric(lc4$coefficients[20]) / as.numeric(lc4$coefficients[16])


lc4.cov.survey <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | survey_id,
                       data = data_DCE_mlogit,
                       model = 'lc',
                       Q = 4,
                       panel = TRUE,
                       method = "nm",
                       iterlim = 1200)

summary(lc4.cov.survey)

lc4.cov.gender <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | Gender,
                       data = data_DCE_mlogit,
                       model = 'lc',
                       Q = 4,
                       panel = TRUE,
                       method = "nm",
                       iterlim = 1200)

summary(lc4.cov.gender)

lc4.cov.age <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | Age,
                    data = data_DCE_mlogit,
                    model = 'lc',
                    Q = 4,
                    panel = TRUE,
                    method = "nm",
                    iterlim = 1200)

summary(lc4.cov.age)

lc4.cov.csp <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | CSPgroup,
                    data = data_DCE_mlogit,
                    model = 'lc',
                    Q = 4,
                    panel = TRUE,
                    method = "nm",
                    iterlim = 1200)

summary(lc4.cov.csp)

lc4.cov.mainveh <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | main_vehicule,
                        data = data_DCE_mlogit,
                        model = 'lc',
                        Q = 4,
                        panel = TRUE,
                        method = "nm",
                        iterlim = 1200)

summary(lc4.cov.mainveh)

lc4.cov.nat <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | nat,
                    data = data_DCE_mlogit,
                    model = 'lc',
                    Q = 4,
                    panel = TRUE,
                    method = "nm",
                    iterlim = 1200)

summary(lc4.cov.nat)


#Create a data frame for plotting:
df <- data.frame(Gender = seq(min(data_DCE_mlogit$Gender),
                             to = max(data_DCE_mlogit$Gender),
                             by = (max(data_DCE_mlogit$Gender) - min(data_DCE_mlogit$Gender))/100))
# Use the class-membership model to calculate the membership probabilities
df <- df %>%
  mutate(p_1 = 1 -
           exp(coef(lc4.cov.gender)["(class)2"] + coef(lc4.cov.gender)["Gender:class2"] * Gender)/
           (1 + exp(coef(lc4.cov.gender)["(class)2"] + coef(lc4.cov.gender)["Gender:class2"] * Gender)) -
           exp(coef(lc4.cov.gender)["(class)3"] + coef(lc4.cov.gender)["Gender:class3"] * Gender)/
           (1 + exp(coef(lc4.cov.gender)["(class)3"] + coef(lc4.cov.gender)["Gender:class3"] * Gender)) -
           exp(coef(lc4.cov.gender)["(class)4"] + coef(lc4.cov.gender)["Gender:class4"] * Gender)/
           (1 + exp(coef(lc4.cov.gender)["(class)4"] + coef(lc4.cov.gender)["Gender:class4"] * Gender)),
         p_2 = exp(coef(lc4.cov.gender)["(class)2"] + coef(lc4.cov.gender)["Gender:class2"] * Gender)/
           (1 + exp(coef(lc4.cov.gender)["(class)2"] + coef(lc4.cov.gender)["Gender:class2"] * Gender)),
         p_3 = exp(coef(lc4.cov.gender)["(class)3"] + coef(lc4.cov.gender)["Gender:class3"] * Gender)/
           (1 + exp(coef(lc4.cov.gender)["(class)3"] + coef(lc4.cov.gender)["Gender:class3"] * Gender)),
         p_4 = exp(coef(lc4.cov.gender)["(class)4"] + coef(lc4.cov.gender)["Gender:class4"] * Gender)/
           (1 + exp(coef(lc4.cov.gender)["(class)4"] + coef(lc4.cov.gender)["Gender:class4"] * Gender))
         ) %>%
  # Pivot longer to put all probabilities in a single column, and label by class
  pivot_longer(cols = -Gender,
               names_to = "Class",
               values_to = "p")
df$Class <- sub("p_","Class_",df$Class)

# Plot
ggplot(df, aes(x = Gender)) +
  geom_line(aes(x = Gender,
                y = p,
                color = Class))

# survey framing long


data_DCE_L_mlogit <- mlogit.data(data_DCE[which(data_DCE$survey_id==672984),],
                                 choice = "choice",
                                 shape = "long",
                                 alt.var = "Scenario",
                                 id.var = "id",
                                 chid.var = "chid")

mnl.rt0_L <- mlogit(choice ~ Temps + Paysage + Acces + Biodiversite + Biome  | 0,
                    data = data_DCE_L_mlogit)
summary(mnl.rt0_L)
2 * length(coef(mnl.rt0_L)) - 2 * mnl.rt0_L$logLik


lc2 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | 1,
            data = data_DCE_L_mlogit,
            model = 'lc',
            Q = 2,
            panel = TRUE,
            method = "bhhh")
summary(lc2)
2 * length(coef(lc2)) - 2 * lc2$logLik$maximum

lc3 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | 1,
            data = data_DCE_L_mlogit,
            model = 'lc',
            Q = 3,
            panel = TRUE,
            method = "bhhh")
summary(lc3)
2 * length(coef(lc3)) - 2 * lc3$logLik$maximum

lc4 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | 1,
            data = data_DCE_L_mlogit,
            model = 'lc',
            Q = 4,
            panel = TRUE,
            method = "bhhh")
summary(lc4)
2 * length(coef(lc4)) - 2 * lc4$logLik$maximum

# classe 1 : saving time
# classe 2 : saving biodiversity whatever the cost
# classe 3 : gain access whatever the cost
# classe 4 : rural landscape

# survey framing short


data_DCE_S_mlogit <- mlogit.data(data_DCE[which(data_DCE$survey_id==355435),],
                                 choice = "choice",
                                 shape = "long",
                                 alt.var = "Scenario",
                                 id.var = "id",
                                 chid.var = "chid")

mnl.rt0_S <- mlogit(choice ~ Temps + Paysage + Acces + Biodiversite + Biome  | 0,
                    data = data_DCE_S_mlogit)
summary(mnl.rt0_S)
2 * length(coef(mnl.rt0_S)) - 2 * mnl.rt0_S$logLik


lc2 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | 1,
            data = data_DCE_S_mlogit,
            model = 'lc',
            Q = 2,# MIXL T
            mixl_t <- gmnl(choice ~ vcost + travel + wait | 1,
                           data = TM,
                           model = "mixl",
                           ranp = c(travel = "n"),
                           R = 50)
            panel = TRUE,
            method = "bhhh")
summary(lc2)
2 * length(coef(lc2)) - 2 * lc2$logLik$maximum

lc3 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | 1,
            data = data_DCE_S_mlogit,
            model = 'lc',
            Q = 3,
            panel = TRUE,
            method = "bhhh")
summary(lc3)
2 * length(coef(lc3)) - 2 * lc3$logLik$maximum

lc4 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 0 | 0 | 0 | 1,
            data = data_DCE_S_mlogit,
            model = 'lc',
            Q = 4,
            panel = TRUE,
            method = "bhhh")
summary(lc4)
2 * length(coef(lc4)) - 2 * lc4$logLik$maximum

# classe 1 : saving time
# classe 2 : saving biodiversity whatever the cost
# classe 3 : gain access whatever the cost
# classe 4 : rural landscape