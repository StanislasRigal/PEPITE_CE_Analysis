library(AER) # Applied Econometrics with R
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(gmnl) # Multinomial Logit Models with Random Parameters
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics
library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax
library(mlogit) # Multinomial Logit Models
library(stargazer) # Well-Formatted Regression and Summary Statistics Tables
library(tibble) # Simple Data Frames
library(tidyr) # Tidy Messy Data

data("TravelMode",
     package = "AER")

Proportion <- TravelMode %>%
  filter(choice == "yes") %>%
  select(mode) %>%
  group_by(mode) %>%
  summarise(no_rows = length(mode))
# Calculate the median of the variables
df <- TravelMode %>%
  group_by(mode) %>%
  summarise(vcost = median(vcost),
            wait = median(wait),
            travel = median(travel))
# Calculate proportions
df$Proportion <- Proportion$no_rows/(nrow(TravelMode)/4)
df %>%
  kable(digits = 3) %>%
  kable_styling()

TM <- mlogit.data(TravelMode,
                  choice = "choice",
                  shape = "long",
                  alt.levels = c("air",
                                 "train",
                                 "bus",
                                 "car"))

#Multinomial logit
mnl0 <- mlogit(choice ~ vcost + travel + wait | 1,
               data = TM)
#Nested logit:
nl <- mlogit(choice ~ vcost + travel + wait | 1,
             data = TM,
             nests = list(land = c( "car",
                                    "bus",
                                    "train"),
                          air = c("air")),
             un.nest.el = TRUE)
#Multinomial probit:
prbt <- mlogit(choice ~ vcost + travel + wait | 1,
               data = TM,
               probit = TRUE)

(nl$coefficients["iv"] - 1) / sqrt(vcov(nl)["iv","iv"])

# MIXL T
mixl_t <- gmnl(choice ~ vcost + travel + wait | 1,
               data = TM,
               model = "mixl",
               ranp = c(travel = "n"),
               R = 50)
mixl_t$logLik$message
summary(mixl_t)

# MIXL W
mixl_w <- gmnl(choice ~ vcost + travel + wait | 1,
               data = TM,
               model = "mixl",
               ranp = c(wait = "n"),
               R = 50)
mixl_w$logLik$message
summary(mixl_w)

# MIXL T&W
mixl <- gmnl(choice ~ vcost + travel + wait | 1,
             data = TM,
             model = "mixl",
             ranp = c(travel = "n", wait = "n"),
             R = 60)
mixl$logLik$message
summary(mixl)

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

data_DCE_numeric <- readRDS("output/data_DCE_numeric.rds")


# both survey merged

data_DCE_mlogit <- mlogit.data(data_DCE_numeric,
                               choice = "choice",
                               shape = "long",
                               alt.var = "Scenario",
                               id.var = "survey_person",
                               chid.var = "chid")


# Temps


mixl_Temps1 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif + CSPgroup_plus +
                      CSPgroup_moins + class_nat + survey_id + journey_duration3 + 
                      main_vehicule_indiv_motor + main_vehicule_commun + Perso_relation_nature,
                    data = data_DCE_mlogit,
                    model = "mixl",
                    ranp = c(Temps = "n",
                             Paysage = "n",
                             Acces = "n",
                             Biodiversite = "n",
                             Biome1 = "n",
                             Biome2 = "n"),
                    mvar = list(Temps = c("survey_id"),
                                Paysage = c("survey_id"),
                                Acces = c("survey_id"),
                                Biodiversite = c("survey_id"),
                                Biome1 = c("survey_id"),
                                Biome2 = c("survey_id")),
                    R = 60)
mixl_Temps1$logLik$message
summary(mixl_Temps1)

saveRDS(mixl_Temps1,"output/mixl_Temps1.rds")



mixl_Temps2 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif + CSPgroup_plus +
                     CSPgroup_moins + class_nat + survey_id + journey_duration3 + 
                     main_vehicule_indiv_motor + main_vehicule_commun + Perso_relation_nature,
                   data = data_DCE_mlogit,
                   model = "mixl",
                   ranp = c(Temps = "n",
                            Paysage = "n",
                            Acces = "n",
                            Biodiversite = "n",
                            Biome1 = "n",
                            Biome2 = "n"),
                   mvar = list(Temps = c("Gender", "Age", "Income","survey_id"),
                               Paysage = c("Gender", "Age", "Income","survey_id"),
                               Acces = c("Gender", "Age", "Income","survey_id"),
                               Biodiversite = c("Gender", "Age", "Income","survey_id"),
                               Biome1 = c("Gender", "Age", "Income","survey_id"),
                               Biome2 = c("Gender", "Age", "Income","survey_id")),
                   R = 60)
mixl_Temps2$logLik$message
summary(mixl_Temps)

saveRDS(mixl_Temps2,"output/mixl_Temps2.rds")


mixl_Temps3 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif + CSPgroup_plus +
                     CSPgroup_moins + class_nat + survey_id + journey_duration3 + 
                     main_vehicule_indiv_motor + main_vehicule_commun + Perso_relation_nature,
                   data = data_DCE_mlogit,
                   model = "mixl",
                   ranp = c(Temps = "n",
                            Paysage = "n",
                            Acces = "n",
                            Biodiversite = "n",
                            Biome1 = "n",
                            Biome2 = "n"),
                   mvar = list(Temps = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat"),
                               Paysage = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat"),
                               Acces = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat"),
                               Biodiversite = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat"),
                               Biome1 = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat"),
                               Biome2 = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat")),
                   R = 60)
mixl_Temps3$logLik$message
summary(mixl_Temps3)

saveRDS(mixl_Temps3,"output/mixl_Temps3.rds")

mixl_Temps4 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif + CSPgroup_plus +
                     CSPgroup_moins + class_nat + survey_id + journey_duration3 + 
                     main_vehicule_indiv_motor + main_vehicule_commun + Perso_relation_nature,
                   data = data_DCE_mlogit,
                   model = "mixl",
                   ranp = c(Temps = "n",
                            Paysage = "n",
                            Acces = "n",
                            Biodiversite = "n",
                            Biome1 = "n",
                            Biome2 = "n"),
                   mvar = list(Temps = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","main_vehicule_commun","journey_duration3"),
                               Paysage = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","main_vehicule_commun","journey_duration3"),
                               Acces = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","main_vehicule_commun","journey_duration3"),
                               Biodiversite = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","main_vehicule_commun","journey_duration3"),
                               Biome1 = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","main_vehicule_commun","journey_duration3"),
                               Biome2 = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","main_vehicule_commun","journey_duration3")),
                   R = 60)
mixl_Temps4$logLik$message
summary(mixl_Temps4)

saveRDS(mixl_Temps4,"output/mixl_Temps4.rds")


mixl_Temps5 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif + CSPgroup_plus +
                      CSPgroup_moins + class_nat + survey_id + journey_duration3 + 
                      main_vehicule_indiv_motor + main_vehicule_commun + Perso_relation_nature,
                    data = data_DCE_mlogit,
                    model = "mixl",
                    ranp = c(Temps = "ln",
                             Paysage = "n",
                             Acces = "n",
                             Biodiversite = "n",
                             Biome1 = "n",
                             Biome2 = "n",
                             asc = "n"),
                    mvar = list(Temps = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","main_vehicule_commun","journey_duration3"),
                                Paysage = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","main_vehicule_commun","journey_duration3"),
                                Acces = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat"),
                                Biodiversite = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat"),
                                Biome1 = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat"),
                                Biome2 = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat"),
                                asc = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","main_vehicule_commun","journey_duration3")),
                    R = 60)
mixl_Temps5$logLik$message
summary(mixl_Temps5)

saveRDS(mixl_Temps5,"output/mixl_Temps5.rds")



test <- data_DCE_numeric[,c("Gender", "Age", "Income", "CSPgroup_inactif", "CSPgroup_plus","CSPgroup_retraite",
                            "CSPgroup_moins", "class_nat", "survey_id", "journey_duration3","Perso_knowledge_biodiversity","Perso_behaviour_nature",
                            "main_vehicule_indiv_motor","main_vehicule_indiv_no_motor", "main_vehicule_commun","Education", "Perso_relation_nature")]
#calculate correlation between each pairwise combination of variables
cor_df <- round(cor(na.omit(test)), 2)

#melt the data frame
melted_cormat <- melt(cor_df)

#create correlation heatmap
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), size = 5) +
  scale_fill_gradient2(low = "blue", high = "red",
                       limit = c(-1,1), name="Correlation") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

mixl_Temps5t <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif +
                       CSPgroup_moins + class_nat + survey_id + journey_duration3 +
                       main_vehicule_indiv_no_motor + main_vehicule_commun + Perso_relation_nature + Perso_behaviour_nature,
                     data = data_DCE_mlogit,
                     model = "mixl",
                     panel=TRUE,
                     ranp = c(Temps = "t",
                              Paysage = "n",
                              Acces = "n",
                              Biodiversite = "n",
                              Biome1 = "n",
                              Biome2 = "n",
                              asc = "n"),
                     mvar = list(Temps = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                 Paysage = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                 Acces = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                 Biodiversite = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                 Biome1 = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                 Biome2 = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                 asc = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3")),
                     R = 2000)

summary(mixl_Temps5t)

saveRDS(mixl_Temps5,"mixl_Temps5t.rds")

mixl_Temps5 <- readRDS("output/mixl_Temps5b.rds") # R 2000



mixl_Temps5t2 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif +
                        CSPgroup_moins + class_nat + survey_id + journey_duration3 +
                        main_vehicule_indiv_no_motor + main_vehicule_commun + Perso_relation_nature + Perso_behaviour_nature,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Temps = "t",
                               Paysage = "n",
                               Acces = "n",
                               Biodiversite = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Temps = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  Paysage = c("Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  Acces = c("Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  Biodiversite = c("Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  Biome1 = c("Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  Biome2 = c("Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  asc = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3")),
                      R = 60)

mixl_Temps5t3 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif +
                        CSPgroup_moins + class_nat + survey_id + journey_duration3 +
                        main_vehicule_indiv_no_motor + main_vehicule_commun + Perso_relation_nature + Perso_behaviour_nature,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Temps = "t",
                               Paysage = "n",
                               Acces = "n",
                               Biodiversite = "n",
                               asc = "n"),
                      mvar = list(Temps = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  Paysage = c("Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  Acces = c("Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  Biodiversite = c("Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  asc = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3")),
                      R = 60)


summary(mixl_Temps5t3)

saveRDS(mixl_Temps5t3,"mixl_Temps5t3.rds")


mixl_Temps5t4 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif +
                        CSPgroup_moins + class_nat + survey_id + journey_duration3 +
                        main_vehicule_indiv_no_motor + main_vehicule_commun + Perso_relation_nature + Perso_behaviour_nature,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Temps = "t",
                               Paysage = "n",
                               Acces = "n",
                               Biodiversite = "n",
                               asc = "n"),
                      mvar = list(Temps = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  Paysage = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  Acces = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  Biodiversite = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3"),
                                  asc = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_commun","main_vehicule_indiv_no_motor","journey_duration3")),
                      R = 60)


summary(mixl_Temps5t4)

saveRDS(mixl_Temps5t4,"mixl_Temps5t4.rds")





mixl_Temps5t5 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif +
                        CSPgroup_moins + class_nat + survey_id + journey_duration3 +
                        main_vehicule_indiv_motor + Perso_relation_nature + Perso_behaviour_nature,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Temps = "t",
                               Paysage = "n",
                               Acces = "n",
                               Biodiversite = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Temps = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_indiv_motor","journey_duration3"),
                                  Paysage = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_indiv_motor","journey_duration3"),
                                  Acces = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_indiv_motor","journey_duration3"),
                                  Biodiversite = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_indiv_motor","journey_duration3"),
                                  asc = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_moins","main_vehicule_indiv_motor","journey_duration3")),
                      R = 60)


summary(mixl_Temps5t5)

saveRDS(mixl_Temps5t5,"mixl_Temps5t5.rds")




mixl_Temps5t6 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif +
                        CSPgroup_moins + CSPgroup_retraite + class_nat + survey_id + journey_duration3 +
                        main_vehicule_indiv_motor + Perso_relation_nature + Perso_behaviour_nature,
                      data = data_DCE_mlogit,
                      model = "mixl",
                      panel=TRUE,
                      ranp = c(Temps = "t",
                               Paysage = "n",
                               Acces = "n",
                               Biodiversite = "n",
                               Biome1 = "n",
                               Biome2 = "n",
                               asc = "n"),
                      mvar = list(Temps = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_retraite","CSPgroup_moins","main_vehicule_indiv_motor","journey_duration3"),
                                  Paysage = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_retraite","CSPgroup_moins","main_vehicule_indiv_motor","journey_duration3"),
                                  Acces = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_retraite","CSPgroup_moins","main_vehicule_indiv_motor","journey_duration3"),
                                  Biodiversite = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_retraite","CSPgroup_moins","main_vehicule_indiv_motor","journey_duration3"),
                                  asc = c("Gender", "Age", "Income","Perso_relation_nature","Perso_behaviour_nature","survey_id","class_nat","CSPgroup_inactif","CSPgroup_retraite","CSPgroup_moins","main_vehicule_indiv_motor","journey_duration3")),
                      R = 2000)


summary(mixl_Temps5t6)

saveRDS(mixl_Temps5t6,"mixl_Temps5t6.rds")

mixl_Temps <- readRDS("output/mixl_Temps5t6.rds") # R 2000
summary(mixl_Temps)


# Retrieve the estimated parameters for unconditional distribution
mu <- coef(mixl_Temps)['Temps']
sigma <- coef(mixl_Temps)['sd.Temps']
# Percent of population with a positive parameter for Temps
1 - ptri(0, mode = coef(mixl_Temps)['Temps'],
         min = coef(mixl_Temps)['Temps']-1.96*coef(mixl_Temps)['sd.Temps'],
         max = coef(mixl_Temps)['Temps']+1.96*coef(mixl_Temps)['sd.Temps'])
# Create a data frame for plotting
df_uncond <- data.frame(x = seq(from = -2,
                        to = 0.75,
                        by = 0.005)) %>%
  # Draw from the normal distribution for x given the mean and sd
  mutate(normal = dtri(x,
                       mode = mu,
                       min = mu-1.96*sigma,
                       max = mu+1.96*sigma))
# Define parameters for the conditional distribution
bn_Temps <- effect.gmnl(mixl_Temps,
                        par = "Temps",# Choose conditional effect
                        effect = "ce")
df_cond <- data.frame(bn_Temps = bn_Temps$mean)

# Plot
ggplot() +
  # Plot the distribution
  geom_area(data = df_uncond,
            aes(x = x,
                y = normal),
            fill = "grey",
            alpha = 0.5) +
  geom_density(data = df_cond,
               aes(x = bn_Temps),
               fill = "orange",
               colour = NA,
               alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  theme_modern() +
  ylab("f(x)") + # Label the y axis
  xlab(expression(beta[n][Temps])) #+ # Label the x axis
  #ggtitle("Unconditional distribution for Temps parameter")

ggsave("output/distrib_temp.png",
       width = 8,
       height = 8,
       dpi = 400)

mu <- coef(mixl_Temps)['Paysage']
sigma <- coef(mixl_Temps)['sd.Paysage']
df_uncond <- data.frame(x = seq(from = -3,to = 3,by = 0.005)) %>%
  mutate(normal = dnorm(x,mean = mu,sd = sigma))
bn_Paysage <- effect.gmnl(mixl_Temps,par = "Paysage",effect = "ce")
df_cond <- data.frame(bn_Paysage = bn_Paysage$mean)

ggplot() +
  geom_area(data = df_uncond, aes(x = x, y = normal),
            fill = "grey", alpha = 0.5) +
  geom_density(data = df_cond, aes(x = bn_Paysage),
               fill = "orange", colour = NA, alpha = 0.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n][Paysage]))

ggsave("output/distrib_paysage.png",
       width = 8,
       height = 8,
       dpi = 400)

mu <- coef(mixl_Temps)['Acces']
sigma <- coef(mixl_Temps)['sd.Acces']
df_uncond <- data.frame(x = seq(from = -3,to = 5,by = 0.005)) %>%
  mutate(normal = dnorm(x,mean = mu,sd = sigma))
bn_Acces <- effect.gmnl(mixl_Temps,par = "Acces",effect = "ce")
df_cond <- data.frame(bn_Acces = bn_Acces$mean)

ggplot() +
  geom_area(data = df_uncond, aes(x = x, y = normal),
            fill = "grey", alpha = 0.5) +
  geom_density(data = df_cond, aes(x = bn_Acces),
               fill = "orange", colour = NA, alpha = 0.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n][Acces]))

ggsave("output/distrib_acces.png",
       width = 8,
       height = 8,
       dpi = 400)


mu <- coef(mixl_Temps)['Biodiversite']
sigma <- coef(mixl_Temps)['sd.Biodiversite']
df_uncond <- data.frame(x = seq(from = -3,to = 3,by = 0.005)) %>%
  mutate(normal = dnorm(x,mean = mu,sd = sigma))
bn_Biodiversite <- effect.gmnl(mixl_Temps,par = "Biodiversite",effect = "ce")
df_cond <- data.frame(bn_Biodiversite = bn_Biodiversite$mean)

ggplot() +
  geom_area(data = df_uncond, aes(x = x, y = normal),
            fill = "grey", alpha = 0.5) +
  geom_density(data = df_cond, aes(x = bn_Biodiversite),
               fill = "orange", colour = NA, alpha = 0.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n][Biodiversite]))

ggsave("output/distrib_biodiversite.png",
       width = 8,
       height = 8,
       dpi = 400)


mu <- coef(mixl_Temps)['asc']
sigma <- coef(mixl_Temps)['sd.asc']
df_uncond <- data.frame(x = seq(from = -10,to = 10,by = 0.005)) %>%
  mutate(normal = dnorm(x,mean = mu,sd = sigma))
bn_asc <- effect.gmnl(mixl_Temps,par = "asc",effect = "ce")
df_cond <- data.frame(bn_asc = bn_asc$mean)

ggplot() +
  geom_area(data = df_uncond, aes(x = x, y = normal),
            fill = "grey", alpha = 0.5) +
  geom_density(data = df_cond, aes(x = bn_asc),
               fill = "orange", colour = NA, alpha = 0.5) +
  geom_hline(yintercept = 0) + geom_vline(xintercept = 0) + 
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n][asc]))

ggsave("output/distrib_asc.png",
       width = 8,
       height = 8,
       dpi = 400)




# willingness to accept time
wtp.gmnl(mixl_Temps, wrt = "Temps")





#### Old code
# Temps

mixl_Temps <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | asc,
             data = data_DCE_mlogit,
             model = "mixl",
             ranp = c(Temps = "n"),
             R = 60)
mixl_Temps$logLik$message
summary(mixl_Temps)

# Retrieve the estimated parameters
mu <- coef(mixl_Temps)['Temps']
sigma <- coef(mixl_Temps)['sd.Temps']
# Create a data frame for plotting
df <- data.frame(x =seq(from = -0.6,
                        to = 0.2,
                        by = 0.005)) %>%
  # Draw from the normal distribution for x given the mean and sd
  mutate(normal = dnorm(x,
                        mean = mu,
                        sd = sigma))
# Same, but only positive values of x
df_p <- data.frame(x = seq(from = 0,
                           to = 0.2,
                           by = 0.005)) %>%
  mutate(normal = dnorm(x,
                        mean = mu,
                        sd = sigma))

# Plot
ggplot() +
  # Plot the distribution
  geom_area(data = df,
            aes(x = x,
                y = normal),
            fill = "orange",
            alpha = 0.5) +
  # Plot the distribution for positive values of x only
  geom_area(data = df_p,
            aes(x = x,
                y = normal),
            fill = "orange",
            alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(beta[n][Temps])) + # Label the x axis
  ggtitle("Unconditional distribution for Temps parameter")

# Percent of population with a positive parameter for Temps
1 - pnorm(0,
          mean = coef(mixl_Temps)['Temps'],
          sd = coef(mixl_Temps)['sd.Temps'])

# Define parameters for the distribution
bn_Temps <- effect.gmnl(mixl_Temps,
                       par = "Temps",
                       # Choose conditional effect
                       effect = "ce")

# Create a data frame for plotting
df <- data.frame(bn_Temps = bn_Temps$mean)
# Plot
ggplot() +
  geom_density(data = df,
               aes(x = bn_Temps),
               fill = "orange",
               alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(beta[n][Temps])) + # Label the x axis
  ggtitle("Conditional distribution for Temps parameter")


# Paysage

mixl_Paysage <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 1,
                   data = data_DCE_mlogit,
                   model = "mixl",
                   ranp = c(Paysage = "n"),
                   R = 60)
mixl_Paysage$logLik$message
summary(mixl_Paysage)

# Retrieve the estimated parameters
mu <- coef(mixl_Paysage)['Paysage']
sigma <- coef(mixl_Paysage)['sd.Paysage']
# Create a data frame for plotting
df <- data.frame(x =seq(from = -0.06,
                        to = 0.1,
                        by = 0.005)) %>%
  # Draw from the normal distribution for x given the mean and sd
  mutate(normal = dnorm(x,
                        mean = mu,
                        sd = sigma))
# Same, but only positive values of x
df_p <- data.frame(x = seq(from = 0,
                           to = 0.1,
                           by = 0.005)) %>%
  mutate(normal = dnorm(x,
                        mean = mu,
                        sd = sigma))

# Plot
ggplot() +
  # Plot the distribution
  geom_area(data = df,
            aes(x = x,
                y = normal),
            fill = "orange",
            alpha = 0.5) +
  # Plot the distribution for positive values of x only
  geom_area(data = df_p,
            aes(x = x,
                y = normal),
            fill = "orange",
            alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(beta[n][Paysage])) + # Label the x axis
  ggtitle("Unconditional distribution for Paysage parameter")

# Percent of population with a positive parameter for Paysage
1 - pnorm(0,
          mean = coef(mixl_Paysage)['Paysage'],
          sd = coef(mixl_Paysage)['sd.Paysage'])

# Define parameters for the distribution
bn_Paysage <- effect.gmnl(mixl_Paysage,
                        par = "Paysage",
                        # Choose conditional effect
                        effect = "ce")

# Create a data frame for plotting
df <- data.frame(bn_Paysage = bn_Paysage$mean)
# Plot
ggplot() +
  geom_density(data = df,
               aes(x = bn_Paysage),
               fill = "orange",
               alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(beta[n][Paysage])) + # Label the x axis
  ggtitle("Conditional distribution for Paysage parameter")


# Define parameters for the distribution of willingness to pay
mu <- coef(mixl_Paysage)['Paysage'] / coef(mixl_Paysage)['Temps']
sigma <- sqrt(coef(mixl_Paysage)['sd.Paysage']^2/ coef(mixl_Paysage)['Temps']^2)
# Create a data frame for plotting
df <- data.frame(x =seq(from = -1, to = 1, by = 0.1)) %>%
  mutate(normal = dnorm(x, mean = mu, sd = sigma))

# Plot
ggplot() +
  geom_area(data = df, aes(x, normal), fill = "orange", alpha = 0.5) +
  # geom_area(data = df_p, aes(x, normal), fill = "orange", alpha = 0.5) +
  #ylim(c(0, 1/(2 * L) + 0.2 * 1/(2 * L))) + # Set the limits of the y axis
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(WTP[n][Temps])) + # Label the x axis
  ggtitle("Unconditional distribution for willingness to Temps for Paysage")


# Define parameters for the distribution
wtp_Paysage <- effect.gmnl(mixl_Paysage,
                        par = "Paysage",
                        # Effects is willingness to pay
                        effect = "wtp",
                        # With respect to 
                        wrt = "Temps")
# Create a data frame for plotting
df <- data.frame(wtp_Paysage = wtp_Paysage$mean)
# Plot
ggplot() +
  geom_density(data = df,
               aes(x = wtp_Paysage),
               fill = "orange",
               alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(WTP[n][Temps])) + # Label the x axis
  ggtitle("Conditional willingness to Temps for Paysage")


# Acces

mixl_Acces <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 1,
                     data = data_DCE_mlogit,
                     model = "mixl",
                     ranp = c(Acces = "n"),
                     R = 60)
mixl_Acces$logLik$message
summary(mixl_Acces)

# Retrieve the estimated parameters
mu <- coef(mixl_Acces)['Acces']
sigma <- coef(mixl_Acces)['sd.Acces']
# Create a data frame for plotting
df <- data.frame(x =seq(from = -0.6,
                        to = 0.2,
                        by = 0.005)) %>%
  # Draw from the normal distribution for x given the mean and sd
  mutate(normal = dnorm(x,
                        mean = mu,
                        sd = sigma))
# Same, but only positive values of x
df_p <- data.frame(x = seq(from = 0,
                           to = 0.2,
                           by = 0.005)) %>%
  mutate(normal = dnorm(x,
                        mean = mu,
                        sd = sigma))

# Plot
ggplot() +
  # Plot the distribution
  geom_area(data = df,
            aes(x = x,
                y = normal),
            fill = "orange",
            alpha = 0.5) +
  # Plot the distribution for positive values of x only
  geom_area(data = df_p,
            aes(x = x,
                y = normal),
            fill = "orange",
            alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(beta[n][Acces])) + # Label the x axis
  ggtitle("Unconditional distribution for Acces parameter")

# Percent of population with a positive parameter for Acces
1 - pnorm(0,
          mean = coef(mixl_Acces)['Acces'],
          sd = coef(mixl_Acces)['sd.Acces'])

# Define parameters for the distribution
bn_Acces <- effect.gmnl(mixl_Acces,
                          par = "Acces",
                          # Choose conditional effect
                          effect = "ce")

# Create a data frame for plotting
df <- data.frame(bn_Acces = bn_Acces$mean)
# Plot
ggplot() +
  geom_density(data = df,
               aes(x = bn_Acces),
               fill = "orange",
               alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(beta[n][Acces])) + # Label the x axis
  ggtitle("Conditional distribution for Acces parameter")


# Biodiversite

mixl_Biodiversite <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 1,
                     data = data_DCE_mlogit,
                     model = "mixl",
                     ranp = c(Biodiversite = "n"),
                     R = 60)
mixl_Biodiversite$logLik$message
summary(mixl_Biodiversite)

# Retrieve the estimated parameters
mu <- coef(mixl_Biodiversite)['Biodiversite']
sigma <- coef(mixl_Biodiversite)['sd.Biodiversite']
# Create a data frame for plotting
df <- data.frame(x =seq(from = -0.6,
                        to = 0.2,
                        by = 0.005)) %>%
  # Draw from the normal distribution for x given the mean and sd
  mutate(normal = dnorm(x,
                        mean = mu,
                        sd = sigma))
# Same, but only positive values of x
df_p <- data.frame(x = seq(from = 0,
                           to = 0.2,
                           by = 0.005)) %>%
  mutate(normal = dnorm(x,
                        mean = mu,
                        sd = sigma))

# Plot
ggplot() +
  # Plot the distribution
  geom_area(data = df,
            aes(x = x,
                y = normal),
            fill = "orange",
            alpha = 0.5) +
  # Plot the distribution for positive values of x only
  geom_area(data = df_p,
            aes(x = x,
                y = normal),
            fill = "orange",
            alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(beta[n][Biodiversite])) + # Label the x axis
  ggtitle("Unconditional distribution for Biodiversite parameter")

# Percent of population with a positive parameter for Biodiversite
1 - pnorm(0,
          mean = coef(mixl_Biodiversite)['Biodiversite'],
          sd = coef(mixl_Biodiversite)['sd.Biodiversite'])

# Define parameters for the distribution
bn_Biodiversite <- effect.gmnl(mixl_Biodiversite,
                          par = "Biodiversite",
                          # Choose conditional effect
                          effect = "ce")

# Create a data frame for plotting
df <- data.frame(bn_Biodiversite = bn_Biodiversite$mean)
# Plot
ggplot() +
  geom_density(data = df,
               aes(x = bn_Biodiversite),
               fill = "orange",
               alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(beta[n][Biodiversite])) + # Label the x axis
  ggtitle("Conditional distribution for Biodiversite parameter")



# Biome

mixl_Biome <- gmnl(choice ~ Temps + Biome + Acces + Biodiversite + Biome | 1,
                     data = data_DCE_mlogit,
                     model = "mixl",
                     ranp = c(Biome = "n"),
                     R = 60)
mixl_Biome$logLik$message
summary(mixl_Biome)

# Retrieve the estimated parameters
mu <- coef(mixl_Biome)['Biome']
sigma <- coef(mixl_Biome)['sd.Biome']
# Create a data frame for plotting
df <- data.frame(x =seq(from = -0.6,
                        to = 0.2,
                        by = 0.005)) %>%
  # Draw from the normal distribution for x given the mean and sd
  mutate(normal = dnorm(x,
                        mean = mu,
                        sd = sigma))
# Same, but only positive values of x
df_p <- data.frame(x = seq(from = 0,
                           to = 0.2,
                           by = 0.005)) %>%
  mutate(normal = dnorm(x,
                        mean = mu,
                        sd = sigma))

# Plot
ggplot() +
  # Plot the distribution
  geom_area(data = df,
            aes(x = x,
                y = normal),
            fill = "orange",
            alpha = 0.5) +
  # Plot the distribution for positive values of x only
  geom_area(data = df_p,
            aes(x = x,
                y = normal),
            fill = "orange",
            alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(beta[n][Biome])) + # Label the x axis
  ggtitle("Unconditional distribution for Biome parameter")

# Percent of population with a positive parameter for Biome
1 - pnorm(0,
          mean = coef(mixl_Biome)['Biome'],
          sd = coef(mixl_Biome)['sd.Biome'])

# Define parameters for the distribution
bn_Biome <- effect.gmnl(mixl_Biome,
                          par = "Biome",
                          # Choose conditional effect
                          effect = "ce")

# Create a data frame for plotting
df <- data.frame(bn_Biome = bn_Biome$mean)
# Plot
ggplot() +
  geom_density(data = df,
               aes(x = bn_Biome),
               fill = "orange",
               alpha = 0.5) +
  geom_hline(yintercept = 0) + # Add y axis
  geom_vline(xintercept = 0) + # Add x axis
  ylab("f(x)") + # Label the y axis
  xlab(expression(beta[n][Biome])) + # Label the x axis
  ggtitle("Conditional distribution for Biome parameter")

