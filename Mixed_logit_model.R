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



test <- data_DCE_numeric[,c("Gender", "Age", "Income","Education", "CSPgroup_plus","CSPgroup_moins","CSPgroup_inactif","CSPgroup_retraite",
                             "class_nat", "Perso_knowledge_biodiversity","Perso_behaviour_nature", "Perso_relation_nature",
                            "journey_duration3","main_vehicule_indiv_motor","main_vehicule_indiv_no_motor", "main_vehicule_commun", "survey_id")]

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
  scale_x_discrete(labels=c("Gender", "Age", "Income", "CSPgroup_inactif"="Inactive SPC", "CSPgroup_plus"="Higher SPC","CSPgroup_retraite"="Retired",
                            "CSPgroup_moins"="Lower SPC", "class_nat"="Habitat naturalness", "survey_id"="Informative framing", "journey_duration3"="Travel time",
                            "Perso_knowledge_biodiversity"="Declared biodiversity knowledge","Perso_behaviour_nature"="Declared environmental behaviour",
                            "main_vehicule_indiv_motor"="Individual motorised transport","main_vehicule_indiv_no_motor"="Individual not motorised transport",
                            "main_vehicule_commun"="Public transport","Education", "Perso_relation_nature"="INS")) +
  scale_y_discrete(labels=c("Gender", "Age", "Income", "CSPgroup_inactif"="Inactive SPC", "CSPgroup_plus"="Higher SPC","CSPgroup_retraite"="Retired",
                            "CSPgroup_moins"="Lower SPC", "class_nat"="Habitat naturalness", "survey_id"="Informative framing", "journey_duration3"="Travel time",
                            "Perso_knowledge_biodiversity"="Declared biodiversity knowledge","Perso_behaviour_nature"="Declared environmental behaviour",
                            "main_vehicule_indiv_motor"="Individual motorised transport","main_vehicule_indiv_no_motor"="Individual not motorised transport",
                            "main_vehicule_commun"="Public transport","Education", "Perso_relation_nature"="INS")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), legend.position = "none")

ggsave("output/correlation_all_variable.png",
       width = 11,
       height = 11,
       dpi = 400)


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
                      R = 2000)


summary(mixl_Temps5t6)

saveRDS(mixl_Temps5t6,"mixl_Temps5t6.rds")



mixl_Temps5t6_retraite <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif +
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


summary(mixl_Temps5t6_retraite)

saveRDS(mixl_Temps5t6_retraite,"mixl_Temps5t6_retraite.rds")


mixl_Temps5t7 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income + CSPgroup_inactif +
                        class_nat + survey_id + journey_duration3 +  main_vehicule_indiv_motor + Perso_relation_nature,
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
                      mvar = list(Temps = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","CSPgroup_inactif","main_vehicule_indiv_motor","journey_duration3"),
                                  Paysage = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","CSPgroup_inactif","main_vehicule_indiv_motor","journey_duration3"),
                                  Acces = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","CSPgroup_inactif","main_vehicule_indiv_motor","journey_duration3"),
                                  Biodiversite = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","CSPgroup_inactif","main_vehicule_indiv_motor","journey_duration3"),
                                  asc = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","CSPgroup_inactif","main_vehicule_indiv_motor","journey_duration3")),
                      R = 60)


summary(mixl_Temps5t7)

saveRDS(mixl_Temps5t7,"mixl_Temps5t7.rds")


mixl_Temps5t8 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income +
                        class_nat + survey_id + journey_duration3 +  main_vehicule_indiv_motor + Perso_relation_nature,
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
                      mvar = list(Temps = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","journey_duration3"),
                                  Paysage = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","journey_duration3"),
                                  Acces = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","journey_duration3"),
                                  Biodiversite = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","journey_duration3"),
                                  asc = c("Gender", "Age", "Income","Perso_relation_nature","survey_id","class_nat","main_vehicule_indiv_motor","journey_duration3")),
                      R = 60)


summary(mixl_Temps5t8)

saveRDS(mixl_Temps5t8,"mixl_Temps5t8.rds")


# entier

mixl_Temps_ak <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  Paysage = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  Acces = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  Biodiversite = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  asc = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel")),
                      R = 60)


summary(mixl_Temps_ak)

saveRDS(mixl_Temps_ak,"mixl_Temps_ak.rds")

# on retire une interaction par attribut

mixl_Temps_al <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","class_nat","job_travel"),
                                  Paysage = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","job_travel"),
                                  Acces = c("Gender", "Age", "Income_cap","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  Biodiversite = c("Gender", "Age","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  asc = c("Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel")),
                      R = 60)


summary(mixl_Temps_al)

saveRDS(mixl_Temps_al,"mixl_Temps_al.rds")



mixl_Temps_am <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","class_nat","job_travel"),
                                  Paysage = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","job_travel"),
                                  Acces = c("Gender", "Age", "Income_cap","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  Biodiversite = c("Gender", "Age","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  asc = c("Income_cap","Perso_relation_nature","Perso_behaviour_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel")),
                      R = 60)


summary(mixl_Temps_am)

saveRDS(mixl_Temps_am,"mixl_Temps_am.rds")


mixl_Temps_an <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","class_nat"),
                                  Paysage = c("Gender", "Age", "Income_cap","Perso_relation_nature","Perso_behaviour_nature","job_travel"),
                                  Acces = c("Gender", "Age", "Income_cap","survey_id","class_nat","job_travel"),
                                  Biodiversite = c("Gender", "Age","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel"),
                                  asc = c("Income_cap","Perso_relation_nature","Perso_knowledge_biodiversity","survey_id","class_nat","job_travel")),
                      R = 60)


summary(mixl_Temps_an)

saveRDS(mixl_Temps_an,"mixl_Temps_an.rds")

mixl_Temps_ao <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age", "Income_cap","Perso_relation_nature","class_nat"),
                                  Paysage = c("Gender", "Age", "Income_cap","Perso_relation_nature","job_travel"),
                                  Acces = c("Gender", "Age","survey_id","class_nat","job_travel"),
                                  Biodiversite = c("Gender", "Age","survey_id","class_nat","job_travel"),
                                  asc = c("Income_cap","Perso_relation_nature","survey_id","class_nat","job_travel")),
                      R = 60)


summary(mixl_Temps_ao)

saveRDS(mixl_Temps_ao,"mixl_Temps_ao.rds")

mixl_Temps_ap <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age", "Income_cap","Perso_relation_nature"),
                                  Paysage = c("Gender", "Age", "Income_cap","Perso_relation_nature"),
                                  Acces = c("Gender", "Age","class_nat","job_travel"),
                                  Biodiversite = c("Gender", "Age","class_nat","job_travel"),
                                  asc = c("Income_cap","Perso_relation_nature","survey_id","job_travel")),
                      R = 60)


summary(mixl_Temps_ap)

saveRDS(mixl_Temps_ap,"mixl_Temps_ap.rds")



mixl_Temps_aq <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age", "Income_cap"),
                                  Paysage = c("Gender", "Age", "Income_cap"),
                                  Acces = c("Gender", "Age","job_travel"),
                                  Biodiversite = c("Gender", "Age","job_travel"),
                                  asc = c("Income_cap","Perso_relation_nature","job_travel")),
                      R = 60)


summary(mixl_Temps_aq)

saveRDS(mixl_Temps_aq,"mixl_Temps_aq.rds")


mixl_Temps_ar <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age"),
                                  Paysage = c("Gender", "Age"),
                                  Acces = c("Gender", "Age","job_travel"),
                                  Biodiversite = c("Gender", "Age"),
                                  asc = c("Perso_relation_nature","job_travel")),
                      R = 60)


summary(mixl_Temps_ar)

saveRDS(mixl_Temps_ar,"mixl_Temps_ar.rds")


mixl_Temps_as <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age"),
                                  Paysage = c("Gender", "Age"),
                                  Acces = c("Gender", "Age","job_travel"),
                                  Biodiversite = c("Age"),
                                  asc = c("Perso_relation_nature","job_travel")),
                      R = 60)


summary(mixl_Temps_as)

saveRDS(mixl_Temps_as,"mixl_Temps_as.rds")

# mixl_Temps_ar le meilleur AIC BIC

AIC_BIC_mixl <- data.frame(model = paste0("M",1:9),
                           AIC = c(AIC(mixl_Temps_ak),AIC(mixl_Temps_al),AIC(mixl_Temps_am),
                                   AIC(mixl_Temps_an),AIC(mixl_Temps_ao),AIC(mixl_Temps_ap),
                                   AIC(mixl_Temps_aq),AIC(mixl_Temps_ar),AIC(mixl_Temps_as)),
                           BIC = c(BIC(mixl_Temps_ak),BIC(mixl_Temps_al),BIC(mixl_Temps_am),
                                   BIC(mixl_Temps_an),BIC(mixl_Temps_ao),BIC(mixl_Temps_ap),
                                   BIC(mixl_Temps_aq),BIC(mixl_Temps_ar),BIC(mixl_Temps_as)))

ggplot(AIC_BIC_mixl, aes(x=model)) +
  geom_point(aes(y=AIC), col="grey") +
  geom_point(aes(y=BIC-200)) + 
  scale_y_continuous(
    name = "AIC",
    sec.axis = sec_axis( trans=~.-200, name="BIC")
  ) +
  theme_modern()




mixl_Temps_at <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age"),
                                  Paysage = c("Gender", "Age"),
                                  Acces = c("Gender", "Age","job_travel"),
                                  Biodiversite = c("Gender", "Age"),
                                  asc = c("Perso_relation_nature","job_travel","survey_id")),
                      R = 60)


summary(mixl_Temps_at)

saveRDS(mixl_Temps_at,"mixl_Temps_at.rds")




mixl_Temps_at2000 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                            job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                          mvar = list(Temps = c("Gender", "Age"),
                                      Paysage = c("Gender", "Age"),
                                      Acces = c("Gender", "Age","job_travel"),
                                      Biodiversite = c("Gender", "Age"),
                                      asc = c("Perso_relation_nature","job_travel","survey_id")),
                          R = 2000)


summary(mixl_Temps_at2000)

saveRDS(mixl_Temps_at2000,"mixl_Temps_at2000.rds")


mixl_Temps_au <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age"),
                                  Paysage = c("Gender", "Age"),
                                  Acces = c("Gender", "Age","job_travel"),
                                  Biodiversite = c("Gender", "Age"),
                                  asc = c("Perso_relation_nature","survey_id")),
                      R = 60)


summary(mixl_Temps_au)

saveRDS(mixl_Temps_au,"mixl_Temps_au.rds")

mixl_Temps_au2000 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                            job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                          mvar = list(Temps = c("Gender", "Age"),
                                      Paysage = c("Gender", "Age"),
                                      Acces = c("Gender", "Age","job_travel"),
                                      Biodiversite = c("Gender", "Age"),
                                      asc = c("Perso_relation_nature","survey_id")),
                          R = 2000)


summary(mixl_Temps_au2000)

saveRDS(mixl_Temps_au2000,"mixl_Temps_au2000.rds")


mixl_Temps_av <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                        job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                      mvar = list(Temps = c("Gender", "Age"),
                                  Paysage = c("Gender", "Age"),
                                  Acces = c("Gender", "Age"),
                                  Biodiversite = c("Gender", "Age","Perso_relation_nature"),
                                  asc = c("Perso_relation_nature","job_travel","survey_id")),
                      R = 60)


summary(mixl_Temps_av)

saveRDS(mixl_Temps_av,"mixl_Temps_av.rds")


mixl_Temps_av2000 <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome + asc | 0 | 0 | Gender + Age + Income_cap + class_nat + survey_id +
                            job_travel + Perso_relation_nature + Perso_behaviour_nature + Perso_knowledge_biodiversity,
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
                          mvar = list(Temps = c("Gender", "Age"),
                                      Paysage = c("Gender", "Age"),
                                      Acces = c("Gender", "Age"),
                                      Biodiversite = c("Gender", "Age","Perso_relation_nature"),
                                      asc = c("Perso_relation_nature","job_travel","survey_id")),
                          R = 2000)


summary(mixl_Temps_av2000)

saveRDS(mixl_Temps_av2000,"mixl_Temps_av2000.rds")


AIC_BIC_mixl2 <- data.frame(model = factor(paste0("M",1:12), levels=paste0("M",1:12)),
                           AIC = c(AIC(mixl_Temps_ak),AIC(mixl_Temps_al),AIC(mixl_Temps_am),
                                   AIC(mixl_Temps_an),AIC(mixl_Temps_ao),AIC(mixl_Temps_ap),
                                   AIC(mixl_Temps_aq),AIC(mixl_Temps_ar),AIC(mixl_Temps_as),
                                   AIC(mixl_Temps_at),AIC(mixl_Temps_au),AIC(mixl_Temps_av)),
                           BIC = c(BIC(mixl_Temps_ak),BIC(mixl_Temps_al),BIC(mixl_Temps_am),
                                   BIC(mixl_Temps_an),BIC(mixl_Temps_ao),BIC(mixl_Temps_ap),
                                   BIC(mixl_Temps_aq),BIC(mixl_Temps_ar),BIC(mixl_Temps_as),
                                   BIC(mixl_Temps_at),BIC(mixl_Temps_au),BIC(mixl_Temps_av)))

ggplot(AIC_BIC_mixl2, aes(x=model)) +
  geom_point(aes(y=AIC), col="grey") +
  geom_point(aes(y=BIC-200)) + 
  scale_y_continuous(
    name = "AIC",
    sec.axis = sec_axis( trans=~.-200, name="BIC")
  ) +
  theme_modern()

ggsave("output/model_selection.png",
       width = 7,
       height = 7,
       dpi = 400)



mixl_Temps <- readRDS("output/mixl_Temps_av2000.rds") # R 2000, smallest AIC
summary(mixl_Temps)




boxLabels <- names(coef(mixl_Temps))[1:(length(coef(mixl_Temps))-7)]
#df <- data.frame(yAxis = length(boxLabels):1,
#                 Attribute = c("Time", "Landscape","Nature use","Biodiversity","Biome peri-urban","Biome rural","ASC",
#                               rep("Time",11),rep("Landscape",11),rep("Nature use",11),rep("Biodiversity",11),rep("ASC",11)),
#                 Variable = c(rep("Main estimate",7),rep(c("Gender","Age","Income","INS","Environmental behaviour","Informative framing","Habitat naturalness",
#                                "Inactive","Lower SPC","Motorised personnal transport","Travel time"),5)),
#                 box_estimate_main = coef(mixl_Temps)[1:length(boxLabels)], 
#                 boxCILow = coef(mixl_Temps)[1:length(boxLabels)]-1.96*summary(mixl_Temps)$CoefTable[1:length(boxLabels),2], 
#                 boxCIHigh = coef(mixl_Temps)[1:length(boxLabels)]+1.96*summary(mixl_Temps)$CoefTable[1:length(boxLabels),2],
#                 var = (summary(mixl_Temps)$CoefTable[1:length(boxLabels),2])^2,
#                 signif = ifelse(summary(mixl_Temps)$CoefTable[1:length(boxLabels),4] < 0.05,"yes","no")
#)

df <- data.frame(yAxis = length(boxLabels):1,
                 Attribute = c("Time", "Landscape","Nature use","Biodiversity","Biome peri-urban","Biome rural","ASC",
                               rep("Time",2),rep("Landscape",2),rep("Nature use",2),rep("Biodiversity",3),rep("ASC",3)),
                 Variable = c(rep("Main estimate",7),rep(c("Gender","Age"),4),"INS",
                              "INS","Commuting","Informative framing"),
                 box_estimate_main = coef(mixl_Temps)[1:length(boxLabels)], 
                 boxCILow = coef(mixl_Temps)[1:length(boxLabels)]-1.96*summary(mixl_Temps)$CoefTable[1:length(boxLabels),2], 
                 boxCIHigh = coef(mixl_Temps)[1:length(boxLabels)]+1.96*summary(mixl_Temps)$CoefTable[1:length(boxLabels),2],
                 var = (summary(mixl_Temps)$CoefTable[1:length(boxLabels),2])^2,
                 signif = ifelse(summary(mixl_Temps)$CoefTable[1:length(boxLabels),4] < 0.05,"yes","no")
                 )

df <- df %>% group_by(Attribute) %>% mutate(box_estimate_interaction = box_estimate_main[which(Variable=="Main estimate")]+box_estimate_main,
                                            sd_interaction = sqrt(var[which(Variable=="Main estimate")]+var))

df[which(df$Variable=="Main estimate"),c("box_estimate_interaction","sd_interaction")] <- NA

#df$CIinteractionHigh <- df$box_estimate_interaction + 1.96*df$sd_interaction
#df$CIinteractionLow <- df$box_estimate_interaction - 1.96*df$sd_interaction
df$CIinteractionHigh <- df$box_estimate_interaction + 1.96*sqrt(df$var)
df$CIinteractionLow <- df$box_estimate_interaction - 1.96*sqrt(df$var)

df <- as.data.frame(df)
df$Attribute <- factor(df$Attribute, levels = c("Time", "Landscape","Nature use","Biodiversity","Biome peri-urban","Biome rural","ASC"))
df$Variable <- factor(df$Variable, levels = c("Main estimate","Gender","Age","INS","Commuting","Informative framing"))

ggplot(df[which(!(df$Attribute %in% c("Biome peri-urban","Biome rural"))),], aes(x=box_estimate_interaction,y = Variable, group=Attribute)) + 
  geom_vline(data=df[which(!(df$Attribute %in% c("Biome peri-urban","Biome rural")) & df$Variable=="Main estimate"),], aes(xintercept = box_estimate_main), linewidth = .25, linetype = "dotted") + 
  geom_vline(xintercept = 0, linewidth = .5, linetype="dashed") + 
  geom_errorbarh(aes(xmax = CIinteractionHigh, xmin = CIinteractionLow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_errorbarh(data=df[which(!(df$Attribute %in% c("Biome peri-urban","Biome rural")) & df$Variable=="Main estimate"),],aes(xmax = boxCIHigh, xmin = boxCILow), linewidth = .5, height = 
                   .2, color = "gray50") +
  geom_point(size = 3.5, aes(color = Attribute, alpha=signif)) + 
  geom_point(data=df[which(!(df$Attribute %in% c("Biome peri-urban","Biome rural")) & df$Variable=="Main estimate"),],size = 3.5, aes(color = Attribute,x=box_estimate_main,alpha=signif)) +
  theme_modern() + theme(legend.position = "none") + scale_alpha_discrete(range = c(0.4, 1)) +
  ylab("") +
  xlab("Estimate") + facet_grid(. ~ Attribute, scales='free')

ggsave("output/main_result.png",
       width = 15,
       height = 7,
       dpi = 400)


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
  xlab(expression(beta[n]["Travel time increase"])) #+ # Label the x axis
  #ggtitle("Unconditional distribution for Temps parameter")

ggsave("output/distrib_temp.png",
       width = 5,
       height = 5,
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
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n][Landscape]))

ggsave("output/distrib_paysage.png",
       width = 5,
       height = 5,
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
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n]["Nature use"]))

ggsave("output/distrib_acces.png",
       width = 5,
       height = 5,
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
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n][Biodiversity]))

ggsave("output/distrib_biodiversite.png",
       width = 5,
       height = 5,
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
  theme_modern() + ylab("f(x)") + xlab(expression(beta[n][ASC]))

ggsave("output/distrib_asc.png",
       width = 5,
       height = 5,
       dpi = 400)




# willingness to accept time
wtp_model <- wtp.gmnl(mixl_Temps, wrt = "Temps")

wtp_landscape <- wtp_model[1,1]+wtp_model[9,1]+wtp_model[10,1]
sd_wtp_landscape <- sqrt(wtp_model[1,2]^2+wtp_model[9,2]^2+wtp_model[10,2]^2)
wtp_natureuse <- wtp_model[2,1]+wtp_model[11,1]+wtp_model[12,1]
sd_wtp_natureuse <- sqrt(wtp_model[2,2]^2+wtp_model[11,2]^2+wtp_model[12,2]^2)
wtp_biodiversity <- wtp_model[3,1]+wtp_model[13,1]+wtp_model[14,1]+wtp_model[15,1]
sd_wtp_biodiversity <- sqrt(wtp_model[3,2]^2+wtp_model[13,2]^2+wtp_model[14,2]^2+wtp_model[15,2]^2)
wtp_biome1 <- wtp_model[4,1]
sd_wtp_biome1 <- wtp_model[4,2]
wtp_biome2 <- wtp_model[5,1]
sd_wtp_biome2 <- wtp_model[5,2]



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

