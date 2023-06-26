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





# both survey merged

data_DCE_mlogit <- mlogit.data(data_DCE_numeric,
                               choice = "choice",
                               shape = "long",
                               alt.var = "Scenario",
                               id.var = "survey_person",
                               chid.var = "chid")

# Temps

mixl_Temps <- gmnl(choice ~ Temps + Paysage + Acces + Biodiversite + Biome | 1,
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

