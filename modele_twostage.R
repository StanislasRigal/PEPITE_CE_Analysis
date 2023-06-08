#################################################################
#                                                               #
#                                                               #
#                 Test modèle Apollo                            #
#                                                               #
#                                                               #
#################################################################

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(xlsx)
library(tidyverse)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "MMNL_wtp_space_MMM_twostage",
  modelDescr      = "Mixed logit model test - Two stage",
  indivID         = "ID",  
  mixing          = TRUE, # Needed to use continuous random heterogeneity
  nCores          = 4, #???
  analyticGrad    = TRUE, # Needs to be set explicitly for models with inter-intra draws (requires extra RAM).
  outputDirectory = "C:/Users/ChloeBeaudet/Documents/Thèse/Acceptabilité sociale/Resultats",
  weights = "poids"
)


# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

### Loading data from package
database = read.csv("C:/Users/ChloeBeaudet/Nextcloud/these/Acceptabilité sociale/Donnees/output/df_final_quartier_MMM_poids.csv",header=TRUE) %>%
  mutate(pct_pop_zus = ifelse(is.na(pct_pop_zus), 0, pct_pop_zus),
         is_zus = ifelse(pct_pop_zus > 0, 1, 0), 
         poids = nouveaux_poids_MMM/419.9616, 
         enfants = ifelse(enfants == 2, 0, 1)) 

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(mu_asc_1 = 0,
                sigma_asc_1 = 0,
                mu_asc_2 = 0,
                sigma_asc_2 = 0,
                asc_3 = 0,
                b_extinction0             =  0,
                mu_log_b_extinction1      =  1,
                sigma_log_b_extinction1   =  2,
                mu_log_b_extinction2      =  0,
                sigma_log_b_extinction2   =  -3,
                mu_log_b_taxe             =  -2,
                sigma_log_b_taxe          =  -1,
                b_couleur0                 = 0,
                mu_log_b_couleur1         =  0,
                sigma_log_b_couleur1      =  1,
                mu_log_b_intensite        =  0,
                sigma_log_b_intensite     =  0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("asc_3", "b_extinction0", "b_couleur0")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobolOwen",
  interNDraws    = 500,
  interUnifDraws = c(),
  interNormDraws = c("draws_extinction1","draws_extinction2","draws_couleur1", "draws_intensite", "draws_taxe", "draw_asc1", "draw_asc2"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["b_asc2"]] =  mu_asc_2 + sigma_asc_2 * draw_asc2 
  randcoeff[["b_asc1"]] =  mu_asc_1 + sigma_asc_1 * draw_asc1 
  randcoeff[["b_taxe"]] = -exp( mu_log_b_taxe + sigma_log_b_taxe * draws_taxe )
  randcoeff[["b_extinction1"]] =  mu_log_b_extinction1 + sigma_log_b_extinction1 * draws_extinction1 
  randcoeff[["b_extinction2"]] = mu_log_b_extinction2 + sigma_log_b_extinction2 * draws_extinction2
  randcoeff[["b_couleur1"]] =  mu_log_b_couleur1 + sigma_log_b_couleur1 * draws_couleur1 
  randcoeff[["b_intensite"]] = mu_log_b_intensite + sigma_log_b_intensite * draws_intensite 
  
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["alt1"]] =  b_taxe * (b_asc1 + b_extinction0 * (extinction_alternative_1 == 0) + 
                                     b_extinction1 * (extinction_alternative_1 == 1) + b_extinction2 * (extinction_alternative_1 == 2) +
                                     b_couleur0 *(couleur_alternative_1 == 0) + b_couleur1 *(couleur_alternative_1 == 1)  + 
                                     b_intensite * intensite_alternative_1 + taxe_alternative_1)
  
  V[["alt2"]] =  b_taxe * (b_asc2 + b_extinction0 * (extinction_alternative_2 == 0) + 
                                     b_extinction1 * (extinction_alternative_2 == 1) + b_extinction2 * (extinction_alternative_2 == 2) +
                                     b_couleur0 *(couleur_alternative_2 == 0) + b_couleur1 *(couleur_alternative_2 == 1)  +
                                     b_intensite * intensite_alternative_2 + taxe_alternative_2)
  
  V[["alt3"]] =  b_taxe * (asc_3 + b_extinction0 * (extinction_alternative_3 == 0) + 
                                     b_extinction1 * (extinction_alternative_3 == 1) + b_extinction2 * (extinction_alternative_3 == 2) +
                                     b_couleur0 *(couleur_alternative_3 == 0) + b_couleur1 *(couleur_alternative_3 == 1)  + 
                                     b_intensite * intensite_alternative_3 + taxe_alternative_3 )
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3 = 3),
    avail         = list(alt1=1, alt2=1, alt3 = 1),
    choiceVar     = choix,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  # weight
  P = apollo_weighting(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #
modelOutput_settings <- list(printPVal = 2)
apollo_modelOutput(model, modelOutput_settings)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)
