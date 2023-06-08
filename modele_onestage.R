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
  modelName       = "MMNL_onestage_extinction",
  modelDescr      = "Mixed logit model - WTP space - One Stage - Interactions avec extinction",
  indivID         = "ID",  
  mixing          = TRUE, # Needed to use continuous random heterogeneity
  nCores          = 4, #???
  analyticGrad    = TRUE, # Needs to be set explicitly for models with inter-intra draws (requires extra RAM).
  outputDirectory = "C:/Users/ChloeBeaudet/Nextcloud/these/Acceptabilité sociale/Resultats"
  #weights = "poids"
)



# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #
# manque 13 revenu
# Densite pop : manque 414 obs = 46 ind, ceux qui n'ont pas indiqué de sous-quartier, ou qui ont déclaré un sous-quartier inclassable
# pour eux : mettre la moyenne des quartiers à proximité / densité de montpellier 
# p our tot_pollum : pas mal de communes autour de montpellier où il n'y a rien
# manque pas mal de code_sous_quartier, code_quartier, "pop", densite_pop, pop_qpv_quartier/ pct_pop_zus (8091 manquant : c énorme), tot_m2

### Loading data from package
database = read.csv("C:/Users/ChloeBeaudet/Nextcloud/these/Acceptabilité sociale/Donnees/output/df_final_quartier_MMM_poids.csv",header=TRUE) %>%
  mutate(pct_pop_zus = ifelse(is.na(pct_pop_zus), 0, pct_pop_zus),
         is_zus = ifelse(pct_pop_zus > 0, 1, 0), 
         poids = nouveaux_poids_MMM/419.9616, 
         enfants = ifelse(enfants == 2, 0, 1)) 

# poids : doivent sommer au nombre d'observations dans l'échantillon, donc
# je divise par 419.9616 = 481276 (nombre de personnes dans la MMM, pris pour le redressement)/1146 (nb de personnes dans échantillon)


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  mu_asc_1 = 0,
  sigma_asc_1 = 0,
  mu_asc_2 = 0,
  sigma_asc_2 = 0,
  asc_3 = 0,
  b_extinction0             =  0,
  mu_log_b_extinction1      =  -10,
  sigma_log_b_extinction1   =  -35,
  mu_log_b_extinction2      =  -1,
  sigma_log_b_extinction2   =  40,
  mu_log_b_taxe             =  -2,
  sigma_log_b_taxe          =  6,
  b_couleur0                 = 0,
  mu_log_b_couleur1         =  -13,
  sigma_log_b_couleur1      =  6,
  mu_log_b_intensite        =  -0.12,
  sigma_log_b_intensite     =  1,
  b_extinction1_genre = 0,
  b_extinction1_age = 0,
  b_extinction1_densite_pop = 0,
  b_extinction1_enfants = 0,
  # b_extinction1_revenu = 0,
  b_extinction1_extinction_commune = 0,
  # b_extinction1_temps_intro = 0,
  # b_extinction1_velo = 0,
  b_extinction1_zus = 0,
  # b_extinction1_pollum = 0,
  b_extinction2_genre = 0,
  b_extinction2_age = 0,
  b_extinction2_densite_pop = 0,
  b_extinction2_enfants = 0,
  # b_extinction2_revenu = 0,
  b_extinction2_extinction_commune = 0,
  # b_extinction2_temps_intro = 0,
  # b_extinction2_velo = 0
  b_extinction2_zus = 0
  # b_extinction2_pollum = 0
  # b_asc_genre = 0,
  # b_asc_age = 0,
  # b_asc_densite_pop = 0,
  # b_asc_enfants = 0,
  # b_asc_revenu = 0,
  # b_asc_extinction_commune = 0,
  # b_asc_temps_intro = 0,
  # b_asc_velo = 0
  # b_asc_zus = 0
  # b_asc_pollum = 0
  # b_couleur_genre = 0,
  # b_couleur_age = 0,
  # b_couleur_densite_pop = 0,
  # b_couleur_enfants = 0,
  # b_couleur_revenu = 0,
  # b_couleur_extinction_commune = 0,
  # b_couleur_temps_intro = 0,
  # b_couleur_velo = 0,
  # b_couleur_zus = 0,
  # b_couleur_pollum = 0,
  # b_couleur_pollum_bleu = 0
  # b_intensite_genre = 0,
  # b_intensite_age = 0,
  # b_intensite_densite_pop = 0,
  # b_intensite_enfants = 0,
  # b_intensite_revenu = 0,
  # b_intensite_extinction_commune = 0,
  # b_intensite_temps_intro = 0,
  # b_intensite_velo = 0
  # b_intensite_zus = 0,
  # b_intensite_pollum = 0
)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
#apollo_fixed = c("b_extinction0", "b_couleur0")
apollo_fixed = c("asc_3", "b_couleur0", "b_extinction0")
# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobolOwen",
  interNDraws    = 5,
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
  V[["alt1"]] =  b_taxe * ( b_asc1 + taxe_alternative_1 + 
                              b_extinction0 * (extinction_alternative_1 == 0) + 
                              b_extinction1 * (extinction_alternative_1 == 1) + 
                              b_extinction2 * (extinction_alternative_1 == 2) +
                              b_couleur0 *(couleur_alternative_1 == 0) + 
                              b_couleur1 *(couleur_alternative_1 == 1)  + 
                              b_intensite * intensite_alternative_1 +
                              # 
                              b_extinction1_genre *  (extinction_alternative_1 == 1 & genre == 2) +
                              b_extinction1_age *  (extinction_alternative_1 == 1) * age +
                              #  b_extinction1_revenu *  (extinction_alternative_1 == 1) * revenu +
                              b_extinction1_densite_pop *  (extinction_alternative_1 == 1) * densite_pop +
                              b_extinction1_enfants *  (extinction_alternative_1 == 1) * (enfants == 1) +
                              b_extinction1_extinction_commune *  (extinction_alternative_1 == 1) * extinction_commune +
                              # b_extinction1_temps_intro *  (extinction_alternative_1 == 1) * temps_information +
                              # b_extinction1_velo *  (extinction_alternative_1 == 1) * PietonCycliste +
                              b_extinction1_zus *  (extinction_alternative_1 == 1) * pct_pop_zus +
                              # b_extinction1_pollum *  (extinction_alternative_1 == 1) * tot_m2 +
                              # 
                              # 
                              b_extinction2_genre *  (extinction_alternative_1 == 2 & genre == 2) +
                              b_extinction2_age *  (extinction_alternative_1 == 2) * age +
                              # b_extinction2_revenu *  (extinction_alternative_1 == 2) * revenu +
                              b_extinction2_densite_pop *  (extinction_alternative_1 == 2) * densite_pop +
                              b_extinction2_enfants *  (extinction_alternative_1 == 2) * (enfants == 1) +
                              b_extinction2_extinction_commune *  (extinction_alternative_1 == 2) * extinction_commune +
                              # b_extinction2_temps_intro *  (extinction_alternative_1 == 2) * temps_information +
                              # b_extinction2_velo *  (extinction_alternative_1 == 2) * PietonCycliste 
                              b_extinction2_zus *  (extinction_alternative_1 == 2) * pct_pop_zus
                            # b_extinction2_pollum *  (extinction_alternative_1 == 2) * tot_m2
                            
                            
                            # b_couleur_genre *  (couleur_alternative_1 == 1 & genre == 2) +
                            # b_couleur_age *  (couleur_alternative_1 == 1) * age +
                            # b_couleur_revenu *  (couleur_alternative_1 == 2) * revenu +
                            # b_couleur_densite_pop *  (couleur_alternative_1 == 1) * densite_pop +
                            # b_couleur_enfants *  (couleur_alternative_1 == 1) * enfants +
                            # b_couleur_extinction_commune *  (couleur_alternative_1 == 1) * extinction_commune +
                            # b_couleur_temps_intro *  (couleur_alternative_1 == 1) * temps_information +
                            # b_couleur_velo *  (couleur_alternative_1 == 1) * PietonCycliste +
                            # b_couleur_zus *  (couleur_alternative_1 == 1) * pct_zus +
                            # b_couleur_pollum *  (couleur_alternative_1 == 1) * tot_m2 +
                            #b_couleur_pollum_bleu *  (couleur_alternative_1 == 1) * bleu_m2 
                            
                            # b_intensite_genre *  intensite_alternative_1 * (genre == 2) +
                            # b_intensite_age *  intensite_alternative_1 * age +
                            # 
                            # b_intensite_revenu *  intensite_alternative_1 * revenu +
                            # b_intensite_densite_pop *  intensite_alternative_1 * densite_pop +
                            # b_intensite_enfants *  intensite_alternative_1 * enfants +
                            # b_intensite_extinction_commune *  intensite_alternative_1 * extinction_commune +
                            #b_intensite_temps_intro *  intensite_alternative_1 * temps_information +
                            # b_intensite_velo *  intensite_alternative_1 * PietonCycliste 
                            # b_intensite_zus *  intensite_alternative_1* pct_zus +
                            #b_intensite_pollum *  intensite_alternative_1 * tot_m2 
                            # 
                            
                            
  )
  
  V[["alt2"]] =  b_taxe * ( b_asc2 + taxe_alternative_2 + b_extinction0 * (extinction_alternative_2 == 0) + 
                              b_extinction1 * (extinction_alternative_2 == 1) + b_extinction2 * (extinction_alternative_2 == 2) +
                              b_couleur0 *(couleur_alternative_2 == 0) + b_couleur1 *(couleur_alternative_2 == 1)  + 
                              b_intensite * intensite_alternative_2 +
                              
                              b_extinction1_genre *  (extinction_alternative_2 == 1 & genre == 2) +
                              b_extinction1_age *  (extinction_alternative_2 == 1) * age +
                              # b_extinction1_revenu *  (extinction_alternative_2 == 1) * revenu +
                              b_extinction1_densite_pop *  (extinction_alternative_2 == 1) * densite_pop +
                              b_extinction1_enfants *  (extinction_alternative_2 == 1) * (enfants == 1) +
                              b_extinction1_extinction_commune *  (extinction_alternative_2 == 1) * extinction_commune +
                              b_extinction1_zus *  (extinction_alternative_2 == 1) * pct_pop_zus +
                              # b_extinction1_pollum *  (extinction_alternative_2 == 1) * tot_m2 +
                              
                              # b_extinction1_temps_intro *  (extinction_alternative_2 == 1) * temps_information +
                              # b_extinction1_velo *  (extinction_alternative_2 == 1) * PietonCycliste +
                              # 
                              b_extinction2_genre *  (extinction_alternative_2 == 2 & genre == 2) +
                              b_extinction2_age *  (extinction_alternative_2 == 2) * age +
                              # b_extinction2_revenu *  (extinction_alternative_2 == 2) * revenu +
                              b_extinction2_densite_pop *  (extinction_alternative_2 == 2) * densite_pop +
                              b_extinction2_enfants *  (extinction_alternative_2 == 2) * (enfants == 1) +
                              b_extinction2_extinction_commune *  (extinction_alternative_2 == 2) * extinction_commune +
                              # b_extinction2_temps_intro *  (extinction_alternative_2 == 2) * temps_information +
                              # b_extinction2_velo *  (extinction_alternative_2 == 2) * PietonCycliste 
                              b_extinction2_zus *  (extinction_alternative_2 == 2) * pct_pop_zus
                            # b_extinction2_pollum *  (extinction_alternative_2 == 2) * tot_m2
                            
                            # b_couleur_genre *  (couleur_alternative_2 == 1 & genre == 2) +
                            # b_couleur_age *  (couleur_alternative_2 == 1) * age +
                            # b_couleur_revenu *  (couleur_alternative_2 == 1) * revenu +
                            # b_couleur_densite_pop *  (couleur_alternative_2 == 1) * densite_pop +
                            # b_couleur_enfants *  (couleur_alternative_2 == 1) * enfants +
                            # b_couleur_extinction_commune *  (couleur_alternative_2 == 1) * extinction_commune +
                            # b_couleur_temps_intro *  (couleur_alternative_2 == 1) * temps_information +
                            # b_couleur_velo *  (couleur_alternative_2 == 1) * PietonCycliste +
                            # b_couleur_zus *  (couleur_alternative_2 == 1) * pct_zus +
                            # b_couleur_pollum *  (couleur_alternative_2 == 1) * tot_m2 +
                            # b_couleur_pollum_bleu *  (couleur_alternative_2 == 1) * bleu_m2 
                            
                            # b_intensite_genre *  intensite_alternative_2 * (genre == 2) +
                            # b_intensite_revenu *  intensite_alternative_2 * revenu +
                            # b_intensite_age *  intensite_alternative_2 * age +
                            # b_intensite_densite_pop *  intensite_alternative_2 * densite_pop +
                            # b_intensite_enfants *  intensite_alternative_2 * enfants +
                            # b_intensite_extinction_commune *  intensite_alternative_2 * extinction_commune +
                            # b_intensite_temps_intro *  intensite_alternative_2 * temps_information +
                            # b_intensite_velo *  intensite_alternative_2 * PietonCycliste 
                            # b_intensite_zus *  intensite_alternative_2 * pct_zus +
                            #b_intensite_pollum *  intensite_alternative_2 * tot_m2 
  )
  
  V[["alt3"]] =   b_taxe * ( taxe_alternative_3 + b_extinction0 * (extinction_alternative_3 == 0) + 
                              b_extinction1 * (extinction_alternative_3 == 1) + b_extinction2 * (extinction_alternative_3 == 2) +
                              b_couleur0 *(couleur_alternative_3 == 0) + b_couleur1 *(couleur_alternative_3 == 1)  + 
                              b_intensite * intensite_alternative_3 +
                              
                              b_extinction1_genre *  (extinction_alternative_3 == 1 & genre == 2) +
                              b_extinction1_age *  (extinction_alternative_3 == 1) * age +
                              # b_extinction1_revenu *  (extinction_alternative_3 == 1) * revenu +
                              b_extinction1_densite_pop *  (extinction_alternative_3 == 1) * densite_pop +
                              b_extinction1_enfants *  (extinction_alternative_3 == 1) * (enfants == 1) +
                              b_extinction1_extinction_commune *  (extinction_alternative_3 == 1) * extinction_commune +
                              # b_extinction1_temps_intro *  (extinction_alternative_3 == 1) * temps_information +
                              # b_extinction1_velo *  (extinction_alternative_3 == 1) * PietonCycliste +
                              b_extinction1_zus *  (extinction_alternative_3 == 1) * pct_pop_zus +
                              # b_extinction1_pollum *  (extinction_alternative_3 == 1) * tot_m2 +
                              # 
                              b_extinction2_genre *  (extinction_alternative_3 == 2 & genre == 2) +
                              b_extinction2_age *  (extinction_alternative_3 == 2) * age +
                              # b_extinction2_revenu *  (extinction_alternative_3 == 2) * revenu +
                              b_extinction2_densite_pop *  (extinction_alternative_3 == 2) * densite_pop +
                              b_extinction2_enfants *  (extinction_alternative_3 == 2) * (enfants == 1) +
                              b_extinction2_extinction_commune *  (extinction_alternative_3 == 2) * extinction_commune +
                              # b_extinction2_temps_intro *  (extinction_alternative_3 == 2) * temps_information +
                              # b_extinction2_velo *  (extinction_alternative_3 == 2) * PietonCycliste 
                              b_extinction2_zus *  (extinction_alternative_3 == 2) * pct_pop_zus
                            # b_extinction2_pollum *  (extinction_alternative_3 == 2) * tot_m2 
                            
                            # b_couleur_genre *  (couleur_alternative_3 == 1 & genre == 2) +
                            # b_couleur_age *  (couleur_alternative_3 == 1) * age +
                            # b_couleur_revenu *  (couleur_alternative_3 == 1) * revenu +
                            # b_couleur_densite_pop *  (couleur_alternative_3 == 1) * densite_pop +
                            # b_couleur_enfants *  (couleur_alternative_3 == 1) * enfants +
                            # b_couleur_extinction_commune *  (couleur_alternative_3 == 1) * extinction_commune +
                            # b_couleur_temps_intro *  (couleur_alternative_3 == 1) * temps_information +
                            # b_couleur_velo *  (couleur_alternative_3 == 1) * PietonCycliste +
                            # b_couleur_zus *  (couleur_alternative_3 == 1) * pct_zus +
                            # b_couleur_pollum *  (couleur_alternative_3 == 1) * tot_m2 +
                            # b_couleur_pollum_bleu *  (couleur_alternative_3 == 1) * bleu_m2 
                            
                            # b_intensite_genre *  intensite_alternative_3 * (genre == 2) +
                            # b_intensite_age *  intensite_alternative_3 * age +
                            # b_intensite_revenu *  intensite_alternative_3 * revenu +
                            # b_intensite_densite_pop *  intensite_alternative_3 * densite_pop +
                            # b_intensite_enfants *  intensite_alternative_3 * enfants +
                            # b_intensite_extinction_commune *  intensite_alternative_3 * extinction_commune +
                            # b_intensite_temps_intro *  intensite_alternative_3 * temps_information +
                            # b_intensite_velo *  intensite_alternative_3 * PietonCycliste +
                            # b_intensite_zus *  intensite_alternative_3 * pct_zus +
                            # b_intensite_pollum * intensite_alternative_3 * tot_m2 
                            # 
                            #b_asc_genre *  (genre == 2) +
                            #b_asc_age  * age +
                            # b_asc_revenu * revenu +
                            #b_asc_densite_pop * densite_pop +
                            #b_asc_enfants * enfants +
                            #b_asc_extinction_commune * extinction_commune +
                            # b_asc_temps_intro  * temps_information +
                            # b_asc_velo * PietonCycliste 
                            # b_asc_pollum * tot_m2 
                            #b_asc_zus * pct_pop_zus
  )
  
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
  #P = apollo_weighting(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

estimate_settings = list(maxIterations = 500)

model = apollo_estimate(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs, estimate_settings)

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

#apollo_saveOutput(model, modelOutput_settings)
