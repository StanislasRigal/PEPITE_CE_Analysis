# Start with a simple multinomial logit model

## Clear memory
rm(list = ls())

## 1) Initialize the current session

apollo_initialise()

## 2) Prepare the inputs apollo_control, database, apollo_beta, and apollo_fixed, for the function apollo_validateInputs()

### Set core controls
apollo_control <- list(
  modelName       = "MNL_preference_space",
  modelDescr      = "Multinomial model on tram choice data",
  indivID         = "id"
)

database <- readRDS("output/data_apollo.rds")

database <- database[order(database$id),c("id","Chosen_scenario","Paysage_25_1","Paysage_75_1","Acces_Non_1","Acces_Oui_1","Biodiversite_faible_1","Biodiversite_moyenne_1","Biodiversite_eleve_1","Biome_urbain_1","Biome_periurbain_1", "Biome_rural_1","Temps_1",
                                          "Paysage_25_2","Paysage_75_2","Acces_Non_2","Acces_Oui_2","Biodiversite_faible_2","Biodiversite_moyenne_2","Biodiversite_eleve_2" ,"Biome_urbain_2","Biome_periurbain_2","Biome_rural_2" ,"Temps_2",
                                          "Paysage_SQ","Acces_SQ","Biodiversite_SQ","Biome_SQ","Temps_SQ")]
#database$ASC_1 <- database$ASC_2 <- 1
#database$ASC_3 <- 0
database$Chosen_scenario <- as.numeric(as.factor(database$Chosen_scenario))

apollo_beta <- c(
  asc = 0,
  b_Paysage_75  = 0,
  b_Acces_Oui  = 0,
  b_Biodiversite_moyenne  = 0,
  b_Biodiversite_eleve  = 0,
  b_Biome_periurbain  = 0,
  b_Biome_rural  = 0,
  b_Temps = 0
)

apollo_fixed <- c()

apollo_inputs <- apollo_validateInputs()


## 3) define the function apollo_probabilities()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  V <- list()
  V[['alt1']] = asc + b_Paysage_75*Paysage_75_1 + b_Acces_Oui*Acces_Oui_1 + b_Biodiversite_moyenne*Biodiversite_moyenne_1 + 
    b_Biodiversite_eleve*Biodiversite_eleve_1 + b_Biome_periurbain*Biome_periurbain_1 + b_Biome_rural*Biome_rural_1 + b_Temps*Temps_1
  V[['alt2']] = asc + b_Paysage_75*Paysage_75_2 + b_Acces_Oui*Acces_Oui_2 + b_Biodiversite_moyenne*Biodiversite_moyenne_2 +
    b_Biodiversite_eleve*Biodiversite_eleve_2 + b_Biome_periurbain*Biome_periurbain_2 + b_Biome_rural*Biome_rural_2 + b_Temps*Temps_2
  V[['alt3']] = 0
  
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = Chosen_scenario,
    V            = V
  )
  
  P[['model']] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}


## Analysis of choice

model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


## Summary of results

apollo_modelOutput(model, list(printPVal = TRUE))


# Willingness to pay

for (i in c("b_Paysage_75","b_Acces_Oui","b_Biodiversite_moyenne","b_Biodiversite_eleve","b_Biome_periurbain","b_Biome_rural")) {
  deltaMethod_settings <- list(operation = "ratio", parName1 = i, parName2 = "b_Temps",
                               multPar1 = -1)
  apollo_deltaMethod(model, deltaMethod_settings)
}





# Continue with a mixed logit model


### Clear memory
rm(list = ls())


### Initialise code
apollo_initialise()

### Set core controls

apollo_control = list(
  modelName       = "MMNL_preference_space",
  modelDescr      = "Mixed logit model on tram choice data",
  indivID         = "id",  
  mixing          = TRUE,
  nCores          = 4,
  outputDirectory = "output"
)

database <- readRDS("output/data_apollo.rds")

database <- database[order(database$id),c("id","Chosen_scenario","Paysage_25_1","Paysage_75_1","Acces_Non_1","Acces_Oui_1","Biodiversite_faible_1","Biodiversite_moyenne_1","Biodiversite_eleve_1","Biome_urbain_1","Biome_periurbain_1", "Biome_rural_1","Temps_1",
                                          "Paysage_25_2","Paysage_75_2","Acces_Non_2","Acces_Oui_2","Biodiversite_faible_2","Biodiversite_moyenne_2","Biodiversite_eleve_2" ,"Biome_urbain_2","Biome_periurbain_2","Biome_rural_2" ,"Temps_2",
                                          "Paysage_SQ","Acces_SQ","Biodiversite_SQ","Biome_SQ","Temps_SQ")]
#database$ASC_1 <- database$ASC_2 <- 1
#database$ASC_3 <- 0
database$Chosen_scenario <- as.numeric(as.factor(database$Chosen_scenario))

apollo_beta <- c(
  asc_alt1 = 0,
  asc_alt2 = 0,
  asc_alt3 = 0,
  sigma_asc_alt1 = -0.5,
  sigma_asc_alt2 = -0.5,
  sigma_asc_alt3 = -0.5,
  mu_b_Temps = 0,
  sigma_b_Temps = -0.5,
  mu_b_Paysage_75 = 0,
  sigma_b_Paysage_75 = -0.5,
  mu_b_Acces_Oui = 0,
  sigma_b_Acces_Oui = -0.5,
  mu_b_Biodiversite_moyenne = 0,
  sigma_b_Biodiversite_moyenne = -0.5,
  mu_b_Biodiversite_eleve = 0,
  sigma_b_Biodiversite_eleve = -0.5,
  mu_b_Biome_periurbain = 0,
  sigma_b_Biome_periurbain = -0.5,
  mu_b_Biome_rural = 0,
  sigma_b_Biome_rural = -0.5
)

apollo_fixed <- c("asc_alt3")

# Set parameters for generating draws

apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 500,
  interUnifDraws = c(),
  interNormDraws = c("draw_alt1","draw_alt2","draw_alt3","draws_Temps","draw_Paysage_75","draw_Acces_Oui","draw_Biodiversite_moyenne","draw_Biodiversite_eleve","draw_Biome_periurbain","draw_Biome_rural"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

## Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["a_alt1"]] = asc_alt1 + sigma_asc_alt1 * draw_alt1
  randcoeff[["a_alt2"]] = asc_alt2 + sigma_asc_alt2 * draw_alt2
  randcoeff[["a_alt3"]] = asc_alt3 + sigma_asc_alt3 * draw_alt3
  randcoeff[["b_Temps"]] = mu_b_Temps + sigma_b_Temps * draws_Temps
  randcoeff[["b_Paysage_75"]] = mu_b_Paysage_75 + sigma_b_Paysage_75 * draw_Paysage_75
  randcoeff[["b_Acces_Oui"]] = mu_b_Acces_Oui + sigma_b_Acces_Oui * draw_Acces_Oui
  randcoeff[["b_Biodiversite_moyenne"]] = mu_b_Biodiversite_moyenne + sigma_b_Biodiversite_moyenne * draw_Biodiversite_moyenne
  randcoeff[["b_Biodiversite_eleve"]] = mu_b_Biodiversite_eleve + sigma_b_Biodiversite_eleve * draw_Biodiversite_eleve
  randcoeff[["b_Biome_periurbain"]] = mu_b_Biome_periurbain + sigma_b_Biome_periurbain * draw_Biome_periurbain
  randcoeff[["b_Biome_rural"]] = mu_b_Biome_rural + sigma_b_Biome_rural * draw_Biome_rural
  
  return(randcoeff)
}

apollo_inputs <- apollo_validateInputs()


## 3) define the function apollo_probabilities()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  V <- list()
  V[['alt1']] = a_alt1 + b_Paysage_75*Paysage_75_1 + b_Acces_Oui*Acces_Oui_1 + b_Biodiversite_moyenne*Biodiversite_moyenne_1 + 
    b_Biodiversite_eleve*Biodiversite_eleve_1 + b_Biome_periurbain*Biome_periurbain_1 + b_Biome_rural*Biome_rural_1 + b_Temps*Temps_1
  V[['alt2']] = a_alt2 + b_Paysage_75*Paysage_75_2 + b_Acces_Oui*Acces_Oui_2 + b_Biodiversite_moyenne*Biodiversite_moyenne_2 +
    b_Biodiversite_eleve*Biodiversite_eleve_2 + b_Biome_periurbain*Biome_periurbain_2 + b_Biome_rural*Biome_rural_2 + b_Temps*Temps_2
  V[['alt3']] = a_alt3
  
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = Chosen_scenario,
    V            = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}


## Model choice analysis

model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


## Summary of results

apollo_modelOutput(model, list(printPVal = TRUE))


# Willingness to pay

for (i in c("b_Paysage_75","b_Acces_Oui","b_Biodiversite_moyenne","b_Biodiversite_eleve","b_Biome_periurbain","b_Biome_rural")) {
  deltaMethod_settings <- list(operation = "ratio", parName1 = i, parName2 = "b_Temps",
                               multPar1 = -1)
  apollo_deltaMethod(model, deltaMethod_settings)
}









# Continue with a latent class model


### Clear memory
rm(list = ls())


### Initialise code
apollo_initialise()

### Set core controls

apollo_control = list(
  modelName       = "LC_no_covariates",
  modelDescr      = "Simple LC model on tram choice data, no covariates in class allocation model",
  indivID         = "id",  
  nCores          = 4,
  outputDirectory = "output"
)

database <- readRDS("output/data_apollo.rds")

database <- database[order(database$id),c("id","Chosen_scenario","Paysage_25_1","Paysage_75_1","Acces_Non_1","Acces_Oui_1","Biodiversite_faible_1","Biodiversite_moyenne_1","Biodiversite_eleve_1","Biome_urbain_1","Biome_periurbain_1", "Biome_rural_1","Temps_1",
                                          "Paysage_25_2","Paysage_75_2","Acces_Non_2","Acces_Oui_2","Biodiversite_faible_2","Biodiversite_moyenne_2","Biodiversite_eleve_2" ,"Biome_urbain_2","Biome_periurbain_2","Biome_rural_2" ,"Temps_2",
                                          "Paysage_SQ","Acces_SQ","Biodiversite_SQ","Biome_SQ","Temps_SQ")]
#database$ASC_1 <- database$ASC_2 <- 1
#database$ASC_3 <- 0
database$Chosen_scenario <- as.numeric(as.factor(database$Chosen_scenario))

apollo_beta <- c(
  asc_alt1 = 0,
  asc_alt2 = 0,
  asc_alt3 = 0,
  b_Temps = 0,
  b_Paysage_75 = 0,
  b_Acces_Oui = 0,
  b_Biodiversite_moyenne = 0,
  b_Biodiversite_eleve = 0,
  b_Biome_periurbain = 0,
  b_Biome_rural = 0,
  )

apollo_fixed <- c("asc_alt3")

# Set parameters for generating draws

apollo_draws = list(
  interDrawsType = "halton",
  interNDraws    = 500,
  interUnifDraws = c(),
  interNormDraws = c("draw_alt1","draw_alt2","draw_alt3","draws_Temps","draw_Paysage_75","draw_Acces_Oui","draw_Biodiversite_moyenne","draw_Biodiversite_eleve","draw_Biome_periurbain","draw_Biome_rural"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

## Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["a_alt1"]] = asc_alt1 + sigma_asc_alt1 * draw_alt1
  randcoeff[["a_alt2"]] = asc_alt2 + sigma_asc_alt2 * draw_alt2
  randcoeff[["a_alt3"]] = asc_alt3 + sigma_asc_alt3 * draw_alt3
  randcoeff[["b_Temps"]] = mu_b_Temps + sigma_b_Temps * draws_Temps
  randcoeff[["b_Paysage_75"]] = mu_b_Paysage_75 + sigma_b_Paysage_75 * draw_Paysage_75
  randcoeff[["b_Acces_Oui"]] = mu_b_Acces_Oui + sigma_b_Acces_Oui * draw_Acces_Oui
  randcoeff[["b_Biodiversite_moyenne"]] = mu_b_Biodiversite_moyenne + sigma_b_Biodiversite_moyenne * draw_Biodiversite_moyenne
  randcoeff[["b_Biodiversite_eleve"]] = mu_b_Biodiversite_eleve + sigma_b_Biodiversite_eleve * draw_Biodiversite_eleve
  randcoeff[["b_Biome_periurbain"]] = mu_b_Biome_periurbain + sigma_b_Biome_periurbain * draw_Biome_periurbain
  randcoeff[["b_Biome_rural"]] = mu_b_Biome_rural + sigma_b_Biome_rural * draw_Biome_rural
  
  return(randcoeff)
}

apollo_inputs <- apollo_validateInputs()


## 3) define the function apollo_probabilities()

apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate"){
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  P <- list()
  
  V <- list()
  V[['alt1']] = a_alt1 + b_Paysage_75*Paysage_75_1 + b_Acces_Oui*Acces_Oui_1 + b_Biodiversite_moyenne*Biodiversite_moyenne_1 + 
    b_Biodiversite_eleve*Biodiversite_eleve_1 + b_Biome_periurbain*Biome_periurbain_1 + b_Biome_rural*Biome_rural_1 + b_Temps*Temps_1
  V[['alt2']] = a_alt2 + b_Paysage_75*Paysage_75_2 + b_Acces_Oui*Acces_Oui_2 + b_Biodiversite_moyenne*Biodiversite_moyenne_2 +
    b_Biodiversite_eleve*Biodiversite_eleve_2 + b_Biome_periurbain*Biome_periurbain_2 + b_Biome_rural*Biome_rural_2 + b_Temps*Temps_2
  V[['alt3']] = a_alt3
  
  mnl_settings <- list(
    alternatives = c(alt1 = 1, alt2 = 2, alt3 = 3),
    avail        = list(alt1 = 1, alt2 = 1, alt3 = 1),
    choiceVar    = Chosen_scenario,
    V            = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  
  return(P)
}


## Model choice analysis

model <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)


## Summary of results

apollo_modelOutput(model, list(printPVal = TRUE))


# Willingness to pay

for (i in c("b_Paysage_75","b_Acces_Oui","b_Biodiversite_moyenne","b_Biodiversite_eleve","b_Biome_periurbain","b_Biome_rural")) {
  deltaMethod_settings <- list(operation = "ratio", parName1 = i, parName2 = "b_Temps",
                               multPar1 = -1)
  apollo_deltaMethod(model, deltaMethod_settings)
}











