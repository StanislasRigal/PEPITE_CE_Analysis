# NEP analysis from https://doi.org/10.1080/13504622.2014.913127

survey_result <- read.csv2("raw_data/Resultats_preference_AM3.csv")

NEP_result <- survey_result[,paste0("NEP",1:15)]

# recode uneven sentence 

NEP_result[,seq(from = 2, to=14, by=2)] <- 6 - NEP_result[,seq(from = 2, to=14, by=2)]

# prepare data for confirmatory factor analysis

NEP_result_long <- melt(data.frame(id=1:nrow(NEP_result), NEP_result), id.vars = "id")

names(NEP_result_long) <- c("id","NEP","value")

NEP_result_long$NEP_dimension <- "limits"
NEP_result_long$NEP_dimension[which(NEP_result_long$NEP %in% c("NEP2","NEP7","NEP12"))] <- "anti-anthropocentrism"
NEP_result_long$NEP_dimension[which(NEP_result_long$NEP %in% c("NEP3","NEP8","NEP13"))] <- "balance"
NEP_result_long$NEP_dimension[which(NEP_result_long$NEP %in% c("NEP4","NEP9","NEP14"))] <- "anti-exemptionalism"
NEP_result_long$NEP_dimension[which(NEP_result_long$NEP %in% c("NEP5","NEP10","NEP15"))] <- "crisis"

NEP.model <- '
limits =~ NEP1 + NEP6 + NEP11
antianthropocentrism =~ NEP2 + NEP7 + NEP12
balance =~ NEP3 + NEP8 + NEP13
antiexemptionalism =~ NEP4 + NEP9 + NEP14
crisis =~ NEP5 + NEP10 + NEP15'

NEP.fit <- cfa(NEP.model, data=NEP_result[-23,])
summary(NEP.fit, standardized=TRUE)
summary(NEP.fit, fit.measures = TRUE)

# NEP individual

NEP_result_ind <- NEP_result

NEP_result_ind$NEP_scale <- apply(NEP_result, 1, sum)/15

NEP_result_ind$NEP_scale2 <- apply(NEP_result[,c(2,4,5,11,14,15)], 1, sum)/6 # with estimate of factor loadings > 0.37

NEP_result_ind$id <- 1:nrow(NEP_result_ind)

# NEP aggregated

NEP_aggregated <- as.matrix(round(table(NEP_result_long[,c("NEP","value")])/26, 2))
NEP_aggregated <- cbind(NEP_aggregated,round(apply(NEP_result, 2, mean),2),round(apply(NEP_result, 2, sd),2),
                        round(apply(NEP_result, 2, function(x){skewness(as.numeric(x))}),2))

colnames(NEP_aggregated) <- c("1","2","3","4","5","Mean","SD","Skewness")

write.csv(NEP_aggregated,"output/NEP_aggregated.csv", row.names = TRUE)

# NEP correlation

round(cor(NEP_result),2)

semPlot::semPaths(NEP.fit, "std")



# preference change

personal_pref <- survey_result[,c("Walk_personal_amont","Nautical_activity_personal_amont","Tourism_personal_amont",
                                  "Freight_personal_amont","Hydraulic_dynamic_personal_amont","Biodiversity_personal_amont",
                                  "Walk_personal_aval","Nautical_activity_personal_aval","Tourism_personal_aval",
                                  "Freight_personal_aval","Hydraulic_dynamic_personal_aval","Biodiversity_personal_aval")]

personal_pref[is.na(personal_pref)] <- 0
personal_pref[personal_pref==-2] <- "Not important at all"
personal_pref[personal_pref==-1] <- "Not important"
personal_pref[personal_pref==0] <- "Neutral"
personal_pref[personal_pref==1] <- "Important"
personal_pref[personal_pref==2] <- "Very important"
personal_pref$id <- 1:nrow(personal_pref)

personal_pref_long <- melt(personal_pref, id.vars = "id")
personal_pref_long$before_after <- ifelse(grepl('aval', personal_pref_long$variable, fixed=TRUE),1,0)
personal_pref_long$usage <- stringr::str_remove(personal_pref_long$variable,"_personal_aval")
personal_pref_long$usage <- stringr::str_remove(personal_pref_long$usage,"_personal_amont")
personal_pref_long$value2 <- personal_pref_long$value
personal_pref_long$value_binary <- ifelse(personal_pref_long$value=="Very important",1,0)
personal_pref_long$value <- factor(personal_pref_long$value, levels = c("Not important at all","Not important","Neutral",
                                                                  "Important","Very important"))
personal_pref_long$value2[which(personal_pref_long$value2=="Not important at all")] <- "Not important"
personal_pref_long$value2[which(personal_pref_long$value2=="Neutral")] <- "Not important"
personal_pref_long$value2 <- factor(personal_pref_long$value2, levels = c("Not important","Important","Very important"))
personal_pref_long$usage_fac <- factor(personal_pref_long$usage, levels = c("Walk","Nautical_activity","Biodiversity","Hydraulic_dynamic","Tourism","Freight"))

personal_pref_long <- merge(personal_pref_long,NEP_result_ind[,c("NEP_scale","NEP_scale2","id")], by="id", all.x=T)

# cumulative link model

personal_pref_clm <- ordinal::clm(value ~ before_after,
                                  data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_clm)

personal_pref_clm2 <- ordinal::clm(value2 ~ before_after,
                                   data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_clm2)



personal_pref_walk_clm <- ordinal::clm(value ~ before_after,
                                  data = personal_pref_long[which(personal_pref_long$usage=="Walk" &
                                                                    personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_walk_clm)  

personal_pref_nautical_activity_clm <- ordinal::clm(value ~ before_after,
                                       data = personal_pref_long[which(personal_pref_long$usage=="Nautical_activity" &
                                                                         personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_nautical_activity_clm)  

personal_pref_tourism_clm <- ordinal::clm(value ~ before_after,
                                       data = personal_pref_long[which(personal_pref_long$usage=="Tourism" &
                                                                         personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_tourism_clm)

personal_pref_freight_clm <- ordinal::clm(value ~ before_after,
                                          data = personal_pref_long[which(personal_pref_long$usage=="Freight" &
                                                                            personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_freight_clm)

personal_pref_hydraulic_dynamic_clm <- ordinal::clm(value ~ before_after,
                                          data = personal_pref_long[which(personal_pref_long$usage=="Hydraulic_dynamic" &
                                                                            personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_hydraulic_dynamic_clm)

personal_pref_biodiversity_clm <- ordinal::clm(value ~ before_after,
                                          data = personal_pref_long[which(personal_pref_long$usage=="Biodiversity" &
                                                                            personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_biodiversity_clm)




personal_pref_walk_clm2 <- ordinal::clm(value2 ~ before_after,
                                       data = personal_pref_long[which(personal_pref_long$usage=="Walk" &
                                                                         personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_walk_clm2)  

personal_pref_nautical_activity_clm2 <- ordinal::clm(value2 ~ before_after,
                                                    data = personal_pref_long[which(personal_pref_long$usage=="Nautical_activity" &
                                                                                      personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_nautical_activity_clm2)  

personal_pref_tourism_clm2 <- ordinal::clm(value2 ~ before_after,
                                          data = personal_pref_long[which(personal_pref_long$usage=="Tourism" &
                                                                            personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_tourism_clm2)

personal_pref_freight_clm2 <- ordinal::clm(value2 ~ before_after,
                                          data = personal_pref_long[which(personal_pref_long$usage=="Freight" &
                                                                            personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_freight_clm2)

personal_pref_hydraulic_dynamic_clm2 <- ordinal::clm(value2 ~ before_after,
                                                    data = personal_pref_long[which(personal_pref_long$usage=="Hydraulic_dynamic" &
                                                                                      personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_hydraulic_dynamic_clm2)

personal_pref_biodiversity_clm2 <- ordinal::clm(value2 ~ before_after,
                                               data = personal_pref_long[which(personal_pref_long$usage=="Biodiversity" &
                                                                                 personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_biodiversity_clm2)






# glm for binary

personal_pref_glm <- glm(value_binary ~ before_after,
                         data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),],
                         family = "binomial")
summary(personal_pref_glm)


personal_pref_walk_glm <- glm(value_binary ~ before_after,
                         data = personal_pref_long[which(personal_pref_long$usage=="Walk" & personal_pref_long$id %in% c(1:18)),],
                         family = "binomial")
summary(personal_pref_walk_glm)


personal_pref_nautical_activity_glm <- glm(value_binary ~ before_after + NEP_scale2, # seul avec in impact
                              data = personal_pref_long[which(personal_pref_long$usage=="Nautical_activity" & personal_pref_long$id %in% c(1:18)),],
                              family = "binomial")
summary(personal_pref_nautical_activity_glm)


personal_pref_tourism_glm <- glm(value_binary ~ before_after,
                              data = personal_pref_long[which(personal_pref_long$usage=="Tourism" & personal_pref_long$id %in% c(1:18)),],
                              family = "binomial")
summary(personal_pref_tourism_glm)


personal_pref_freight_glm <- glm(value_binary ~ before_after,
                              data = personal_pref_long[which(personal_pref_long$usage=="Freight" & personal_pref_long$id %in% c(1:18)),],
                              family = "binomial")
summary(personal_pref_freight_glm)


personal_pref_hydraulic_dynamic_glm <- glm(value_binary ~ before_after,
                              data = personal_pref_long[which(personal_pref_long$usage=="Hydraulic_dynamic" & personal_pref_long$id %in% c(1:18)),],
                              family = "binomial")
summary(personal_pref_hydraulic_dynamic_glm)


personal_pref_biodiversity_glm <- glm(value_binary ~ before_after,
                              data = personal_pref_long[which(personal_pref_long$usage=="Biodiversity" & personal_pref_long$id %in% c(1:18)),],
                              family = "binomial")
summary(personal_pref_biodiversity_glm)

# test exact fisher : ccl pas de lien significatif entre variable et avant après

fisher.test(table(personal_pref_long$value_binary[which(personal_pref_long$usage=="Biodiversity" & personal_pref_long$id %in% c(1:18))],
                  personal_pref_long$before_after[which(personal_pref_long$usage=="Biodiversity" & personal_pref_long$id %in% c(1:18))]))

fisher.test(table(personal_pref_long$value[which(personal_pref_long$usage=="Biodiversity" & personal_pref_long$id %in% c(1:18))],
                  personal_pref_long$before_after[which(personal_pref_long$usage=="Biodiversity" & personal_pref_long$id %in% c(1:18))]))


# Multivariate Ordinal Models

personal_pref_short <- dcast(personal_pref_long, id + before_after + NEP_scale2 ~ usage, value.var = "value")
personal_pref_short$Biodiversity <- factor(personal_pref_short$Biodiversity, levels = c("Not important at all","Not important","Neutral","Important","Very important"))
personal_pref_short$Freight <- factor(personal_pref_short$Freight, levels = c("Not important at all","Not important","Neutral","Important","Very important"))
personal_pref_short$Hydraulic_dynamic <- factor(personal_pref_short$Hydraulic_dynamic, levels = c("Not important at all","Not important","Neutral","Important","Very important"))
personal_pref_short$Nautical_activity <- factor(personal_pref_short$Nautical_activity, levels = c("Not important at all","Not important","Neutral","Important","Very important"))
personal_pref_short$Tourism <- factor(personal_pref_short$Tourism, levels = c("Not important at all","Not important","Neutral","Important","Very important"))
personal_pref_short$Walk <- factor(personal_pref_short$Walk, levels = c("Not important at all","Not important","Neutral","Important","Very important"))

mod_bivariate <- mvord(formula = MMO2(Biodiversity,Freight,Hydraulic_dynamic, Nautical_activity, Tourism,Walk) ~ 0 + before_after + NEP_scale2,
                       link = mvlogit(df = 8L),
                       data = personal_pref_short[which(personal_pref_short$id %in% c(1:18)),])
summary(mod_bivariate)

mod_bivariate <- mvord(formula = MMO(usage) ~ 0 + before_after + NEP_scale2,
                       link = mvlogit(df = 8L),
                       data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),])
summary(mod_bivariate)


# ranking des usages du canal

pref_usage <- clm(value ~ usage,
                  data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),])
summary(pref_usage)
pref_usage$convergence

pref_usage_pr_walk <- clm(value ~ usage_fac,
                  data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),])
summary(pref_usage_pr_walk)

pref_usage2 <- clm(value ~ usage + before_after + NEP_scale2,
                  data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),])
summary(pref_usage2)

ggplot()

