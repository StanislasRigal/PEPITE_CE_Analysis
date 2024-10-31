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

ggplot(NEP_result_ind, aes(x=NEP_scale2)) + 
  geom_histogram(binwidth=0.2, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  xlab("Participant's NEP") + ylab("Number") +
  theme_modern()


ggsave("output/NEPS.png",
       width = 8,
       height = 6,
       dpi = 400)

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

personal_pref_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                  data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_clm)

personal_pref_clm2 <- ordinal::clm(value2 ~ before_after,
                                   data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_clm2)



personal_pref_walk_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                  data = personal_pref_long[which(personal_pref_long$usage=="Walk" &
                                                                    personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_walk_clm)  

personal_pref_nautical_activity_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                       data = personal_pref_long[which(personal_pref_long$usage=="Nautical_activity" &
                                                                         personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_nautical_activity_clm)  

personal_pref_tourism_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                       data = personal_pref_long[which(personal_pref_long$usage=="Tourism" &
                                                                         personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_tourism_clm)

personal_pref_freight_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                          data = personal_pref_long[which(personal_pref_long$usage=="Freight" &
                                                                            personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_freight_clm)

personal_pref_hydraulic_dynamic_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                          data = personal_pref_long[which(personal_pref_long$usage=="Hydraulic_dynamic" &
                                                                            personal_pref_long$id %in% c(1:18)),])  
summary(personal_pref_hydraulic_dynamic_clm)

personal_pref_biodiversity_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
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

mod_bivariate <- mvord(formula = MMO2(Biodiversity, Nautical_activity, Tourism, Freight, Walk) ~ 0 + before_after + NEP_scale2,
                       link = mvlogit(df = 8L),
                       data = personal_pref_short[which(personal_pref_short$id %in% c(1:18)),])
summary(mod_bivariate)


# ranking des usages du canal

pref_usage <- clm(value ~ usage,
                  data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),])
summary(pref_usage)
pref_usage$convergence

pref_usage_pr_walk <- clm(value ~ usage_fac,
                  data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),])
summary(pref_usage_pr_walk)
pref_plot <- as.data.frame(summary(pref_usage_pr_walk)$coefficients[5:9,])
pref_plot$Usage <- gsub("usage_fac","",row.names(pref_plot))
pref_plot$Usage <- gsub("_"," ",pref_plot$Usage)
pref_plot$Usage <- factor(pref_plot$Usage, levels=c("Biodiversity","Hydraulic dynamic","Nautical activity","Tourism","Freight"))

pref_usage2 <- clm(value ~ usage + before_after + NEP_scale2,
                  data = personal_pref_long[which(personal_pref_long$id %in% c(1:18)),])
summary(pref_usage2)

ggplot(pref_plot, aes(color=Usage, x=Usage)) +
  geom_point(aes(y=Estimate), size=4) +
  geom_errorbar(aes(ymin=Estimate - 1.96*`Std. Error`, ymax=Estimate + 1.96*`Std. Error`),width = 0.2) +
  theme_modern() + scale_color_viridis_d()+
  geom_hline(yintercept = 0, linetype="dashed") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none")


# preference territory now

territory_pref <- survey_result[,c("Walk_now_amont","Nautical_activity_now_amont","Tourism_now_amont",
                                  "Freight_now_amont","Hydraulic_dynamic_now_amont","Biodiversity_now_amont",
                                  "Walk_now_aval","Nautical_activity_now_aval","Tourism_now_aval",
                                  "Freight_now_aval","Hydraulic_dynamic_now_aval","Biodiversity_now_aval")]

territory_pref[is.na(territory_pref)] <- 0
territory_pref[territory_pref==-2] <- "Not important at all"
territory_pref[territory_pref==-1] <- "Not important"
territory_pref[territory_pref==0] <- "Neutral"
territory_pref[territory_pref==1] <- "Important"
territory_pref[territory_pref==2] <- "Very important"
territory_pref$id <- 1:nrow(territory_pref)

territory_pref_long <- melt(territory_pref, id.vars = "id")
territory_pref_long$before_after <- ifelse(grepl('aval', territory_pref_long$variable, fixed=TRUE),1,0)
territory_pref_long$usage <- stringr::str_remove(territory_pref_long$variable,"_now_aval")
territory_pref_long$usage <- stringr::str_remove(territory_pref_long$usage,"_now_amont")
territory_pref_long$value2 <- territory_pref_long$value
territory_pref_long$value_binary <- ifelse(territory_pref_long$value=="Very important",1,0)
territory_pref_long$value <- factor(territory_pref_long$value, levels = c("Not important at all","Not important","Neutral",
                                                                        "Important","Very important"))
territory_pref_long$value2[which(territory_pref_long$value2=="Not important at all")] <- "Not important"
territory_pref_long$value2[which(territory_pref_long$value2=="Neutral")] <- "Not important"
territory_pref_long$value2 <- factor(territory_pref_long$value2, levels = c("Not important","Important","Very important"))
territory_pref_long$usage_fac <- factor(territory_pref_long$usage, levels = c("Walk","Nautical_activity","Biodiversity","Hydraulic_dynamic","Tourism","Freight"))

territory_pref_long <- merge(territory_pref_long,NEP_result_ind[,c("NEP_scale","NEP_scale2","id")], by="id", all.x=T)


territory_pref_usage_pr_walk <- clm(value ~ usage_fac,
                          data = territory_pref_long[which(territory_pref_long$id %in% c(1:18)),])
summary(territory_pref_usage_pr_walk)
territory_pref_plot <- as.data.frame(summary(territory_pref_usage_pr_walk)$coefficients[5:9,])
territory_pref_plot$Usage <- gsub("usage_fac","",row.names(territory_pref_plot))
territory_pref_plot$Usage <- gsub("_"," ",territory_pref_plot$Usage)
territory_pref_plot$Usage <- factor(territory_pref_plot$Usage, levels=c("Biodiversity","Hydraulic dynamic","Nautical activity","Tourism","Freight"))

pref_plot2 <- rbind(pref_plot,territory_pref_plot)
pref_plot2$Scale <- c(rep("Personal",5),rep("Territory",5))

ggplot(pref_plot2, aes(color=Scale, x=Usage)) +
  geom_point(aes(y=Estimate, group=Scale), size=4) +
  geom_errorbar(aes(ymin=Estimate - 1.96*`Std. Error`, ymax=Estimate + 1.96*`Std. Error`),width = 0.2) +
  theme_modern() + scale_color_viridis_d()+
  geom_hline(yintercept = 0, linetype="dashed") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none")

territory_pref_walk_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                       data = territory_pref_long[which(territory_pref_long$usage=="Walk" &
                                                                         territory_pref_long$id %in% c(1:18)),])  
summary(territory_pref_walk_clm)  

territory_pref_nautical_activity_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                                    data = territory_pref_long[which(territory_pref_long$usage=="Nautical_activity" &
                                                                                      territory_pref_long$id %in% c(1:18)),])  
summary(territory_pref_nautical_activity_clm)  

territory_pref_tourism_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                          data = territory_pref_long[which(territory_pref_long$usage=="Tourism" &
                                                                            territory_pref_long$id %in% c(1:18)),])  
summary(territory_pref_tourism_clm)

territory_pref_freight_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                          data = territory_pref_long[which(territory_pref_long$usage=="Freight" &
                                                                            territory_pref_long$id %in% c(1:18)),])  
summary(territory_pref_freight_clm)

territory_pref_hydraulic_dynamic_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                                    data = territory_pref_long[which(territory_pref_long$usage=="Hydraulic_dynamic" &
                                                                                      territory_pref_long$id %in% c(1:18)),])  
summary(territory_pref_hydraulic_dynamic_clm)

territory_pref_biodiversity_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                               data = territory_pref_long[which(territory_pref_long$usage=="Biodiversity" &
                                                                                 territory_pref_long$id %in% c(1:18)),])  
summary(territory_pref_biodiversity_clm)


fisher.test(table(territory_pref_long$value[which(territory_pref_long$usage=="Biodiversity" & territory_pref_long$id %in% c(1:18))],
                  territory_pref_long$before_after[which(territory_pref_long$usage=="Biodiversity" & territory_pref_long$id %in% c(1:18))]))



# preference territory future

territory_future_pref <- survey_result[,c("Walk_future_amont","Nautical_activity_future_amont","Tourism_future_amont",
                                   "Freight_future_amont","Hydraulic_dynamic_future_amont","Biodiversity_future_amont",
                                   "Walk_future_aval","Nautical_activity_future_aval","Tourism_future_aval",
                                   "Freight_future_aval","Hydraulic_dynamic_future_aval","Biodiversity_future_aval")]

territory_future_pref[is.na(territory_future_pref)] <- 0
territory_future_pref[territory_future_pref==-2] <- "Not important at all"
territory_future_pref[territory_future_pref==-1] <- "Not important"
territory_future_pref[territory_future_pref==0] <- "Neutral"
territory_future_pref[territory_future_pref==1] <- "Important"
territory_future_pref[territory_future_pref==2] <- "Very important"
territory_future_pref$id <- 1:nrow(territory_future_pref)

territory_future_pref_long <- melt(territory_future_pref, id.vars = "id")
territory_future_pref_long$before_after <- ifelse(grepl('aval', territory_future_pref_long$variable, fixed=TRUE),1,0)
territory_future_pref_long$usage <- stringr::str_remove(territory_future_pref_long$variable,"_future_aval")
territory_future_pref_long$usage <- stringr::str_remove(territory_future_pref_long$usage,"_future_amont")
territory_future_pref_long$value2 <- territory_future_pref_long$value
territory_future_pref_long$value_binary <- ifelse(territory_future_pref_long$value=="Very important",1,0)
territory_future_pref_long$value <- factor(territory_future_pref_long$value, levels = c("Not important at all","Not important","Neutral",
                                                                          "Important","Very important"))
territory_future_pref_long$value2[which(territory_future_pref_long$value2=="Not important at all")] <- "Not important"
territory_future_pref_long$value2[which(territory_future_pref_long$value2=="Neutral")] <- "Not important"
territory_future_pref_long$value2 <- factor(territory_future_pref_long$value2, levels = c("Not important","Important","Very important"))
territory_future_pref_long$usage_fac <- factor(territory_future_pref_long$usage, levels = c("Walk","Nautical_activity","Biodiversity","Hydraulic_dynamic","Tourism","Freight"))

territory_future_pref_long <- merge(territory_future_pref_long,NEP_result_ind[,c("NEP_scale","NEP_scale2","id")], by="id", all.x=T)


territory_future_pref_usage_pr_walk <- clm(value ~ usage_fac,
                                    data = territory_future_pref_long[which(territory_future_pref_long$id %in% c(1:18)),])
summary(territory_future_pref_usage_pr_walk)
territory_future_pref_plot <- as.data.frame(summary(territory_future_pref_usage_pr_walk)$coefficients[5:9,])
territory_future_pref_plot$Usage <- gsub("usage_fac","",row.names(territory_future_pref_plot))
territory_future_pref_plot$Usage <- gsub("_"," ",territory_future_pref_plot$Usage)
territory_future_pref_plot$Usage <- factor(territory_future_pref_plot$Usage, levels=c("Biodiversity","Hydraulic dynamic","Nautical activity","Tourism","Freight"))

pref_plot3 <- rbind(pref_plot,territory_pref_plot,territory_future_pref_plot)
pref_plot3$Scale <- c(rep("Personal",5),rep("Territory",5),rep("Territory future",5))

ggplot(pref_plot3, aes(color=Scale, x=Usage)) +
  geom_point(aes(y=Estimate, group=Scale), size=4) +
  geom_errorbar(aes(ymin=Estimate - 1.96*`Std. Error`, ymax=Estimate + 1.96*`Std. Error`),width = 0.2) +
  theme_modern() + scale_color_viridis_d()+
  geom_hline(yintercept = 0, linetype="dashed") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position = "none")


territory_future_pref_walk_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                        data = territory_future_pref_long[which(territory_future_pref_long$usage=="Walk" &
                                                                           territory_future_pref_long$id %in% c(1:18)),])  
summary(territory_future_pref_walk_clm)  

territory_future_pref_nautical_activity_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                                     data = territory_future_pref_long[which(territory_future_pref_long$usage=="Nautical_activity" &
                                                                                        territory_future_pref_long$id %in% c(1:18)),])  
summary(territory_future_pref_nautical_activity_clm)  

territory_future_pref_tourism_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                           data = territory_future_pref_long[which(territory_future_pref_long$usage=="Tourism" &
                                                                              territory_future_pref_long$id %in% c(1:18)),])  
summary(territory_future_pref_tourism_clm)

territory_future_pref_freight_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                           data = territory_future_pref_long[which(territory_future_pref_long$usage=="Freight" &
                                                                              territory_future_pref_long$id %in% c(1:18)),])  
summary(territory_future_pref_freight_clm)

territory_future_pref_hydraulic_dynamic_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                                     data = territory_future_pref_long[which(territory_future_pref_long$usage=="Hydraulic_dynamic" &
                                                                                        territory_future_pref_long$id %in% c(1:18)),])  
summary(territory_future_pref_hydraulic_dynamic_clm)

territory_future_pref_biodiversity_clm <- ordinal::clm(value ~ before_after + NEP_scale2,
                                                data = territory_future_pref_long[which(territory_future_pref_long$usage=="Biodiversity" &
                                                                                   territory_future_pref_long$id %in% c(1:18)),])  
summary(territory_future_pref_biodiversity_clm)


fisher.test(table(territory_future_pref_long$value[which(territory_future_pref_long$usage=="Biodiversity" & territory_future_pref_long$id %in% c(1:18))],
                  territory_future_pref_long$before_after[which(territory_future_pref_long$usage=="Biodiversity" & territory_future_pref_long$id %in% c(1:18))]))




all_pref_long <- rbind(personal_pref_long,territory_pref_long,territory_future_pref_long)
all_pref_long$Scale <-  c(rep("Personal",nrow(personal_pref_long)),
                          rep("Territory",nrow(territory_pref_long)),
                          rep("Territory future",nrow(territory_future_pref_long)))
all_pref_usage_pr_walk <- clm(value ~ usage_fac + Scale + before_after + NEP_scale2,
                              data = all_pref_long[which(all_pref_long$id %in% c(1:18)),])
all_pref_usage_pr_walk <- clm(value ~ usage_fac+usage_fac:before_after + Scale + NEP_scale2,
                              data = all_pref_long[which(all_pref_long$id %in% c(1:18)),])
summary(all_pref_usage_pr_walk)
all_pref_plot <- as.data.frame(summary(all_pref_usage_pr_walk)$coefficients[5:9,])
all_pref_plot$Usage <- gsub("usage_fac","",row.names(all_pref_plot))
all_pref_plot$Usage <- gsub("_"," ",all_pref_plot$Usage)
all_pref_plot$Usage <- factor(all_pref_plot$Usage, levels=c("Biodiversity","Hydraulic dynamic","Nautical activity","Tourism","Freight"))
all_pref_plot$Scale <- "after"

all_pref_plot$corrected_estimate <- all_pref_plot$Estimate
all_pref_plot$corrected_estimate[which(all_pref_plot$Usage=="Nautical activity")] <- all_pref_plot$Estimate[which(all_pref_plot$Usage=="Nautical activity")] + summary(all_pref_usage_pr_walk)$coef[14,1]
all_pref_plot$corrected_estimate[which(all_pref_plot$Usage=="Biodiversity")] <- all_pref_plot$Estimate[which(all_pref_plot$Usage=="Biodiversity")] + summary(all_pref_usage_pr_walk)$coef[15,1]
all_pref_plot$corrected_estimate[which(all_pref_plot$Usage=="Hydraulic dynamic")] <- all_pref_plot$Estimate[which(all_pref_plot$Usage=="Hydraulic dynamic")] + summary(all_pref_usage_pr_walk)$coef[16,1]
all_pref_plot$corrected_estimate[which(all_pref_plot$Usage=="Tourism")] <- all_pref_plot$Estimate[which(all_pref_plot$Usage=="Tourism")] + summary(all_pref_usage_pr_walk)$coef[17,1]
all_pref_plot$corrected_estimate[which(all_pref_plot$Usage=="Freight")] <- all_pref_plot$Estimate[which(all_pref_plot$Usage=="Freight")] + summary(all_pref_usage_pr_walk)$coef[18,1]

all_pref_plot$corrected_sd <- all_pref_plot$`Std. Error`
all_pref_plot$corrected_sd[which(all_pref_plot$Usage=="Nautical activity")] <- sqrt(all_pref_plot$`Std. Error`[which(all_pref_plot$Usage=="Nautical activity")]^2 + summary(all_pref_usage_pr_walk)$coef[14,2]^2)
all_pref_plot$corrected_sd[which(all_pref_plot$Usage=="Biodiversity")] <- sqrt(all_pref_plot$`Std. Error`[which(all_pref_plot$Usage=="Biodiversity")]^2 + summary(all_pref_usage_pr_walk)$coef[15,2]^2)
all_pref_plot$corrected_sd[which(all_pref_plot$Usage=="Hydraulic dynamic")] <- sqrt(all_pref_plot$`Std. Error`[which(all_pref_plot$Usage=="Hydraulic dynamic")]^2 + summary(all_pref_usage_pr_walk)$coef[16,2]^2)
all_pref_plot$corrected_sd[which(all_pref_plot$Usage=="Tourism")] <- sqrt(all_pref_plot$`Std. Error`[which(all_pref_plot$Usage=="Tourism")]^2 + summary(all_pref_usage_pr_walk)$coef[17,2]^2)
all_pref_plot$corrected_sd[which(all_pref_plot$Usage=="Freight")] <- sqrt(all_pref_plot$`Std. Error`[which(all_pref_plot$Usage=="Freight")]^2 + summary(all_pref_usage_pr_walk)$coef[18,2]^2)

all_pref_plot_before <- all_pref_plot
all_pref_plot_before$corrected_estimate <- all_pref_plot_before$Estimate
all_pref_plot_before$corrected_sd <- all_pref_plot_before$`Std. Error`
all_pref_plot_before$Scale <- "before"

pref_plot3$corrected_estimate <- pref_plot3$Estimate
pref_plot3$corrected_estimate[which(pref_plot3$Scale=="Territory")] <- pref_plot3$Estimate[which(pref_plot3$Scale=="Territory")] + summary(all_pref_usage_pr_walk)$coef[10,1]
pref_plot3$corrected_estimate[which(pref_plot3$Scale=="Territory future")] <- pref_plot3$Estimate[which(pref_plot3$Scale=="Territory future")] + summary(all_pref_usage_pr_walk)$coef[11,1]

pref_plot3$corrected_sd <- pref_plot3$`Std. Error`
pref_plot3$corrected_sd[which(pref_plot3$Scale=="Territory")] <- sqrt(pref_plot3$`Std. Error`[which(pref_plot3$Scale=="Territory")]^2 + summary(all_pref_usage_pr_walk)$coef[10,2]^2)
pref_plot3$corrected_sd[which(pref_plot3$Scale=="Territory future")] <- sqrt(pref_plot3$`Std. Error`[which(pref_plot3$Scale=="Territory future")]^2 + summary(all_pref_usage_pr_walk)$coef[11,2]^2)


ggplot(pref_plot3, aes(color=Scale, x=Usage)) +
  geom_point(aes(y=corrected_estimate, group=Scale), size=4, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=corrected_estimate - 1.96*`Std. Error`, ymax=corrected_estimate + 1.96*`Std. Error`),width = 0.2, position = position_dodge(width=0.5)) +
  theme_modern() + scale_color_viridis_d()+
  geom_hline(yintercept = 0, linetype="dashed") + ylab("Estimates") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title = element_blank())

pref_plot4 <- rbind(pref_plot3,all_pref_plot_before,all_pref_plot)
pref_plot4$Scale <- factor(pref_plot4$Scale,levels = c("Personal","Territory","Territory future","before","after"))

ggplot(droplevels(pref_plot4[which(pref_plot4$Scale %in% c("Personal","Territory","Territory future")),]),aes(color=Scale, x=Usage)) +
  geom_point(aes(y=corrected_estimate, group=Scale), size=4, position = position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=corrected_estimate - 1.96*`Std. Error`, ymax=corrected_estimate + 1.96*`Std. Error`),width = 0.2, position = position_dodge(width=0.5)) +
  theme_modern() + scale_color_viridis_d()+
  geom_hline(yintercept = 0, linetype="dashed") + ylab("Estimates") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title = element_blank(), )

ggsave("output/ranking1.png",
       width = 10,
       height = 6,
       dpi = 400)

ggplot(droplevels(pref_plot4[which(pref_plot4$Scale %in% c("before","after")),]),aes(shape=Scale, x=Usage)) +
  geom_point(aes(y=corrected_estimate, group=Scale), size=4, position = position_dodge(width=0.4)) +
  geom_errorbar(aes(ymin=corrected_estimate - 1.96*`Std. Error`, ymax=corrected_estimate + 1.96*`Std. Error`),width = 0.2, position = position_dodge(width=0.4)) +
  theme_modern() + scale_shape_manual(values = c("before"=0,"after"=2)) +
  geom_hline(yintercept = 0, linetype="dashed") + ylab("Estimates") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.title = element_blank())

ggsave("output/ranking2.png",
       width = 8,
       height = 6,
       dpi = 400)

#

choix_fiche <- read.csv2("raw_data/choix_fiche.csv")
choix_fiche$`Scenario 1` <- choix_fiche$somme.standard*choix_fiche$scenario1/sum(choix_fiche$somme.standard,na.rm = TRUE)
choix_fiche$`Scenario 2` <- choix_fiche$somme.standard*choix_fiche$scenario2/sum(choix_fiche$somme.standard,na.rm = TRUE)
choix_fiche$`Scenario 3` <- choix_fiche$somme.standard*choix_fiche$scenario3/sum(choix_fiche$somme.standard,na.rm = TRUE)
choix_fiche$`Scenario 4` <- choix_fiche$somme.standard*choix_fiche$scenario4/sum(choix_fiche$somme.standard,na.rm = TRUE)
choix_fiche$`Scenario 5` <- choix_fiche$somme.standard*choix_fiche$scenario5/sum(choix_fiche$somme.standard,na.rm = TRUE)

choix_fiche$`Scenario 1 (weighted)` <- choix_fiche$somme.pondérée*choix_fiche$scenario1/sum(choix_fiche$somme.pondérée,na.rm = TRUE)
choix_fiche$`Scenario 2 (weighted)` <- choix_fiche$somme.pondérée*choix_fiche$scenario2/sum(choix_fiche$somme.pondérée,na.rm = TRUE)
choix_fiche$`Scenario 3 (weighted)` <- choix_fiche$somme.pondérée*choix_fiche$scenario3/sum(choix_fiche$somme.pondérée,na.rm = TRUE)
choix_fiche$`Scenario 4 (weighted)` <- choix_fiche$somme.pondérée*choix_fiche$scenario4/sum(choix_fiche$somme.pondérée,na.rm = TRUE)
choix_fiche$`Scenario 5 (weighted)` <- choix_fiche$somme.pondérée*choix_fiche$scenario5/sum(choix_fiche$somme.pondérée,na.rm = TRUE)

apply(choix_fiche[-nrow(choix_fiche),c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5")],2,sum)
apply(choix_fiche[-nrow(choix_fiche),c("Scenario 1 (weighted)","Scenario 2 (weighted)","Scenario 3 (weighted)","Scenario 4 (weighted)","Scenario 5 (weighted)")],2,sum)

ggplot(melt(choix_fiche[-nrow(choix_fiche),c("Scenario 1","Scenario 2","Scenario 3","Scenario 4","Scenario 5")])) +
  geom_bar(aes(x=variable,y=value,fill=variable),stat = "identity") + coord_cartesian(ylim = c(2, 3.5)) +
  ylab("Score") +
  scale_fill_brewer() + theme_modern() + theme(legend.position = "none",
                                               axis.text.x = element_text(angle = 45, hjust = 1),
                                               axis.title.x = element_blank())

ggplot(melt(choix_fiche[-nrow(choix_fiche),c("Scenario 1 (weighted)","Scenario 2 (weighted)","Scenario 3 (weighted)","Scenario 4 (weighted)","Scenario 5 (weighted)")])) +
  geom_bar(aes(x=variable,y=value,fill=variable),stat = "identity") + coord_cartesian(ylim = c(2, 3.5)) +
  ylab("Score") +
  scale_fill_brewer() + theme_modern() + theme(legend.position = "none",
                                              axis.text.x = element_text(angle = 45, hjust = 1),
                                              axis.title.x = element_blank())

ggsave("output/scenario_choice.png",
       width = 8,
       height = 6,
       dpi = 400)
 