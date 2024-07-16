library(sf)
library(mapview)
library(exactextractr)
library(terra)
library(raster)
library(ggplot2)
library(viridis)
library(ggthemes)
library(reshape2)
library(plyr)
library(dplyr)
library(see)

### tram en projet

tram_trace1 <- read_sf("raw_data/tram_trace.kml",c("AA_Projets"))
tram_trace2 <- read_sf("raw_data/tram_trace.kml",c("ANGERS_ligne_B"))
tram_trace3 <- read_sf("raw_data/tram_trace.kml",c("AUBAGNE_Val'Tram_sept_2025"))
tram_trace4 <- read_sf("raw_data/tram_trace.kml",c("BORDEAUX_A_ext_Merignac_dec_2021"))
tram_trace5 <- read_sf("raw_data/tram_trace.kml",c("MARSEILLE_T3_ext_nord_sud_fin_2025"))
tram_trace6 <- read_sf("raw_data/tram_trace.kml",c("MONTPELLIER_L5"))

tram_trace1 <- tram_trace1[grep("PARIS",tram_trace1$Name,invert = TRUE),]
tram_trace6 <- tram_trace6[grep("initial",tram_trace6$Name,invert = TRUE),]

tram_trace <- rbind(tram_trace1,tram_trace2,tram_trace3,
                    tram_trace4,tram_trace5,tram_trace6)
mapview(tram_trace)


nat <- rast(raster("raw_data/Donnees_cartographiques/Layer4_FINAL.tif"))
nat1 <- rast(raster("raw_data/Donnees_cartographiques/Layer1_FINAL.tif"))
nat2 <- rast(raster("raw_data/Donnees_cartographiques/Layer2_FINAL.tif"))
nat3 <- rast(raster("raw_data/Donnees_cartographiques/Layer3_FINAL.tif"))

tram_trace <- st_transform(tram_trace, crs(nat))
tram_trace_buff <- st_buffer(tram_trace,6.5)

nat_tram_mean <- exact_extract(nat,tram_trace_buff,"mean")
nat_tram_all <- exact_extract(nat,tram_trace_buff)
nat1_tram_mean <- exact_extract(nat1,tram_trace_buff,"mean")
nat1_tram_all <- exact_extract(nat1,tram_trace_buff)
nat2_tram_mean <- exact_extract(nat2,tram_trace_buff,"mean")
nat2_tram_all <- exact_extract(nat2,tram_trace_buff)
nat3_tram_mean <- exact_extract(nat3,tram_trace_buff,"mean")
nat3_tram_all <- exact_extract(nat3,tram_trace_buff)

nat_tram_all_plot <- nat1_tram_all_plot <- nat2_tram_all_plot <- nat3_tram_all_plot <- list()
for(i in 1:length(nat_tram_all)){
  nat_tram_all_plot[[i]] <- data.frame(nat_tram_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_tram_all_plot[[i]] <- data.frame(nat1_tram_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_tram_all_plot[[i]] <- data.frame(nat2_tram_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_tram_all_plot[[i]] <- data.frame(nat3_tram_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
}

tram_trace_buff_vect <- vect(tram_trace_buff)
nat_masked <- list()
for(i in 1:nrow(tram_trace_buff_vect)){
  nat_crop <- crop(nat,tram_trace_buff_vect[i,])
  nat_masked[i] <- mask(nat_crop, tram_trace_buff_vect[i,])
}

nat_masked_df <- list()
for(i in 1:length(nat_masked)){
  nat_masked_spdf <- as(raster(nat_masked[[i]]), "SpatialPixelsDataFrame")
  nat_masked_df_tempo <- as.data.frame(nat_masked_spdf)
  colnames(nat_masked_df_tempo) <- c("value", "x", "y")
  nat_masked_df[[i]] <- nat_masked_df_tempo
}


for(i in 1:length(nat_masked_df)){
  outfile1 <- paste("output/trace_tram_",i,".png",sep="")
  outfile2 <- paste("output/hist_tram_",i,".png",sep="")
  
  ggplot() +  
    geom_tile(data=nat_masked_df[[i]], aes(x=x, y=y, fill=value), alpha=0.8) + 
    scale_fill_viridis(limits = c(50, 500)) +
    coord_equal() +
    theme_map() +
    theme(legend.position = "none")
  
  ggsave(outfile1,
         width = 6,
         height = 4,
         dpi = 400)
  
  ggplot() +  
    geom_col(data=nat_tram_all_plot[[i]], aes(x=value, y= cover, fill=value), alpha=0.8, position = 'identity') + 
    scale_fill_viridis(limits = c(50, 500)) + labs(y= "Cover impacted", x="Naturalness") +
    xlim(c(50, 500)) +
    theme_modern() + theme(legend.position = "none")
  
  ggsave(outfile2,
         width = 6,
         height = 4,
         dpi = 400)
}

### tram existant

tram_extant1 <- read_sf("raw_data/tram_trace.kml",c("Angers"))
tram_extant2 <- read_sf("raw_data/tram_trace.kml",c("Aubagne"))
tram_extant3a <- read_sf("raw_data/tram_trace.kml",c("A_La Gardette_Floirac Dravemont/Le Haillan Rostand")) #Bordeaux
tram_extant3b <- read_sf("raw_data/tram_trace.kml",c("B_Pessac France Alouette/Berges de la Garonne"))
tram_extant3c <- read_sf("raw_data/tram_trace.kml",c("C_Parc des Expositions_Gare de Blanquefort/Villenave Pyrénées"))
tram_extant3d <- read_sf("raw_data/tram_trace.kml",c("D_Carle Vernet/Cantinolles"))
tram_extant4 <- read_sf("raw_data/tram_trace.kml",c("Brest_Tracés")) 
tram_extant5 <- read_sf("raw_data/tram_trace.kml",c("Caen_Tracés")) 
tram_extant6a <- read_sf("raw_data/tram_trace.kml",c("T1_IUT Feyssine/Debourg")) # Lyon
tram_extant6b <- read_sf("raw_data/tram_trace.kml",c("T2_Saint-Priest Bel Air/Perrache"))
tram_extant6c <- read_sf("raw_data/tram_trace.kml",c("T3_Meyzieu Panettes/Gare Part-Dieu Villette"))
tram_extant6d <- read_sf("raw_data/tram_trace.kml",c("T4_Hôpital Feyzin/La Doua Gaston Berger"))
tram_extant6e <- read_sf("raw_data/tram_trace.kml",c("Lyon_Tracés"))
tram_extant7a <- read_sf("raw_data/tram_trace.kml",c("T1_Noailles/Les Caillols")) # Marseille
tram_extant7b <- read_sf("raw_data/tram_trace.kml",c("T2"))
tram_extant7c <- read_sf("raw_data/tram_trace.kml",c("Marseille_Tracés"))
tram_extant8a <- read_sf("raw_data/tram_trace.kml",c("Ligne 1_Mosson/Odysseum")) # Montpellier
tram_extant8b <- read_sf("raw_data/tram_trace.kml",c("Ligne 2_St Jean de Védas Centre/Jacou"))
tram_extant8c <- read_sf("raw_data/tram_trace.kml",c("Ligne 3_Juvignac/Perol Etang de l'Or"))
tram_extant8d <- read_sf("raw_data/tram_trace.kml",c("Montpellier_Tracés"))
tram_extant9a <- read_sf("raw_data/tram_trace.kml",c("L1_François Miterrand/Beaujoire ou Ranzay")) # Nantes
tram_extant9b <- read_sf("raw_data/tram_trace.kml",c("L2_Gare de Pont Rousseau/Orvault Grand Val"))
tram_extant9c <- read_sf("raw_data/tram_trace.kml",c("L3_Neustrie/Marcel Paul"))
tram_extant10a <- read_sf("raw_data/tram_trace.kml",c("A_Parc des Sports/Graffenstaden")) # Strasbourg
tram_extant10b <- read_sf("raw_data/tram_trace.kml",c("B_ Lingolsheim Tiergaertel/Hoenheim Gare"))
tram_extant10c <- read_sf("raw_data/tram_trace.kml",c("C_Gare Centrale/Neuhof Rodolphe Reuss"))
tram_extant10d <- read_sf("raw_data/tram_trace.kml",c("D_Poteries/Kehl Rathaus"))
tram_extant10e <- read_sf("raw_data/tram_trace.kml",c("E_Campus d'Illkirch/Robertsau L'escale"))
tram_extant10f <- read_sf("raw_data/tram_trace.kml",c("F_Elsau/Place d'Islande"))
tram_extant11 <- read_sf("raw_data/tram_trace.kml",c("Tours_Tracés"))

tram_extant <- rbind(tram_extant1,tram_extant2,tram_extant3a,tram_extant3b,tram_extant3c,tram_extant3d,
                    tram_extant4,tram_extant5,tram_extant6a,tram_extant6b,tram_extant6c,tram_extant6d,tram_extant6e,
                    tram_extant7a,tram_extant7b,tram_extant7c,tram_extant8a,tram_extant8b,tram_extant8c,tram_extant8d,tram_extant9a,tram_extant9b,tram_extant9c,
                    tram_extant10a,tram_extant10b,tram_extant10c,tram_extant10d,tram_extant10e,tram_extant10f,tram_extant11)
mapview(tram_extant)
tram_extant <- tram_extant[c(1:106,108:114),] # 107 en Allemagne

ville_tram_extant <- c(rep("Angers",nrow(tram_extant1)),rep("Aubagne",nrow(tram_extant2)),
                       rep("Bordeaux",nrow(tram_extant3a)),rep("Bordeaux",nrow(tram_extant3b)),rep("Bordeaux",nrow(tram_extant3c)),rep("Bordeaux",nrow(tram_extant3d)),
                       rep("Brest",nrow(tram_extant4)),rep("Caen",nrow(tram_extant5)),
                       rep("Lyon",nrow(tram_extant6a)),rep("Lyon",nrow(tram_extant6b)),rep("Lyon",nrow(tram_extant6c)),rep("Lyon",nrow(tram_extant6d)),rep("Lyon",nrow(tram_extant6e)),
                       rep("Marseille",nrow(tram_extant7a)),rep("Marseille",nrow(tram_extant7b)),rep("Marseille",nrow(tram_extant7c)),
                       rep("Montpellier",nrow(tram_extant8a)),rep("Montpellier",nrow(tram_extant8b)),rep("Montpellier",nrow(tram_extant8c)),rep("Montpellier",nrow(tram_extant8d)),
                       rep("Nantes",nrow(tram_extant9a)),rep("Nantes",nrow(tram_extant9b)),rep("Nantes",nrow(tram_extant9c)),
                       rep("Strasbourg",nrow(tram_extant10a)),rep("Strasbourg",nrow(tram_extant10b)),rep("Strasbourg",nrow(tram_extant10c)),rep("Strasbourg",nrow(tram_extant10d)),rep("Strasbourg",(nrow(tram_extant10e)-1)),rep("Strasbourg",nrow(tram_extant10f)),
                       rep("Tours",nrow(tram_extant11)))

nat <- rast(raster("raw_data/Donnees_cartographiques/Layer4_FINAL.tif"))

tram_extant <- st_transform(tram_extant, crs(nat))
tram_extant_buff <- st_buffer(tram_extant,6.5)

nat_tram_extant_mean <- exact_extract(nat,tram_extant_buff,"mean")
nat_tram_extant_all <- exact_extract(nat,tram_extant_buff)
nat1_tram_extant_mean <- exact_extract(nat1,tram_extant_buff,"mean")
nat1_tram_extant_all <- exact_extract(nat1,tram_extant_buff)
nat2_tram_extant_mean <- exact_extract(nat2,tram_extant_buff,"mean")
nat2_tram_extant_all <- exact_extract(nat2,tram_extant_buff)
nat3_tram_extant_mean <- exact_extract(nat3,tram_extant_buff,"mean")
nat3_tram_extant_all <- exact_extract(nat3,tram_extant_buff)

nat_tram_extant_all_plot <- nat1_tram_extant_all_plot <- nat2_tram_extant_all_plot <- nat3_tram_extant_all_plot <- list()
for(i in 1:length(nat_tram_extant_all)){
  nat_tram_extant_all_plot[[i]] <- data.frame(nat_tram_extant_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_tram_extant_all_plot[[i]] <- data.frame(nat1_tram_extant_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_tram_extant_all_plot[[i]] <- data.frame(nat2_tram_extant_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_tram_extant_all_plot[[i]] <- data.frame(nat3_tram_extant_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
}

tram_extant_buff_vect <- vect(tram_extant_buff)
nat_masked_extant <- list()
for(i in 1:nrow(tram_extant_buff_vect)){
  nat_crop <- crop(nat,tram_extant_buff_vect[i,])
  nat_masked_extant[i] <- mask(nat_crop, tram_extant_buff_vect[i,])
}

nat_masked_extant_df <- list()
for(i in 1:length(nat_masked_extant)){
  nat_masked_extant_spdf <- as(raster(nat_masked_extant[[i]]), "SpatialPixelsDataFrame")
  nat_masked_extant_df_tempo <- as.data.frame(nat_masked_extant_spdf)
  colnames(nat_masked_extant_df_tempo) <- c("value", "x", "y")
  nat_masked_extant_df[[i]] <- nat_masked_extant_df_tempo
}


# naturalité tram existant vs projet

project_trace_13m <- do.call(rbind.data.frame, nat_tram_all_plot)
extant_trace_13m <- do.call(rbind.data.frame, nat_tram_extant_all_plot)
plot_trace_13m <- rbind(data.frame(project_trace_13m,variable="Tram projects"),data.frame(extant_trace_13m,variable="Tram extant"))

ggplot(plot_trace_13m, aes(x = variable, y = value, weight = cover, fill=variable)) + 
  geom_boxplot(width=0.6, col="#0219f3") + 
  scale_fill_manual(name="alpha", values=alpha(c("#0219f3","#8d98ff"),0.5)) + theme_modern() +
  labs(y="Naturalness") + theme(axis.title.x = element_blank(),
                                legend.position = "none")

project_trace_13m_nat1 <- do.call(rbind.data.frame, nat1_tram_all_plot)
extant_trace_13m_nat1 <- do.call(rbind.data.frame, nat1_tram_extant_all_plot)
plot_trace_13m_nat1 <- rbind(data.frame(project_trace_13m_nat1,variable="Tram projects"),data.frame(extant_trace_13m_nat1,variable="Tram extant"))

ggplot(plot_trace_13m_nat1, aes(x = variable, y = value, weight = cover, fill=variable)) + 
  geom_boxplot(width=0.6, col="#0219f3") + 
  scale_fill_manual(name="alpha", values=alpha(c("#0219f3","#8d98ff"),0.5)) + theme_modern() +
  labs(y="Naturalness") + theme(axis.title.x = element_blank(),
                                legend.position = "none")

project_trace_13m_nat2 <- do.call(rbind.data.frame, nat2_tram_all_plot)
extant_trace_13m_nat2 <- do.call(rbind.data.frame, nat2_tram_extant_all_plot)
plot_trace_13m_nat2 <- rbind(data.frame(project_trace_13m_nat2,variable="Tram projects"),data.frame(extant_trace_13m_nat2,variable="Tram extant"))

ggplot(plot_trace_13m_nat2, aes(x = variable, y = value, weight = cover, fill=variable)) + 
  geom_boxplot(width=0.6, col="#0219f3") + 
  scale_fill_manual(name="alpha", values=alpha(c("#0219f3","#8d98ff"),0.5)) + theme_modern() +
  labs(y="Naturalness") + theme(axis.title.x = element_blank(),
                                legend.position = "none")

project_trace_13m_nat3 <- do.call(rbind.data.frame, nat3_tram_all_plot)
extant_trace_13m_nat3 <- do.call(rbind.data.frame, nat3_tram_extant_all_plot)
plot_trace_13m_nat3 <- rbind(data.frame(project_trace_13m_nat3,variable="Tram projects"),data.frame(extant_trace_13m_nat3,variable="Tram extant"))

ggplot(plot_trace_13m_nat3, aes(x = variable, y = value, weight = cover, fill=variable)) + 
  geom_boxplot(width=0.6, col="#0219f3") + 
  scale_fill_manual(name="alpha", values=alpha(c("#0219f3","#8d98ff"),0.5)) + theme_modern() +
  labs(y="Naturalness") + theme(axis.title.x = element_blank(),
                                legend.position = "none")

ggsave("output/tram_nat_extant_project.png",
       width = 5,
       height = 5,
       dpi = 400)

summary(lm(value~variable,plot_trace_13m, weights = cover))
summary(lm(value~variable,plot_trace_13m_nat1, weights = cover))
summary(lm(value~variable,plot_trace_13m_nat2, weights = cover))
summary(lm(value~variable,plot_trace_13m_nat3, weights = cover))

# the same for other buffer values

dist_effect_tram <- dist_effect_tram_nat1 <- dist_effect_tram_nat2 <- dist_effect_tram_nat3 <- data.frame(dist=NA,effect=NA, pval=NA)

for(i in seq(from=1, to=50, by=2)){
  
  tram_trace_buff_compare <- st_buffer(tram_trace,i)
  nat_tram_all_compare <- exact_extract(nat,tram_trace_buff_compare)
  nat_tram_all_compare <- do.call(rbind.data.frame, nat_tram_all_compare)
  nat_tram_all_compare_plot <- data.frame(nat_tram_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_tram_all_compare <- exact_extract(nat1,tram_trace_buff_compare)
  nat1_tram_all_compare <- do.call(rbind.data.frame, nat1_tram_all_compare)
  nat1_tram_all_compare_plot <- data.frame(nat1_tram_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_tram_all_compare <- exact_extract(nat2,tram_trace_buff_compare)
  nat2_tram_all_compare <- do.call(rbind.data.frame, nat2_tram_all_compare)
  nat2_tram_all_compare_plot <- data.frame(nat2_tram_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_tram_all_compare <- exact_extract(nat3,tram_trace_buff_compare)
  nat3_tram_all_compare <- do.call(rbind.data.frame, nat3_tram_all_compare)
  nat3_tram_all_compare_plot <- data.frame(nat3_tram_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  tram_extant_buff_compare <- st_buffer(tram_extant,i)
  nat_tram_extant_all_compare <- exact_extract(nat,tram_extant_buff_compare)
  nat_tram_extant_all_compare <- do.call(rbind.data.frame, nat_tram_extant_all_compare)
  nat_tram_extant_all_compare_plot <- data.frame(nat_tram_extant_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_tram_extant_all_compare <- exact_extract(nat1,tram_extant_buff_compare)
  nat1_tram_extant_all_compare <- do.call(rbind.data.frame, nat1_tram_extant_all_compare)
  nat1_tram_extant_all_compare_plot <- data.frame(nat1_tram_extant_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_tram_extant_all_compare <- exact_extract(nat2,tram_extant_buff_compare)
  nat2_tram_extant_all_compare <- do.call(rbind.data.frame, nat2_tram_extant_all_compare)
  nat2_tram_extant_all_compare_plot <- data.frame(nat2_tram_extant_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_tram_extant_all_compare <- exact_extract(nat3,tram_extant_buff_compare)
  nat3_tram_extant_all_compare <- do.call(rbind.data.frame, nat3_tram_extant_all_compare)
  nat3_tram_extant_all_compare_plot <- data.frame(nat3_tram_extant_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  plot_trace_compare <- rbind(data.frame(nat_tram_all_compare_plot,variable="Tram projects"),data.frame(nat_tram_extant_all_compare_plot,variable="Tram extant"))
  plot_trace_compare_nat1 <- rbind(data.frame(nat1_tram_all_compare_plot,variable="Tram projects"),data.frame(nat1_tram_extant_all_compare_plot,variable="Tram extant"))
  plot_trace_compare_nat2 <- rbind(data.frame(nat2_tram_all_compare_plot,variable="Tram projects"),data.frame(nat2_tram_extant_all_compare_plot,variable="Tram extant"))
  plot_trace_compare_nat3 <- rbind(data.frame(nat3_tram_all_compare_plot,variable="Tram projects"),data.frame(nat3_tram_extant_all_compare_plot,variable="Tram extant"))
  
  results <- summary(lm(value~variable,plot_trace_compare, weights = cover))
  results_nat1 <- summary(lm(value~variable,plot_trace_compare_nat1, weights = cover))
  results_nat2 <- summary(lm(value~variable,plot_trace_compare_nat2, weights = cover))
  results_nat3 <- summary(lm(value~variable,plot_trace_compare_nat3, weights = cover))
  
  dist_effect_tram <- rbind(dist_effect_tram,data.frame(dist=i, effect=results$coef[2,1], pval=results$coef[2,4]))
  dist_effect_tram_nat1 <- rbind(dist_effect_tram_nat1,data.frame(dist=i, effect=results_nat1$coef[2,1], pval=results_nat1$coef[2,4]))
  dist_effect_tram_nat2 <- rbind(dist_effect_tram_nat2,data.frame(dist=i, effect=results_nat2$coef[2,1], pval=results_nat2$coef[2,4]))
  dist_effect_tram_nat3 <- rbind(dist_effect_tram_nat3,data.frame(dist=i, effect=results_nat3$coef[2,1], pval=results_nat3$coef[2,4]))
  
}



### example of tree effect on natrualness (Montpellier ligne 1, most frequented ligne in France)

ex_tram_arbre0 <- read_sf("raw_data/traces_TRAMWAY_2021_tree.kml",c("Montpellier_pas_arbre"))
ex_tram_arbre1 <- read_sf("raw_data/traces_TRAMWAY_2021_tree.kml",c("Montpellier_arbre_un"))
ex_tram_arbre2 <- read_sf("raw_data/traces_TRAMWAY_2021_tree.kml",c("Montpellier_arbre_deux"))

ex_tram_arbre0 <- st_transform(ex_tram_arbre0, crs(nat))
ex_tram_arbre0_buff <- st_buffer(ex_tram_arbre0,16) # see buffer size effet below
nat_ex_tram_arbre0 <- exact_extract(nat,ex_tram_arbre0_buff)
nat_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat_ex_tram_arbre0)
nat_ex_tram_arbre0_plot <- data.frame(nat_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat1_ex_tram_arbre0 <- exact_extract(nat1,ex_tram_arbre0_buff)
nat1_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat1_ex_tram_arbre0)
nat1_ex_tram_arbre0_plot <- data.frame(nat1_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat2_ex_tram_arbre0 <- exact_extract(nat2,ex_tram_arbre0_buff)
nat2_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat2_ex_tram_arbre0)
nat2_ex_tram_arbre0_plot <- data.frame(nat2_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat3_ex_tram_arbre0 <- exact_extract(nat3,ex_tram_arbre0_buff)
nat3_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat3_ex_tram_arbre0)
nat3_ex_tram_arbre0_plot <- data.frame(nat3_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))

ex_tram_arbre1 <- st_transform(ex_tram_arbre1, crs(nat))
ex_tram_arbre1_buff <- st_buffer(ex_tram_arbre1,16)
nat_ex_tram_arbre1 <- exact_extract(nat,ex_tram_arbre1_buff)
nat_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat_ex_tram_arbre1)
nat_ex_tram_arbre1_plot <- data.frame(nat_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat1_ex_tram_arbre1 <- exact_extract(nat1,ex_tram_arbre1_buff)
nat1_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat1_ex_tram_arbre1)
nat1_ex_tram_arbre1_plot <- data.frame(nat1_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat2_ex_tram_arbre1 <- exact_extract(nat2,ex_tram_arbre1_buff)
nat2_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat2_ex_tram_arbre1)
nat2_ex_tram_arbre1_plot <- data.frame(nat2_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat3_ex_tram_arbre1 <- exact_extract(nat3,ex_tram_arbre1_buff)
nat3_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat3_ex_tram_arbre1)
nat3_ex_tram_arbre1_plot <- data.frame(nat3_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))

ex_tram_arbre2 <- st_transform(ex_tram_arbre2, crs(nat))
ex_tram_arbre2_buff <- st_buffer(ex_tram_arbre2,16)
nat_ex_tram_arbre2 <- exact_extract(nat,ex_tram_arbre2_buff)
nat_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat_ex_tram_arbre2)
nat_ex_tram_arbre2_plot <- data.frame(nat_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat1_ex_tram_arbre2 <- exact_extract(nat1,ex_tram_arbre2_buff)
nat1_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat1_ex_tram_arbre2)
nat1_ex_tram_arbre2_plot <- data.frame(nat1_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat2_ex_tram_arbre2 <- exact_extract(nat2,ex_tram_arbre2_buff)
nat2_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat2_ex_tram_arbre2)
nat2_ex_tram_arbre2_plot <- data.frame(nat2_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
nat3_ex_tram_arbre2 <- exact_extract(nat3,ex_tram_arbre2_buff)
nat3_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat3_ex_tram_arbre2)
nat3_ex_tram_arbre2_plot <- data.frame(nat3_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))

plot_ex_tram_arbre <- rbind(data.frame(nat_ex_tram_arbre0_plot,variable="no_tree"),
                            data.frame(nat_ex_tram_arbre1_plot,variable="few_trees"),
                            data.frame(nat_ex_tram_arbre2_plot,variable="many_trees"))
plot_ex_tram_arbre$variable2 <- ifelse(plot_ex_tram_arbre$variable=="no_tree","no_tree","trees")

ggplot(plot_ex_tram_arbre, aes(x = variable2, y = value, weight = cover)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) + theme_modern() +
  labs(y="Naturalness") + theme(axis.title.x = element_blank())

summary(lm(value~variable2,plot_ex_tram_arbre, weights = cover))

plot_ex_tram_arbre_nat1 <- rbind(data.frame(nat1_ex_tram_arbre0_plot,variable="no_tree"),
                            data.frame(nat1_ex_tram_arbre1_plot,variable="few_trees"),
                            data.frame(nat1_ex_tram_arbre2_plot,variable="many_trees"))
plot_ex_tram_arbre_nat1$variable2 <- ifelse(plot_ex_tram_arbre_nat1$variable=="no_tree","no_tree","trees")

ggplot(plot_ex_tram_arbre_nat1, aes(x = variable2, y = value, weight = cover)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) + theme_modern() +
  labs(y="Integrity") + theme(axis.title.x = element_blank())

summary(lm(value~variable2,plot_ex_tram_arbre_nat1, weights = cover))

plot_ex_tram_arbre_nat2 <- rbind(data.frame(nat2_ex_tram_arbre0_plot,variable="no_tree"),
                                 data.frame(nat2_ex_tram_arbre1_plot,variable="few_trees"),
                                 data.frame(nat2_ex_tram_arbre2_plot,variable="many_trees"))
plot_ex_tram_arbre_nat2$variable2 <- ifelse(plot_ex_tram_arbre_nat2$variable=="no_tree","no_tree","trees")

ggplot(plot_ex_tram_arbre_nat2, aes(x = variable2, y = value, weight = cover)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) + theme_modern() +
  labs(y="Spontaneity") + theme(axis.title.x = element_blank())

summary(lm(value~variable2,plot_ex_tram_arbre_nat2, weights = cover))

plot_ex_tram_arbre_nat3 <- rbind(data.frame(nat3_ex_tram_arbre0_plot,variable="no_tree"),
                                 data.frame(nat3_ex_tram_arbre1_plot,variable="few_trees"),
                                 data.frame(nat3_ex_tram_arbre2_plot,variable="many_trees"))
plot_ex_tram_arbre_nat3$variable2 <- ifelse(plot_ex_tram_arbre_nat3$variable=="no_tree","no_tree","trees")

ggplot(plot_ex_tram_arbre_nat3, aes(x = variable2, y = value, weight = cover)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) + theme_modern() +
  labs(y="Continuity") + theme(axis.title.x = element_blank())

summary(lm(value~variable2,plot_ex_tram_arbre_nat3, weights = cover))






ex_tram_arbre0_buff_vect <- vect(ex_tram_arbre0_buff)
nat_crop0 <- crop(nat,ex_tram_arbre0_buff)
nat_masked_extant0 <- mask(nat_crop0, ex_tram_arbre0_buff_vect)
nat_masked_extant_spdf0 <- as(raster(nat_masked_extant0), "SpatialPixelsDataFrame")
nat_masked_extant_df0 <- as.data.frame(nat_masked_extant_spdf0)
colnames(nat_masked_extant_df0) <- c("value", "x", "y")

ggplot() +  
  geom_tile(data=nat_masked_extant_df0, aes(x=x, y=y, fill=value), alpha=0.8) + 
  scale_fill_viridis(limits = c(50, 500)) +
  coord_equal() +
  theme_map() +
  theme(legend.position = "none")

ex_tram_arbre1_buff_vect <- vect(ex_tram_arbre1_buff)
nat_crop1 <- crop(nat,ex_tram_arbre1_buff)
nat_masked_extant1 <- mask(nat_crop1, ex_tram_arbre1_buff_vect)
nat_masked_extant_spdf1 <- as(raster(nat_masked_extant1), "SpatialPixelsDataFrame")
nat_masked_extant_df1 <- as.data.frame(nat_masked_extant_spdf1)
colnames(nat_masked_extant_df1) <- c("value", "x", "y")

ggplot() +  
  geom_tile(data=nat_masked_extant_df1, aes(x=x, y=y, fill=value), alpha=0.8) + 
  scale_fill_viridis(limits = c(50, 500)) +
  coord_equal() +
  theme_map() +
  theme(legend.position = "none")

ex_tram_arbre2_buff_vect <- vect(ex_tram_arbre2_buff)
nat_crop2 <- crop(nat,ex_tram_arbre2_buff)
nat_masked_extant2 <- mask(nat_crop2, ex_tram_arbre2_buff_vect)
nat_masked_extant_spdf2 <- as(raster(nat_masked_extant2), "SpatialPixelsDataFrame")
nat_masked_extant_df2 <- as.data.frame(nat_masked_extant_spdf2)
colnames(nat_masked_extant_df2) <- c("value", "x", "y")

ggplot() +  
  geom_tile(data=nat_masked_extant_df2, aes(x=x, y=y, fill=value), alpha=0.8) + 
  scale_fill_viridis(limits = c(50, 500)) +
  coord_equal() +
  theme_map() +
  theme(legend.position = "none")


### plot figure 4
library(ggridges)
library(hrbrthemes)

### get  average naturalness center and periph

nat_com3 <- readRDS("output/nat_com3.rds") 

list_ville <- data.frame(com_centre=c(rep("Angers",5),rep("Aubagne",4),rep("Avignon",1),rep("Bordeaux",2),
                                      rep("Grenoble",2),rep("Le Havre",2),rep("Lille",15),rep("Lyon",24),
                                      rep("Montpellier",4),rep("Nantes",6),rep("Nice",5),rep("Rouen",17),
                                      rep("Strasbourg",4),rep("Toulouse",6),rep("Tours",7),rep("Geneve",1),
                                      rep("Caen",2),rep("Paris",30)),
                         com_peri=c(
                           "Avrillé",  "Beaucouzé",  "Les Ponts-de-Cé",  "Saint-Barthélemy-d'Anjou",
                           "Verrières-en-Anjou","Auriol","La Bouilladisse","La Destrousse",
                           "Roquevaire","Le Pontet","Eysines","Villenave-d'Ornon",
                           "La Tronche",  "Saint-Martin-d'Hères","Harfleur","Montivilliers",
                           "Faches-Thumesnil","Haubourdin","Hem","La Madeleine",
                           "Loos","Marquette-lez-Lille","Neuville-en-Ferrain","Roubaix",
                           "Saint-André-lez-Lille","Seclin","Templemars","Tourcoing",
                           "Wambrechies","Wattignies","Wattrelos","Bron",
                           "Caluire-et-Cuire","Champagne-au-Mont-d'Or",  "Chaponost",  "Charbonnières-les-Bains",
                           "Chassieu",  "Dardilly",  "Décines-Charpieu",  "Écully",
                           "Francheville",  "Genas",  "La Mulatière",  "Meyzieu",
                           "Oullins",  "Pierre-Bénite",  "Rillieux-la-Pape",  "Saint-Fons",
                           "Saint-Genis-Laval",  "Saint-Priest",  "Sainte-Foy-lès-Lyon",  "Tassin-la-Demi-Lune",
                           "Vaulx-en-Velin",  "Vénissieux",  "Villeurbanne",  "Clapiers",
                           "Lavérune",  "Montferrier-sur-Lez",  "Saint-Jean-de-Védas",  "Bouguenais",
                           "Carquefou",  "La Chapelle-sur-Erdre",  "Orvault",  "Rezé",
                           "Saint-Herblain",  "Cagnes-sur-Mer",  "Drap",  "La Trinité",
                           "Saint-André-de-la-Roche",  "Saint-Laurent-du-Var", "Barentin",  "Grand-Couronne",
                           "La Londe",  "Le Grand-Quevilly",  "Le Petit-Quevilly",  "Orival",
                           "Petit-Couronne",  "Saint-Aubin-lès-Elbeuf",  "Sotteville-lès-Rouen",  "Tourville-la-Rivière",
                           "Déville-lès-Rouen",  "Malaunay",  "Maromme",  "Notre-Dame-de-Bondeville",
                           "Elbeuf",  "Pavilly",  "Caudebec-lès-Elbeuf",  "Bischheim",
                           "Eckbolsheim",  "Schiltigheim",  "Wolfisheim",  "Aussonne",
                           "Auzeville-Tolosane",  "Beauzelle",  "Blagnac",  "Colomiers",
                           "Cornebarrieu",  "Chambray-lès-Tours",  "Fondettes",  "Joué-lès-Tours",
                           "La Riche",  "Saint-Avertin",  "Saint-Cyr-sur-Loire",  "Saint-Pierre-des-Corps",
                           "Ferney-Voltaire",  "Saint-Contest",  "Bretteville-sur-Odon",  "Antony",
                           "Saint-Germain-en-Laye",  "Athis-Mons",  "Saint-Denis",  "Longjumeau",
                           "Morsang-sur-Orge",  "Montreuil",  "Achères",  "Grigny",
                           "Romainville",  "Champlan",  "Noisy-le-Sec",  "Juvisy-sur-Orge",
                           "Épinay-sur-Orge",  "Évry-Courcouronnes",  "Colombes",  "Paray-Vieille-Poste",
                           "Châtenay-Malabry",  "Savigny-sur-Orge",  "Bois-Colombes",  "Poissy",
                           "Clamart",  "Palaiseau",  "Chilly-Mazarin",  "Fontenay-sous-Bois",
                           "Ris-Orangis",  "Massy",  "Aubervilliers",  "Asnières-sur-Seine",
                           "Rosny-sous-Bois")
)

list_ville <- list_ville[which(list_ville$com_centre!="Paris"),]

fig4_data <-  rbind(data.frame(nat_com3[which(nat_com3$com_name %in% unique(list_ville$com_centre)),],variable="com_center"),
                    data.frame(nat_com3[which(nat_com3$com_name %in% unique(list_ville$com_peri)),],variable="com_peri"))

fig4_data$com_name <- fig4_data$com_code <- NULL

### add naturalness tram and project

project_tram_mean <- ldply(nat_tram_all_plot, .fun = function(x){return(weighted.mean(x$value,x$cover, na.rm=TRUE))})
extant_trace_mean <- ldply(nat_tram_extant_all_plot, .fun = function(x){return(weighted.mean(x$value,x$cover, na.rm=TRUE))})

fig4_data <- rbind(fig4_data,data.frame(nat=unlist(project_tram_mean),variable="project_tram_area"),
                   data.frame(nat=unlist(extant_trace_mean),variable="extant_tram"))

### retrieve proportion of commune area affected by tram

spdf <- geojson_read("raw_data/georef-france-commune-arrondissement-municipal-millesime.geojson",  what = "sp")

spdf <- st_as_sf(spdf)
tram_trace_buff_16 <- st_buffer(tram_trace,16)
spdf <- st_transform(spdf, crs = st_crs(tram_trace_buff_16))

tram_trace_com <- st_intersection(tram_trace_buff_16,spdf)
tram_trace_com$area <- as.numeric(st_area(tram_trace_com))
st_geometry(tram_trace_com) <- NULL
com_affected <- data.frame(tram_trace_com %>% group_by(com_name) %>% summarize(area_affected=sum(area)))

com_affected_total <- spdf[which(spdf$com_name %in% com_affected$com_name),]
com_affected_total$area <- as.numeric(st_area(com_affected_total))
st_geometry(com_affected_total) <- NULL
com_affected_total <- data.frame(com_affected_total %>% group_by(com_name) %>% summarize(area_total=sum(area)))

com_affected_prop <- merge(com_affected,com_affected_total,by="com_name")
com_affected_prop$prop <- com_affected_prop$area_affected/com_affected_prop$area_total

### naturalness of affected commune (current, after tram without tree, after tram with trees)

nat_affected_com_current <- exact_extract(nat,spdf[which(spdf$com_name %in% com_affected$com_name),])

nat_affected_com_current_mean <- ldply(nat_affected_com_current, .fun = function(x){return(weighted.mean(x$value,x$cover, na.rm=TRUE))})
nat_affected_com_current_mean$com_name <- spdf[which(spdf$com_name %in% com_affected$com_name),]$com_name
nat_affected_com_current_mean$area <- as.numeric(st_area(spdf[which(spdf$com_name %in% com_affected$com_name),]))
nat_affected_com_current_mean <- data.frame(nat_affected_com_current_mean %>% group_by(com_name) %>% summarize(com_current=weighted.mean(V1,area)))


tram_trace_com <- st_intersection(tram_trace_buff_16,spdf[which(spdf$com_name %in% com_affected$com_name),])
tram_trace_com <- tram_trace_com %>% group_by(com_name) %>% dplyr::summarise(across(geometry, ~ sf::st_combine(.)), .groups = "keep") %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "drop")
no_tram_trace_com <- st_difference(spdf[which(spdf$com_name %in% com_affected$com_name),],tram_trace_buff_16)
no_tram_trace_com <- no_tram_trace_com %>% group_by(com_name) %>% dplyr::summarise(across(geometry, ~ sf::st_combine(.)), .groups = "keep") %>% 
  dplyr::summarise(across(geometry, ~ sf::st_union(.)), .groups = "drop")

nat_affected_com_ex_tram <- exact_extract(nat,no_tram_trace_com)
nat_affected_com_in_tram <- exact_extract(nat,tram_trace_com)

nat_affected_com_ex_tram_mean <- ldply(nat_affected_com_ex_tram, .fun = function(x){return(weighted.mean(x$value,x$cover, na.rm=TRUE))})
nat_affected_com_ex_tram_mean$com_name <- no_tram_trace_com$com_name

nat_affected_com_in_tram_mean <- ldply(nat_affected_com_in_tram, .fun = function(x){return(weighted.mean(x$value,x$cover, na.rm=TRUE))})
nat_affected_com_in_tram_mean$com_name <- no_tram_trace_com$com_name
nat_affected_com_in_tram_mean$nat_post_tram <- nat_affected_com_in_tram_mean$V1 - dist_effect_tram$effect[9]
nat_affected_com_in_tram_mean$nat_post_tram_tree <- nat_affected_com_in_tram_mean$nat_post_tram + summary(lm(value~variable2,plot_ex_tram_arbre, weights = cover))$coef[2,1]
names(nat_affected_com_in_tram_mean)[1] <- "nat_current"

nat_affected_com_post_tram_mean <- merge(nat_affected_com_ex_tram_mean,nat_affected_com_in_tram_mean,by="com_name")
nat_affected_com_post_tram_mean <- merge(nat_affected_com_post_tram_mean,com_affected_prop, by="com_name")

nat_affected_com_post_tram_mean$com_post_tram <- nat_affected_com_post_tram_mean$V1*(1-nat_affected_com_post_tram_mean$prop)+nat_affected_com_post_tram_mean$nat_post_tram*nat_affected_com_post_tram_mean$prop
nat_affected_com_post_tram_mean$com_post_tram_tree <- nat_affected_com_post_tram_mean$V1*(1-nat_affected_com_post_tram_mean$prop)+nat_affected_com_post_tram_mean$nat_post_tram_tree*nat_affected_com_post_tram_mean$prop

nat_affected_com_final <- merge(nat_affected_com_post_tram_mean,nat_affected_com_current_mean,by="com_name")

#nat_affected_com_final_long <- melt(nat_affected_com_final[,c("com_post_tram","com_post_tram_tree","com_current")])
#names(nat_affected_com_final_long)[2] <- "nat"
nat_affected_com_final_long <- melt(nat_affected_com_final[,c("com_current","nat_current","nat_post_tram","nat_post_tram_tree")])
names(nat_affected_com_final_long)[2] <- "nat"

### plot fig 4

fig4_data <- rbind(data.frame(nat_com3[which(nat_com3$com_name %in% unique(list_ville$com_centre)),],variable="com_center")[,c("nat","variable")],
                   nat_affected_com_final_long)
names(fig4_data)[1] <- "Naturalness"
fig4_data$variable[which(fig4_data$variable=="com_center")] <- "Center"
fig4_data$variable[which(fig4_data$variable=="com_current")] <- "Peripheric"
fig4_data$variable[which(fig4_data$variable=="nat_current")] <- "Tram project right-of-way (current)"
fig4_data$variable[which(fig4_data$variable=="nat_post_tram")] <- "Tram project right-of-way (expected)"
fig4_data$variable[which(fig4_data$variable=="nat_post_tram_tree")] <- "Tram project right-of-way (trees)"

ggplot(fig4_data, aes(x = Naturalness, y = variable, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 1.5, rel_min_height = 0.01) +
  scale_fill_viridis() + 
  theme_minimal() +
  geom_segment(aes(x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Center")]),
               xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Center")]),
               y=1,yend=2.3),linetype="dashed") +
  geom_segment(aes(x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Peripheric")]),
                   xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Peripheric")]),
                   y=2,yend=2.63),linetype="dashed") +
  geom_segment(aes(x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project right-of-way (current)")]),
                   xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project right-of-way (current)")]),
                   y=3,yend=3.7),linetype="dashed") +
  geom_segment(aes(x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project right-of-way (expected)")]),
                   xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project right-of-way (expected)")]),
                   y=4,yend=4.7),linetype="dashed") +
  geom_segment(aes(x = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project right-of-way (trees)")]),
                   xend = mean(fig4_data$Naturalness[which(fig4_data$variable=="Tram project right-of-way (trees)")]),
                   y=5,yend=5.7),linetype="dashed") +
  theme(
    legend.position="none",
    axis.title.y = element_blank(),
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

ggsave("output/figure4.png",
       width = 8,
       height = 6,
       dpi = 400)

### test influence distance buffer

dist_effect <- dist_effect_nat1 <- dist_effect_nat2 <- dist_effect_nat3 <- data.frame(dist=NA,effect=NA, pval=NA)
dist_effect2 <- dist_effect2_nat1 <- dist_effect2_nat2 <- dist_effect2_nat3 <- data.frame(dist=NA, effect1=NA, pval1=NA, effect2=NA, pval2=NA)

for(i in seq(from=1, to=50, by=1)){
  
  ex_tram_arbre0 <- st_transform(ex_tram_arbre0, crs(nat))
  ex_tram_arbre0_buff <- st_buffer(ex_tram_arbre0,i)
  nat_ex_tram_arbre0 <- exact_extract(nat,ex_tram_arbre0_buff)
  nat_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat_ex_tram_arbre0)
  nat_ex_tram_arbre0_plot <- data.frame(nat_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_ex_tram_arbre0 <- exact_extract(nat1,ex_tram_arbre0_buff)
  nat1_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat1_ex_tram_arbre0)
  nat1_ex_tram_arbre0_plot <- data.frame(nat1_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_ex_tram_arbre0 <- exact_extract(nat2,ex_tram_arbre0_buff)
  nat2_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat2_ex_tram_arbre0)
  nat2_ex_tram_arbre0_plot <- data.frame(nat2_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_ex_tram_arbre0 <- exact_extract(nat3,ex_tram_arbre0_buff)
  nat3_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat3_ex_tram_arbre0)
  nat3_ex_tram_arbre0_plot <- data.frame(nat3_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  ex_tram_arbre1 <- st_transform(ex_tram_arbre1, crs(nat))
  ex_tram_arbre1_buff <- st_buffer(ex_tram_arbre1,i)
  nat_ex_tram_arbre1 <- exact_extract(nat,ex_tram_arbre1_buff)
  nat_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat_ex_tram_arbre1)
  nat_ex_tram_arbre1_plot <- data.frame(nat_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_ex_tram_arbre1 <- exact_extract(nat1,ex_tram_arbre1_buff)
  nat1_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat1_ex_tram_arbre1)
  nat1_ex_tram_arbre1_plot <- data.frame(nat1_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_ex_tram_arbre1 <- exact_extract(nat2,ex_tram_arbre1_buff)
  nat2_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat2_ex_tram_arbre1)
  nat2_ex_tram_arbre1_plot <- data.frame(nat2_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_ex_tram_arbre1 <- exact_extract(nat3,ex_tram_arbre1_buff)
  nat3_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat3_ex_tram_arbre1)
  nat3_ex_tram_arbre1_plot <- data.frame(nat3_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  ex_tram_arbre2 <- st_transform(ex_tram_arbre2, crs(nat))
  ex_tram_arbre2_buff <- st_buffer(ex_tram_arbre2,i)
  nat_ex_tram_arbre2 <- exact_extract(nat,ex_tram_arbre2_buff)
  nat_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat_ex_tram_arbre2)
  nat_ex_tram_arbre2_plot <- data.frame(nat_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat1_ex_tram_arbre2 <- exact_extract(nat1,ex_tram_arbre2_buff)
  nat1_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat1_ex_tram_arbre2)
  nat1_ex_tram_arbre2_plot <- data.frame(nat1_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat2_ex_tram_arbre2 <- exact_extract(nat2,ex_tram_arbre2_buff)
  nat2_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat2_ex_tram_arbre2)
  nat2_ex_tram_arbre2_plot <- data.frame(nat2_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  nat3_ex_tram_arbre2 <- exact_extract(nat3,ex_tram_arbre2_buff)
  nat3_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat3_ex_tram_arbre2)
  nat3_ex_tram_arbre2_plot <- data.frame(nat3_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  plot_ex_tram_arbre <- rbind(data.frame(nat_ex_tram_arbre0_plot,variable="0_tree"),
                              data.frame(nat_ex_tram_arbre1_plot,variable="1_trees"),
                              data.frame(nat_ex_tram_arbre2_plot,variable="2_trees"))
  plot_ex_tram_arbre$variable2 <- ifelse(plot_ex_tram_arbre$variable=="0_tree","0_tree","1_trees")
  
  result <- summary(lm(value~variable2,plot_ex_tram_arbre, weights = cover))
  result2 <- summary(lm(value~variable,plot_ex_tram_arbre, weights = cover))
  
  dist_effect <- rbind(dist_effect,data.frame(dist=i, effect=result$coef[2,1], pval=result$coef[2,4]))
  dist_effect2 <- rbind(dist_effect2,data.frame(dist=i, effect1=result2$coef[2,1], pval1=result2$coef[2,4], effect2=result2$coef[3,1], pval2=result2$coef[3,4]))
  
  plot_ex_tram_arbre_nat1 <- rbind(data.frame(nat1_ex_tram_arbre0_plot,variable="0_tree"),
                              data.frame(nat1_ex_tram_arbre1_plot,variable="1_trees"),
                              data.frame(nat1_ex_tram_arbre2_plot,variable="2_trees"))
  plot_ex_tram_arbre_nat1$variable2 <- ifelse(plot_ex_tram_arbre_nat1$variable=="0_tree","0_tree","1_trees")
  
  result_nat1 <- summary(lm(value~variable2,plot_ex_tram_arbre_nat1, weights = cover))
  result2_nat1 <- summary(lm(value~variable,plot_ex_tram_arbre_nat1, weights = cover))
  
  dist_effect_nat1 <- rbind(dist_effect_nat1,data.frame(dist=i, effect=result_nat1$coef[2,1], pval=result_nat1$coef[2,4]))
  dist_effect2_nat1 <- rbind(dist_effect2_nat1,data.frame(dist=i, effect1=result2_nat1$coef[2,1], pval1=result2_nat1$coef[2,4], effect2=result2_nat1$coef[3,1], pval2=result2_nat1$coef[3,4]))
  
  plot_ex_tram_arbre_nat2 <- rbind(data.frame(nat2_ex_tram_arbre0_plot,variable="0_tree"),
                                   data.frame(nat2_ex_tram_arbre1_plot,variable="1_trees"),
                                   data.frame(nat2_ex_tram_arbre2_plot,variable="2_trees"))
  plot_ex_tram_arbre_nat2$variable2 <- ifelse(plot_ex_tram_arbre_nat2$variable=="0_tree","0_tree","1_trees")
  
  result_nat2 <- summary(lm(value~variable2,plot_ex_tram_arbre_nat2, weights = cover))
  result2_nat2 <- summary(lm(value~variable,plot_ex_tram_arbre_nat2, weights = cover))
  
  dist_effect_nat2 <- rbind(dist_effect_nat2,data.frame(dist=i, effect=result_nat2$coef[2,1], pval=result_nat2$coef[2,4]))
  dist_effect2_nat2 <- rbind(dist_effect2_nat2,data.frame(dist=i, effect1=result2_nat2$coef[2,1], pval1=result2_nat2$coef[2,4], effect2=result2_nat2$coef[3,1], pval2=result2_nat2$coef[3,4]))
  
  plot_ex_tram_arbre_nat3 <- rbind(data.frame(nat3_ex_tram_arbre0_plot,variable="0_tree"),
                                   data.frame(nat3_ex_tram_arbre1_plot,variable="1_trees"),
                                   data.frame(nat3_ex_tram_arbre2_plot,variable="2_trees"))
  plot_ex_tram_arbre_nat3$variable2 <- ifelse(plot_ex_tram_arbre_nat3$variable=="0_tree","0_tree","1_trees")
  
  result_nat3 <- summary(lm(value~variable2,plot_ex_tram_arbre_nat3, weights = cover))
  result2_nat3 <- summary(lm(value~variable,plot_ex_tram_arbre_nat3, weights = cover))
  
  dist_effect_nat3 <- rbind(dist_effect_nat3,data.frame(dist=i, effect=result_nat3$coef[2,1], pval=result_nat3$coef[2,4]))
  dist_effect2_nat3 <- rbind(dist_effect2_nat3,data.frame(dist=i, effect1=result2_nat3$coef[2,1], pval1=result2_nat3$coef[2,4], effect2=result2_nat3$coef[3,1], pval2=result2_nat3$coef[3,4]))
  
}

# nombre d'arbres

D <- as.numeric(sum(st_length(tram_trace)))
for(d in c(5:15)){
  nt <- (D/d+1)*2
  print(nt)
}

