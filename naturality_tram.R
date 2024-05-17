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

tram_trace <- rbind(tram_trace1,tram_trace2,tram_trace3,
                    tram_trace4,tram_trace5,tram_trace6)
tram_trace <- tram_trace[1:24,] # 25 tracé initial l5
mapview(tram_trace)


nat <- rast(raster("raw_data/Donnees_cartographiques/Layer4_FINAL.tif"))

tram_trace <- st_transform(tram_trace, crs(nat))
tram_trace_buff <- st_buffer(tram_trace,6.5)

nat_tram_mean <- exact_extract(nat,tram_trace_buff,"mean")
nat_tram_all <- exact_extract(nat,tram_trace_buff)

nat_tram_all_plot <- list()
for(i in 1:length(nat_tram_all)){
  nat_tram_all_plot[[i]] <- data.frame(nat_tram_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
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

nat_tram_extant_all_plot <- list()
for(i in 1:length(nat_tram_extant_all)){
  nat_tram_extant_all_plot[[i]] <- data.frame(nat_tram_extant_all[[i]] %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
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

ggsave("output/tram_nat_extant_project.png",
       width = 5,
       height = 5,
       dpi = 400)

summary(lm(value~variable,plot_trace_13m, weights = cover))

# the same for other buffer values

dist_effect_tram <- data.frame(dist=NA,effect=NA, pval=NA)

for(i in seq(from=1, to=50, by=2)){
  
  tram_trace_buff_compare <- st_buffer(tram_trace,i)
  nat_tram_all_compare <- exact_extract(nat,tram_trace_buff_compare)
  nat_tram_all_compare <- do.call(rbind.data.frame, nat_tram_all_compare)
  nat_tram_all_compare_plot <- data.frame(nat_tram_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  tram_extant_buff_compare <- st_buffer(tram_extant,i)
  nat_tram_extant_all_compare <- exact_extract(nat,tram_extant_buff_compare)
  nat_tram_extant_all_compare <- do.call(rbind.data.frame, nat_tram_extant_all_compare)
  nat_tram_extant_all_compare_plot <- data.frame(nat_tram_extant_all_compare %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  plot_trace_compare <- rbind(data.frame(nat_tram_all_compare_plot,variable="Tram projects"),data.frame(nat_tram_extant_all_compare_plot,variable="Tram extant"))
  
  results <- summary(lm(value~variable,plot_trace_compare, weights = cover))
  
  dist_effect_tram <- rbind(dist_effect_tram,data.frame(dist=i, effect=results$coef[2,1], pval=results$coef[2,4]))
  
}



### example of tree effect on natrualness (Montpellier ligne 1, most frequented ligne in France)

ex_tram_arbre0 <- read_sf("raw_data/traces_TRAMWAY_2021_tree.kml",c("Montpellier_pas_arbre"))
ex_tram_arbre1 <- read_sf("raw_data/traces_TRAMWAY_2021_tree.kml",c("Montpellier_arbre_un"))
ex_tram_arbre2 <- read_sf("raw_data/traces_TRAMWAY_2021_tree.kml",c("Montpellier_arbre_deux"))

nat <- rast(raster("raw_data/Donnees_cartographiques/Layer4_FINAL.tif"))

ex_tram_arbre0 <- st_transform(ex_tram_arbre0, crs(nat))
ex_tram_arbre0_buff <- st_buffer(ex_tram_arbre0,10)
nat_ex_tram_arbre0 <- exact_extract(nat,ex_tram_arbre0_buff)
nat_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat_ex_tram_arbre0)
nat_ex_tram_arbre0_plot <- data.frame(nat_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))

ex_tram_arbre1 <- st_transform(ex_tram_arbre1, crs(nat))
ex_tram_arbre1_buff <- st_buffer(ex_tram_arbre1,10)
nat_ex_tram_arbre1 <- exact_extract(nat,ex_tram_arbre1_buff)
nat_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat_ex_tram_arbre1)
nat_ex_tram_arbre1_plot <- data.frame(nat_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))

ex_tram_arbre2 <- st_transform(ex_tram_arbre2, crs(nat))
ex_tram_arbre2_buff <- st_buffer(ex_tram_arbre2,10)
nat_ex_tram_arbre2 <- exact_extract(nat,ex_tram_arbre2_buff)
nat_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat_ex_tram_arbre2)
nat_ex_tram_arbre2_plot <- data.frame(nat_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))

plot_ex_tram_arbre <- rbind(data.frame(nat_ex_tram_arbre0_plot,variable="no_tree"),
                            data.frame(nat_ex_tram_arbre1_plot,variable="few_trees"),
                            data.frame(nat_ex_tram_arbre2_plot,variable="many_trees"))
plot_ex_tram_arbre$variable2 <- ifelse(plot_ex_tram_arbre$variable=="no_tree","no_tree","trees")

ggplot(plot_ex_tram_arbre, aes(x = variable2, y = value, weight = cover)) + 
  geom_boxplot(width=0.6,  colour = I("#3366FF")) + theme_modern() +
  labs(y="Naturalness") + theme(axis.title.x = element_blank())

summary(lm(value~variable2,plot_ex_tram_arbre, weights = cover))

###test influence distance buffer

dist_effect <- data.frame(dist=NA,effect=NA, pval=NA)
dist_effect2 <- data.frame(dist=NA, effect1=NA, pval1=NA, effect2=NA, pval2=NA)

for(i in seq(from=1, to=50, by=2)){
  
  ex_tram_arbre0 <- st_transform(ex_tram_arbre0, crs(nat))
  ex_tram_arbre0_buff <- st_buffer(ex_tram_arbre0,i)
  nat_ex_tram_arbre0 <- exact_extract(nat,ex_tram_arbre0_buff)
  nat_ex_tram_arbre0_all <- do.call(rbind.data.frame, nat_ex_tram_arbre0)
  nat_ex_tram_arbre0_plot <- data.frame(nat_ex_tram_arbre0_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  ex_tram_arbre1 <- st_transform(ex_tram_arbre1, crs(nat))
  ex_tram_arbre1_buff <- st_buffer(ex_tram_arbre1,i)
  nat_ex_tram_arbre1 <- exact_extract(nat,ex_tram_arbre1_buff)
  nat_ex_tram_arbre1_all <- do.call(rbind.data.frame, nat_ex_tram_arbre1)
  nat_ex_tram_arbre1_plot <- data.frame(nat_ex_tram_arbre1_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  ex_tram_arbre2 <- st_transform(ex_tram_arbre2, crs(nat))
  ex_tram_arbre2_buff <- st_buffer(ex_tram_arbre2,i)
  nat_ex_tram_arbre2 <- exact_extract(nat,ex_tram_arbre2_buff)
  nat_ex_tram_arbre2_all <- do.call(rbind.data.frame, nat_ex_tram_arbre2)
  nat_ex_tram_arbre2_plot <- data.frame(nat_ex_tram_arbre2_all %>% group_by(value) %>% summarize(cover=sum(coverage_fraction)))
  
  plot_ex_tram_arbre <- rbind(data.frame(nat_ex_tram_arbre0_plot,variable="0_tree"),
                              data.frame(nat_ex_tram_arbre1_plot,variable="1_trees"),
                              data.frame(nat_ex_tram_arbre2_plot,variable="2_trees"))
  plot_ex_tram_arbre$variable2 <- ifelse(plot_ex_tram_arbre$variable=="0_tree","0_tree","1_trees")
  
  result <- summary(lm(value~variable2,plot_ex_tram_arbre, weights = cover))
  result2 <- summary(lm(value~variable,plot_ex_tram_arbre, weights = cover))
  
  dist_effect <- rbind(dist_effect,data.frame(dist=i, effect=result$coef[2,1], pval=result$coef[2,4]))
  dist_effect2 <- rbind(dist_effect2,data.frame(dist=i, effect1=result2$coef[2,1], pval1=result2$coef[2,4], effect2=result2$coef[3,1], pval2=result2$coef[3,4]))
  
}

# nombre d'arbres

D <- as.numeric(sum(st_length(tram_trace)))
for(d in c(5:15)){
  nt <- (D/d+1)*2
  print(nt)
}

