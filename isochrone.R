# Test isochrone function
iso <- osrmIsochrone(loc = c(3.88916,43.608239), breaks = seq(from = 0,to = 15, by = 5))
class(iso)

st_geometry(iso) <- st_collection_extract(x = st_geometry(iso), 
                                           type = "POLYGON")
iso <- as(iso, "Spatial")

# add drive time description to be later used as a legend
iso@data$drive_times <- factor(paste(iso@data$isomin, "to", iso@data$isomax, "min"))

# color palette for each area
factpal <- colorFactor(rev(heat.colors(5)), iso@data$drive_times)

# draw map
leaflet() %>% 
  setView(3.88916, 43.608239, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron", group="Greyscale") %>% 
  addMarkers(lng = 3.88916, lat = 43.608239, popup = "100 Chalmers Street") %>% 
  addPolygons(fill=TRUE, stroke=TRUE, color = "black",
              fillColor = ~factpal(iso@data$drive_times),
              weight=0.5, fillOpacity=0.2,
              data = iso, popup = iso@data$drive_times,
              group = "Drive Time") %>% 
  # Legend
  addLegend("bottomright", pal = factpal, values = iso@data$drive_time,   title = "Drive Time")


# commune geo from https://public.opendatasoft.com/explore/dataset/georef-france-commune-arrondissement-municipal-millesime/export/?disjunctive.reg_name&disjunctive.dep_name&disjunctive.arrdep_name&disjunctive.ze2020_name&disjunctive.bv2012_name&disjunctive.epci_name&disjunctive.ept_name&disjunctive.com_name&disjunctive.com_arm_name&disjunctive.com_arm_is_mountain_area&sort=year&location=6,46.97276,3.93311&basemap=jawg.light
spdf <- geojson_read("raw_data/georef-france-commune-arrondissement-municipal-millesime.geojson",  what = "sp")

# commune at max 15 min drive from tram expansion

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

source(file = "functions.R")
nat_com3 <- readRDS("output/nat_com3.rds")

res_isochrone <- ddply(list_ville, .(com_centre,com_peri), .fun = get_neigh_com,
                       pop_data = socecohispol_com[,c("com_name","codgeo","pot_fin_hab",
                                                    "med_disp","p_csp_cadpis","p_csp_arcomce",
                                                    "p_csp_agr","p_csp_empl","p_csp_inter",
                                                    "p_csp_ouvr","nb_inact1564","pc_inact1564",
                                                    "dens_pop","part_domtrav_voit","car_mov_evol",
                                                    "ratio_fh","part_pop1529","part_pop3044","part_pop4559",
                                                    "part_pop6074","part_pop_65p","p_csp_retr","pop","size")],
                       nat_data = nat_com3,
                       .progress = "text"
)


# Naturality classes from isochrones

res_isochrone_final <- res_isochrone[which(!(res_isochrone$com_centre %in% c("Paris")) & !(res_isochrone$com_peri %in% c("Saint-Fons")) & !(res_isochrone$com_name %in% c(unique(list_ville$com_centre),"Paris","Saint-Fons"))),]

com_isochrone <- data.frame(res_isochrone_final %>% distinct(com_code, com_name, .keep_all=TRUE))

com_isochrone_split <- split(com_isochrone, cut2(com_isochrone$nat, g=4))

com_isochrone$class_nat <- NA
com_isochrone$class_nat[which(com_isochrone$nat<227)] <- "Naturality --"
com_isochrone$class_nat[which(com_isochrone$nat>=227 & com_isochrone$nat<256)] <- "Naturality -"
com_isochrone$class_nat[which(com_isochrone$nat>=256)] <- "Naturality +"

saveRDS(com_isochrone,"output/com_isochrone.rds")
