data <- read.csv("results-survey672984.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")


# LimeSurvey Field type: F
data[, 1] <- as.numeric(data[, 1])
attributes(data)$variable.labels[1] <- "id"
names(data)[1] <- "id"
# LimeSurvey Field type: DATETIME23.2
data[, 2] <- as.character(data[, 2])
attributes(data)$variable.labels[2] <- "submitdate"
names(data)[2] <- "submitdate"
# LimeSurvey Field type: F
data[, 3] <- as.numeric(data[, 3])
attributes(data)$variable.labels[3] <- "lastpage"
names(data)[3] <- "lastpage"
# LimeSurvey Field type: A
data[, 4] <- as.character(data[, 4])
attributes(data)$variable.labels[4] <- "startlanguage"
names(data)[4] <- "startlanguage"
# LimeSurvey Field type: A
data[, 5] <- as.character(data[, 5])
attributes(data)$variable.labels[5] <- "seed"
names(data)[5] <- "seed"
# LimeSurvey Field type: DATETIME23.2
data[, 6] <- as.character(data[, 6])
attributes(data)$variable.labels[6] <- "startdate"
names(data)[6] <- "startdate"
# LimeSurvey Field type: DATETIME23.2
data[, 7] <- as.character(data[, 7])
attributes(data)$variable.labels[7] <- "datestamp"
names(data)[7] <- "datestamp"
# LimeSurvey Field type: A
data[, 8] <- as.character(data[, 8])
attributes(data)$variable.labels[8] <- "ipaddr"
names(data)[8] <- "ipaddr"
# LimeSurvey Field type: A
data[, 9] <- as.character(data[, 9])
attributes(data)$variable.labels[9] <- ""
names(data)[9] <- "Qparam"
# LimeSurvey Field type: A
data[, 10] <- as.character(data[, 10])
attributes(data)$variable.labels[10] <- "Quelle est votre commune (code postal et nom) de résidence :"
data[, 10] <- factor(data[, 10],
                     levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","A17","A18","A19","A20","A21","A22","A23","A24","A25","A26","A27","A28","A29","A30","A31","A32","A33","A34","A35","A36","A37","A38","A39","A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A50","A51","A52","A53","A54","A55","A56","A57","A58","A59","A60","A61","A62","A63","A64","A65","A66","A67","A68","A69","A70","A71","A72","A73","A74","A75","A76","A77","A78","A79","A80","A81","A82","A83","A84","A85","A86","A87","A88","A89","A90","A91","A92","A93","A94","A95","A96","A97","A98","A99","A100","A101","A102","A103","A104","A105","A106","A107","A108","A109","A110","A111","A112","A113","A114","A115","A116","A117","A118","A119","A120","A121","A122","A123","A124","A125","A126","A127","A128","A129","A130","A131","A132","A133","A134","A135","A136","A137","A138","A139","A140","A141","A142","A143","A144","A145","A146","A147","A148","A149","A150","A151","A152","A153","A154","A155","A156","A157","A158","A159","A160","A161","A162","A163","A164","A165","A166","A167","A168","A169","A170","A171","A172","A173","A174","A175","A176","A177","A178","A179","A180","A181","A182","A183","A184","A185","A186","A187","A188","A189","A190","A191","A192","A193","A194","A195","A196","A197","A198","A199","A200","A201","A202","A203","A204","A205","A206","A207","A208","A209","A210","A211","A212","A213","A214","A215","A216","A217","A218","A219","A220","A221","A222","A223","A224","A225","A226","A227","A228","A229","A230","A231","A232","A233","A234","A235","A236","A237","A238","A239","A240","A241","A242","A243","A244","A245","A246","A247","A248","A249","A250","A251","A252","A253","A254","A255","A256","A257","A258","A259","A260","A261","A262","A263","A264","A265","A266","A267","A268","A269","A270","A271","A272","A273","A274","A275","A276","A277","A278","A279","A280","A281","A282","A283","A284","A285","A286","A287","A288","A289","A290","A291","A292","A293","A294","A295","A296","A297","A298","A299","A300","A301","A302","A303","A304","A305","A306","A307","A308","A309","A310","A311","A312","A313","A314","A315","A316","A317","A318","A319","A320","A321","A322","A323","A324","A325","A326","A327","A328","A329","A330","A331","A332","A333","A334","A335","A336","A337","A338","A339","A340","A341","A342","A343","A344","A345","A346","A347","A348","A349","A350","A351","A352","A353","A354","A355","A356","A357","A358","A359","A360","A361","A362","A363","A364","A365","A366","A367","A368","A369","A370","A371","A372","A373","A374","A375","A376","A377","A378","A379","A380","A381","A382","A383","A384","A385","A386","A387","A388","A389","A390","A391","A392","A393","A394","A395","A396","A397","A398","A399","A400","A401","A402","A403","A404","A405","A406","A407","A408","A409","A410","A411","A412","A413","A414","A415","A416","A417","A418","A419","A420","A421","A422","A423","A424","A425","A426","A427","A428","A429","A430","A431","A432","A433","A434","A435","A436","A437","A438","A439","A440","A441","A442","A443","A444","A445","A446","A447","A448","A449","A450","A451","A452","A453","A454","A455","A456","A457","A458","A459","A460","A461","A462","A463","A464","A465","A466","A467","A468","A469","A470","A471","A472","A473","A474","A475","A476","A477","A478","A479","A480","A481","A482","A483","A484","A485","A486","A487","A488","A489","A490","A491","A492","A493","A494","A495","A496","A497","A498","A499","A500","A501","A502","A503","A504","A505","A506","A507","A508","A509","A510","A511","A512","A513","A514","A515","A516","A517","A518","A519","A520","A521","A522","A523","A524","A525","A526","A527","A528","A529","A530","A531","A532","A533","A534","A535","A536","A537","A538","A539","A540","A541","A542","A543","A544","A545","A546","A547","A548","A549","A550","A551","A552","A553","A554","A555","A556","A557","A558","A559","A560","A561","A562","A563","A564","A565","A566","A567","A568"),
                     labels=c("ALIZAY - 27460", "AMFREVILLE ST AMAND - 27370", "LES MONTS DU ROUMOIS - 27520", "CRIQUEBEUF SUR SEINE - 27340", "LES DAMPS - 27340", "HONGUEMARE GUENOUVILLE - 27310", "MANDEVILLE - 27370", "TERRES DE BORD - 27340", "ST OUEN DE THOUBERVILLE - 27310", "LA SAUSSAYE - 27370", "LE THUIT DE L OISON - 27370", "TOURVILLE LA CAMPAGNE - 27370", "DEYME - 31450", "DONNEVILLE - 31450", "GAGNAC SUR GARONNE - 31150", "PECHABOU - 31320", "RAMONVILLE ST AGNE - 31520", "ST ORENS DE GAMEVILLE - 31650", "TOURNEFEUILLE - 31170", "AYGUEMORTE LES GRAVES - 33640", "FLOIRAC - 33270", "CLAPIERS - 34830", "MONTFERRIER SUR LEZ - 34980", "PRADES LE LEZ - 34730", "ST CLEMENT DE RIVIERE - 34980", "ST GELY DU FESC - 34980", "PERNAY - 37230", "BIVIERS - 38330", "MEYLAN - 38240", "POISAT - 38320", "ST MARTIN LE VINOUX - 38950", "INDRE - 44610", "ST JEAN DE BOISEAU - 44640", "BOUCHEMAINE - 49080", "CORZE - 49140", "ST BARTHELEMY D ANJOU - 49124", "ST JEAN DE LA CROIX - 49130", "ST LAMBERT LA POTHERIE - 49070", "ST LEGER DE LINIERES - 49070", "TRELAZE - 49800", "RIVES DU LOIR EN ANJOU - 49140", "MIONNAY - 01390", "MIRIBEL - 01700", "PREVESSIN MOENS - 01280", "VERSONNEX - 01210", "CAGNES SUR MER - 06800", "LA COLLE SUR LOUP - 06480", "TOURRETTE LEVENS - 06690", "PEYPIN - 13124", "ROQUEVAIRE - 13360", "ANISY - 14610", "LE FRESNE CAMILLY - 14480", "LOUVIGNY - 14111", "MAY SUR ORNE - 14320", "MONDRAINVILLE - 14210", "ROSEL - 14740", "ROTS - 14740", "ST GERMAIN LA BLANCHE HERBE - 14280", "ST MARTIN DE FONTENAY - 14320", "TOURVILLE SUR ODON - 14210", "CIVRIEUX D AZERGUES - 69380", "DARDILLY - 69570", "ECULLY - 69130", "FONTAINES SUR SAONE - 69270", "MARCILLY D AZERGUES - 69380", "ORLIENAS - 69530", "PIERRE BENITE - 69310", "STE FOY LES LYON - 69110",
                              "ST ROMAIN AU MONT D OR - 69270", "VOURLES - 69390", "CHAPONNAY - 69970", "FEYZIN - 69320", "ST LAURENT DE MURE - 69720", "ST SYMPHORIEN D OZON - 69360", "SATHONAY VILLAGE - 69580", "SEREZIN DU RHONE - 69360", "BIHOREL - 76420", "BONSECOURS - 76240", "CANTELEU - 76380", "DEVILLE LES ROUEN - 76250", "ELBEUF - 76500", "EPOUVILLE - 76133", "FONTENAY - 76290", "MALAUNAY - 76770", "LE MESNIL ESNARD - 76240", "MONT ST AIGNAN - 76130", "NOTRE DAME DU BEC - 76133", "OCTEVILLE SUR MER - 76930", "PAVILLY - 76570", "QUEVREVILLE LA POTERIE - 76520", "ST AUBIN LES ELBEUF - 76410", "ST AUBIN ROUTOT - 76430", "ST ETIENNE DU ROUVRAY - 76800", "ST PIERRE LES ELBEUF - 76320", "SAUSSAY - 76760", "SOTTEVILLE LES ROUEN - 76300", "LA VAUPALIERE - 76150", "YMARE - 76520", "ALTHEN DES PALUDS - 84210", "MORIERES LES AVIGNON - 84310", "LE PONTET - 84130", "SORGUES - 84700", "CHEVRY - 01170", "NEYRON - 01700", "TRAMOYES - 01390", "FERNEY VOLTAIRE - 01210", "CADOLIVE - 13950", "BARON SUR ODON - 14210", "CAMBES EN PLAINE - 14610", "CARPIQUET - 14650", "FONTAINE ETOUPEFOUR - 14790", "HEROUVILLE ST CLAIR - 14200", "MATHIEU - 14920", "MOUEN - 14790", "ST ANDRE SUR ORNE - 14320", "ST GENIS POUILLY - 01630", "ST MAURICE DE BEYNOST - 01700", "ST MANVIEU NORREY - 14740", "SEGNY - 01170", "VERSON - 14790", "ST PAUL DE VENCE - 06570", "FALICON - 06950", "LA GAUDE - 06610", "ST ANDRE DE LA ROCHE - 06730", "ORNEX - 01210", "AUTHIE - 14280", "BRETTEVILLE SUR ODON - 14760", "CORMELLES LE ROYAL - 14123", "ETERVILLE - 14930", "GRAINVILLE SUR ODON - 14210", "ROTS - 14980", "THAON - 14610", "DRAP - 06340", "ST LAURENT DU VAR - 06700", "LA TRINITE - 06340", "VILLENEUVE LOUBET - 06270", "BELCODENE - 13720", "LA BOUILLADISSE - 13720", "LA DESTROUSSE - 13112",
                              "COLOMBY ANGUERNY - 14610", "CAIRON - 14610", "FLEURY SUR ORNE - 14123", "IFS - 14123", "MALTOT - 14930", "LE BEC THOMAS - 27370", "BOSROUMOIS - 27670", "CRESTOT - 27110", "HECTOMARE - 27110", "LE MANOIR - 27460", "VILLONS LES BUISSONS - 14610", "ST DENIS DES MONTS - 27520", "ST DIDIER DES BOIS - 27370", "ST GERMAIN DE PASQUIER - 27370", "SURTAUVILLE - 27400", "LE TRONCQ - 27110", "AURIOL - 13390", "GREASQUE - 13850", "ST SAVOURNIN - 13119", "BASLY - 14610", "EPRON - 14610", "ST CONTEST - 14280", "AUSSONNE - 31840", "CORNEBARRIEU - 31700", "MERVILLA - 31320", "SEILH - 31840", "VIEILLE TOULOUSE - 31320", "VIGOULET AUZIL - 31320", "BEGLES - 33130", "GRADIGNAN - 33170", "MARTILLAC - 33650", "ST MEDARD D EYRANS - 33650", "JACOU - 34830", "MIREVAL - 34110", "GRAND BOURGTHEROULDE - 27520", "PIGNAN - 34570", "ST VINCENT DE BARBEYRARGUES - 34730", "LA HARENGERE - 27370", "MARTOT - 27340", "ST OUEN DE PONTCHEUIL - 27370", "ST PIERRE DES FLEURS - 27370", "ST PIERRE DU BOSGUERARD - 27370", "VRAIVILLE - 27370", "MONTBAZON - 37250", "MONTS - 37260", "ST CYR SUR LOIRE - 37540", "ST GENOUPH - 37510", "ST PIERRE DES CORPS - 37700", "GIERES - 38610", "MONTBONNOT ST MARTIN - 38330", "CAUMONT - 27310", "LA HAYE MALHERBE - 27400", "TERRES DE BORD - 27400", "ST CYR LA CAMPAGNE - 27370", "ST OUEN DU TILLEUL - 27670", "LA TRINITE DE THOUBERVILLE - 27310", "AUZEVILLE TOLOSANE - 31320", "BLAGNAC - 31700", "CASTANET TOLOSAN - 31320", "ESCALQUENS - 31750", "MONDONVILLE - 31700", "ST PAUL SUR SAVE - 31530", "LES MONTS DU ROUMOIS - 27370", "BLANQUEFORT - 33290", "BOULIAC - 33270", "BEAUZELLE - 31700", "CANEJAN - 33610", "CRASVILLE - 27400", "FENOUILLET - 31150", "MERIGNAC - 33700", "FOUQUEVILLE - 27370", "LABEGE - 31670", "LA HAYE DU THEIL - 27370", "LESPINASSE - 31150", "BOUAYE - 44830", "BOUGUENAIS - 44340", "IGOVILLE - 27460", "CARQUEFOU - 44470", "CHEIX EN RETZ - 44640", "MERVILLE - 31330", "COUERON - 44220", "MONTAIGUT SUR SAVE - 31530", "LA MONTAGNE - 44620", "VILLENAVE D ORNON - 33140", "PONT DE L ARCHE - 27340", "ST ALBAN - 31140", "REZE - 44400", "ST AIGNAN GRANDLIEU - 44860", "LA SALVETAT ST GILLES - 31880", "ST SEBASTIEN SUR LOIRE - 44230", "SAUTRON - 44880", "LES SORINIERES - 44840", "JUVIGNAC - 34990", "LAVERUNE - 34880", "MURVIEL LES MONTPELLIER - 34570", "TEYRAN - 34820", "VILLENEUVE LES MAGUELONE - 34750", "LE BOUSCAT - 33110", "BRUGES - 33520", "CADAUJAC - 33140", "EYSINES - 33320", "LATRESNE - 33360", "BALLAN MIRE - 37510", "CHAMBRAY LES TOURS - 37170", "FONDETTES - 37230", "JOUE LES TOURS - 37300", "TALENCE - 33400", "ST AVERTIN - 37550", "ST ROCH - 37390", "LA VILLE AUX DAMES - 37700", "FABREGUES - 34690", "BRESSON - 38320", "LATTES - 34970", "BEAUCOUZE - 49070", "BRIOLLAY - 49125",
                              "CORENC - 38700", "CANTENAY EPINARD - 49460", "ECHIROLLES - 38130", "VENDARGUES - 34740", "LES GARENNES SUR LOIRE - 49320", "LONGUENEE EN ANJOU - 49770", "MONTREUIL JUIGNE - 49460", "LES PONTS DE CE - 49130", "VERRIERES EN ANJOU - 49112", "VERRIERES EN ANJOU - 49480", "BELBERAUD - 31450", "BRAX - 31490", "COLOMIERS - 31770", "DAUX - 31700", "VENON - 38610", "LEGUEVIN - 31490", "PECHBUSQUE - 31320", "PIBRAC - 31820", "POMPERTUZAT - 31450", "REBIGUE - 31320", "CHANCEAUX SUR CHOISILLE - 37390", "CHARENTILLY - 37390", "ESVRES - 37320", "LA MEMBROLLE SUR CHOISILLE - 37390", "MONTLOUIS SUR LOIRE - 37270", "NOTRE DAME D OE - 37390", "PARCAY MESLAY - 37210", "VEIGNE - 37250", "ST MARTIN D HERES - 38400", "LE HAILLAN - 33185", "LE TAILLAN MEDOC - 33320", "CASTELNAU LE LEZ - 34170", "LE CRES - 34920", "ST GEORGES D ORQUES - 34680", "ST JEAN DE VEDAS - 34430", "SAUSSAN - 34570", "LA CHAPELLE SUR ERDRE - 44240", "PONT ST MARTIN - 44860", "LUYNES - 37230", "METTRAY - 37390", "LA RICHE - 37520", "VERETZ - 37270", "DOMENE - 38420", "EYBENS - 38320", "LA TRONCHE - 38700", "ORVAULT - 44700", "ST LEGER LES VIGNES - 44710", "STE LUCE SUR LOIRE - 44980", "ST MARS DU DESERT - 44850", "LONGUENEE EN ANJOU - 49220", "ST MARTIN DU FOUILLOUX - 49170", "AVRILLE - 49240", "CAMPHIN EN CAREMBAULT - 59133", "COMINES - 59560", "LE PLESSIS GRAMMOIRE - 49124", "ENGLOS - 59320", "ENNEVELIN - 59710", "ERQUINGHEM LE SEC - 59320", "ST MELAINE SUR AUBANCE - 49610", "SARRIGNE - 49800", "FACHES THUMESNIL - 59155", "BRAINS - 44830", "LAMBERSART - 59130", "LESQUIN - 59810", "LOOS - 59120", "MAUVES SUR LOIRE - 44470", "LA NEUVILLE - 59239", "NOYELLES LES SECLIN - 59139", "ST HERBLAIN - 44800", "RONCHIN - 59790", "THOUARE SUR LOIRE - 44470", "TREILLIERES - 44119", "SEQUEDIN - 59320", "TOURMIGNIES - 59551", "VENDEVILLE - 59175", "WASQUEHAL - 59290", "WATTIGNIES - 59139", "ECOUFLANT - 49000", "LES GARENNES SUR LOIRE - 49610", "MURS ERIGNE - 49610", "STE GEMMES SUR LOIRE - 49130", "ST LEGER DE LINIERES - 49170", "CARVIN - 62220", "VILLENEUVE D ASCQ - 59491", "ATTICHES - 59551", "BOUSBECQUE - 59166", "CAPINGHEM - 59160", "DEULEMONT - 59890", "HEM - 59510", "LOMPRET - 59840", "LYS LEZ LANNOY - 59390", "MARCQ EN BAROEUL - 59700", "PHALEMPIN - 59133", "PREMESQUES - 59840", "ST ANDRE LEZ LILLE - 59350", "TRESSIN - 59152", "WAMBRECHIES - 59118", "WARNETON - 59560", "DINGSHEIM - 67370", "ECKBOLSHEIM - 67201", "HANGENBIETEN - 67980", "ANSTAING - 59152", "HURTIGHEIM - 67117", "LAMPERTHEIM - 67450", "BONDUES - 59910", "MITTELHAUSBERGEN - 67206", "CHEMY - 59147", "EMMERIN - 59320", "FRETIN - 59273", "GONDECOURT - 59147", "HALLENNES LEZ HAUBOURDIN - 59320", "HAUBOURDIN - 59320", "LANNOY - 59390", "LINSELLES - 59126", "MARQUETTE LEZ LILLE - 59520", "MERIGNIES - 59710", "PONT A MARCQ - 59710", "SECLIN - 59113", "WILLEMS - 59780", "BRIGNAIS - 69530", "COLLONGES AU MONT D OR - 69660", "FRANCHEVILLE - 69340",
                              "MONTAGNY - 69700", "OULLINS - 69600", "TASSIN LA DEMI LUNE - 69160", "JONAGE - 69330", "ST PRIEST - 69800", "SATHONAY CAMP - 69580", "TOUSSIEU - 69780", "LIBERCOURT - 62820", "AVELIN - 59710", "BEAUCAMPS LIGNY - 59134", "CARNIN - 59112", "CROIX - 59170", "ENNETIERES EN WEPPES - 59320", "FOREST SUR MARQUE - 59510", "FOURNES EN WEPPES - 59134", "LA MADELEINE - 59110", "MONS EN BAROEUL - 59370", "PERENCHIES - 59840", "RONCQ - 59223", "SAILLY LEZ LANNOY - 59390", "TOUFFLERS - 59390", "BELBEUF - 76240", "BOSC GUERARD ST ADRIEN - 76710", "BOUVILLE - 76360", "BUTOT - 76890", "FRENEUSE - 76410", "GAINNEVILLE - 76700", "LE GRAND QUEVILLY - 76120", "LE HOULME - 76770", "MAROMME - 76150", "MESNIL PANNEVILLE - 76570", "BREUSCHWICKERSHEIM - 67112", "OISSEL - 76350", "ORIVAL - 76500", "GRIESHEIM SUR SOUFFEL - 67370", "HANDSCHUHEIM - 67117", "HOENHEIM - 67800", "VAL DE LA HAYE - 76380", "VENDENHEIM - 67550", "WIWERSHEIM - 67370", "WOLFISHEIM - 67202", "BRINDAS - 69126", "CAILLOUX SUR FONTAINES - 69270", "CALUIRE ET CUIRE - 69300", "CHAMPAGNE AU MONT D OR - 69410", "CHAPONOST - 69630", "FLEURIEU SUR SAONE - 69250", "FONTAINES ST MARTIN - 69270", "GREZIEU LA VARENNE - 69290", "LENTILLY - 69210", "LIMONEST - 69760", "LISSIEU - 69380", "SOUCIEU EN JARREST - 69510", "ST GENIS LES OLLIERES - 69290", "OIGNIES - 62590", "LA TOUR DE SALVAGNY - 69890", "VERNAISON - 69390", "DECINES CHARPIEU - 69150", "MONTANAY - 69250", "PUSIGNAN - 69330", "ST BONNET DE MURE - 69720", "ACHENHEIM - 67204", "ENTZHEIM - 67960", "ITTENHEIM - 67117", "MUNDOLSHEIM - 67450", "OBERHAUSBERGEN - 67205", "OBERSCHAEFFOLSHEIM - 67203", "QUATZENHEIM - 67117", "REICHSTETT - 67116", "SCHILTIGHEIM - 67300", "SOUFFELWEYERSHEIM - 67460", "STUTZHEIM OFFENHEIM - 67370", "LA WANTZENAU - 67610", "CHARLY - 69390", "COUZON AU MONT D OR - 69270", "IRIGNY - 69540", "LA MULATIERE - 69350", "ROCHETAILLEE SUR SAONE - 69270", "ST CYR AU MONT D OR - 69450", "ST DIDIER AU MONT D OR - 69370", "TALUYERS - 69440", "CORBAS - 69960", "JONS - 69330", "MEYZIEU - 69330", "MIONS - 69780", "RILLIEUX LA PAPE - 69140", "BISCHHEIM - 67800", "LES AUTHIEUX SUR LE PORT ST OUEN - 76520", "HOERDT - 67720", "HOLTZHEIM - 67810", "KOLBSHEIM - 67120", "LINGOLSHEIM - 67380", "ESLETTES - 76710", "PFULGRIESHEIM - 67370", "HARFLEUR - 76700", "HAUTOT SUR SEINE - 76113", "HERMEVILLE - 76280", "LIMESY - 76570", "NOTRE DAME DE BONDEVILLE - 76960", "PETIT COURONNE - 76650", "STE AUSTREBERTHE - 76570", "ST LAURENT DE BREVEDENT - 76700", "ST MARTIN DU BEC - 76133", "ST PIERRE DE VARENGEVILLE - 76480", "VILLENEUVE D ASCQ - 59493", "VILLENEUVE D ASCQ - 59650", "SOTTEVILLE SOUS LE VAL - 76410",
                              "VILLERS ECALLES - 76360", "ESCOBECQUES - 59320", "FRELINGHIEN - 59236", "HALLUIN - 59250", "BRON - 69500", "CHARBONNIERES LES BAINS - 69260", "HOUPLIN ANCOISNE - 59263", "CRAPONNE - 69290", "DOMMARTIN - 69380", "LEERS - 59115", "LEZENNES - 59260", "MARCY L ETOILE - 69280", "MESSIMY - 69510", "MILLERY - 69390", "MOUVAUX - 59420", "NEUVILLE EN FERRAIN - 59960", "POLEYMIEUX AU MONT D OR - 69250", "STE CONSORCE - 69280", "ST GENIS LAVAL - 69230", "QUESNOY SUR DEULE - 59890", "ROUBAIX - 59100", "VAULX EN VELIN - 69120", "VENISSIEUX - 69200", "VILLEURBANNE - 69100", "CHASSIEU - 69680", "GENAS - 69740", "SANTES - 59211", "ST PIERRE DE CHANDIEU - 69780", "SOLAIZE - 69360", "TEMPLEMARS - 59175", "TOURCOING - 59200", "VERLINGHEM - 59237", "WATTRELOS - 59150", "AMFREVILLE LA MI VOIE - 76920", "ANCEAUMEVILLE - 76710", "BARENTIN - 76360", "BLACQUEVILLE - 76190", "CAUVILLE SUR MER - 76930", "CLEON - 76410", "CROIX MARE - 76190", "DARNETAL - 76160", "EMANVILLE - 76570", "FONTAINE LA MALLET - 76290", "GONFREVILLE L ORCHER - 76700", "GOUY - 76520", "HOUPPEVILLE - 76770", "ISNEAUVILLE - 76230", "LA LONDE - 76500", "MANNEVILLETTE - 76290", "MONTIGNY - 76380", "MONTVILLE - 76710", "PISSY POVILLE - 76360", "ROGERVILLE - 76700", "ROLLEVILLE - 76133", "ST AUBIN CELLOVILLE - 76520", "ST LEGER DU BOURG DENIS - 76160", "ST MARTIN DU MANOIR - 76290", "ST PAER - 76480", "ST SATURNIN LES AVIGNON - 84450", "BOIS GUILLAUME - 76230", "LA BOUILLE - 76530", "CAUDEBEC LES ELBEUF - 76320", "CIDEVILLE - 76570", "FRESQUIENNES - 76570", "GOUPILLIERES - 76570", "GRAND COURONNE - 76530", "HENOUVILLE - 76840", "HUGLEVILLE EN CAUX - 76570", "MANEGLISE - 76133", "MONTIVILLIERS - 76290", "MOULINEAUX - 76530", "OUDALLE - 76430", "LE PETIT QUEVILLY - 76140", "ROUMARE - 76480", "SAINNEVILLE - 76430", "ST JEAN DU CARDONNAY - 76150", "ST MARTIN DE BOSCHERVILLE - 76840", "SIERVILLE - 76690", "TOURVILLE LA RIVIERE - 76410", "ERNOLSHEIM BRUCHE - 67120", "VEDENE - 84270", "NIEDERHAUSBERGEN - 67207", "OSTWALD - 67540", "ENTRAIGUES SUR LA SORGUE - 84320"))
names(data)[10] <- "Q01"
# LimeSurvey Field type: A
data[, 11] <- as.character(data[, 11])
attributes(data)$variable.labels[11] <- "[Autre] Quelle est votre commune (code postal et nom) de résidence :"
names(data)[11] <- "Q01_other"
# LimeSurvey Field type: A
data[, 12] <- as.character(data[, 12])
attributes(data)$variable.labels[12] <- "Vous êtes :"
data[, 12] <- factor(data[, 12], levels=c("A1","A2","A3"),labels=c("Femme", "Homme", "Préfère ne pas répondre"))
names(data)[12] <- "Q01b"
# LimeSurvey Field type: A
data[, 13] <- as.character(data[, 13])
attributes(data)$variable.labels[13] <- "[Autre] Vous êtes :"
names(data)[13] <- "Q01b_other"
# LimeSurvey Field type: A
data[, 14] <- as.character(data[, 14])
attributes(data)$variable.labels[14] <- "Dans quelle tranche d’âge vous situez-vous ?"
data[, 14] <- factor(data[, 14], levels=c("A1","A2","A3","A4"),labels=c("18 à 29 ans", "30 à 44 ans", "45 à 59 ans", "60 ans et plus"))
names(data)[14] <- "Q01c"
# LimeSurvey Field type: A
data[, 15] <- as.character(data[, 15])
attributes(data)$variable.labels[15] <- "Dans quelle catégorie socio-professionnelle êtes-vous ?"
data[, 15] <- factor(data[, 15], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9"),labels=c("Agriculteurs", "Artisans, commerçants et chefs d\'entreprise", "Cadres et professions intellectuelles supérieures (professions libérales, cadres administratifs et techniques de la fonc", "Professions intermédiaires (professions de l\'enseignement primaire et professionnel et du sport, professions intermédiai", "Employés (employés administratifs de la fonction publique, agents de service et auxiliaires de santé, policiers, militai", "Ouvriers et conducteurs de transport", "Étudiants", "Retraités", "Sans emploi"))
names(data)[15] <- "Q01d"
# LimeSurvey Field type: A
data[, 16] <- as.character(data[, 16])
attributes(data)$variable.labels[16] <- "Vous avez été ciblé par cette enquête car vous vous situez dans une zone où un tramway existe déjà ou est en projet. Dans l’hypothèse où ce tramway serait accessible depuis votre domicile à moins de 15 minutes environ en voiture ou 30 minutes en vélo de votre domicile, l’utiliseriez-vous ?   "
data[, 16] <- factor(data[, 16], levels=c("A2","A4"),labels=c("Oui", "Non"))
names(data)[16] <- "Q02"
# LimeSurvey Field type: A
data[, 17] <- as.character(data[, 17])
attributes(data)$variable.labels[17] <- "Si non, pourquoi ?"
names(data)[17] <- "Q03"
# LimeSurvey Field type: F
data[, 18] <- as.numeric(data[, 18])
attributes(data)$variable.labels[18] <- "[professionnelle (y compris pour suivre ses études).] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 18] <- factor(data[, 18], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[18] <- "QI4_SQ002"
# LimeSurvey Field type: F
data[, 19] <- as.numeric(data[, 19])
attributes(data)$variable.labels[19] <- "[participation à la vie associative.] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 19] <- factor(data[, 19], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[19] <- "QI4_SQ003"
# LimeSurvey Field type: F
data[, 20] <- as.numeric(data[, 20])
attributes(data)$variable.labels[20] <- "[activités domestiques (courses, rendez-vous médicaux, animal de compagnie, etc.).] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 20] <- factor(data[, 20], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[20] <- "QI4_SQ004"
# LimeSurvey Field type: F
data[, 21] <- as.numeric(data[, 21])
attributes(data)$variable.labels[21] <- "[loisirs (activités sportives, sociales, artistiques, culturelles, shopping, etc.).] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 21] <- factor(data[, 21], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[21] <- "QI4_SQ005"
# LimeSurvey Field type: F
data[, 22] <- as.numeric(data[, 22])
attributes(data)$variable.labels[22] <- "[accompagnement/transport des membres de votre foyer pour l’une des raisons pré-citées.] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 22] <- factor(data[, 22], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[22] <- "QI4_SQ006"
# LimeSurvey Field type: A
data[, 23] <- as.character(data[, 23])
attributes(data)$variable.labels[23] <- "[Autre] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
names(data)[23] <- "QI4_other"
# LimeSurvey Field type: F
data[, 24] <- as.numeric(data[, 24])
attributes(data)$variable.labels[24] <- "[Raison professionnelle (y compris pour suivre ses études).] Combien de temps mettez-vous en moyenne pour réaliser ce trajet (aller simple) ?  Indiquez le temps (en minutes) dédié à chacune des raisons sélectionnées ci-dessus dans votre trajet et vérifiez que le temps total est correct."
names(data)[24] <- "QI5_SQ002"
# LimeSurvey Field type: F
data[, 25] <- as.numeric(data[, 25])
attributes(data)$variable.labels[25] <- "[Participation à la vie associative.] Combien de temps mettez-vous en moyenne pour réaliser ce trajet (aller simple) ?  Indiquez le temps (en minutes) dédié à chacune des raisons sélectionnées ci-dessus dans votre trajet et vérifiez que le temps total est correct."
names(data)[25] <- "QI5_SQ003"
# LimeSurvey Field type: F
data[, 26] <- as.numeric(data[, 26])
attributes(data)$variable.labels[26] <- "[Activités domestiques (courses, rendez-vous médicaux, animal de compagnie, etc.).] Combien de temps mettez-vous en moyenne pour réaliser ce trajet (aller simple) ?  Indiquez le temps (en minutes) dédié à chacune des raisons sélectionnées ci-dessus dans votre trajet et vérifiez que le temps total est correct."
names(data)[26] <- "QI5_SQ004"
# LimeSurvey Field type: F
data[, 27] <- as.numeric(data[, 27])
attributes(data)$variable.labels[27] <- "[Loisirs (activités sportives, sociales, artistiques, culturelles, shopping).] Combien de temps mettez-vous en moyenne pour réaliser ce trajet (aller simple) ?  Indiquez le temps (en minutes) dédié à chacune des raisons sélectionnées ci-dessus dans votre trajet et vérifiez que le temps total est correct."
names(data)[27] <- "QI5_SQ005"
# LimeSurvey Field type: F
data[, 28] <- as.numeric(data[, 28])
attributes(data)$variable.labels[28] <- "[Accompagnement/transport des membres de votre foyer pour l’une de ces raisons (école, travail, etc.).] Combien de temps mettez-vous en moyenne pour réaliser ce trajet (aller simple) ?  Indiquez le temps (en minutes) dédié à chacune des raisons sélectionnées ci-dessus dans votre trajet et vérifiez que le temps total est correct."
names(data)[28] <- "QI5_SQ006"
# LimeSurvey Field type: A
data[, 29] <- as.character(data[, 29])
attributes(data)$variable.labels[29] <- "[Classement 1] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
data[, 29] <- factor(data[, 29], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("à pied", "trottinette", "vélo", "bus", "métro / RER métropolitain", "train (TER / Intercité / TGV)", "tramway", "moto, scooter", "voiture"))
names(data)[29] <- "QI6_1"
# LimeSurvey Field type: A
data[, 30] <- as.character(data[, 30])
attributes(data)$variable.labels[30] <- "[Classement 2] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
data[, 30] <- factor(data[, 30], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("à pied", "trottinette", "vélo", "bus", "métro / RER métropolitain", "train (TER / Intercité / TGV)", "tramway", "moto, scooter", "voiture"))
names(data)[30] <- "QI6_2"
# LimeSurvey Field type: A
data[, 31] <- as.character(data[, 31])
attributes(data)$variable.labels[31] <- "[Classement 3] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
data[, 31] <- factor(data[, 31], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("à pied", "trottinette", "vélo", "bus", "métro / RER métropolitain", "train (TER / Intercité / TGV)", "tramway", "moto, scooter", "voiture"))
names(data)[31] <- "QI6_3"
# LimeSurvey Field type: A
data[, 32] <- as.character(data[, 32])
attributes(data)$variable.labels[32] <- "[Classement 4] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
data[, 32] <- factor(data[, 32], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("à pied", "trottinette", "vélo", "bus", "métro / RER métropolitain", "train (TER / Intercité / TGV)", "tramway", "moto, scooter", "voiture"))
names(data)[32] <- "QI6_4"
# LimeSurvey Field type: A
data[, 33] <- as.character(data[, 33])
attributes(data)$variable.labels[33] <- "[Classement 5] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
data[, 33] <- factor(data[, 33], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("à pied", "trottinette", "vélo", "bus", "métro / RER métropolitain", "train (TER / Intercité / TGV)", "tramway", "moto, scooter", "voiture"))
names(data)[33] <- "QI6_5"
# LimeSurvey Field type: A
data[, 34] <- as.character(data[, 34])
attributes(data)$variable.labels[34] <- "[Classement 6] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
data[, 34] <- factor(data[, 34], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("à pied", "trottinette", "vélo", "bus", "métro / RER métropolitain", "train (TER / Intercité / TGV)", "tramway", "moto, scooter", "voiture"))
names(data)[34] <- "QI6_6"
# LimeSurvey Field type: A
data[, 35] <- as.character(data[, 35])
attributes(data)$variable.labels[35] <- "[Classement 7] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
data[, 35] <- factor(data[, 35], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("à pied", "trottinette", "vélo", "bus", "métro / RER métropolitain", "train (TER / Intercité / TGV)", "tramway", "moto, scooter", "voiture"))
names(data)[35] <- "QI6_7"
# LimeSurvey Field type: A
data[, 36] <- as.character(data[, 36])
attributes(data)$variable.labels[36] <- "[Classement 8] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
data[, 36] <- factor(data[, 36], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("à pied", "trottinette", "vélo", "bus", "métro / RER métropolitain", "train (TER / Intercité / TGV)", "tramway", "moto, scooter", "voiture"))
names(data)[36] <- "QI6_8"
# LimeSurvey Field type: A
data[, 37] <- as.character(data[, 37])
attributes(data)$variable.labels[37] <- "[Classement 9] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
data[, 37] <- factor(data[, 37], levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("à pied", "trottinette", "vélo", "bus", "métro / RER métropolitain", "train (TER / Intercité / TGV)", "tramway", "moto, scooter", "voiture"))
names(data)[37] <- "QI6_9"
# LimeSurvey Field type: A
data[, 38] <- as.character(data[, 38])
attributes(data)$variable.labels[38] <- "Le moyen de transport individuel que vous utilisez est-il partagé ?"
data[, 38] <- factor(data[, 38], levels=c("A2","A3","A4","A5"),labels=c("Non (véhicule individuel et usage individuel)", "Oui, en co-voiturage (foyer ou collègues/voisins)", "Oui, véhicule de location/abonnement", "Oui, véhicule de location/abonnement et en co-voiturage"))
names(data)[38] <- "QI7"
# LimeSurvey Field type: F
data[, 39] <- as.numeric(data[, 39])
attributes(data)$variable.labels[39] <- "[professionnelle (y compris pour suivre ses études).] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 39] <- factor(data[, 39], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[39] <- "QI8_SQ001"
# LimeSurvey Field type: F
data[, 40] <- as.numeric(data[, 40])
attributes(data)$variable.labels[40] <- "[participation à la vie associative.] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 40] <- factor(data[, 40], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[40] <- "QI8_SQ002"
# LimeSurvey Field type: F
data[, 41] <- as.numeric(data[, 41])
attributes(data)$variable.labels[41] <- "[activités domestiques (courses, rendez-vous médicaux, animal de compagnie, etc.).] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 41] <- factor(data[, 41], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[41] <- "QI8_SQ003"
# LimeSurvey Field type: F
data[, 42] <- as.numeric(data[, 42])
attributes(data)$variable.labels[42] <- "[loisirs (activités sportives, sociales, artistiques, culturelles, shopping, etc.).] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 42] <- factor(data[, 42], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[42] <- "QI8_SQ004"
# LimeSurvey Field type: F
data[, 43] <- as.numeric(data[, 43])
attributes(data)$variable.labels[43] <- "[accompagnement/transport des membres de votre foyer pour l’une des raisons pré-citées.] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 43] <- factor(data[, 43], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[43] <- "QI8_SQ005"
# LimeSurvey Field type: A
data[, 44] <- as.character(data[, 44])
attributes(data)$variable.labels[44] <- "[Autre] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
names(data)[44] <- "QI8_other"
# LimeSurvey Field type: A
data[, 45] <- as.character(data[, 45])
attributes(data)$variable.labels[45] <- "Avez-vous bien visualisé la vidéo ? Si oui, vous allez maintenant répondre à une carte de choix exemple pour vous familiariser avec l\'exercice, puis aux 12 cartes de choix."
data[, 45] <- factor(data[, 45], levels=c("A1"),labels=c("Oui"))
names(data)[45] <- "Qvid"
# LimeSurvey Field type: A
data[, 46] <- as.character(data[, 46])
attributes(data)$variable.labels[46] <- "[Scénario choisi :]"
data[, 46] <- factor(data[, 46], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[46] <- "QCEex_SQ001"
# LimeSurvey Field type: A
data[, 47] <- as.character(data[, 47])
attributes(data)$variable.labels[47] <- "Avez-vous bien compris l\'exercice de choix ? Après avoir cliqué sur Suivant, vous allez devoir répondre à 12 cartes similaires à celle qui vient d\'être présentée en exemple, avec à chaque fois 2 scénarios qui combinent 4 attributs impactant le temps de trajet, et 1 scénario de référence qui n\'implique pas de temps de trajet supplémentaire."
data[, 47] <- factor(data[, 47], levels=c("A1","A2","A3","A4"),labels=c("Oui, j\'ai parfaitement compris l\'exercice", "Oui, je pense avoir compris l\'exercice", "Non, je ne pense pas avoir compris l\'exercice", "Non, je n\'ai pas du tout compris l\'exercice"))
names(data)[47] <- "QCEex2"
# LimeSurvey Field type: A
data[, 48] <- as.character(data[, 48])
attributes(data)$variable.labels[48] <- "[Scénario choisi]"
data[, 48] <- factor(data[, 48], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[48] <- "QCE1_SQ001"
# LimeSurvey Field type: A
data[, 49] <- as.character(data[, 49])
attributes(data)$variable.labels[49] <- "[Scénario choisi]"
data[, 49] <- factor(data[, 49], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[49] <- "QCE2_SQ001"
# LimeSurvey Field type: A
data[, 50] <- as.character(data[, 50])
attributes(data)$variable.labels[50] <- "[Scénario choisi]"
data[, 50] <- factor(data[, 50], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[50] <- "QCE3_SQ001"
# LimeSurvey Field type: A
data[, 51] <- as.character(data[, 51])
attributes(data)$variable.labels[51] <- "[Scénario choisi]"
data[, 51] <- factor(data[, 51], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[51] <- "QCE4_SQ001"
# LimeSurvey Field type: A
data[, 52] <- as.character(data[, 52])
attributes(data)$variable.labels[52] <- "[Scénario choisi]"
data[, 52] <- factor(data[, 52], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[52] <- "QCE5_SQ001"
# LimeSurvey Field type: A
data[, 53] <- as.character(data[, 53])
attributes(data)$variable.labels[53] <- "[Scénario choisi]"
data[, 53] <- factor(data[, 53], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[53] <- "QCE6_SQ001"
# LimeSurvey Field type: A
data[, 54] <- as.character(data[, 54])
attributes(data)$variable.labels[54] <- "[Scénario choisi]"
data[, 54] <- factor(data[, 54], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[54] <- "QCE7_SQ001"
# LimeSurvey Field type: A
data[, 55] <- as.character(data[, 55])
attributes(data)$variable.labels[55] <- "[Scénario choisi]"
data[, 55] <- factor(data[, 55], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[55] <- "QCE8_SQ001"
# LimeSurvey Field type: A
data[, 56] <- as.character(data[, 56])
attributes(data)$variable.labels[56] <- "[Scénario choisi]"
data[, 56] <- factor(data[, 56], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[56] <- "QCE9_SQ001"
# LimeSurvey Field type: A
data[, 57] <- as.character(data[, 57])
attributes(data)$variable.labels[57] <- "[Scénario choisi]"
data[, 57] <- factor(data[, 57], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[57] <- "QCE10_SQ001"
# LimeSurvey Field type: A
data[, 58] <- as.character(data[, 58])
attributes(data)$variable.labels[58] <- "[Scénario choisi]"
data[, 58] <- factor(data[, 58], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[58] <- "QCE11_SQ001"
# LimeSurvey Field type: A
data[, 59] <- as.character(data[, 59])
attributes(data)$variable.labels[59] <- "[Scénario choisi]"
data[, 59] <- factor(data[, 59], levels=c("A1","A2","A3"),labels=c("Scénario 1", "Scénario 2", "Scénario de référence"))
names(data)[59] <- "QCE12_SQ001"
# LimeSurvey Field type: F
data[, 60] <- as.numeric(data[, 60])
attributes(data)$variable.labels[60] <- "[Chaque attribut a été important au moins une fois] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 60] <- factor(data[, 60], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[60] <- "QIII9_SQ002"
# LimeSurvey Field type: F
data[, 61] <- as.numeric(data[, 61])
attributes(data)$variable.labels[61] <- "[Paysage] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 61] <- factor(data[, 61], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[61] <- "QIII9_SQ003"
# LimeSurvey Field type: F
data[, 62] <- as.numeric(data[, 62])
attributes(data)$variable.labels[62] <- "[Richesse et abondance d\'espèces] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 62] <- factor(data[, 62], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[62] <- "QIII9_SQ004"
# LimeSurvey Field type: F
data[, 63] <- as.numeric(data[, 63])
attributes(data)$variable.labels[63] <- "[Type d\'espèces (urbaines, periurbaines, rurales)] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 63] <- factor(data[, 63], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[63] <- "QIII9_SQ005"
# LimeSurvey Field type: F
data[, 64] <- as.numeric(data[, 64])
attributes(data)$variable.labels[64] <- "[Accessibilité au parc] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 64] <- factor(data[, 64], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[64] <- "QIII9_SQ006"
# LimeSurvey Field type: F
data[, 65] <- as.numeric(data[, 65])
attributes(data)$variable.labels[65] <- "[Temps de transport supplémentaire] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 65] <- factor(data[, 65], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[65] <- "QIII9_SQ007"
# LimeSurvey Field type: F
data[, 66] <- as.numeric(data[, 66])
attributes(data)$variable.labels[66] <- "[Je n\'ai pas assez de temps donc je préfère le trajet le plus rapide.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 66] <- factor(data[, 66], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[66] <- "QIII10_SQ001"
# LimeSurvey Field type: F
data[, 67] <- as.numeric(data[, 67])
attributes(data)$variable.labels[67] <- "[L\'augmentation du temps de trajet est trop importante à l’échelle de mon trajet.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 67] <- factor(data[, 67], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[67] <- "QIII10_SQ002"
# LimeSurvey Field type: F
data[, 68] <- as.numeric(data[, 68])
attributes(data)$variable.labels[68] <- "[Je n\'aime pas le tramway (vous pourrez en détailler les raisons dans une question ultérieure).] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 68] <- factor(data[, 68], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[68] <- "QIII10_SQ003"
# LimeSurvey Field type: F
data[, 69] <- as.numeric(data[, 69])
attributes(data)$variable.labels[69] <- "[Les enjeux biodiversité ou de protection de la nature ne me concernent pas.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 69] <- factor(data[, 69], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[69] <- "QIII10_SQ004"
# LimeSurvey Field type: F
data[, 70] <- as.numeric(data[, 70])
attributes(data)$variable.labels[70] <- "[C\'est au gouvernement d\'agir pour la biodiversité, pas au citoyens.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 70] <- factor(data[, 70], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[70] <- "QIII10_SQ005"
# LimeSurvey Field type: F
data[, 71] <- as.numeric(data[, 71])
attributes(data)$variable.labels[71] <- "[Je n\'arrivais pas à bien distinguer les différences entre les scénarios.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 71] <- factor(data[, 71], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[71] <- "QIII10_SQ006"
# LimeSurvey Field type: A
data[, 72] <- as.character(data[, 72])
attributes(data)$variable.labels[72] <- "[Autre] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
names(data)[72] <- "QIII10_other"
# LimeSurvey Field type: F
data[, 73] <- as.numeric(data[, 73])
attributes(data)$variable.labels[73] <- "[Je n\'ai pas assez de temps donc je préfère le trajet le plus rapide.] Pour quelle(s) raison(s) avez-vous parfois préféré le scénario de référence (schéma du bas) ?"
data[, 73] <- factor(data[, 73], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[73] <- "QIII11_SQ001"
# LimeSurvey Field type: F
data[, 74] <- as.numeric(data[, 74])
attributes(data)$variable.labels[74] <- "[L\'augmentation du temps de trajet est trop importante à l’échelle de mon trajet.] Pour quelle(s) raison(s) avez-vous parfois préféré le scénario de référence (schéma du bas) ?"
data[, 74] <- factor(data[, 74], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[74] <- "QIII11_SQ002"
# LimeSurvey Field type: A
data[, 75] <- as.character(data[, 75])
attributes(data)$variable.labels[75] <- "[Autre] Pour quelle(s) raison(s) avez-vous parfois préféré le scénario de référence (schéma du bas) ?"
names(data)[75] <- "QIII11_other"
# LimeSurvey Field type: A
data[, 76] <- as.character(data[, 76])
attributes(data)$variable.labels[76] <- "[Note] Comment avez-vous trouvé cet exercice de sélection de choix ?"
data[, 76] <- factor(data[, 76], levels=c("A1","A2","A3","A5","A6"),labels=c("Très difficile", "Assez difficile", "Plutôt facile", "Très facile", "Je ne sais pas"))
names(data)[76] <- "QIII12_SQ001"
# LimeSurvey Field type: A
data[, 77] <- as.character(data[, 77])
attributes(data)$variable.labels[77] <- "[Note] Les scénarios proposés vont ont-ils semblé réalistes ?"
data[, 77] <- factor(data[, 77], levels=c("A1","A2","A3","A4","A5"),labels=c("Toujours réalistes", "Assez souvent réalistes", "Assez souvent peu réalistes", "Toujours peu réalistes", "Je ne sais pas"))
names(data)[77] <- "QIII13_SQ001"
# LimeSurvey Field type: A
data[, 78] <- as.character(data[, 78])
attributes(data)$variable.labels[78] <- "[Le tramway peut offrir une alternative de transport plus rapide.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 78] <- factor(data[, 78], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[78] <- "QIII15_SQ001"
# LimeSurvey Field type: A
data[, 79] <- as.character(data[, 79])
attributes(data)$variable.labels[79] <- "[Le tramway peut offrir une alternative de transport moins cher.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 79] <- factor(data[, 79], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[79] <- "QIII15_SQ002"
# LimeSurvey Field type: A
data[, 80] <- as.character(data[, 80])
attributes(data)$variable.labels[80] <- "[Le tramway peut offrir une alternative de transport plus écologique.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 80] <- factor(data[, 80], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[80] <- "QIII15_SQ003"
# LimeSurvey Field type: A
data[, 81] <- as.character(data[, 81])
attributes(data)$variable.labels[81] <- "[Le tramway peut offrir une alternative de transport plus pratique.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 81] <- factor(data[, 81], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[81] <- "QIII15_SQ004"
# LimeSurvey Field type: A
data[, 82] <- as.character(data[, 82])
attributes(data)$variable.labels[82] <- "[La préservation de la nature est un des enjeux majeurs de notre société] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 82] <- factor(data[, 82], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[82] <- "QIII15_SQ005"
# LimeSurvey Field type: A
data[, 83] <- as.character(data[, 83])
attributes(data)$variable.labels[83] <- "[Les changements de comportements individuels ont un rôle pour préserver la nature.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 83] <- factor(data[, 83], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[83] <- "QIII15_SQ006"
# LimeSurvey Field type: A
data[, 84] <- as.character(data[, 84])
attributes(data)$variable.labels[84] <- "[Je préfère l’autonomie et le confort du véhicule individuel aux transports collectifs.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 84] <- factor(data[, 84], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[84] <- "QIII14_SQ001"
# LimeSurvey Field type: A
data[, 85] <- as.character(data[, 85])
attributes(data)$variable.labels[85] <- "[Les transports collectifs comme le tramway ne sont pas fiables (ex : retards).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 85] <- factor(data[, 85], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[85] <- "QIII14_SQ002"
# LimeSurvey Field type: A
data[, 86] <- as.character(data[, 86])
attributes(data)$variable.labels[86] <- "[Je n’ai pas d’accès à une station de tramway facilement et/ou il n\'y a pas de solution pour garer ma voiture ou mon vélo à proximité.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 86] <- factor(data[, 86], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[86] <- "QIII14_SQ003"
# LimeSurvey Field type: A
data[, 87] <- as.character(data[, 87])
attributes(data)$variable.labels[87] <- "[Le tramway est trop lent.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 87] <- factor(data[, 87], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[87] <- "QIII14_SQ004"
# LimeSurvey Field type: A
data[, 88] <- as.character(data[, 88])
attributes(data)$variable.labels[88] <- "[Le tramway implique un itinéraire trop compliqué (ex : avec des changements).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 88] <- factor(data[, 88], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[88] <- "QIII14_SQ010"
# LimeSurvey Field type: A
data[, 89] <- as.character(data[, 89])
attributes(data)$variable.labels[89] <- "[L’utilisation du tramway n’est pas pratique dans mon organisation quotidienne (ex : transport de matériel, enfants, PMR, etc.).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 89] <- factor(data[, 89], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[89] <- "QIII14_SQ005"
# LimeSurvey Field type: A
data[, 90] <- as.character(data[, 90])
attributes(data)$variable.labels[90] <- "[Je ne pense pas que le développement des transports en commun soit une solution pour répondre aux enjeux climatiques.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 90] <- factor(data[, 90], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[90] <- "QIII14_SQ006"
# LimeSurvey Field type: A
data[, 91] <- as.character(data[, 91])
attributes(data)$variable.labels[91] <- "[Je n’aime pas prendre les transports en commun (ex :  proximité avec d’autres personnes, bruit, risque d’être debout, etc).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 91] <- factor(data[, 91], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[91] <- "QIII14_SQ007"
# LimeSurvey Field type: A
data[, 92] <- as.character(data[, 92])
attributes(data)$variable.labels[92] <- "[Les infrastructures de transport (routes, réseau ferré) n’ont pas d’impact sur la nature.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 92] <- factor(data[, 92], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[92] <- "QIII14_SQ008"
# LimeSurvey Field type: A
data[, 93] <- as.character(data[, 93])
attributes(data)$variable.labels[93] <- "[Les changements de comportements individuels ne permettront pas (à eux seuls) de conserver la nature.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 93] <- factor(data[, 93], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[93] <- "QIII14_SQ009"
# LimeSurvey Field type: A
data[, 94] <- as.character(data[, 94])
attributes(data)$variable.labels[94] <- "[Cet aménagement va créer encore plus de bouchons dans la ville.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
data[, 94] <- factor(data[, 94], levels=c("A1","A2","A3","A4","A5"),labels=c("Tout à fait d’accord", "Plutôt d’accord", "Plutôt pas d’accord", "Pas du tout d’accord", "Je ne sais pas"))
names(data)[94] <- "QIII14_SQ011"
# LimeSurvey Field type: F
data[, 95] <- as.numeric(data[, 95])
attributes(data)$variable.labels[95] <- "[Le développement d\'un tramway est déjà important.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 95] <- factor(data[, 95], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[95] <- "QIII16_SQ001"
# LimeSurvey Field type: F
data[, 96] <- as.numeric(data[, 96])
attributes(data)$variable.labels[96] <- "[Zones piétonnes étendues.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 96] <- factor(data[, 96], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[96] <- "QIII16_SQ002"
# LimeSurvey Field type: F
data[, 97] <- as.numeric(data[, 97])
attributes(data)$variable.labels[97] <- "[Itinéraires cyclables continus et protégés.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 97] <- factor(data[, 97], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[97] <- "QIII16_SQ003"
# LimeSurvey Field type: F
data[, 98] <- as.numeric(data[, 98])
attributes(data)$variable.labels[98] <- "[Bus à haut niveau de service (bénéficiant d’une voie réservée).] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 98] <- factor(data[, 98], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[98] <- "QIII16_SQ004"
# LimeSurvey Field type: F
data[, 99] <- as.numeric(data[, 99])
attributes(data)$variable.labels[99] <- "[Co-voiturage.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 99] <- factor(data[, 99], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[99] <- "QIII16_SQ005"
# LimeSurvey Field type: F
data[, 100] <- as.numeric(data[, 100])
attributes(data)$variable.labels[100] <- "[Augmenter la part de télétravail.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 100] <- factor(data[, 100], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[100] <- "QIII16_SQ006"
# LimeSurvey Field type: F
data[, 101] <- as.numeric(data[, 101])
attributes(data)$variable.labels[101] <- "[Changer de travail pour être plus proche de son domicile.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 101] <- factor(data[, 101], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[101] <- "QIII16_SQ007"
# LimeSurvey Field type: F
data[, 102] <- as.numeric(data[, 102])
attributes(data)$variable.labels[102] <- "[Repenser la commune pour diminuer le besoin de déplacement en voiture.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 102] <- factor(data[, 102], levels=c(1,0),labels=c("Oui", "Non sélectionné"))
names(data)[102] <- "QIII16_SQ008"
# LimeSurvey Field type: A
data[, 103] <- as.character(data[, 103])
attributes(data)$variable.labels[103] <- "[Autre] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
names(data)[103] <- "QIII16_other"
# LimeSurvey Field type: A
data[, 104] <- as.character(data[, 104])
attributes(data)$variable.labels[104] <- "Quelle connaissance avez-vous du projet d\'extention de tramway dans votre commune ou les communes voisines ?"
data[, 104] <- factor(data[, 104], levels=c("A1","A2","A3"),labels=c("Je ne suis pas au courant de ce projet.", "J\'ai entendu parler de ce projet mais sans plus d\'information.", "Je connais bien ce projet."))
names(data)[104] <- "QIV18"
# LimeSurvey Field type: A
data[, 105] <- as.character(data[, 105])
attributes(data)$variable.labels[105] <- "Quel est votre niveau d’étude ?"
data[, 105] <- factor(data[, 105], levels=c("A1","A2","A3","A4"),labels=c("Primaire (certificat d’études)", "Secondaire court (CAP, BEP) ou niveau baccalauréat", "Secondaire long (Technicien)", "Supérieur (Ingénieur, Master, Doctorat)"))
names(data)[105] <- "QIV19"
# LimeSurvey Field type: F
data[, 106] <- as.numeric(data[, 106])
attributes(data)$variable.labels[106] <- "[Adultes (vous compris) :] Combien d’adultes et d’enfants vivent dans votre ménage ?"
names(data)[106] <- "QIV21_SQ001"
# LimeSurvey Field type: F
data[, 107] <- as.numeric(data[, 107])
attributes(data)$variable.labels[107] <- "[Enfants (0-10 ans) :] Combien d’adultes et d’enfants vivent dans votre ménage ?"
names(data)[107] <- "QIV21_SQ002"
# LimeSurvey Field type: F
data[, 108] <- as.numeric(data[, 108])
attributes(data)$variable.labels[108] <- "[Enfants (11-17 ans) :] Combien d’adultes et d’enfants vivent dans votre ménage ?"
names(data)[108] <- "QIV21_SQ003"
# LimeSurvey Field type: A
data[, 109] <- as.character(data[, 109])
attributes(data)$variable.labels[109] <- "Dans quelle tranche de revenus disponibles (revenus d\'activité nets des cotisations sociales, indemnités de chômage, retraites et pensions, revenus du patrimoine) se situe votre ménage ?"
data[, 109] <- factor(data[, 109], levels=c("A1","A2","A3","A4","A5","A6","A7","A8","A9","A10"),labels=c("Moins de 1200 €", "Entre 1200 et 1500 €", "Entre 1500 et 1800 €", "Entre 1800 et 2100 €", "Entre 2100 et 2600 €", "Entre 2600 et 3100 €", "Entre 3100 et 3500 €", "Entre 3500 et 4200 €", "Entre 4200 et 5400 €", "Plus de 5400 €"))
names(data)[109] <- "QIV22"
# LimeSurvey Field type: A
data[, 110] <- as.character(data[, 110])
attributes(data)$variable.labels[110] <- "Faites-vous ou avez-vous fait partie d’un collectif ou d’une association dont l’objectif est/était la protection de la nature (si oui, vous pouvez préciser l’objectif en question) ?"
data[, 110] <- factor(data[, 110], levels=c("A1","A2"),labels=c("Oui", "Non"))
names(data)[110] <- "QIV23"
# LimeSurvey Field type: A
data[, 111] <- as.character(data[, 111])
attributes(data)$variable.labels[111] <- "[Commentaire] Faites-vous ou avez-vous fait partie d’un collectif ou d’une association dont l’objectif est/était la protection de la nature (si oui, vous pouvez préciser l’objectif en question) ?"
names(data)[111] <- "QIV23_comment"
# LimeSurvey Field type: A
data[, 112] <- as.character(data[, 112])
attributes(data)$variable.labels[112] <- "Pratiquez-vous régulièrement des activités de pleine nature (ex : randonnée, cueillette de champignons, chasse/pêche, etc.) (si oui, vous pouvez préciser l’activité) ?"
data[, 112] <- factor(data[, 112], levels=c("A1","A2"),labels=c("Oui", "Non"))
names(data)[112] <- "QIV24"
# LimeSurvey Field type: A
data[, 113] <- as.character(data[, 113])
attributes(data)$variable.labels[113] <- "[Commentaire] Pratiquez-vous régulièrement des activités de pleine nature (ex : randonnée, cueillette de champignons, chasse/pêche, etc.) (si oui, vous pouvez préciser l’activité) ?"
names(data)[113] <- "QIV24_comment"
# LimeSurvey Field type: A
data[, 114] <- as.character(data[, 114])
attributes(data)$variable.labels[114] <- "Portez-vous une attention particulière aux critères environnementaux lors de vos achats (ex : agriculture biologique, provenance locale, etc.) (si oui, vous pouvez précise le critère) ?"
data[, 114] <- factor(data[, 114], levels=c("A1","A2"),labels=c("Oui", "Non"))
names(data)[114] <- "QIV25"
# LimeSurvey Field type: A
data[, 115] <- as.character(data[, 115])
attributes(data)$variable.labels[115] <- "[Commentaire] Portez-vous une attention particulière aux critères environnementaux lors de vos achats (ex : agriculture biologique, provenance locale, etc.) (si oui, vous pouvez précise le critère) ?"
names(data)[115] <- "QIV25_comment"
# LimeSurvey Field type: A
data[, 116] <- as.character(data[, 116])
attributes(data)$variable.labels[116] <- "Quel est votre degré de connaissance sur la nature en général ?"
data[, 116] <- factor(data[, 116], levels=c("A2","A3","A4"),labels=c("Limité", "Moyen", "Élevé"))
names(data)[116] <- "QIV26"
# LimeSurvey Field type: A
data[, 117] <- as.character(data[, 117])
attributes(data)$variable.labels[117] <- "En moyenne, à quelle fréquence estimez-vous être en contact avec la nature ?"
data[, 117] <- factor(data[, 117], levels=c("A2","A3","A4","A5"),labels=c("Tous les jours", "Une ou plusieurs fois par semaine", "Une ou plusieurs fois par mois", "Rarement ou jamais"))
names(data)[117] <- "QIV27"
# LimeSurvey Field type: A
data[, 118] <- as.character(data[, 118])
attributes(data)$variable.labels[118] <- "[Numéro de la figure] Dans l\'image ci-dessous, quelle figure décrit le mieux votre relation avec la Nature :"
data[, 118] <- factor(data[, 118], levels=c("A2","A3","A4","A5","A6","A7","A8"),labels=c("1", "2", "3", "4", "5", "6", "7"))
names(data)[118] <- "QVnew_SQ001"
# LimeSurvey Field type: A
data[, 119] <- as.character(data[, 119])
attributes(data)$variable.labels[119] <- "Commune (code postal et nom) où se situe votre activité principale :"
data[, 119] <- factor(data[, 119],
                      levels=c("A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","A13","A14","A15","A16","A17","A18","A19","A20","A21","A22","A23","A24","A25","A26","A27","A28","A29","A30","A31","A32","A33","A34","A35","A36","A37","A38","A39","A40","A41","A42","A43","A44","A45","A46","A47","A48","A49","A50","A51","A52","A53","A54","A55","A56","A57","A58","A59","A60","A61","A62","A63","A64","A65","A66","A67","A68","A69","A70","A71","A72","A73","A74","A75","A76","A77","A78","A79","A80","A81","A82","A83","A84","A85","A86","A87","A88","A89","A90","A91","A92","A93","A94","A95","A96","A97","A98","A99","A100","A101","A102","A103","A104","A105","A106","A107","A108","A109","A110","A111","A112","A113","A114","A115","A116","A117","A118","A119","A120","A121","A122","A123","A124","A125","A126","A127","A128","A129","A130","A131","A132","A133","A134","A135","A136","A137","A138","A139","A140","A141","A142","A143","A144","A145","A146","A147","A148","A149","A150","A151","A152","A153","A154","A155","A156","A157","A158","A159","A160","A161","A162","A163","A164","A165","A166","A167","A168","A169","A170","A171","A172","A173","A174","A175","A176","A177","A178","A179","A180","A181","A182","A183","A184","A185","A186","A187","A188","A189","A190","A191","A192","A193","A194","A195","A196","A197","A198","A199","A200","A201","A202","A203","A204","A205","A206","A207","A208","A209","A210","A211","A212","A213","A214","A215","A216","A217","A218","A219","A220","A221","A222","A223","A224","A225","A226","A227","A228","A229","A230","A231","A232","A233","A234","A235","A236","A237","A238","A239","A240","A241","A242","A243","A244","A245","A246","A247","A248","A249","A250","A251","A252","A253","A254","A255","A256","A257","A258","A259","A260","A261","A262","A263","A264","A265","A266","A267","A268","A269","A270","A271","A272","A273","A274","A275","A276","A277","A278","A279","A280","A281","A282","A283","A284","A285","A286","A287","A288","A289","A290","A291","A292","A293","A294","A295","A296","A297","A298","A299","A300","A301","A302","A303","A304","A305","A306","A307","A308","A309","A310","A311","A312","A313","A314","A315","A316","A317","A318","A319","A320","A321","A322","A323","A324","A325","A326","A327","A328","A329","A330","A331","A332","A333","A334","A335","A336","A337","A338","A339","A340","A341","A342","A343","A344","A345","A346","A347","A348","A349","A350","A351","A352","A353","A354","A355","A356","A357","A358","A359","A360","A361","A362","A363","A364","A365","A366","A367","A368","A369","A370","A371","A372","A373","A374","A375","A376","A377","A378","A379","A380","A381","A382","A383","A384","A385","A386","A387","A388","A389","A390","A391","A392","A393","A394","A395","A396","A397","A398","A399","A400","A401","A402","A403","A404","A405","A406","A407","A408","A409","A410","A411","A412","A413","A414","A415","A416","A417","A418","A419","A420","A421","A422","A423","A424","A425","A426","A427","A428","A429","A430","A431","A432","A433","A434","A435","A436","A437","A438","A439","A440","A441","A442","A443","A444","A445","A446","A447","A448","A449","A450","A451","A452","A453","A454","A455","A456","A457","A458","A459","A460","A461","A462","A463","A464","A465","A466","A467","A468","A469","A470","A471","A472","A473","A474","A475","A476","A477","A478","A479","A480","A481","A482","A483","A484","A485","A486","A487","A488","A489","A490","A491","A492","A493","A494","A495","A496","A497","A498","A499","A500","A501","A502","A503","A504","A505","A506","A507","A508","A509","A510","A511","A512","A513","A514","A515","A516","A517","A518","A519","A520","A521","A522","A523","A524","A525","A526","A527","A528","A529","A530","A531","A532","A533","A534","A535","A536","A537","A538","A539","A540","A541","A542","A543","A544","A545","A546","A547","A548","A549","A550","A551","A552","A553","A554","A555","A556","A557","A558","A559","A560","A561","A562","A563","A564","A565","A566","A567","A568","A569","A570","A571","A572","A573","A574","A575","A576","A577","A578","A579","A580","A581","A582","A583","A584"),
                      labels=c("ALIZAY - 27460", "AMFREVILLE ST AMAND - 27370", "LES MONTS DU ROUMOIS - 27520", "CRIQUEBEUF SUR SEINE - 27340", "LES DAMPS - 27340", "HONGUEMARE GUENOUVILLE - 27310", "MANDEVILLE - 27370", "TERRES DE BORD - 27340", "ST OUEN DE THOUBERVILLE - 27310", "LA SAUSSAYE - 27370", "LE THUIT DE L OISON - 27370", "TOURVILLE LA CAMPAGNE - 27370", "DEYME - 31450", "DONNEVILLE - 31450", "GAGNAC SUR GARONNE - 31150", "PECHABOU - 31320", "RAMONVILLE ST AGNE - 31520", "ST ORENS DE GAMEVILLE - 31650", "TOURNEFEUILLE - 31170", "AYGUEMORTE LES GRAVES - 33640", "FLOIRAC - 33270", "CLAPIERS - 34830", "MONTFERRIER SUR LEZ - 34980", "PRADES LE LEZ - 34730", "ST CLEMENT DE RIVIERE - 34980", "ST GELY DU FESC - 34980", "PERNAY - 37230", "BIVIERS - 38330", "MEYLAN - 38240", "POISAT - 38320", "ST MARTIN LE VINOUX - 38950", "INDRE - 44610", "ST JEAN DE BOISEAU - 44640", "BOUCHEMAINE - 49080", "CORZE - 49140", "ST BARTHELEMY D ANJOU - 49124", "ST JEAN DE LA CROIX - 49130", "ST LAMBERT LA POTHERIE - 49070", "ST LEGER DE LINIERES - 49070", "TRELAZE - 49800", "RIVES DU LOIR EN ANJOU - 49140", "MIONNAY - 01390", "MIRIBEL - 01700", "PREVESSIN MOENS - 01280", "VERSONNEX - 01210", "CAGNES SUR MER - 06800", "LA COLLE SUR LOUP - 06480", "TOURRETTE LEVENS - 06690", "PEYPIN - 13124", "ROQUEVAIRE - 13360", "ANISY - 14610", "LE FRESNE CAMILLY - 14480", "LOUVIGNY - 14111", "MAY SUR ORNE - 14320", "MONDRAINVILLE - 14210", "ROSEL - 14740", "ROTS - 14740", "ST GERMAIN LA BLANCHE HERBE - 14280", "ST MARTIN DE FONTENAY - 14320", "TOURVILLE SUR ODON - 14210", "CIVRIEUX D AZERGUES - 69380", "DARDILLY - 69570", "ECULLY - 69130", "FONTAINES SUR SAONE - 69270", "MARCILLY D AZERGUES - 69380", "ORLIENAS - 69530", "PIERRE BENITE - 69310", "STE FOY LES LYON - 69110", "ST ROMAIN AU MONT D OR - 69270", "VOURLES - 69390", "CHAPONNAY - 69970",
                               "FEYZIN - 69320", "ST LAURENT DE MURE - 69720", "ST SYMPHORIEN D OZON - 69360", "SATHONAY VILLAGE - 69580", "SEREZIN DU RHONE - 69360", "BIHOREL - 76420", "BONSECOURS - 76240", "CANTELEU - 76380", "DEVILLE LES ROUEN - 76250", "ELBEUF - 76500", "EPOUVILLE - 76133", "FONTENAY - 76290", "MALAUNAY - 76770", "LE MESNIL ESNARD - 76240", "MONT ST AIGNAN - 76130", "NOTRE DAME DU BEC - 76133", "OCTEVILLE SUR MER - 76930", "PAVILLY - 76570", "QUEVREVILLE LA POTERIE - 76520", "ST AUBIN LES ELBEUF - 76410", "ST AUBIN ROUTOT - 76430", "ST ETIENNE DU ROUVRAY - 76800", "ST PIERRE LES ELBEUF - 76320", "SAUSSAY - 76760", "SOTTEVILLE LES ROUEN - 76300", "LA VAUPALIERE - 76150", "YMARE - 76520", "ALTHEN DES PALUDS - 84210", "MORIERES LES AVIGNON - 84310", "LE PONTET - 84130", "SORGUES - 84700", "CHEVRY - 01170", "NEYRON - 01700", "TRAMOYES - 01390", "FERNEY VOLTAIRE - 01210", "CADOLIVE - 13950", "BARON SUR ODON - 14210", "CAMBES EN PLAINE - 14610", "CARPIQUET - 14650", "FONTAINE ETOUPEFOUR - 14790", "HEROUVILLE ST CLAIR - 14200", "MATHIEU - 14920", "MOUEN - 14790", "ST ANDRE SUR ORNE - 14320", "ST GENIS POUILLY - 01630", "ST MAURICE DE BEYNOST - 01700", "ST MANVIEU NORREY - 14740", "SEGNY - 01170", "VERSON - 14790", "ST PAUL DE VENCE - 06570", "FALICON - 06950", "LA GAUDE - 06610", "ST ANDRE DE LA ROCHE - 06730", "ORNEX - 01210", "AUTHIE - 14280", "BRETTEVILLE SUR ODON - 14760", "CORMELLES LE ROYAL - 14123", "ETERVILLE - 14930", "GRAINVILLE SUR ODON - 14210", "ROTS - 14980", "THAON - 14610", "DRAP - 06340", "ST LAURENT DU VAR - 06700", "LA TRINITE - 06340", "VILLENEUVE LOUBET - 06270", "BELCODENE - 13720", "LA BOUILLADISSE - 13720", "LA DESTROUSSE - 13112", "COLOMBY ANGUERNY - 14610", "CAIRON - 14610", "FLEURY SUR ORNE - 14123", "IFS - 14123", "MALTOT - 14930", "LE BEC THOMAS - 27370", "BOSROUMOIS - 27670", "CRESTOT - 27110", "HECTOMARE - 27110", "LE MANOIR - 27460", "VILLONS LES BUISSONS - 14610", "ST DENIS DES MONTS - 27520", "ST DIDIER DES BOIS - 27370", "ST GERMAIN DE PASQUIER - 27370", "SURTAUVILLE - 27400", "LE TRONCQ - 27110", "AURIOL - 13390", "GREASQUE - 13850", "ST SAVOURNIN - 13119", "BASLY - 14610", "EPRON - 14610", "ST CONTEST - 14280", "AUSSONNE - 31840", "CORNEBARRIEU - 31700", "MERVILLA - 31320", "SEILH - 31840", "VIEILLE TOULOUSE - 31320", "VIGOULET AUZIL - 31320", "BEGLES - 33130", "GRADIGNAN - 33170", "MARTILLAC - 33650", "ST MEDARD D EYRANS - 33650", "JACOU - 34830", "MIREVAL - 34110", "GRAND BOURGTHEROULDE - 27520", "PIGNAN - 34570", "ST VINCENT DE BARBEYRARGUES - 34730", "LA HARENGERE - 27370", "MARTOT - 27340", "ST OUEN DE PONTCHEUIL - 27370", "ST PIERRE DES FLEURS - 27370", "ST PIERRE DU BOSGUERARD - 27370", "VRAIVILLE - 27370", "MONTBAZON - 37250", "MONTS - 37260", "ST CYR SUR LOIRE - 37540",
                               "ST GENOUPH - 37510", "ST PIERRE DES CORPS - 37700", "GIERES - 38610", "MONTBONNOT ST MARTIN - 38330", "CAUMONT - 27310", "LA HAYE MALHERBE - 27400", "TERRES DE BORD - 27400", "ST CYR LA CAMPAGNE - 27370", "ST OUEN DU TILLEUL - 27670", "LA TRINITE DE THOUBERVILLE - 27310", "AUZEVILLE TOLOSANE - 31320", "BLAGNAC - 31700", "CASTANET TOLOSAN - 31320", "ESCALQUENS - 31750", "MONDONVILLE - 31700", "ST PAUL SUR SAVE - 31530", "LES MONTS DU ROUMOIS - 27370", "BLANQUEFORT - 33290", "BOULIAC - 33270", "BEAUZELLE - 31700", "CANEJAN - 33610", "CRASVILLE - 27400", "FENOUILLET - 31150", "MERIGNAC - 33700", "FOUQUEVILLE - 27370", "LABEGE - 31670", "LA HAYE DU THEIL - 27370", "LESPINASSE - 31150", "BOUAYE - 44830", "BOUGUENAIS - 44340", "IGOVILLE - 27460", "CARQUEFOU - 44470", "CHEIX EN RETZ - 44640", "MERVILLE - 31330", "COUERON - 44220", "MONTAIGUT SUR SAVE - 31530", "LA MONTAGNE - 44620", "VILLENAVE D ORNON - 33140", "PONT DE L ARCHE - 27340", "ST ALBAN - 31140", "REZE - 44400", "ST AIGNAN GRANDLIEU - 44860", "LA SALVETAT ST GILLES - 31880", "ST SEBASTIEN SUR LOIRE - 44230", "SAUTRON - 44880", "LES SORINIERES - 44840", "JUVIGNAC - 34990", "LAVERUNE - 34880", "MURVIEL LES MONTPELLIER - 34570", "TEYRAN - 34820", "VILLENEUVE LES MAGUELONE - 34750", "LE BOUSCAT - 33110", "BRUGES - 33520", "CADAUJAC - 33140", "EYSINES - 33320", "LATRESNE - 33360", "BALLAN MIRE - 37510", "CHAMBRAY LES TOURS - 37170", "FONDETTES - 37230", "JOUE LES TOURS - 37300", "TALENCE - 33400", "ST AVERTIN - 37550", "ST ROCH - 37390", "LA VILLE AUX DAMES - 37700", "FABREGUES - 34690", "BRESSON - 38320", "LATTES - 34970", "BEAUCOUZE - 49070", "BRIOLLAY - 49125", "CORENC - 38700", "CANTENAY EPINARD - 49460", "ECHIROLLES - 38130", "VENDARGUES - 34740", "LES GARENNES SUR LOIRE - 49320", "LONGUENEE EN ANJOU - 49770", "MONTREUIL JUIGNE - 49460", "LES PONTS DE CE - 49130", "VERRIERES EN ANJOU - 49112", "VERRIERES EN ANJOU - 49480", "BELBERAUD - 31450", "BRAX - 31490", "COLOMIERS - 31770", "DAUX - 31700", "VENON - 38610", "LEGUEVIN - 31490", "PECHBUSQUE - 31320", "PIBRAC - 31820", "POMPERTUZAT - 31450", "REBIGUE - 31320", "CHANCEAUX SUR CHOISILLE - 37390", "CHARENTILLY - 37390", "ESVRES - 37320", "LA MEMBROLLE SUR CHOISILLE - 37390", "MONTLOUIS SUR LOIRE - 37270", "NOTRE DAME D OE - 37390", "PARCAY MESLAY - 37210", "VEIGNE - 37250", "ST MARTIN D HERES - 38400", "LE HAILLAN - 33185", "LE TAILLAN MEDOC - 33320", "CASTELNAU LE LEZ - 34170", "LE CRES - 34920", "ST GEORGES D ORQUES - 34680", "ST JEAN DE VEDAS - 34430", "SAUSSAN - 34570", "LA CHAPELLE SUR ERDRE - 44240", "PONT ST MARTIN - 44860", "LUYNES - 37230", "METTRAY - 37390", "LA RICHE - 37520", "VERETZ - 37270", "DOMENE - 38420", "EYBENS - 38320", "LA TRONCHE - 38700", "ORVAULT - 44700", "ST LEGER LES VIGNES - 44710", "STE LUCE SUR LOIRE - 44980",
                               "ST MARS DU DESERT - 44850", "LONGUENEE EN ANJOU - 49220", "ST MARTIN DU FOUILLOUX - 49170", "AVRILLE - 49240", "CAMPHIN EN CAREMBAULT - 59133", "COMINES - 59560", "LE PLESSIS GRAMMOIRE - 49124", "ENGLOS - 59320", "ENNEVELIN - 59710", "ERQUINGHEM LE SEC - 59320", "ST MELAINE SUR AUBANCE - 49610", "SARRIGNE - 49800", "FACHES THUMESNIL - 59155", "BRAINS - 44830", "LAMBERSART - 59130", "LESQUIN - 59810", "LOOS - 59120", "MAUVES SUR LOIRE - 44470", "LA NEUVILLE - 59239", "NOYELLES LES SECLIN - 59139", "ST HERBLAIN - 44800", "RONCHIN - 59790", "THOUARE SUR LOIRE - 44470", "TREILLIERES - 44119", "SEQUEDIN - 59320", "TOURMIGNIES - 59551", "VENDEVILLE - 59175", "WASQUEHAL - 59290", "WATTIGNIES - 59139", "ECOUFLANT - 49000", "LES GARENNES SUR LOIRE - 49610", "MURS ERIGNE - 49610", "STE GEMMES SUR LOIRE - 49130", "ST LEGER DE LINIERES - 49170", "CARVIN - 62220", "VILLENEUVE D ASCQ - 59491", "ATTICHES - 59551", "BOUSBECQUE - 59166", "CAPINGHEM - 59160", "DEULEMONT - 59890", "HEM - 59510", "LOMPRET - 59840", "LYS LEZ LANNOY - 59390", "MARCQ EN BAROEUL - 59700", "PHALEMPIN - 59133", "PREMESQUES - 59840", "ST ANDRE LEZ LILLE - 59350", "TRESSIN - 59152", "WAMBRECHIES - 59118", "WARNETON - 59560", "DINGSHEIM - 67370", "ECKBOLSHEIM - 67201", "HANGENBIETEN - 67980", "ANSTAING - 59152", "HURTIGHEIM - 67117", "LAMPERTHEIM - 67450", "BONDUES - 59910", "MITTELHAUSBERGEN - 67206", "CHEMY - 59147", "EMMERIN - 59320", "FRETIN - 59273", "GONDECOURT - 59147", "HALLENNES LEZ HAUBOURDIN - 59320", "HAUBOURDIN - 59320", "LANNOY - 59390", "LINSELLES - 59126", "MARQUETTE LEZ LILLE - 59520", "MERIGNIES - 59710", "PONT A MARCQ - 59710", "SECLIN - 59113", "WILLEMS - 59780", "BRIGNAIS - 69530", "COLLONGES AU MONT D OR - 69660", "FRANCHEVILLE - 69340", "MONTAGNY - 69700", "OULLINS - 69600", "TASSIN LA DEMI LUNE - 69160", "JONAGE - 69330", "ST PRIEST - 69800", "SATHONAY CAMP - 69580", "TOUSSIEU - 69780", "LIBERCOURT - 62820", "AVELIN - 59710", "BEAUCAMPS LIGNY - 59134", "CARNIN - 59112", "CROIX - 59170", "ENNETIERES EN WEPPES - 59320", "FOREST SUR MARQUE - 59510", "FOURNES EN WEPPES - 59134", "LA MADELEINE - 59110", "MONS EN BAROEUL - 59370", "PERENCHIES - 59840", "RONCQ - 59223", "SAILLY LEZ LANNOY - 59390", "TOUFFLERS - 59390", "BELBEUF - 76240", "BOSC GUERARD ST ADRIEN - 76710", "BOUVILLE - 76360", "BUTOT - 76890", "FRENEUSE - 76410", "GAINNEVILLE - 76700", "LE GRAND QUEVILLY - 76120", "LE HOULME - 76770", "MAROMME - 76150", "MESNIL PANNEVILLE - 76570", "BREUSCHWICKERSHEIM - 67112", "OISSEL - 76350", "ORIVAL - 76500", "GRIESHEIM SUR SOUFFEL - 67370",
                               "HANDSCHUHEIM - 67117", "HOENHEIM - 67800", "VAL DE LA HAYE - 76380", "VENDENHEIM - 67550", "WIWERSHEIM - 67370", "WOLFISHEIM - 67202", "BRINDAS - 69126", "CAILLOUX SUR FONTAINES - 69270", "CALUIRE ET CUIRE - 69300", "CHAMPAGNE AU MONT D OR - 69410", "CHAPONOST - 69630", "FLEURIEU SUR SAONE - 69250", "FONTAINES ST MARTIN - 69270", "GREZIEU LA VARENNE - 69290", "LENTILLY - 69210", "LIMONEST - 69760", "LISSIEU - 69380", "SOUCIEU EN JARREST - 69510", "ST GENIS LES OLLIERES - 69290", "OIGNIES - 62590", "LA TOUR DE SALVAGNY - 69890", "VERNAISON - 69390", "DECINES CHARPIEU - 69150", "MONTANAY - 69250", "PUSIGNAN - 69330", "ST BONNET DE MURE - 69720", "ACHENHEIM - 67204", "ENTZHEIM - 67960", "ITTENHEIM - 67117", "MUNDOLSHEIM - 67450", "OBERHAUSBERGEN - 67205", "OBERSCHAEFFOLSHEIM - 67203", "QUATZENHEIM - 67117", "REICHSTETT - 67116", "SCHILTIGHEIM - 67300", "SOUFFELWEYERSHEIM - 67460", "STUTZHEIM OFFENHEIM - 67370", "LA WANTZENAU - 67610", "CHARLY - 69390", "COUZON AU MONT D OR - 69270", "IRIGNY - 69540", "LA MULATIERE - 69350", "ROCHETAILLEE SUR SAONE - 69270", "ST CYR AU MONT D OR - 69450", "ST DIDIER AU MONT D OR - 69370", "TALUYERS - 69440", "CORBAS - 69960", "JONS - 69330", "MEYZIEU - 69330", "MIONS - 69780", "RILLIEUX LA PAPE - 69140", "BISCHHEIM - 67800", "LES AUTHIEUX SUR LE PORT ST OUEN - 76520", "HOERDT - 67720", "HOLTZHEIM - 67810", "KOLBSHEIM - 67120", "LINGOLSHEIM - 67380", "ESLETTES - 76710", "PFULGRIESHEIM - 67370", "HARFLEUR - 76700", "HAUTOT SUR SEINE - 76113", "HERMEVILLE - 76280", "LIMESY - 76570", "NOTRE DAME DE BONDEVILLE - 76960", "PETIT COURONNE - 76650", "STE AUSTREBERTHE - 76570", "ST LAURENT DE BREVEDENT - 76700", "ST MARTIN DU BEC - 76133", "ST PIERRE DE VARENGEVILLE - 76480", "VILLENEUVE D ASCQ - 59493", "VILLENEUVE D ASCQ - 59650", "SOTTEVILLE SOUS LE VAL - 76410", "VILLERS ECALLES - 76360", "ESCOBECQUES - 59320", "FRELINGHIEN - 59236", "HALLUIN - 59250", "BRON - 69500", "CHARBONNIERES LES BAINS - 69260", "HOUPLIN ANCOISNE - 59263", "CRAPONNE - 69290", "DOMMARTIN - 69380", "LEERS - 59115", "LEZENNES - 59260", "MARCY L ETOILE - 69280", "MESSIMY - 69510", "MILLERY - 69390", "MOUVAUX - 59420", "NEUVILLE EN FERRAIN - 59960", "POLEYMIEUX AU MONT D OR - 69250", "STE CONSORCE - 69280", "ST GENIS LAVAL - 69230", "QUESNOY SUR DEULE - 59890", "ROUBAIX - 59100", "VAULX EN VELIN - 69120", "VENISSIEUX - 69200", "VILLEURBANNE - 69100", "CHASSIEU - 69680", "GENAS - 69740", "SANTES - 59211", "ST PIERRE DE CHANDIEU - 69780", "SOLAIZE - 69360", "TEMPLEMARS - 59175", "TOURCOING - 59200", "VERLINGHEM - 59237", "WATTRELOS - 59150", "AMFREVILLE LA MI VOIE - 76920", "ANCEAUMEVILLE - 76710", "BARENTIN - 76360", "BLACQUEVILLE - 76190", "CAUVILLE SUR MER - 76930", "CLEON - 76410",
                               "CROIX MARE - 76190", "DARNETAL - 76160", "EMANVILLE - 76570", "FONTAINE LA MALLET - 76290", "GONFREVILLE L ORCHER - 76700", "GOUY - 76520", "HOUPPEVILLE - 76770", "ISNEAUVILLE - 76230", "LA LONDE - 76500", "MANNEVILLETTE - 76290", "MONTIGNY - 76380", "MONTVILLE - 76710", "PISSY POVILLE - 76360", "ROGERVILLE - 76700", "ROLLEVILLE - 76133", "ST AUBIN CELLOVILLE - 76520", "ST LEGER DU BOURG DENIS - 76160", "ST MARTIN DU MANOIR - 76290", "ST PAER - 76480", "ST SATURNIN LES AVIGNON - 84450", "BOIS GUILLAUME - 76230", "LA BOUILLE - 76530", "CAUDEBEC LES ELBEUF - 76320", "CIDEVILLE - 76570", "FRESQUIENNES - 76570", "GOUPILLIERES - 76570", "GRAND COURONNE - 76530", "HENOUVILLE - 76840", "HUGLEVILLE EN CAUX - 76570", "MANEGLISE - 76133", "MONTIVILLIERS - 76290", "MOULINEAUX - 76530", "OUDALLE - 76430", "LE PETIT QUEVILLY - 76140", "ROUMARE - 76480", "SAINNEVILLE - 76430", "ST JEAN DU CARDONNAY - 76150", "ST MARTIN DE BOSCHERVILLE - 76840", "SIERVILLE - 76690", "TOURVILLE LA RIVIERE - 76410", "ERNOLSHEIM BRUCHE - 67120", "VEDENE - 84270", "NIEDERHAUSBERGEN - 67207", "OSTWALD - 67540", "ENTRAIGUES SUR LA SORGUE - 84320", "NANTES - 44", "NICE - 06", "AUBAGNE - 13400", "ROUEN - 76", "CAEN - 14", "TOULOUSE - 31", "BORDEAUX - 33", "MONTPELLIER - 34", "GRENOBLE - 38", "ANGERS - 49", "LILLE - 59", "STRASBOURG - 67", "AVIGNON - 84", "LE HAVRE - 76600", "LYON - 69", "GENEVE"))
names(data)[119] <- "QIV28"
# LimeSurvey Field type: A
data[, 120] <- as.character(data[, 120])
attributes(data)$variable.labels[120] <- "[Autre] Commune (code postal et nom) où se situe votre activité principale :"
names(data)[120] <- "QIV28_other"
# LimeSurvey Field type: A
data[, 121] <- as.character(data[, 121])
attributes(data)$variable.labels[121] <- "Merci pour votre participation, nous mettrons en ligne les résultats de cette enquête nationale sur le site du projet PÉPITE (Préférences sociales pour des caractéristiques Écologiques et Paysagères d’Infrastructures de Transports à l’échelle de tErritoires). Ces résultats feront également l’objet d’une publication scientifique.  Si vous êtes intéressé(e) par le sujet et souhaitez obtenir davantage d’information ou prolonger la discussion, contactez-nous à l’adresse suivante : enquete.pepite@gmail.com.     Avez-vous des remarques sur cette enquête ou plus largement sur le sujet de l’intégration écologique et paysagère des infrastructures de transport ?"
names(data)[121] <- "Qfin"
