# load data from survey 672984

data <- read.csv("raw_data/results-survey672984.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")


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
data[, 10] <- as.factor(data[, 10])
attributes(data)$variable.labels[10] <- "Quelle est votre commune (code postal et nom) de résidence :"
names(data)[10] <- "Q01"
# LimeSurvey Field type: A
data[, 11] <- as.character(data[, 11])
attributes(data)$variable.labels[11] <- "[Autre] Quelle est votre commune (code postal et nom) de résidence :"
names(data)[11] <- "Q01_other"
# LimeSurvey Field type: A
data[, 12] <- as.factor(data[, 12])
names(data)[12] <- "Q01b"
# LimeSurvey Field type: A
data[, 13] <- as.character(data[, 13])
attributes(data)$variable.labels[13] <- "[Autre] Vous êtes :"
names(data)[13] <- "Q01b_other"
# LimeSurvey Field type: A
data[, 14] <- as.factor(data[, 14])
names(data)[14] <- "Q01c"
# LimeSurvey Field type: A
data[, 15] <- as.factor(data[, 15])
names(data)[15] <- "Q01d"
# LimeSurvey Field type: A
data[, 16] <- as.factor(data[, 16])
names(data)[16] <- "Q02"
# LimeSurvey Field type: A
data[, 17] <- as.character(data[, 17])
attributes(data)$variable.labels[17] <- "Si non, pourquoi ?"
names(data)[17] <- "Q03"
# LimeSurvey Field type: F
data[, 18] <- as.numeric(as.factor(data[, 18]))
attributes(data)$variable.labels[18] <- "[professionnelle (y compris pour suivre ses études).] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 18] <- factor(data[, 18], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[18] <- "QI4_SQ002"
# LimeSurvey Field type: F
data[, 19] <- as.numeric(as.factor(data[, 19]))
attributes(data)$variable.labels[19] <- "[participation à la vie associative.] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 19] <- factor(data[, 19], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[19] <- "QI4_SQ003"
# LimeSurvey Field type: F
data[, 20] <- as.numeric(as.factor(data[, 20]))
attributes(data)$variable.labels[20] <- "[activités domestiques (courses, rendez-vous médicaux, animal de compagnie, etc.).] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 20] <- factor(data[, 20], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[20] <- "QI4_SQ004"
# LimeSurvey Field type: F
data[, 21] <- as.numeric(as.factor(data[, 21]))
attributes(data)$variable.labels[21] <- "[loisirs (activités sportives, sociales, artistiques, culturelles, shopping, etc.).] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 21] <- factor(data[, 21], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[21] <- "QI4_SQ005"
# LimeSurvey Field type: F
data[, 22] <- as.numeric(as.factor(data[, 22]))
attributes(data)$variable.labels[22] <- "[accompagnement/transport des membres de votre foyer pour l’une des raisons pré-citées.] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 22] <- factor(data[, 22], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
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
names(data)[29] <- "QI6_1"
# LimeSurvey Field type: A
data[, 30] <- as.character(data[, 30])
attributes(data)$variable.labels[30] <- "[Classement 2] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[30] <- "QI6_2"
# LimeSurvey Field type: A
data[, 31] <- as.character(data[, 31])
attributes(data)$variable.labels[31] <- "[Classement 3] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[31] <- "QI6_3"
# LimeSurvey Field type: A
data[, 32] <- as.character(data[, 32])
attributes(data)$variable.labels[32] <- "[Classement 4] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[32] <- "QI6_4"
# LimeSurvey Field type: A
data[, 33] <- as.character(data[, 33])
attributes(data)$variable.labels[33] <- "[Classement 5] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[33] <- "QI6_5"
# LimeSurvey Field type: A
data[, 34] <- as.character(data[, 34])
attributes(data)$variable.labels[34] <- "[Classement 6] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[34] <- "QI6_6"
# LimeSurvey Field type: A
data[, 35] <- as.character(data[, 35])
attributes(data)$variable.labels[35] <- "[Classement 7] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[35] <- "QI6_7"
# LimeSurvey Field type: A
data[, 36] <- as.character(data[, 36])
attributes(data)$variable.labels[36] <- "[Classement 8] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[36] <- "QI6_8"
# LimeSurvey Field type: A
data[, 37] <- as.character(data[, 37])
attributes(data)$variable.labels[37] <- "[Classement 9] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[37] <- "QI6_9"
# LimeSurvey Field type: A
data[, 38] <- as.character(data[, 38])
attributes(data)$variable.labels[38] <- "Le moyen de transport individuel que vous utilisez est-il partagé ?"
names(data)[38] <- "QI7"
# LimeSurvey Field type: F
data[, 39] <- as.numeric(as.factor(data[, 39]))
attributes(data)$variable.labels[39] <- "[professionnelle (y compris pour suivre ses études).] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 39] <- factor(data[, 39], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[39] <- "QI8_SQ001"
# LimeSurvey Field type: F
data[, 40] <- as.numeric(as.factor(data[, 40]))
attributes(data)$variable.labels[40] <- "[participation à la vie associative.] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 40] <- factor(data[, 40], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[40] <- "QI8_SQ002"
# LimeSurvey Field type: F
data[, 41] <- as.numeric(as.factor(data[, 41]))
attributes(data)$variable.labels[41] <- "[activités domestiques (courses, rendez-vous médicaux, animal de compagnie, etc.).] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 41] <- factor(data[, 41], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[41] <- "QI8_SQ003"
# LimeSurvey Field type: F
data[, 42] <- as.numeric(as.factor(data[, 42]))
attributes(data)$variable.labels[42] <- "[loisirs (activités sportives, sociales, artistiques, culturelles, shopping, etc.).] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 42] <- factor(data[, 42], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[42] <- "QI8_SQ004"
# LimeSurvey Field type: F
data[, 43] <- as.numeric(as.factor(data[, 43]))
attributes(data)$variable.labels[43] <- "[accompagnement/transport des membres de votre foyer pour l’une des raisons pré-citées.] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 43] <- factor(data[, 43], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[43] <- "QI8_SQ005"
# LimeSurvey Field type: A
data[, 44] <- as.character(data[, 44])
attributes(data)$variable.labels[44] <- "[Autre] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
names(data)[44] <- "QI8_other"
# LimeSurvey Field type: A
data[, 45] <- as.character(data[, 45])
attributes(data)$variable.labels[45] <- "Avez-vous bien visualisé la vidéo ? Si oui, vous allez maintenant répondre à une carte de choix exemple pour vous familiariser avec l\'exercice, puis aux 12 cartes de choix."
names(data)[45] <- "Qvid"
# LimeSurvey Field type: A
data[, 46] <- as.character(data[, 46])
attributes(data)$variable.labels[46] <- "[Scénario choisi :]"
names(data)[46] <- "QCEex_SQ001"
# LimeSurvey Field type: A
data[, 47] <- as.character(data[, 47])
attributes(data)$variable.labels[47] <- "Avez-vous bien compris l\'exercice de choix ? Après avoir cliqué sur Suivant, vous allez devoir répondre à 12 cartes similaires à celle qui vient d\'être présentée en exemple, avec à chaque fois 2 scénarios qui combinent 4 attributs impactant le temps de trajet, et 1 scénario de référence qui n\'implique pas de temps de trajet supplémentaire."
names(data)[47] <- "QCEex2"
# LimeSurvey Field type: A
data[, 48] <- as.character(data[, 48])
attributes(data)$variable.labels[48] <- "[Scénario choisi]"
names(data)[48] <- "QCE1_SQ001"
# LimeSurvey Field type: A
data[, 49] <- as.character(data[, 49])
attributes(data)$variable.labels[49] <- "[Scénario choisi]"
names(data)[49] <- "QCE2_SQ001"
# LimeSurvey Field type: A
data[, 50] <- as.character(data[, 50])
attributes(data)$variable.labels[50] <- "[Scénario choisi]"
names(data)[50] <- "QCE3_SQ001"
# LimeSurvey Field type: A
data[, 51] <- as.character(data[, 51])
attributes(data)$variable.labels[51] <- "[Scénario choisi]"
names(data)[51] <- "QCE4_SQ001"
# LimeSurvey Field type: A
data[, 52] <- as.character(data[, 52])
attributes(data)$variable.labels[52] <- "[Scénario choisi]"
names(data)[52] <- "QCE5_SQ001"
# LimeSurvey Field type: A
data[, 53] <- as.character(data[, 53])
attributes(data)$variable.labels[53] <- "[Scénario choisi]"
names(data)[53] <- "QCE6_SQ001"
# LimeSurvey Field type: A
data[, 54] <- as.character(data[, 54])
attributes(data)$variable.labels[54] <- "[Scénario choisi]"
names(data)[54] <- "QCE7_SQ001"
# LimeSurvey Field type: A
data[, 55] <- as.character(data[, 55])
attributes(data)$variable.labels[55] <- "[Scénario choisi]"
names(data)[55] <- "QCE8_SQ001"
# LimeSurvey Field type: A
data[, 56] <- as.character(data[, 56])
attributes(data)$variable.labels[56] <- "[Scénario choisi]"
names(data)[56] <- "QCE9_SQ001"
# LimeSurvey Field type: A
data[, 57] <- as.character(data[, 57])
attributes(data)$variable.labels[57] <- "[Scénario choisi]"
names(data)[57] <- "QCE10_SQ001"
# LimeSurvey Field type: A
data[, 58] <- as.character(data[, 58])
attributes(data)$variable.labels[58] <- "[Scénario choisi]"
names(data)[58] <- "QCE11_SQ001"
# LimeSurvey Field type: A
data[, 59] <- as.character(data[, 59])
attributes(data)$variable.labels[59] <- "[Scénario choisi]"
names(data)[59] <- "QCE12_SQ001"
# LimeSurvey Field type: F
data[, 60] <- as.numeric(as.factor(data[, 60]))
attributes(data)$variable.labels[60] <- "[Chaque attribut a été important au moins une fois] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 60] <- factor(data[, 60], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[60] <- "QIII9_SQ002"
# LimeSurvey Field type: F
data[, 61] <- as.numeric(as.factor(data[, 61]))
attributes(data)$variable.labels[61] <- "[Paysage] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 61] <- factor(data[, 61], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[61] <- "QIII9_SQ003"
# LimeSurvey Field type: F
data[, 62] <- as.numeric(as.factor(data[, 62]))
attributes(data)$variable.labels[62] <- "[Richesse et abondance d\'espèces] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 62] <- factor(data[, 62], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[62] <- "QIII9_SQ004"
# LimeSurvey Field type: F
data[, 63] <- as.numeric(as.factor(data[, 63]))
attributes(data)$variable.labels[63] <- "[Type d\'espèces (urbaines, periurbaines, rurales)] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 63] <- factor(data[, 63], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[63] <- "QIII9_SQ005"
# LimeSurvey Field type: F
data[, 64] <- as.numeric(as.factor(data[, 64]))
attributes(data)$variable.labels[64] <- "[Accessibilité au parc] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 64] <- factor(data[, 64], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[64] <- "QIII9_SQ006"
# LimeSurvey Field type: F
data[, 65] <- as.numeric(as.factor(data[, 65]))
attributes(data)$variable.labels[65] <- "[Temps de transport supplémentaire] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 65] <- factor(data[, 65], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[65] <- "QIII9_SQ007"
# LimeSurvey Field type: F
data[, 66] <- as.numeric(as.factor(data[, 66]))
attributes(data)$variable.labels[66] <- "[Je n\'ai pas assez de temps donc je préfère le trajet le plus rapide.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 66] <- factor(data[, 66], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[66] <- "QIII10_SQ001"
# LimeSurvey Field type: F
data[, 67] <- as.numeric(as.factor(data[, 67]))
attributes(data)$variable.labels[67] <- "[L\'augmentation du temps de trajet est trop importante à l’échelle de mon trajet.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 67] <- factor(data[, 67], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[67] <- "QIII10_SQ002"
# LimeSurvey Field type: F
data[, 68] <- as.numeric(as.factor(data[, 68]))
attributes(data)$variable.labels[68] <- "[Je n\'aime pas le tramway (vous pourrez en détailler les raisons dans une question ultérieure).] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 68] <- factor(data[, 68], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[68] <- "QIII10_SQ003"
# LimeSurvey Field type: F
data[, 69] <- as.numeric(as.factor(data[, 69]))
attributes(data)$variable.labels[69] <- "[Les enjeux biodiversité ou de protection de la nature ne me concernent pas.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 69] <- factor(data[, 69], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[69] <- "QIII10_SQ004"
# LimeSurvey Field type: F
data[, 70] <- as.numeric(as.factor(data[, 70]))
attributes(data)$variable.labels[70] <- "[C\'est au gouvernement d\'agir pour la biodiversité, pas au citoyens.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 70] <- factor(data[, 70], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[70] <- "QIII10_SQ005"
# LimeSurvey Field type: F
data[, 71] <- as.numeric(as.factor(data[, 71]))
attributes(data)$variable.labels[71] <- "[Je n\'arrivais pas à bien distinguer les différences entre les scénarios.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 71] <- factor(data[, 71], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[71] <- "QIII10_SQ006"
# LimeSurvey Field type: A
data[, 72] <- as.character(data[, 72])
attributes(data)$variable.labels[72] <- "[Autre] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
names(data)[72] <- "QIII10_other"
# LimeSurvey Field type: F
data[, 73] <- as.numeric(as.factor(data[, 73]))
attributes(data)$variable.labels[73] <- "[Je n\'ai pas assez de temps donc je préfère le trajet le plus rapide.] Pour quelle(s) raison(s) avez-vous parfois préféré le scénario de référence (schéma du bas) ?"
data[, 73] <- factor(data[, 73], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[73] <- "QIII11_SQ001"
# LimeSurvey Field type: F
data[, 74] <- as.numeric(as.factor(data[, 74]))
attributes(data)$variable.labels[74] <- "[L\'augmentation du temps de trajet est trop importante à l’échelle de mon trajet.] Pour quelle(s) raison(s) avez-vous parfois préféré le scénario de référence (schéma du bas) ?"
data[, 74] <- factor(data[, 74], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[74] <- "QIII11_SQ002"
# LimeSurvey Field type: A
data[, 75] <- as.character(data[, 75])
attributes(data)$variable.labels[75] <- "[Autre] Pour quelle(s) raison(s) avez-vous parfois préféré le scénario de référence (schéma du bas) ?"
names(data)[75] <- "QIII11_other"
# LimeSurvey Field type: A
data[, 76] <- as.character(data[, 76])
attributes(data)$variable.labels[76] <- "[Note] Comment avez-vous trouvé cet exercice de sélection de choix ?"
names(data)[76] <- "QIII12_SQ001"
# LimeSurvey Field type: A
data[, 77] <- as.character(data[, 77])
attributes(data)$variable.labels[77] <- "[Note] Les scénarios proposés vont ont-ils semblé réalistes ?"
names(data)[77] <- "QIII13_SQ001"
# LimeSurvey Field type: A
data[, 78] <- as.character(data[, 78])
attributes(data)$variable.labels[78] <- "[Le tramway peut offrir une alternative de transport plus rapide.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[78] <- "QIII15_SQ001"
# LimeSurvey Field type: A
data[, 79] <- as.character(data[, 79])
attributes(data)$variable.labels[79] <- "[Le tramway peut offrir une alternative de transport moins cher.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[79] <- "QIII15_SQ002"
# LimeSurvey Field type: A
data[, 80] <- as.character(data[, 80])
attributes(data)$variable.labels[80] <- "[Le tramway peut offrir une alternative de transport plus écologique.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[80] <- "QIII15_SQ003"
# LimeSurvey Field type: A
data[, 81] <- as.character(data[, 81])
attributes(data)$variable.labels[81] <- "[Le tramway peut offrir une alternative de transport plus pratique.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[81] <- "QIII15_SQ004"
# LimeSurvey Field type: A
data[, 82] <- as.character(data[, 82])
attributes(data)$variable.labels[82] <- "[La préservation de la nature est un des enjeux majeurs de notre société] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[82] <- "QIII15_SQ005"
# LimeSurvey Field type: A
data[, 83] <- as.character(data[, 83])
attributes(data)$variable.labels[83] <- "[Les changements de comportements individuels ont un rôle pour préserver la nature.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[83] <- "QIII15_SQ006"
# LimeSurvey Field type: A
data[, 84] <- as.character(data[, 84])
attributes(data)$variable.labels[84] <- "[Je préfère l’autonomie et le confort du véhicule individuel aux transports collectifs.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[84] <- "QIII14_SQ001"
# LimeSurvey Field type: A
data[, 85] <- as.character(data[, 85])
attributes(data)$variable.labels[85] <- "[Les transports collectifs comme le tramway ne sont pas fiables (ex : retards).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[85] <- "QIII14_SQ002"
# LimeSurvey Field type: A
data[, 86] <- as.character(data[, 86])
attributes(data)$variable.labels[86] <- "[Je n’ai pas d’accès à une station de tramway facilement et/ou il n\'y a pas de solution pour garer ma voiture ou mon vélo à proximité.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[86] <- "QIII14_SQ003"
# LimeSurvey Field type: A
data[, 87] <- as.character(data[, 87])
attributes(data)$variable.labels[87] <- "[Le tramway est trop lent.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[87] <- "QIII14_SQ004"
# LimeSurvey Field type: A
data[, 88] <- as.character(data[, 88])
attributes(data)$variable.labels[88] <- "[Le tramway implique un itinéraire trop compliqué (ex : avec des changements).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[88] <- "QIII14_SQ010"
# LimeSurvey Field type: A
data[, 89] <- as.character(data[, 89])
attributes(data)$variable.labels[89] <- "[L’utilisation du tramway n’est pas pratique dans mon organisation quotidienne (ex : transport de matériel, enfants, PMR, etc.).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[89] <- "QIII14_SQ005"
# LimeSurvey Field type: A
data[, 90] <- as.character(data[, 90])
attributes(data)$variable.labels[90] <- "[Je ne pense pas que le développement des transports en commun soit une solution pour répondre aux enjeux climatiques.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[90] <- "QIII14_SQ006"
# LimeSurvey Field type: A
data[, 91] <- as.character(data[, 91])
attributes(data)$variable.labels[91] <- "[Je n’aime pas prendre les transports en commun (ex :  proximité avec d’autres personnes, bruit, risque d’être debout, etc).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[91] <- "QIII14_SQ007"
# LimeSurvey Field type: A
data[, 92] <- as.character(data[, 92])
attributes(data)$variable.labels[92] <- "[Les infrastructures de transport (routes, réseau ferré) n’ont pas d’impact sur la nature.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[92] <- "QIII14_SQ008"
# LimeSurvey Field type: A
data[, 93] <- as.character(data[, 93])
attributes(data)$variable.labels[93] <- "[Les changements de comportements individuels ne permettront pas (à eux seuls) de conserver la nature.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[93] <- "QIII14_SQ009"
# LimeSurvey Field type: A
data[, 94] <- as.character(data[, 94])
attributes(data)$variable.labels[94] <- "[Cet aménagement va créer encore plus de bouchons dans la ville.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[94] <- "QIII14_SQ011"
# LimeSurvey Field type: F
data[, 95] <- as.numeric(as.factor(data[, 95]))
attributes(data)$variable.labels[95] <- "[Le développement d\'un tramway est déjà important.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 95] <- factor(data[, 95], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[95] <- "QIII16_SQ001"
# LimeSurvey Field type: F
data[, 96] <- as.numeric(as.factor(data[, 96]))
attributes(data)$variable.labels[96] <- "[Zones piétonnes étendues.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 96] <- factor(data[, 96], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[96] <- "QIII16_SQ002"
# LimeSurvey Field type: F
data[, 97] <- as.numeric(as.factor(data[, 97]))
attributes(data)$variable.labels[97] <- "[Itinéraires cyclables continus et protégés.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 97] <- factor(data[, 97], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[97] <- "QIII16_SQ003"
# LimeSurvey Field type: F
data[, 98] <- as.numeric(as.factor(data[, 98]))
attributes(data)$variable.labels[98] <- "[Bus à haut niveau de service (bénéficiant d’une voie réservée).] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 98] <- factor(data[, 98], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[98] <- "QIII16_SQ004"
# LimeSurvey Field type: F
data[, 99] <- as.numeric(as.factor(data[, 99]))
attributes(data)$variable.labels[99] <- "[Co-voiturage.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 99] <- factor(data[, 99], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[99] <- "QIII16_SQ005"
# LimeSurvey Field type: F
data[, 100] <- as.numeric(as.factor(data[, 100]))
attributes(data)$variable.labels[100] <- "[Augmenter la part de télétravail.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 100] <- factor(data[, 100], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[100] <- "QIII16_SQ006"
# LimeSurvey Field type: F
data[, 101] <- as.numeric(as.factor(data[, 101]))
attributes(data)$variable.labels[101] <- "[Changer de travail pour être plus proche de son domicile.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 101] <- factor(data[, 101], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[101] <- "QIII16_SQ007"
# LimeSurvey Field type: F
data[, 102] <- as.numeric(as.factor(data[, 102]))
attributes(data)$variable.labels[102] <- "[Repenser la commune pour diminuer le besoin de déplacement en voiture.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 102] <- factor(data[, 102], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[102] <- "QIII16_SQ008"
# LimeSurvey Field type: A
data[, 103] <- as.character(data[, 103])
attributes(data)$variable.labels[103] <- "[Autre] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
names(data)[103] <- "QIII16_other"
# LimeSurvey Field type: A
data[, 104] <- as.character(data[, 104])
attributes(data)$variable.labels[104] <- "Quelle connaissance avez-vous du projet d\'extention de tramway dans votre commune ou les communes voisines ?"
names(data)[104] <- "QIV18"
# LimeSurvey Field type: A
data[, 105] <- as.character(data[, 105])
attributes(data)$variable.labels[105] <- "Quel est votre niveau d’étude ?"
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
names(data)[109] <- "QIV22"
# LimeSurvey Field type: A
data[, 110] <- as.character(data[, 110])
attributes(data)$variable.labels[110] <- "Faites-vous ou avez-vous fait partie d’un collectif ou d’une association dont l’objectif est/était la protection de la nature (si oui, vous pouvez préciser l’objectif en question) ?"
names(data)[110] <- "QIV23"
# LimeSurvey Field type: A
data[, 111] <- as.character(data[, 111])
attributes(data)$variable.labels[111] <- "[Commentaire] Faites-vous ou avez-vous fait partie d’un collectif ou d’une association dont l’objectif est/était la protection de la nature (si oui, vous pouvez préciser l’objectif en question) ?"
names(data)[111] <- "QIV23_comment"
# LimeSurvey Field type: A
data[, 112] <- as.character(data[, 112])
attributes(data)$variable.labels[112] <- "Pratiquez-vous régulièrement des activités de pleine nature (ex : randonnée, cueillette de champignons, chasse/pêche, etc.) (si oui, vous pouvez préciser l’activité) ?"
names(data)[112] <- "QIV24"
# LimeSurvey Field type: A
data[, 113] <- as.character(data[, 113])
attributes(data)$variable.labels[113] <- "[Commentaire] Pratiquez-vous régulièrement des activités de pleine nature (ex : randonnée, cueillette de champignons, chasse/pêche, etc.) (si oui, vous pouvez préciser l’activité) ?"
names(data)[113] <- "QIV24_comment"
# LimeSurvey Field type: A
data[, 114] <- as.character(data[, 114])
attributes(data)$variable.labels[114] <- "Portez-vous une attention particulière aux critères environnementaux lors de vos achats (ex : agriculture biologique, provenance locale, etc.) (si oui, vous pouvez précise le critère) ?"
names(data)[114] <- "QIV25"
# LimeSurvey Field type: A
data[, 115] <- as.character(data[, 115])
attributes(data)$variable.labels[115] <- "[Commentaire] Portez-vous une attention particulière aux critères environnementaux lors de vos achats (ex : agriculture biologique, provenance locale, etc.) (si oui, vous pouvez précise le critère) ?"
names(data)[115] <- "QIV25_comment"
# LimeSurvey Field type: A
data[, 116] <- as.character(data[, 116])
attributes(data)$variable.labels[116] <- "Quel est votre degré de connaissance sur la nature en général ?"
names(data)[116] <- "QIV26"
# LimeSurvey Field type: A
data[, 117] <- as.character(data[, 117])
attributes(data)$variable.labels[117] <- "En moyenne, à quelle fréquence estimez-vous être en contact avec la nature ?"
names(data)[117] <- "QIV27"
# LimeSurvey Field type: A
data[, 118] <- as.character(data[, 118])
attributes(data)$variable.labels[118] <- "[Numéro de la figure] Dans l\'image ci-dessous, quelle figure décrit le mieux votre relation avec la Nature :"
names(data)[118] <- "QVnew_SQ001"
# LimeSurvey Field type: A
data[, 119] <- as.character(data[, 119])
attributes(data)$variable.labels[119] <- "Commune (code postal et nom) où se situe votre activité principale :"
names(data)[119] <- "QIV28"
# LimeSurvey Field type: A
data[, 120] <- as.character(data[, 120])
attributes(data)$variable.labels[120] <- "[Autre] Commune (code postal et nom) où se situe votre activité principale :"
names(data)[120] <- "QIV28_other"
# LimeSurvey Field type: A
data[, 121] <- as.character(data[, 121])
attributes(data)$variable.labels[121] <- "Merci pour votre participation, nous mettrons en ligne les résultats de cette enquête nationale sur le site du projet PÉPITE (Préférences sociales pour des caractéristiques Écologiques et Paysagères d’Infrastructures de Transports à l’échelle de tErritoires). Ces résultats feront également l’objet d’une publication scientifique.  Si vous êtes intéressé(e) par le sujet et souhaitez obtenir davantage d’information ou prolonger la discussion, contactez-nous à l’adresse suivante : enquete.pepite@gmail.com.     Avez-vous des remarques sur cette enquête ou plus largement sur le sujet de l’intégration écologique et paysagère des infrastructures de transport ?"
names(data)[121] <- "Qfin"

data_672984 <- data


# load data from survey 355435


data <- read.csv("raw_data/results-survey355435.csv", quote = "'\"", na.strings=c("", "\"\""), stringsAsFactors=FALSE, fileEncoding="UTF-8-BOM")

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
data[, 10] <- as.factor(data[, 10])
attributes(data)$variable.labels[10] <- "Quelle est votre commune (code postal et nom) de résidence :"
names(data)[10] <- "Q01"
# LimeSurvey Field type: A
data[, 11] <- as.character(data[, 11])
attributes(data)$variable.labels[11] <- "[Autre] Quelle est votre commune (code postal et nom) de résidence :"
names(data)[11] <- "Q01_other"
# LimeSurvey Field type: A
data[, 12] <- as.factor(data[, 12])
names(data)[12] <- "Q01b"
# LimeSurvey Field type: A
data[, 13] <- as.character(data[, 13])
attributes(data)$variable.labels[13] <- "[Autre] Vous êtes :"
names(data)[13] <- "Q01b_other"
# LimeSurvey Field type: A
data[, 14] <- as.factor(data[, 14])
names(data)[14] <- "Q01c"
# LimeSurvey Field type: A
data[, 15] <- as.factor(data[, 15])
names(data)[15] <- "Q01d"
# LimeSurvey Field type: A
data[, 16] <- as.factor(data[, 16])
names(data)[16] <- "Q02"
# LimeSurvey Field type: A
data[, 17] <- as.character(data[, 17])
attributes(data)$variable.labels[17] <- "Si non, pourquoi ?"
names(data)[17] <- "Q03"
# LimeSurvey Field type: F
data[, 18] <- as.numeric(as.factor(data[, 18]))
attributes(data)$variable.labels[18] <- "[professionnelle (y compris pour suivre ses études).] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 18] <- factor(data[, 18], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[18] <- "QI4_SQ002"
# LimeSurvey Field type: F
data[, 19] <- as.numeric(as.factor(data[, 19]))
attributes(data)$variable.labels[19] <- "[participation à la vie associative.] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 19] <- factor(data[, 19], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[19] <- "QI4_SQ003"
# LimeSurvey Field type: F
data[, 20] <- as.numeric(as.factor(data[, 20]))
attributes(data)$variable.labels[20] <- "[activités domestiques (courses, rendez-vous médicaux, animal de compagnie, etc.).] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 20] <- factor(data[, 20], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[20] <- "QI4_SQ004"
# LimeSurvey Field type: F
data[, 21] <- as.numeric(as.factor(data[, 21]))
attributes(data)$variable.labels[21] <- "[loisirs (activités sportives, sociales, artistiques, culturelles, shopping, etc.).] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 21] <- factor(data[, 21], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[21] <- "QI4_SQ005"
# LimeSurvey Field type: F
data[, 22] <- as.numeric(as.factor(data[, 22]))
attributes(data)$variable.labels[22] <- "[accompagnement/transport des membres de votre foyer pour l’une des raisons pré-citées.] Quelle raison principale concerne votre déplacement quotidien le plus fréquent (si ce déplacement est lié à plusieurs raisons, vous pouvez cocher plusieurs cases) ?"
data[, 22] <- factor(data[, 22], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
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
names(data)[29] <- "QI6_1"
# LimeSurvey Field type: A
data[, 30] <- as.character(data[, 30])
attributes(data)$variable.labels[30] <- "[Classement 2] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[30] <- "QI6_2"
# LimeSurvey Field type: A
data[, 31] <- as.character(data[, 31])
attributes(data)$variable.labels[31] <- "[Classement 3] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[31] <- "QI6_3"
# LimeSurvey Field type: A
data[, 32] <- as.character(data[, 32])
attributes(data)$variable.labels[32] <- "[Classement 4] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[32] <- "QI6_4"
# LimeSurvey Field type: A
data[, 33] <- as.character(data[, 33])
attributes(data)$variable.labels[33] <- "[Classement 5] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[33] <- "QI6_5"
# LimeSurvey Field type: A
data[, 34] <- as.character(data[, 34])
attributes(data)$variable.labels[34] <- "[Classement 6] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[34] <- "QI6_6"
# LimeSurvey Field type: A
data[, 35] <- as.character(data[, 35])
attributes(data)$variable.labels[35] <- "[Classement 7] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[35] <- "QI6_7"
# LimeSurvey Field type: A
data[, 36] <- as.character(data[, 36])
attributes(data)$variable.labels[36] <- "[Classement 8] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[36] <- "QI6_8"
# LimeSurvey Field type: A
data[, 37] <- as.character(data[, 37])
attributes(data)$variable.labels[37] <- "[Classement 9] Quel est le moyen de locomotion que vous utilisez le plus pour réaliser ce déplacement ? Si vous utilisez plusieurs moyens de locomotion au cours de ce trajet, veuillez les classer, le moyen de locomotion le plus utilisé étant situé en haut de la liste de droite."
names(data)[37] <- "QI6_9"
# LimeSurvey Field type: A
data[, 38] <- as.character(data[, 38])
attributes(data)$variable.labels[38] <- "Le moyen de transport individuel que vous utilisez est-il partagé ?"
names(data)[38] <- "QI7"
# LimeSurvey Field type: F
data[, 39] <- as.numeric(as.factor(data[, 39]))
attributes(data)$variable.labels[39] <- "[professionnelle (y compris pour suivre ses études).] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 39] <- factor(data[, 39], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[39] <- "QI8_SQ001"
# LimeSurvey Field type: F
data[, 40] <- as.numeric(as.factor(data[, 40]))
attributes(data)$variable.labels[40] <- "[participation à la vie associative.] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 40] <- factor(data[, 40], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[40] <- "QI8_SQ002"
# LimeSurvey Field type: F
data[, 41] <- as.numeric(as.factor(data[, 41]))
attributes(data)$variable.labels[41] <- "[activités domestiques (courses, rendez-vous médicaux, animal de compagnie, etc.).] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 41] <- factor(data[, 41], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[41] <- "QI8_SQ003"
# LimeSurvey Field type: F
data[, 42] <- as.numeric(as.factor(data[, 42]))
attributes(data)$variable.labels[42] <- "[loisirs (activités sportives, sociales, artistiques, culturelles, shopping, etc.).] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 42] <- factor(data[, 42], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[42] <- "QI8_SQ004"
# LimeSurvey Field type: F
data[, 43] <- as.numeric(as.factor(data[, 43]))
attributes(data)$variable.labels[43] <- "[accompagnement/transport des membres de votre foyer pour l’une des raisons pré-citées.] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
data[, 43] <- factor(data[, 43], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[43] <- "QI8_SQ005"
# LimeSurvey Field type: A
data[, 44] <- as.character(data[, 44])
attributes(data)$variable.labels[44] <- "[Autre] Dans l’hypothèse où un tramway serait accessible depuis votre domicile et utilisable pour les raisons suivantes, pour quelle(s) activité(s) principale(s) seriez-vous susceptible d’emprunter le tramway ?"
names(data)[44] <- "QI8_other"
# LimeSurvey Field type: A
data[, 45] <- as.character(data[, 45])
attributes(data)$variable.labels[45] <- "Avez-vous bien visualisé la vidéo ? Si oui, vous allez maintenant répondre à une carte de choix exemple pour vous familiariser avec l\'exercice, puis aux 12 cartes de choix."
names(data)[45] <- "Qvid"
# LimeSurvey Field type: A
data[, 46] <- as.character(data[, 46])
attributes(data)$variable.labels[46] <- "[Scénario choisi :]"
names(data)[46] <- "QCEex_SQ001"
# LimeSurvey Field type: A
data[, 47] <- as.character(data[, 47])
attributes(data)$variable.labels[47] <- "Avez-vous bien compris l\'exercice de choix ? Après avoir cliqué sur Suivant, vous allez devoir répondre à 12 cartes similaires à celle qui vient d\'être présentée en exemple, avec à chaque fois 2 scénarios qui combinent 4 attributs impactant le temps de trajet, et 1 scénario de référence qui n\'implique pas de temps de trajet supplémentaire."
names(data)[47] <- "QCEex2"
# LimeSurvey Field type: A
data[, 48] <- as.character(data[, 48])
attributes(data)$variable.labels[48] <- "[Scénario choisi]"
names(data)[48] <- "QCE1_SQ001"
# LimeSurvey Field type: A
data[, 49] <- as.character(data[, 49])
attributes(data)$variable.labels[49] <- "[Scénario choisi]"
names(data)[49] <- "QCE2_SQ001"
# LimeSurvey Field type: A
data[, 50] <- as.character(data[, 50])
attributes(data)$variable.labels[50] <- "[Scénario choisi]"
names(data)[50] <- "QCE3_SQ001"
# LimeSurvey Field type: A
data[, 51] <- as.character(data[, 51])
attributes(data)$variable.labels[51] <- "[Scénario choisi]"
names(data)[51] <- "QCE4_SQ001"
# LimeSurvey Field type: A
data[, 52] <- as.character(data[, 52])
attributes(data)$variable.labels[52] <- "[Scénario choisi]"
names(data)[52] <- "QCE5_SQ001"
# LimeSurvey Field type: A
data[, 53] <- as.character(data[, 53])
attributes(data)$variable.labels[53] <- "[Scénario choisi]"
names(data)[53] <- "QCE6_SQ001"
# LimeSurvey Field type: A
data[, 54] <- as.character(data[, 54])
attributes(data)$variable.labels[54] <- "[Scénario choisi]"
names(data)[54] <- "QCE7_SQ001"
# LimeSurvey Field type: A
data[, 55] <- as.character(data[, 55])
attributes(data)$variable.labels[55] <- "[Scénario choisi]"
names(data)[55] <- "QCE8_SQ001"
# LimeSurvey Field type: A
data[, 56] <- as.character(data[, 56])
attributes(data)$variable.labels[56] <- "[Scénario choisi]"
names(data)[56] <- "QCE9_SQ001"
# LimeSurvey Field type: A
data[, 57] <- as.character(data[, 57])
attributes(data)$variable.labels[57] <- "[Scénario choisi]"
names(data)[57] <- "QCE10_SQ001"
# LimeSurvey Field type: A
data[, 58] <- as.character(data[, 58])
attributes(data)$variable.labels[58] <- "[Scénario choisi]"
names(data)[58] <- "QCE11_SQ001"
# LimeSurvey Field type: A
data[, 59] <- as.character(data[, 59])
attributes(data)$variable.labels[59] <- "[Scénario choisi]"
names(data)[59] <- "QCE12_SQ001"
# LimeSurvey Field type: F
data[, 60] <- as.numeric(as.factor(data[, 60]))
attributes(data)$variable.labels[60] <- "[Chaque attribut a été important au moins une fois] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 60] <- factor(data[, 60], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[60] <- "QIII9_SQ002"
# LimeSurvey Field type: F
data[, 61] <- as.numeric(as.factor(data[, 61]))
attributes(data)$variable.labels[61] <- "[Paysage] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 61] <- factor(data[, 61], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[61] <- "QIII9_SQ003"
# LimeSurvey Field type: F
data[, 62] <- as.numeric(as.factor(data[, 62]))
attributes(data)$variable.labels[62] <- "[Richesse et abondance d\'espèces] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 62] <- factor(data[, 62], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[62] <- "QIII9_SQ004"
# LimeSurvey Field type: F
data[, 63] <- as.numeric(as.factor(data[, 63]))
attributes(data)$variable.labels[63] <- "[Type d\'espèces (urbaines, periurbaines, rurales)] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 63] <- factor(data[, 63], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[63] <- "QIII9_SQ005"
# LimeSurvey Field type: F
data[, 64] <- as.numeric(as.factor(data[, 64]))
attributes(data)$variable.labels[64] <- "[Accessibilité au parc] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 64] <- factor(data[, 64], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[64] <- "QIII9_SQ006"
# LimeSurvey Field type: F
data[, 65] <- as.numeric(as.factor(data[, 65]))
attributes(data)$variable.labels[65] <- "[Temps de transport supplémentaire] Y a-t-il des critères que vous n\'avez pas du tout pris en compte pour faire vos choix ?"
data[, 65] <- factor(data[, 65], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[65] <- "QIII9_SQ007"
# LimeSurvey Field type: F
data[, 66] <- as.numeric(as.factor(data[, 66]))
attributes(data)$variable.labels[66] <- "[Je n\'ai pas assez de temps donc je préfère le trajet le plus rapide.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 66] <- factor(data[, 66], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[66] <- "QIII10_SQ001"
# LimeSurvey Field type: F
data[, 67] <- as.numeric(as.factor(data[, 67]))
attributes(data)$variable.labels[67] <- "[L\'augmentation du temps de trajet est trop importante à l’échelle de mon trajet.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 67] <- factor(data[, 67], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[67] <- "QIII10_SQ002"
# LimeSurvey Field type: F
data[, 68] <- as.numeric(as.factor(data[, 68]))
attributes(data)$variable.labels[68] <- "[Je n\'aime pas le tramway (vous pourrez en détailler les raisons dans une question ultérieure).] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 68] <- factor(data[, 68], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[68] <- "QIII10_SQ003"
# LimeSurvey Field type: F
data[, 69] <- as.numeric(as.factor(data[, 69]))
attributes(data)$variable.labels[69] <- "[Les enjeux biodiversité ou de protection de la nature ne me concernent pas.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 69] <- factor(data[, 69], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[69] <- "QIII10_SQ004"
# LimeSurvey Field type: F
data[, 70] <- as.numeric(as.factor(data[, 70]))
attributes(data)$variable.labels[70] <- "[C\'est au gouvernement d\'agir pour la biodiversité, pas au citoyens.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 70] <- factor(data[, 70], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[70] <- "QIII10_SQ005"
# LimeSurvey Field type: F
data[, 71] <- as.numeric(as.factor(data[, 71]))
attributes(data)$variable.labels[71] <- "[Je n\'arrivais pas à bien distinguer les différences entre les scénarios.] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
data[, 71] <- factor(data[, 71], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[71] <- "QIII10_SQ006"
# LimeSurvey Field type: A
data[, 72] <- as.character(data[, 72])
attributes(data)$variable.labels[72] <- "[Autre] Pour quelle(s) raison(s) avez-vous toujours préféré le scénario de référence (schéma du bas) ?"
names(data)[72] <- "QIII10_other"
# LimeSurvey Field type: F
data[, 73] <- as.numeric(as.factor(data[, 73]))
attributes(data)$variable.labels[73] <- "[Je n\'ai pas assez de temps donc je préfère le trajet le plus rapide.] Pour quelle(s) raison(s) avez-vous parfois préféré le scénario de référence (schéma du bas) ?"
data[, 73] <- factor(data[, 73], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[73] <- "QIII11_SQ001"
# LimeSurvey Field type: F
data[, 74] <- as.numeric(as.factor(data[, 74]))
attributes(data)$variable.labels[74] <- "[L\'augmentation du temps de trajet est trop importante à l’échelle de mon trajet.] Pour quelle(s) raison(s) avez-vous parfois préféré le scénario de référence (schéma du bas) ?"
data[, 74] <- factor(data[, 74], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[74] <- "QIII11_SQ002"
# LimeSurvey Field type: A
data[, 75] <- as.character(data[, 75])
attributes(data)$variable.labels[75] <- "[Autre] Pour quelle(s) raison(s) avez-vous parfois préféré le scénario de référence (schéma du bas) ?"
names(data)[75] <- "QIII11_other"
# LimeSurvey Field type: A
data[, 76] <- as.character(data[, 76])
attributes(data)$variable.labels[76] <- "[Note] Comment avez-vous trouvé cet exercice de sélection de choix ?"
names(data)[76] <- "QIII12_SQ001"
# LimeSurvey Field type: A
data[, 77] <- as.character(data[, 77])
attributes(data)$variable.labels[77] <- "[Note] Les scénarios proposés vont ont-ils semblé réalistes ?"
names(data)[77] <- "QIII13_SQ001"
# LimeSurvey Field type: A
data[, 78] <- as.character(data[, 78])
attributes(data)$variable.labels[78] <- "[Le tramway peut offrir une alternative de transport plus rapide.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[78] <- "QIII15_SQ001"
# LimeSurvey Field type: A
data[, 79] <- as.character(data[, 79])
attributes(data)$variable.labels[79] <- "[Le tramway peut offrir une alternative de transport moins cher.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[79] <- "QIII15_SQ002"
# LimeSurvey Field type: A
data[, 80] <- as.character(data[, 80])
attributes(data)$variable.labels[80] <- "[Le tramway peut offrir une alternative de transport plus écologique.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[80] <- "QIII15_SQ003"
# LimeSurvey Field type: A
data[, 81] <- as.character(data[, 81])
attributes(data)$variable.labels[81] <- "[Le tramway peut offrir une alternative de transport plus pratique.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[81] <- "QIII15_SQ004"
# LimeSurvey Field type: A
data[, 82] <- as.character(data[, 82])
attributes(data)$variable.labels[82] <- "[La préservation de la nature est un des enjeux majeurs de notre société] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[82] <- "QIII15_SQ005"
# LimeSurvey Field type: A
data[, 83] <- as.character(data[, 83])
attributes(data)$variable.labels[83] <- "[Les changements de comportements individuels ont un rôle pour préserver la nature.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[83] <- "QIII15_SQ006"
# LimeSurvey Field type: A
data[, 84] <- as.character(data[, 84])
attributes(data)$variable.labels[84] <- "[Je préfère l’autonomie et le confort du véhicule individuel aux transports collectifs.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[84] <- "QIII14_SQ001"
# LimeSurvey Field type: A
data[, 85] <- as.character(data[, 85])
attributes(data)$variable.labels[85] <- "[Les transports collectifs comme le tramway ne sont pas fiables (ex : retards).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[85] <- "QIII14_SQ002"
# LimeSurvey Field type: A
data[, 86] <- as.character(data[, 86])
attributes(data)$variable.labels[86] <- "[Je n’ai pas d’accès à une station de tramway facilement et/ou il n\'y a pas de solution pour garer ma voiture ou mon vélo à proximité.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[86] <- "QIII14_SQ003"
# LimeSurvey Field type: A
data[, 87] <- as.character(data[, 87])
attributes(data)$variable.labels[87] <- "[Le tramway est trop lent.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[87] <- "QIII14_SQ004"
# LimeSurvey Field type: A
data[, 88] <- as.character(data[, 88])
attributes(data)$variable.labels[88] <- "[Le tramway implique un itinéraire trop compliqué (ex : avec des changements).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[88] <- "QIII14_SQ010"
# LimeSurvey Field type: A
data[, 89] <- as.character(data[, 89])
attributes(data)$variable.labels[89] <- "[L’utilisation du tramway n’est pas pratique dans mon organisation quotidienne (ex : transport de matériel, enfants, PMR, etc.).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[89] <- "QIII14_SQ005"
# LimeSurvey Field type: A
data[, 90] <- as.character(data[, 90])
attributes(data)$variable.labels[90] <- "[Je ne pense pas que le développement des transports en commun soit une solution pour répondre aux enjeux climatiques.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[90] <- "QIII14_SQ006"
# LimeSurvey Field type: A
data[, 91] <- as.character(data[, 91])
attributes(data)$variable.labels[91] <- "[Je n’aime pas prendre les transports en commun (ex :  proximité avec d’autres personnes, bruit, risque d’être debout, etc).] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[91] <- "QIII14_SQ007"
# LimeSurvey Field type: A
data[, 92] <- as.character(data[, 92])
attributes(data)$variable.labels[92] <- "[Les infrastructures de transport (routes, réseau ferré) n’ont pas d’impact sur la nature.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[92] <- "QIII14_SQ008"
# LimeSurvey Field type: A
data[, 93] <- as.character(data[, 93])
attributes(data)$variable.labels[93] <- "[Les changements de comportements individuels ne permettront pas (à eux seuls) de conserver la nature.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[93] <- "QIII14_SQ009"
# LimeSurvey Field type: A
data[, 94] <- as.character(data[, 94])
attributes(data)$variable.labels[94] <- "[Cet aménagement va créer encore plus de bouchons dans la ville.] Pourriez-vous indiquer si vous êtes en accord ou en désaccord avec les affirmations suivantes :"
names(data)[94] <- "QIII14_SQ011"
# LimeSurvey Field type: F
data[, 95] <- as.numeric(as.factor(data[, 95]))
attributes(data)$variable.labels[95] <- "[Le développement d\'un tramway est déjà important.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 95] <- factor(data[, 95], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[95] <- "QIII16_SQ001"
# LimeSurvey Field type: F
data[, 96] <- as.numeric(as.factor(data[, 96]))
attributes(data)$variable.labels[96] <- "[Zones piétonnes étendues.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 96] <- factor(data[, 96], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[96] <- "QIII16_SQ002"
# LimeSurvey Field type: F
data[, 97] <- as.numeric(as.factor(data[, 97]))
attributes(data)$variable.labels[97] <- "[Itinéraires cyclables continus et protégés.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 97] <- factor(data[, 97], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[97] <- "QIII16_SQ003"
# LimeSurvey Field type: F
data[, 98] <- as.numeric(as.factor(data[, 98]))
attributes(data)$variable.labels[98] <- "[Bus à haut niveau de service (bénéficiant d’une voie réservée).] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 98] <- factor(data[, 98], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[98] <- "QIII16_SQ004"
# LimeSurvey Field type: F
data[, 99] <- as.numeric(as.factor(data[, 99]))
attributes(data)$variable.labels[99] <- "[Co-voiturage.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 99] <- factor(data[, 99], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[99] <- "QIII16_SQ005"
# LimeSurvey Field type: F
data[, 100] <- as.numeric(as.factor(data[, 100]))
attributes(data)$variable.labels[100] <- "[Augmenter la part de télétravail.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 100] <- factor(data[, 100], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[100] <- "QIII16_SQ006"
# LimeSurvey Field type: F
data[, 101] <- as.numeric(as.factor(data[, 101]))
attributes(data)$variable.labels[101] <- "[Changer de travail pour être plus proche de son domicile.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 101] <- factor(data[, 101], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[101] <- "QIII16_SQ007"
# LimeSurvey Field type: F
data[, 102] <- as.numeric(as.factor(data[, 102]))
attributes(data)$variable.labels[102] <- "[Repenser la commune pour diminuer le besoin de déplacement en voiture.] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
data[, 102] <- factor(data[, 102], levels=c(3,2),labels=c("Oui", "Non sélectionné"))
names(data)[102] <- "QIII16_SQ008"
# LimeSurvey Field type: A
data[, 103] <- as.character(data[, 103])
attributes(data)$variable.labels[103] <- "[Autre] Au lieu du développement d\'un tramway, quel autre mode de déplacement ou organisation de vie préféreriez-vous ?"
names(data)[103] <- "QIII16_other"
# LimeSurvey Field type: A
data[, 104] <- as.character(data[, 104])
attributes(data)$variable.labels[104] <- "Quelle connaissance avez-vous du projet d\'extention de tramway dans votre commune ou les communes voisines ?"
names(data)[104] <- "QIV18"
# LimeSurvey Field type: A
data[, 105] <- as.character(data[, 105])
attributes(data)$variable.labels[105] <- "Quel est votre niveau d’étude ?"
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
names(data)[109] <- "QIV22"
# LimeSurvey Field type: A
data[, 110] <- as.character(data[, 110])
attributes(data)$variable.labels[110] <- "Faites-vous ou avez-vous fait partie d’un collectif ou d’une association dont l’objectif est/était la protection de la nature (si oui, vous pouvez préciser l’objectif en question) ?"
names(data)[110] <- "QIV23"
# LimeSurvey Field type: A
data[, 111] <- as.character(data[, 111])
attributes(data)$variable.labels[111] <- "[Commentaire] Faites-vous ou avez-vous fait partie d’un collectif ou d’une association dont l’objectif est/était la protection de la nature (si oui, vous pouvez préciser l’objectif en question) ?"
names(data)[111] <- "QIV23_comment"
# LimeSurvey Field type: A
data[, 112] <- as.character(data[, 112])
attributes(data)$variable.labels[112] <- "Pratiquez-vous régulièrement des activités de pleine nature (ex : randonnée, cueillette de champignons, chasse/pêche, etc.) (si oui, vous pouvez préciser l’activité) ?"
names(data)[112] <- "QIV24"
# LimeSurvey Field type: A
data[, 113] <- as.character(data[, 113])
attributes(data)$variable.labels[113] <- "[Commentaire] Pratiquez-vous régulièrement des activités de pleine nature (ex : randonnée, cueillette de champignons, chasse/pêche, etc.) (si oui, vous pouvez préciser l’activité) ?"
names(data)[113] <- "QIV24_comment"
# LimeSurvey Field type: A
data[, 114] <- as.character(data[, 114])
attributes(data)$variable.labels[114] <- "Portez-vous une attention particulière aux critères environnementaux lors de vos achats (ex : agriculture biologique, provenance locale, etc.) (si oui, vous pouvez précise le critère) ?"
names(data)[114] <- "QIV25"
# LimeSurvey Field type: A
data[, 115] <- as.character(data[, 115])
attributes(data)$variable.labels[115] <- "[Commentaire] Portez-vous une attention particulière aux critères environnementaux lors de vos achats (ex : agriculture biologique, provenance locale, etc.) (si oui, vous pouvez précise le critère) ?"
names(data)[115] <- "QIV25_comment"
# LimeSurvey Field type: A
data[, 116] <- as.character(data[, 116])
attributes(data)$variable.labels[116] <- "Quel est votre degré de connaissance sur la nature en général ?"
names(data)[116] <- "QIV26"
# LimeSurvey Field type: A
data[, 117] <- as.character(data[, 117])
attributes(data)$variable.labels[117] <- "En moyenne, à quelle fréquence estimez-vous être en contact avec la nature ?"
names(data)[117] <- "QIV27"
# LimeSurvey Field type: A
data[, 118] <- as.character(data[, 118])
attributes(data)$variable.labels[118] <- "[Numéro de la figure] Dans l\'image ci-dessous, quelle figure décrit le mieux votre relation avec la Nature :"
names(data)[118] <- "QVnew_SQ001"
# LimeSurvey Field type: A
data[, 119] <- as.character(data[, 119])
attributes(data)$variable.labels[119] <- "Commune (code postal et nom) où se situe votre activité principale :"
names(data)[119] <- "QIV28"
# LimeSurvey Field type: A
data[, 120] <- as.character(data[, 120])
attributes(data)$variable.labels[120] <- "[Autre] Commune (code postal et nom) où se situe votre activité principale :"
names(data)[120] <- "QIV28_other"
# LimeSurvey Field type: A
data[, 121] <- as.character(data[, 121])
attributes(data)$variable.labels[121] <- "Merci pour votre participation, nous mettrons en ligne les résultats de cette enquête nationale sur le site du projet PÉPITE (Préférences sociales pour des caractéristiques Écologiques et Paysagères d’Infrastructures de Transports à l’échelle de tErritoires). Ces résultats feront également l’objet d’une publication scientifique.  Si vous êtes intéressé(e) par le sujet et souhaitez obtenir davantage d’information ou prolonger la discussion, contactez-nous à l’adresse suivante : enquete.pepite@gmail.com.     Avez-vous des remarques sur cette enquête ou plus largement sur le sujet de l’intégration écologique et paysagère des infrastructures de transport ?"
names(data)[121] <- "Qfin"

data_355435 <- data


# merge both survey while keeping survey id

data_355435$survey_id <- 355435
data_672984$survey_id <- 672984

names(data_672984) <- sub(".*..Dur","Dur" , names(data_672984))
names(data_355435) <- sub(".*..Dur","Dur" , names(data_355435))

data_all <- rbind(data_355435,data_672984)


# remove non complete responses

## not to the end, warning: including page 19 where end a lot of respondents
data_end_19 <- data_all[which(data_all$lastpage %in% c(17:19)),]

data_clean <- droplevels(data_all[which(data_all$lastpage>16),])


## remove duplicates
data_duplicate <- data_clean[which(data_clean$Qparam %in% names(which(table(data_clean$Qparam)>1))),]

data_clean <- droplevels(data_clean[which(!(data_clean$Qparam %in% names(which(table(data_clean$Qparam)>1)))),])

data_duplicate <- data_duplicate[seq(from=1, to=nrow(data_duplicate), by=2),]
data_clean <- rbind(data_clean,data_duplicate)


## note when less than 200s (3 min 20) for video
hist(data_clean$Durée.pour.le.groupe...Cartes.de.choix...présentation[data_clean$Durée.pour.le.groupe...Cartes.de.choix...présentation<300])
data_clean$time_video_short <- NA
data_clean$time_video_short[which(data_clean$Durée.pour.le.groupe...Cartes.de.choix...présentation>=200)] <- "No"
data_clean$time_video_short[which(data_clean$Durée.pour.le.groupe...Cartes.de.choix...présentation<200)] <- "Yes"

## remove when exercise not understood at all
data_clean <- droplevels(data_clean[which(!(data_clean$QCEex2 == "Non, je n'ai pas du tout compris l'exercice")),])

