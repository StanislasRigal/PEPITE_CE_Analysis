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

data_355435$survey_id <- 355435 #court
data_672984$survey_id <- 672984 #long

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

## clean "other" postal codes
data_clean$post_code_home <- as.character(data_clean$Q01)
data_clean$post_code_home[which(data_clean$post_code_home=="Autre")] <- NA

commune_liste_poste <- read.csv("raw_data/laposte_hexasmal.csv",header=TRUE, sep = ";", colClasses = rep("character",6))
commune_liste_poste$com_code <- paste0(commune_liste_poste$nom_de_la_commune,spe=" - ",commune_liste_poste$code_postal)

table(data_clean$Q01_other)

data_clean$post_code_home[which(data_clean$Q01_other %in% c("01300","31160","33290","33640","34270",
                                                            "34660","38150","44130","49290 ","59220","59600",
                                                            "59660 ","68190","69250","99","Bouches du Rhône ","Monts",
                                                            "Petit Caux ", "Lyo ","marseille","Marseille","MARSEILLE","Marseille ","MARSEILLE "))] <- NA
data_clean$post_code_home[which(data_clean$Q01_other=="01480 Savigneux")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="01480" & commune_liste_poste$nom_de_la_commune=="SAVIGNEUX")]
data_clean$post_code_home[which(data_clean$Q01_other %in% c("13007","13007 Marseille"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13007" & commune_liste_poste$libelle_d_acheminement =="MARSEILLE")]
data_clean$post_code_home[which(data_clean$Q01_other %in% c("13013","13013 marseille","13013 MARSEILLE","13013 Marseille"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13013" & commune_liste_poste$libelle_d_acheminement=="MARSEILLE")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="13014")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13014" & commune_liste_poste$libelle_d_acheminement=="MARSEILLE")]
data_clean$post_code_home[which(data_clean$Q01_other%in% c("13015","13015 marseille"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13015" & commune_liste_poste$libelle_d_acheminement=="MARSEILLE")]
data_clean$post_code_home[which(data_clean$Q01_other=="13016 estaque")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13016" & commune_liste_poste$libelle_d_acheminement=="MARSEILLE")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="13820")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13820" & commune_liste_poste$nom_de_la_commune=="ENSUES LA REDONNE")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="14200")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="14200" & commune_liste_poste$nom_de_la_commune=="HEROUVILLE ST CLAIR")]
data_clean$post_code_home[which(data_clean$Q01_other=="20000 ajaccio")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="20000" & commune_liste_poste$nom_de_la_commune=="AJACCIO")]
data_clean$post_code_home[which(data_clean$Q01_other=="27320 saint Germain sur avre")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="27320" & commune_liste_poste$nom_de_la_commune=="ST GERMAIN SUR AVRE")]
data_clean$post_code_home[which(data_clean$Q01_other=="30720 RIBAUTE LES TAVERNES")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="30720" & commune_liste_poste$nom_de_la_commune=="RIBAUTE LES TAVERNES")]
data_clean$post_code_home[which(data_clean$Q01_other=="33230 coutras")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="33230" & commune_liste_poste$nom_de_la_commune=="COUTRAS")]
data_clean$post_code_home[which(data_clean$Q01_other=="33700 - Mérignac")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="33700" & commune_liste_poste$nom_de_la_commune=="MERIGNAC")]
data_clean$post_code_home[which(data_clean$Q01_other=="34230 pouzols ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34230" & commune_liste_poste$nom_de_la_commune=="POUZOLS")]
data_clean$post_code_home[which(data_clean$Q01_other %in% c("34270 Saint-Mathieu-de-Tréviers "))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34270" & commune_liste_poste$nom_de_la_commune=="ST MATHIEU DE TREVIERS")]
data_clean$post_code_home[which(data_clean$Q01_other=="34290 Alignan du vent ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34290" & commune_liste_poste$nom_de_la_commune=="ALIGNAN DU VENT")]
data_clean$post_code_home[which(data_clean$Q01_other=="34970")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34970" & commune_liste_poste$nom_de_la_commune=="LATTES")]
data_clean$post_code_home[which(data_clean$Q01_other=="38000 Grenoble")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="38000" & commune_liste_poste$nom_de_la_commune=="GRENOBLE")]
data_clean$post_code_home[which(data_clean$Q01_other=="44000")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44000" & commune_liste_poste$nom_de_la_commune=="NANTES")]
data_clean$post_code_home[which(data_clean$Q01_other=="44130 Blain")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44130" & commune_liste_poste$nom_de_la_commune=="BLAIN")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="44130 Fay de bretagne")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44130" & commune_liste_poste$nom_de_la_commune=="FAY DE BRETAGNE")]
data_clean$post_code_home[which(data_clean$Q01_other=="44150 Ancenis ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44150" & commune_liste_poste$nom_de_la_commune=="ANCENIS ST GEREON")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="44150 Vair sur Loire ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44150" & commune_liste_poste$nom_de_la_commune=="VAIR SUR LOIRE")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="44190 GETIGNE ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44190" & commune_liste_poste$nom_de_la_commune=="GETIGNE")]
data_clean$post_code_home[which(data_clean$Q01_other %in% c("44880","44880 sautron"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44880" & commune_liste_poste$nom_de_la_commune=="SAUTRON")]
data_clean$post_code_home[which(data_clean$Q01_other %in% c("49000 ANGERS","49000","49000 Angers "))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49000" & commune_liste_poste$nom_de_la_commune=="ANGERS")]
data_clean$post_code_home[which(data_clean$Q01_other=="49110 Saint-Rémy-en-Mauges")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49110" & commune_liste_poste$nom_de_la_commune=="MONTREVAULT SUR EVRE")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="49140 cornillé les caves ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49140" & commune_liste_poste$nom_de_la_commune=="CORNILLE LES CAVES")]
data_clean$post_code_home[which(data_clean$Q01_other=="59140 DUNKERQUE")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59140" & commune_liste_poste$nom_de_la_commune=="DUNKERQUE")]
data_clean$post_code_home[which(data_clean$Q01_other=="59278")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59278" & commune_liste_poste$nom_de_la_commune=="ESCAUTPONT")]
data_clean$post_code_home[which(data_clean$Q01_other=="59400 CAMBRAI")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59400" & commune_liste_poste$nom_de_la_commune=="CAMBRAI")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="59410")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59410" & commune_liste_poste$nom_de_la_commune=="ANZIN")]
data_clean$post_code_home[which(data_clean$Q01_other=="59660 Merville")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59660" & commune_liste_poste$nom_de_la_commune=="MERVILLE")]
data_clean$post_code_home[which(data_clean$Q01_other=="67350 ETTENDORF ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="67350" & commune_liste_poste$nom_de_la_commune=="ETTENDORF")]
data_clean$post_code_home[which(data_clean$Q01_other=="67350 UHRWILLER")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="67350" & commune_liste_poste$nom_de_la_commune=="UHRWILLER")]
data_clean$post_code_home[which(data_clean$Q01_other=="69007")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="69007" & commune_liste_poste$libelle_d_acheminement=="LYON")]
data_clean$post_code_home[which(data_clean$Q01_other=="69100")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="69100" & commune_liste_poste$nom_de_la_commune=="VILLEURBANNE")]
data_clean$post_code_home[which(data_clean$Q01_other=="69190 Saint fons ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="69190" & commune_liste_poste$nom_de_la_commune=="ST FONS")]
data_clean$post_code_home[which(data_clean$Q01_other=="73370 Le Bourget-du-Lac")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="73370" & commune_liste_poste$nom_de_la_commune=="LE BOURGET DU LAC")]
data_clean$post_code_home[which(data_clean$Q01_other=="76270 Bouelles")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="76270" & commune_liste_poste$nom_de_la_commune=="BOUELLES")]
data_clean$post_code_home[which(data_clean$Q01_other=="76370 Petit-Caux")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="76370" & commune_liste_poste$nom_de_la_commune=="PETIT CAUX")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="Agnin ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="38150" & commune_liste_poste$nom_de_la_commune=="AGNIN")]
data_clean$post_code_home[which(data_clean$Q01_other %in% c("Angers ","ANGERS ","angers 49000"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49000" & commune_liste_poste$nom_de_la_commune=="ANGERS")]
data_clean$post_code_home[which(data_clean$Q01_other=="anzin 59410")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59410" & commune_liste_poste$nom_de_la_commune=="ANZIN")]
data_clean$post_code_home[which(data_clean$Q01_other=="BASSAN")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34290" & commune_liste_poste$nom_de_la_commune=="BASSAN")]
data_clean$post_code_home[which(data_clean$Q01_other=="Blain")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44130" & commune_liste_poste$nom_de_la_commune=="BLAIN")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="Caen ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="14000" & commune_liste_poste$nom_de_la_commune=="CAEN")]
data_clean$post_code_home[which(data_clean$Q01_other=="CAGNOTTE")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="40300" & commune_liste_poste$nom_de_la_commune=="CAGNOTTE")]
data_clean$post_code_home[which(data_clean$Q01_other=="Denain ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59220" & commune_liste_poste$nom_de_la_commune=="DENAIN")]
data_clean$post_code_home[which(data_clean$Q01_other=="Ettendorf 67350")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="67350" & commune_liste_poste$nom_de_la_commune=="ETTENDORF")]
data_clean$post_code_home[which(data_clean$Q01_other=="Gratentour 31150")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="31150" & commune_liste_poste$nom_de_la_commune=="GRATENTOUR")]
data_clean$post_code_home[which(data_clean$Q01_other=="illats")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="33720" & commune_liste_poste$nom_de_la_commune=="ILLATS")]
data_clean$post_code_home[which(data_clean$Q01_other=="Manosque ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="04100" & commune_liste_poste$nom_de_la_commune=="MANOSQUE")]
data_clean$post_code_home[which(data_clean$Q01_other=="Marcilly la Campagne 27320")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="27320" & commune_liste_poste$nom_de_la_commune=="MARCILLY LA CAMPAGNE")]
data_clean$post_code_home[which(data_clean$Q01_other=="Marseille 13007")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13007" & commune_liste_poste$libelle_d_acheminement=="MARSEILLE")]
data_clean$post_code_home[which(data_clean$Q01_other=="Marseille 13013")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13013" & commune_liste_poste$libelle_d_acheminement=="MARSEILLE")][1]
data_clean$post_code_home[which(data_clean$Q01_other %in% c("marseille 13016","MARSEILLE 13016","Marseille 13016 "))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13016" & commune_liste_poste$libelle_d_acheminement=="MARSEILLE")][1]
data_clean$post_code_home[which(data_clean$Q01_other %in% c("maubeuge","Maubeuge","Maubeuge ","maubeuge 59600","Maubeuge 59600"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59600" & commune_liste_poste$nom_de_la_commune=="MAUBEUGE")][1]
data_clean$post_code_home[which(data_clean$Q01_other %in% c("Montpellier","Montpellier "))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34000" & commune_liste_poste$nom_de_la_commune=="MONTPELLIER")]
data_clean$post_code_home[which(data_clean$Q01_other=="Montpellier 34090")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34090" & commune_liste_poste$nom_de_la_commune=="MONTPELLIER")]
data_clean$post_code_home[which(data_clean$Q01_other=="Montrevault sur evre ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49110" & commune_liste_poste$nom_de_la_commune=="MONTREVAULT SUR EVRE")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="Nantes")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44000" & commune_liste_poste$nom_de_la_commune=="NANTES")]
data_clean$post_code_home[which(data_clean$Q01_other=="Nantes 44300")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44300" & commune_liste_poste$nom_de_la_commune=="NANTES")]
data_clean$post_code_home[which(data_clean$Q01_other=="NEUVILLE SUR SAONE")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="69250" & commune_liste_poste$nom_de_la_commune=="NEUVILLE SUR SAONE")]
data_clean$post_code_home[which(data_clean$Q01_other=="Pont d’Ain ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="01160" & commune_liste_poste$nom_de_la_commune=="PONT D AIN")]
data_clean$post_code_home[which(data_clean$Q01_other=="Roussillon 38150")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="38150" & commune_liste_poste$nom_de_la_commune=="ROUSSILLON")]
data_clean$post_code_home[which(data_clean$Q01_other=="Saint Mathieu de Tréviers ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34270" & commune_liste_poste$nom_de_la_commune=="ST MATHIEU DE TREVIERS")]
data_clean$post_code_home[which(data_clean$Q01_other=="saint saire")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="76270" & commune_liste_poste$nom_de_la_commune=="ST SAIRE")]
data_clean$post_code_home[which(data_clean$Q01_other=="Salaise sur sanne 38150")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="38150" & commune_liste_poste$nom_de_la_commune=="SALAISE SUR SANNE")]
data_clean$post_code_home[which(data_clean$Q01_other %in% c("Servian","Servian 34290"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34290" & commune_liste_poste$nom_de_la_commune=="SERVIAN")]
data_clean$post_code_home[which(data_clean$Q01_other=="St fons")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="69190" & commune_liste_poste$nom_de_la_commune=="ST FONS")]
data_clean$post_code_home[which(data_clean$Q01_other=="ST PIERRE MONTLIMART 49110")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49110" & commune_liste_poste$nom_de_la_commune=="MONTREVAULT SUR EVRE")][1]
data_clean$post_code_home[which(data_clean$Q01_other=="Villemur-sur-Tarn 31340")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="31340" & commune_liste_poste$nom_de_la_commune=="VILLEMUR SUR TARN")]
data_clean$post_code_home[which(data_clean$Q01_other=="Villeneuve loubet ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="06270" & commune_liste_poste$nom_de_la_commune=="VILLENEUVE LOUBET")]
data_clean$post_code_home[which(data_clean$Q01_other=="Wavrechain-sous-Denain ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59220" & commune_liste_poste$nom_de_la_commune=="WAVRECHAIN SOUS DENAIN")]

#data_clean$post_code_home[which(!(data_clean$post_code_home %in% levels(data_clean$Q01)))] <- NA


data_clean$post_code_work <- as.character(data_clean$QIV28)
data_clean$post_code_work[which(data_clean$post_code_work=="Autre")] <- NA

table(data_clean$QIV28_other)

data_clean$post_code_work[which(data_clean$QIV28_other %in% c("je ne travaille pas","sur le terroire Français et à l'étranger","38150","44150","68190","69250","Sophia Antipolis","Zwevegem Belgique","Namur 5000 (Belgique)"))] <- NA
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("13002 marseille"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13002" & commune_liste_poste$libelle_d_acheminement =="MARSEILLE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("13005"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13005" & commune_liste_poste$libelle_d_acheminement =="MARSEILLE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("13007","13007 Marseille ","13007 MARSEILLE "))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13007" & commune_liste_poste$libelle_d_acheminement =="MARSEILLE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("13011"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13011" & commune_liste_poste$libelle_d_acheminement =="MARSEILLE")][1]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("13013","13013 Marseille","13013 MARSEILLE","Marseille 13013","Marseille13013"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13013" & commune_liste_poste$libelle_d_acheminement =="MARSEILLE")][1]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("13014 marseille"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13014" & commune_liste_poste$libelle_d_acheminement =="MARSEILLE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("13015","Marseille 13015"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13015" & commune_liste_poste$libelle_d_acheminement =="MARSEILLE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("13016 lestaque ", "marseille 13016", "L'ESTAQUE 13016"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13016" & commune_liste_poste$libelle_d_acheminement =="MARSEILLE")][1]
data_clean$post_code_work[which(data_clean$QIV28_other=="13820")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13820" & commune_liste_poste$nom_de_la_commune=="ENSUES LA REDONNE")][1]
data_clean$post_code_work[which(data_clean$QIV28_other=="14000")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="14000" & commune_liste_poste$nom_de_la_commune=="CAEN")]
data_clean$post_code_work[which(data_clean$QIV28_other=="14200")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="14200" & commune_liste_poste$nom_de_la_commune=="HEROUVILLE ST CLAIR")]
data_clean$post_code_work[which(data_clean$QIV28_other=="20000 ajaccio ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="20000" & commune_liste_poste$nom_de_la_commune=="AJACCIO")]
data_clean$post_code_work[which(data_clean$QIV28_other=="27100")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="27100" & commune_liste_poste$nom_de_la_commune=="VAL DE REUIL")]
data_clean$post_code_work[which(data_clean$QIV28_other=="27320 saint Germain sur avre ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="27320" & commune_liste_poste$nom_de_la_commune=="ST GERMAIN SUR AVRE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("30100","30100 ALES"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="30100" & commune_liste_poste$nom_de_la_commune=="ALES")]
data_clean$post_code_work[which(data_clean$QIV28_other=="33127 - Martignas-sur-Jalle ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="33127" & commune_liste_poste$nom_de_la_commune=="MARTIGNAS SUR JALLE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="33600 Pessac")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="33600" & commune_liste_poste$nom_de_la_commune=="PESSAC")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("34270 Saint-Mathieu-de-Tréviers ","Saint Mathieu de Tréviers "))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34270" & commune_liste_poste$nom_de_la_commune=="ST MATHIEU DE TREVIERS")]
data_clean$post_code_work[which(data_clean$QIV28_other=="34290 BASSAN")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34290" & commune_liste_poste$nom_de_la_commune=="BASSAN")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("34500 beziers","34500 Béziers"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34500" & commune_liste_poste$nom_de_la_commune=="BEZIERS")]
data_clean$post_code_work[which(data_clean$QIV28_other=="34660")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34660" & commune_liste_poste$nom_de_la_commune=="COURNONTERRAL")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("38150 agnin"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="38150" & commune_liste_poste$nom_de_la_commune=="AGNIN")]
data_clean$post_code_work[which(data_clean$QIV28_other=="38150 Roussillon")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="38150" & commune_liste_poste$nom_de_la_commune=="ROUSSILLON")]
data_clean$post_code_work[which(data_clean$QIV28_other=="38150 salaise sur sanne ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="38150" & commune_liste_poste$nom_de_la_commune=="SALAISE SUR SANNE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="44000")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44000" & commune_liste_poste$nom_de_la_commune=="NANTES")]
data_clean$post_code_work[which(data_clean$QIV28_other=="44120 VERTOU ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44120" & commune_liste_poste$nom_de_la_commune=="VERTOU")][1]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("44130 BLAIN","blain 44130"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44130" & commune_liste_poste$nom_de_la_commune=="BLAIN")][1]
data_clean$post_code_work[which(data_clean$QIV28_other=="44150 Ancenis ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44150" & commune_liste_poste$nom_de_la_commune=="ANCENIS ST GEREON")][1]
data_clean$post_code_work[which(data_clean$QIV28_other=="44150 Vair sur Loire ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44150" & commune_liste_poste$nom_de_la_commune=="VAIR SUR LOIRE")][1]
data_clean$post_code_work[which(data_clean$QIV28_other=="44800")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44800" & commune_liste_poste$nom_de_la_commune=="ST HERBLAIN")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("44880 sautron"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44880" & commune_liste_poste$nom_de_la_commune=="SAUTRON")]
data_clean$post_code_work[which(data_clean$QIV28_other=="49000")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49000" & commune_liste_poste$nom_de_la_commune=="ANGERS")]
data_clean$post_code_work[which(data_clean$QIV28_other=="49124 st Barthélemy d'anjou ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49124" & commune_liste_poste$nom_de_la_commune=="ST BARTHELEMY D ANJOU")]
data_clean$post_code_work[which(data_clean$QIV28_other=="49140 jarze")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49140" & commune_liste_poste$nom_de_la_commune=="JARZE VILLAGES")][1]
data_clean$post_code_work[which(data_clean$QIV28_other=="49240")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49240" & commune_liste_poste$nom_de_la_commune=="AVRILLE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="49290 Chalonnes sur loire ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49290" & commune_liste_poste$nom_de_la_commune=="CHALONNES SUR LOIRE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="59140 Dunkerque")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59140" & commune_liste_poste$nom_de_la_commune=="DUNKERQUE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="59220 Denain")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59220" & commune_liste_poste$nom_de_la_commune=="DENAIN")]
data_clean$post_code_work[which(data_clean$QIV28_other=="59278 Escautpont ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59278" & commune_liste_poste$nom_de_la_commune=="ESCAUTPONT")]
data_clean$post_code_work[which(data_clean$QIV28_other=="59400 CAMBRAI")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59400" & commune_liste_poste$nom_de_la_commune=="CAMBRAI")][1]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("59410","59410 Anzin"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59410" & commune_liste_poste$nom_de_la_commune=="ANZIN")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("59600","59600 Maubeuge "))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59600" & commune_liste_poste$nom_de_la_commune=="MAUBEUGE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("59660","59660 Merville"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59660" & commune_liste_poste$nom_de_la_commune=="MERVILLE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("67350 ETTENDORF ","Ettendorf 67350"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="67350" & commune_liste_poste$nom_de_la_commune=="ETTENDORF")]
data_clean$post_code_work[which(data_clean$QIV28_other=="67930 BEINHEIM")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="67930" & commune_liste_poste$nom_de_la_commune=="BEINHEIM")]
data_clean$post_code_work[which(data_clean$QIV28_other=="69003 LYON")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="69003" & commune_liste_poste$libelle_d_acheminement=="LYON")]
data_clean$post_code_work[which(data_clean$QIV28_other=="69007")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="69007" & commune_liste_poste$libelle_d_acheminement=="LYON")]
data_clean$post_code_work[which(data_clean$QIV28_other=="69100")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="69100" & commune_liste_poste$nom_de_la_commune=="VILLEURBANNE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("69190 Saint fons ","St fons"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="69190" & commune_liste_poste$nom_de_la_commune=="ST FONS")]
data_clean$post_code_work[which(data_clean$QIV28_other=="73000 Chambéry")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="73000" & commune_liste_poste$nom_de_la_commune=="CHAMBERY")]
data_clean$post_code_work[which(data_clean$QIV28_other=="76270 Bouelles")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="76270" & commune_liste_poste$nom_de_la_commune=="BOUELLES")]
data_clean$post_code_work[which(data_clean$QIV28_other=="76370 Petit-Caux")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="76370" & commune_liste_poste$nom_de_la_commune=="PETIT CAUX")][1]
data_clean$post_code_work[which(data_clean$QIV28_other=="Amboise 37400")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="37400" & commune_liste_poste$nom_de_la_commune=="AMBOISE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("Angers ","ANGERS "))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49000" & commune_liste_poste$nom_de_la_commune=="ANGERS")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Assas 34820")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34820" & commune_liste_poste$nom_de_la_commune=="ASSAS")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Beaupréau ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49600" & commune_liste_poste$nom_de_la_commune=="BEAUPREAU EN MAUGES")][5]
data_clean$post_code_work[which(data_clean$QIV28_other=="Bessières 31660")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="31660" & commune_liste_poste$nom_de_la_commune=="BESSIERES")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Bourgoin ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="38300" & commune_liste_poste$nom_de_la_commune=="BOURGOIN JALLIEU")][1]
data_clean$post_code_work[which(data_clean$QIV28_other=="CAGNOTTE")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="40300" & commune_liste_poste$nom_de_la_commune=="CAGNOTTE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Carquefou")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44470" & commune_liste_poste$nom_de_la_commune=="CARQUEFOU")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Colombier saugnieu")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="69125" & commune_liste_poste$nom_de_la_commune=="COLOMBIER SAUGNIEU")][1]
data_clean$post_code_work[which(data_clean$QIV28_other=="coutras 33230")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="33230" & commune_liste_poste$nom_de_la_commune=="COUTRAS")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Dieppe ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="76200" & commune_liste_poste$nom_de_la_commune=="DIEPPE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Gratentour 31150")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="31150" & commune_liste_poste$nom_de_la_commune=="GRATENTOUR")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Le fuilet")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="49270" & commune_liste_poste$nom_de_la_commune=="MONTREVAULT SUR EVRE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Le Tholonet")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="83340" & commune_liste_poste$nom_de_la_commune=="LE THORONET")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Ludon medoc ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="33290" & commune_liste_poste$nom_de_la_commune=="LUDON MEDOC")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Manosque")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="04100" & commune_liste_poste$nom_de_la_commune=="MANOSQUE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Marcilly la Campagne 27320")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="27320" & commune_liste_poste$nom_de_la_commune=="MARCILLY LA CAMPAGNE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("marseille","Marseille","MARSEILLE","Marseille ","MARSEILLE "))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13001" & commune_liste_poste$libelle_d_acheminement=="MARSEILLE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Martin église ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="76370" & commune_liste_poste$nom_de_la_commune=="MARTIN EGLISE")]
data_clean$post_code_work[which(data_clean$QIV28_other %in% c("maubauge 59600","maubeuge","Maubeuge"))] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59600" & commune_liste_poste$nom_de_la_commune=="MAUBEUGE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Montpellier 34090")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34090" & commune_liste_poste$nom_de_la_commune=="MONTPELLIER")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Nantes")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44000" & commune_liste_poste$nom_de_la_commune=="NANTES")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Nantes 44300")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="44300" & commune_liste_poste$nom_de_la_commune=="NANTES")]
data_clean$post_code_work[which(data_clean$QIV28_other=="NEUVILLE SUR SAONE")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="69250" & commune_liste_poste$nom_de_la_commune=="NEUVILLE SUR SAONE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="PEZENAS")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34120" & commune_liste_poste$nom_de_la_commune=="PEZENAS")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Plan de Cuques ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13380" & commune_liste_poste$nom_de_la_commune=="PLAN DE CUQUES")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Pont d’Ain ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="01160" & commune_liste_poste$nom_de_la_commune=="PONT D AIN")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Pouzols ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="34230" & commune_liste_poste$nom_de_la_commune=="POUZOLS")]
data_clean$post_code_work[which(data_clean$QIV28_other=="saint clair du rhone")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="38370" & commune_liste_poste$nom_de_la_commune=="ST CLAIR DU RHONE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Saint Paul de vence")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="06570" & commune_liste_poste$nom_de_la_commune=="ST PAUL DE VENCE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Saint Savournin")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="13119" & commune_liste_poste$nom_de_la_commune=="ST SAVOURNIN")]
data_clean$post_code_work[which(data_clean$QIV28_other=="saint-saire")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="76270" & commune_liste_poste$nom_de_la_commune=="ST SAIRE")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Tours 37100")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="37100" & commune_liste_poste$nom_de_la_commune=="TOURS")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Verrière le buisson ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="91370" & commune_liste_poste$nom_de_la_commune=="VERRIERES LE BUISSON")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Vieux reng ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59600" & commune_liste_poste$nom_de_la_commune=="VIEUX RENG")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Villeneuve loubet ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="06270" & commune_liste_poste$nom_de_la_commune=="VILLENEUVE LOUBET")]
data_clean$post_code_work[which(data_clean$QIV28_other=="Wavrechain sous Denain ")] <- commune_liste_poste$com_code[which(commune_liste_poste$code_postal=="59220" & commune_liste_poste$nom_de_la_commune=="WAVRECHAIN SOUS DENAIN")]

commune_liste_poste_unique <- data.frame(commune_liste_poste %>% distinct(code_commune_insee, code_postal, .keep_all=TRUE))

data_clean_com <- merge(data_clean,commune_liste_poste_unique, by.x="post_code_home", by.y="com_code", all.x=TRUE)

# add naturality value and class

com_isochrone <- readRDS("output/com_isochrone.rds")
data_clean_com_nat <- merge(data_clean_com,com_isochrone, by.x="code_commune_insee", by.y="com_code", all.x=TRUE)

# add naturality for communes not initially in the 570

nat_com3 <- readRDS("output/nat_com3.rds")

data_clean_com_nat_com_in_570 <- data_clean_com_nat[which(data_clean_com_nat$post_code_home %in% levels(data_clean$Q01)),]
data_clean_com_nat_com_out_570 <- data_clean_com_nat[which(!(data_clean_com_nat$post_code_home %in% levels(data_clean$Q01))),]

data_clean_com_nat_com_out_570$nat <- data_clean_com_nat_com_out_570$class_nat <- NULL

nat_com_class <- nat_com3[,c("com_code","nat")]
nat_com_class$class_nat <- "Naturality --"
nat_com_class$class_nat[which(nat_com_class$nat>=227 & nat_com_class$nat<256)] <- "Naturality -"
nat_com_class$class_nat[which(nat_com_class$nat>=256)] <- "Naturality +"

data_clean_com_nat_com_out_570 <- merge(data_clean_com_nat_com_out_570,nat_com_class, by.x="code_commune_insee", by.y="com_code", all.x=TRUE)

data_clean_com_nat <- rbind(data_clean_com_nat_com_in_570, data_clean_com_nat_com_out_570)

# look at mean duration travel
data_duration <- data_clean_com_nat[,c("QI5_SQ002","QI5_SQ003","QI5_SQ004","QI5_SQ005","QI5_SQ006")]
data_duration$QI5_SQ002[which(is.na(data_duration$QI5_SQ002))] <- 0
data_duration$QI5_SQ003[which(is.na(data_duration$QI5_SQ003))] <- 0
data_duration$QI5_SQ004[which(is.na(data_duration$QI5_SQ004))] <- 0
data_duration$QI5_SQ005[which(is.na(data_duration$QI5_SQ005))] <- 0
data_duration$QI5_SQ006[which(is.na(data_duration$QI5_SQ006))] <- 0
data_duration$all <- data_duration$QI5_SQ002 + data_duration$QI5_SQ003 + data_duration$QI5_SQ004 + data_duration$QI5_SQ005 +data_duration$QI5_SQ006
hist(data_duration$all)
data_duration2 <- split(data_duration, cut2(data_duration$all, g=6))
length(which(data_duration$all<21))
length(which(data_duration$all>20 & data_duration$all<31))
length(which(data_duration$all>30 & data_duration$all<41))
length(which(data_duration$all>40 & data_duration$all<61))
length(which(data_duration$all>60 & data_duration$all<91))
length(which(data_duration$all>90))

# Save clean response database
saveRDS(data_clean_com_nat,file="output/data_clean_com_nat.rds")


# prepare data for choice analysis with apollo

## add choice descritpion
choice_description <- read.csv("raw_data/choice_description_widest.csv", header=TRUE)

## one row per individual per choice
data_clean_long <- melt(data_clean_com_nat, measure.vars=c(unique(choice_description$Choice.situation)))
names(data_clean_long)[which(names(data_clean_long) %in% c("variable","value"))] <- c("Choice.situation","Chosen_scenario")

## merge responses with data description
data_apollo <- merge(data_clean_long,choice_description, by=c("Choice.situation"), all.x = TRUE)

## select useful columns and set readable names
data_apollo <- data_apollo[,c("id","Chosen_scenario","Paysage_25_1","Paysage_75_1","Acces_Non_1","Acces_Oui_1","Biodiversite_faible_1","Biodiversite_moyenne_1","Biodiversite_eleve_1","Biome_urbain_1","Biome_periurbain_1", "Biome_rural_1","Temps_1",
                              "Paysage_25_2","Paysage_75_2","Acces_Non_2","Acces_Oui_2","Biodiversite_faible_2","Biodiversite_moyenne_2","Biodiversite_eleve_2" ,"Biome_urbain_2","Biome_periurbain_2","Biome_rural_2" ,"Temps_2",
                              "Paysage_SQ","Acces_SQ","Biodiversite_SQ","Biome_SQ","Temps_SQ",
                        "Q01b","Q01c","Q01d","QIV19","QIV21_SQ001","QIV21_SQ002","QIV21_SQ003","QIV22",
                        "QI4_SQ002","QI4_SQ003","QI4_SQ004","QI4_SQ005","QI4_SQ006","QI4_other",
                        "QI5_SQ002","QI5_SQ003","QI5_SQ004","QI5_SQ005","QI5_SQ006",
                        "QI6_1","QI6_2","QI6_3","QI6_4","QI7",
                        "QI8_SQ001","QI8_SQ002","QI8_SQ003","QI8_SQ004","QI8_SQ005","QI8_other",
                        "QIII9_SQ002","QIII9_SQ003","QIII9_SQ004","QIII9_SQ005","QIII9_SQ006","QIII9_SQ007",
                        "QIII10_SQ001","QIII10_SQ002","QIII10_SQ003","QIII10_SQ004","QIII10_SQ005","QIII10_SQ006","QIII10_other",
                        "QIII11_SQ001","QIII11_SQ002","QIII11_other","QIII12_SQ001","QIII13_SQ001",
                        "QIII15_SQ001","QIII15_SQ002","QIII15_SQ003","QIII15_SQ004","QIII15_SQ005","QIII15_SQ006",
                        "QIII14_SQ001","QIII14_SQ002","QIII14_SQ003","QIII14_SQ004","QIII14_SQ005","QIII14_SQ010","QIII14_SQ006","QIII14_SQ007","QIII14_SQ008","QIII14_SQ009","QIII14_SQ011",
                        "QIII16_SQ001","QIII16_SQ002","QIII16_SQ003","QIII16_SQ004","QIII16_SQ005","QIII16_SQ006","QIII16_SQ007","QIII16_SQ008","QIII16_other","QIV18",
                        "QIV23","QIV23_comment","QIV24","QIV24_comment","QIV25","QIV25_comment","QIV26","QIV27","QVnew_SQ001",
                        "survey_id","time_video_short","post_code_home","post_code_work","com_centre","com_peri","com_name",
                        "pot_fin_hab","med_disp","p_csp_cadpis","p_csp_arcomce","p_csp_agr","p_csp_empl","p_csp_inter","p_csp_ouvr",
                        "nb_inact1564","pc_inact1564","dens_pop","part_domtrav_voit","car_mov_evol","ratio_fh",
                        "part_pop1529","part_pop3044","part_pop4559","part_pop6074","part_pop_65p","p_csp_retr",
                        "pop","size","nat","class_nat")]

names(data_apollo) <- c("id","Chosen_scenario","Paysage_25_1","Paysage_75_1","Acces_Non_1","Acces_Oui_1","Biodiversite_faible_1","Biodiversite_moyenne_1","Biodiversite_eleve_1","Biome_urbain_1","Biome_periurbain_1", "Biome_rural_1","Temps_1",
                        "Paysage_25_2","Paysage_75_2","Acces_Non_2","Acces_Oui_2","Biodiversite_faible_2","Biodiversite_moyenne_2","Biodiversite_eleve_2" ,"Biome_urbain_2","Biome_periurbain_2","Biome_rural_2" ,"Temps_2",
                        "Paysage_SQ","Acces_SQ","Biodiversite_SQ","Biome_SQ","Temps_SQ",
                        "Gender","Age","CSP","Education","Nb_adult","Nb_children_small","Nb_children_big","Income",
                        "most_freq_journey_professionel","most_freq_journey_associative","most_freq_journey_domestic","most_freq_journey_leisure","most_freq_journey_driver","most_freq_journey_other",
                        "time_professionel","time_associative","time_domestic","time_leisure","time_driver",
                        "vehicule_1","vehicule_2","vehicule_3","vehicule_4","carpool",
                        "hypo_most_freq_journey_professionel","hypo_most_freq_journey_associative","hypo_most_freq_journey_domestic","hypo_most_freq_journey_leisure","hypo_most_freq_journey_driver","hypo_most_freq_journey_other",
                        "all_attribute_important","non_important_paysage","non_important_biodiversity","non_important_biome","non_important_access","non_important_time",
                        "SQ_all_not_enough_time","SQ_all_too_important_time_increase","SQ_all_protest1","SQ_all_protest2","SQ_all_protest3","SQ_all_protest4","SQ_all_other",
                        "SQ_one_not_enough_time","SQ_one_too_important_time_increase","SQ_one_other","Difficulty_CE","Realistic_CE",
                        "Agree_tram_faster","Agree_tram_cheaper","Agree_tram_more_ecological","Agree_tram_more_practical","Agree_protect_nature_major","Agree_individual_effect",
                        "Agree_pref_individual_vehicule","Agree_tram_not_fiable","Agree_tram_not_accessible","Agree_tram_too_slow","Agree_tram_not_practicable","Agree_tram_journey_too_complicate","Agree_public_transport_not_a_solution","Agree_dislike_public_transport","Agree_infra_no_impact","Agree_individual_not_enough","Agree_tram_equal_more_traffic_jam",
                        "Alternative_tram_important","Alternative_pedestrian","Alternative_bicycle","Alternative_bus","Alternative_carpool","Alternative_remote_work","Alternative_closer_work","Alternative_change_city","Alternative_other","Knowledge_project",
                        "Perso_belong_eco_NGO","Perso_belong_eco_NGO_comment","Perso_nature_activity","Perso_nature_activity_comment","Perso_eco_criteria_shopping","Perso_eco_criteria_shopping_comment","Perso_knowledge_biodiversity","Perso_frequency_nature","Perso_relation_nature",
                        "survey_id","time_video_short","post_code_home","post_code_work","com_centre","com_peri","com_name",
                        "pot_fin_hab","med_disp","p_csp_cadpis","p_csp_arcomce","p_csp_agr","p_csp_empl","p_csp_inter","p_csp_ouvr",
                        "nb_inact1564","pc_inact1564","dens_pop","part_domtrav_voit","car_mov_evol","ratio_fh",
                        "part_pop1529","part_pop3044","part_pop4559","part_pop6074","part_pop_65p","p_csp_retr",
                        "pop","size","nat","class_nat")

# regroup age class, csp class...

saveRDS(data_apollo,file="output/data_apollo.rds")


# prepare data for choice analysis without apollo

## add choice descritpion
choice_description_short <- read.csv("raw_data/choice_description.csv", header=TRUE)

## one row per individual per choice
data_clean_long <- melt(data_clean_com_nat, measure.vars=c(unique(choice_description_short$Choice.situation)))
names(data_clean_long)[which(names(data_clean_long) %in% c("variable","value"))] <- c("Choice.situation","Chosen_scenario")

## merge responses with data description
data_DCE <- merge(data_clean_long,choice_description_short, by=c("Choice.situation"), all.x = TRUE)

data_DCE$choice <- ifelse(data_DCE$Chosen_scenario==data_DCE$Scenario,1,0)
data_DCE$survey_person <- paste0(data_DCE$survey_id,sep="_", data_DCE$id)
data_DCE$chid <- paste0(data_DCE$Choice.situation,sep="_",data_DCE$survey_person)

## select useful columns and set readable names
data_DCE <- data_DCE[,c("id","Scenario","choice","chid","survey_person","Paysage","Acces","Biodiversite","Biome","Temps",
                              "Q01b","Q01c","Q01d","QIV19","QIV21_SQ001","QIV21_SQ002","QIV21_SQ003","QIV22",
                              "QI4_SQ002","QI4_SQ003","QI4_SQ004","QI4_SQ005","QI4_SQ006","QI4_other",
                              "QI5_SQ002","QI5_SQ003","QI5_SQ004","QI5_SQ005","QI5_SQ006",
                              "QI6_1","QI6_2","QI6_3","QI6_4","QI7",
                              "QI8_SQ001","QI8_SQ002","QI8_SQ003","QI8_SQ004","QI8_SQ005","QI8_other",
                              "QIII9_SQ002","QIII9_SQ003","QIII9_SQ004","QIII9_SQ005","QIII9_SQ006","QIII9_SQ007",
                              "QIII10_SQ001","QIII10_SQ002","QIII10_SQ003","QIII10_SQ004","QIII10_SQ005","QIII10_SQ006","QIII10_other",
                              "QIII11_SQ001","QIII11_SQ002","QIII11_other","QIII12_SQ001","QIII13_SQ001",
                              "QIII15_SQ001","QIII15_SQ002","QIII15_SQ003","QIII15_SQ004","QIII15_SQ005","QIII15_SQ006",
                              "QIII14_SQ001","QIII14_SQ002","QIII14_SQ003","QIII14_SQ004","QIII14_SQ005","QIII14_SQ010","QIII14_SQ006","QIII14_SQ007","QIII14_SQ008","QIII14_SQ009","QIII14_SQ011",
                              "QIII16_SQ001","QIII16_SQ002","QIII16_SQ003","QIII16_SQ004","QIII16_SQ005","QIII16_SQ006","QIII16_SQ007","QIII16_SQ008","QIII16_other","QIV18",
                              "QIV23","QIV23_comment","QIV24","QIV24_comment","QIV25","QIV25_comment","QIV26","QIV27","QVnew_SQ001",
                              "survey_id","time_video_short","post_code_home","post_code_work","com_centre","com_peri","com_name",
                              "pot_fin_hab","med_disp","p_csp_cadpis","p_csp_arcomce","p_csp_agr","p_csp_empl","p_csp_inter","p_csp_ouvr",
                              "nb_inact1564","pc_inact1564","dens_pop","part_domtrav_voit","car_mov_evol","ratio_fh",
                              "part_pop1529","part_pop3044","part_pop4559","part_pop6074","part_pop_65p","p_csp_retr",
                              "pop","size","nat","class_nat")]

names(data_DCE) <- c("id","Scenario","choice","chid","survey_person","Paysage","Acces","Biodiversite","Biome","Temps",
                        "Gender","Age","CSP","Education","Nb_adult","Nb_children_small","Nb_children_big","Income",
                        "most_freq_journey_professionel","most_freq_journey_associative","most_freq_journey_domestic","most_freq_journey_leisure","most_freq_journey_driver","most_freq_journey_other",
                        "time_professionel","time_associative","time_domestic","time_leisure","time_driver",
                        "vehicule_1","vehicule_2","vehicule_3","vehicule_4","carpool",
                        "hypo_most_freq_journey_professionel","hypo_most_freq_journey_associative","hypo_most_freq_journey_domestic","hypo_most_freq_journey_leisure","hypo_most_freq_journey_driver","hypo_most_freq_journey_other",
                        "all_attribute_important","non_important_paysage","non_important_biodiversity","non_important_biome","non_important_access","non_important_time",
                        "SQ_all_not_enough_time","SQ_all_too_important_time_increase","SQ_all_protest1","SQ_all_protest2","SQ_all_protest3","SQ_all_protest4","SQ_all_other",
                        "SQ_one_not_enough_time","SQ_one_too_important_time_increase","SQ_one_other","Difficulty_CE","Realistic_CE",
                        "Agree_tram_faster","Agree_tram_cheaper","Agree_tram_more_ecological","Agree_tram_more_practical","Agree_protect_nature_major","Agree_individual_effect",
                        "Agree_pref_individual_vehicule","Agree_tram_not_fiable","Agree_tram_not_accessible","Agree_tram_too_slow","Agree_tram_not_practicable","Agree_tram_journey_too_complicate","Agree_public_transport_not_a_solution","Agree_dislike_public_transport","Agree_infra_no_impact","Agree_individual_not_enough","Agree_tram_equal_more_traffic_jam",
                        "Alternative_tram_important","Alternative_pedestrian","Alternative_bicycle","Alternative_bus","Alternative_carpool","Alternative_remote_work","Alternative_closer_work","Alternative_change_city","Alternative_other","Knowledge_project",
                        "Perso_belong_eco_NGO","Perso_belong_eco_NGO_comment","Perso_nature_activity","Perso_nature_activity_comment","Perso_eco_criteria_shopping","Perso_eco_criteria_shopping_comment","Perso_knowledge_biodiversity","Perso_frequency_nature","Perso_relation_nature",
                        "survey_id","time_video_short","post_code_home","post_code_work","com_centre","com_peri","com_name",
                        "pot_fin_hab","med_disp","p_csp_cadpis","p_csp_arcomce","p_csp_agr","p_csp_empl","p_csp_inter","p_csp_ouvr",
                        "nb_inact1564","pc_inact1564","dens_pop","part_domtrav_voit","car_mov_evol","ratio_fh",
                        "part_pop1529","part_pop3044","part_pop4559","part_pop6074","part_pop_65p","p_csp_retr",
                        "pop","size","nat","class_nat")


saveRDS(data_DCE,file="output/data_DCE.rds")
