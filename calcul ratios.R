#calculs ratios financiers 
GRDF_fusion_ratios_fi_BDD_par_ex$RNE_normalise <- ""
GRDF_fusion_ratios_fi_BDD_par_ex$RNE_parK <- ""
GRDF_fusion_ratios_fi_BDD_par_ex$EBE_normalise <- ""
GRDF_fusion_ratios_fi_BDD_par_ex$EBE_parK <- ""
GRDF_fusion_ratios_fi_BDD_par_ex$EBIT_normalise <- ""
GRDF_fusion_ratios_fi_BDD_par_ex$EBIT_parK <- ""
GRDF_fusion_ratios_fi_BDD_par_ex$CAF <- ""
for (siren in GRDF_fusion_ratios_fi_BDD_par_ex$siren) {
  exercice <- data.frame(id = GRDF_fusion_ratios_fi_BDD_tt_ex$id[GRDF_fusion_ratios_fi_BDD_tt_ex$siren == siren],RNE= "",RNE_norm ="", EBE="",EBE_norm="",EBIT="",EBIT_norm="", date= "")
  for (i in exercice$id){
    #normalisation de l'exercice par la production annuelle enregistrée en GWh
    exercice$RNE_norm[exercice$id==i] <- GRDF_fusion_ratios_fi_BDD_tt_ex$Resultat_net[GRDF_fusion_ratios_fi_BDD_tt_ex$id == i]/GRDF_fusion_ratios_fi_BDD_tt_ex$`Quantité annuelle injectée (en MWh)`[GRDF_fusion_ratios_fi_BDD_tt_ex$id == i]*1000
    exercice$RNE[exercice$id==i] <- GRDF_fusion_ratios_fi_BDD_tt_ex$Resultat_net[GRDF_fusion_ratios_fi_BDD_tt_ex$id == i]
    exercice$date[exercice$id==i] <- GRDF_fusion_ratios_fi_BDD_tt_ex$annee[GRDF_fusion_ratios_fi_BDD_tt_ex$id == i]
    exercice$EBE_norm[exercice$id==i] <- GRDF_fusion_ratios_fi_BDD_tt_ex$EBE[GRDF_fusion_ratios_fi_BDD_tt_ex$id == i]/GRDF_fusion_ratios_fi_BDD_tt_ex$`Quantité annuelle injectée (en MWh)`[GRDF_fusion_ratios_fi_BDD_tt_ex$id == i]*1000
    exercice$EBE[exercice$id==i] <- GRDF_fusion_ratios_fi_BDD_tt_ex$EBE[GRDF_fusion_ratios_fi_BDD_tt_ex$id == i]
    exercice$EBIT_norm[exercice$id==i] <- GRDF_fusion_ratios_fi_BDD_tt_ex$EBIT[GRDF_fusion_ratios_fi_BDD_tt_ex$id == i]/GRDF_fusion_ratios_fi_BDD_tt_ex$`Quantité annuelle injectée (en MWh)`[GRDF_fusion_ratios_fi_BDD_tt_ex$id == i]*1000
    exercice$EBIT[exercice$id==i] <- GRDF_fusion_ratios_fi_BDD_tt_ex$EBIT[GRDF_fusion_ratios_fi_BDD_tt_ex$id == i]
    exercice$CAF[exercice$id==i] <- GRDF_fusion_ratios_fi_BDD_tt_ex$T[GRDF_fusion_ratios_fi_BDD_tt_ex$id == i]
    }
  exercice <- exercice[order(exercice$date),]
  exercice$RNE <- as.numeric(exercice$RNE)
  exercice$RNE_norm <- as.numeric(exercice$RNE_norm)
  exercice <- exercice[exercice$RNE!=0,]
  if (as.numeric(length(exercice$id)) >=3) {
    RNE_normalise<- (exercice[length(exercice$RNE),2]*3+exercice[length(exercice$RNE)-1,2]*2+exercice[length(exercice$RNE),2]*1)/6
    RNE_normalise_prod <- (exercice[length(exercice$RNE_norm),3]*3+exercice[length(exercice$RNE_norm)-1,3]*2+exercice[length(exercice$RNE_norm)-2,3]*1)/6
  } else if (as.numeric(length(exercice$id)) == 2) {
    RNE_normalise <- (exercice[length(exercice$RNE),2]*2+exercice[length(exercice$RNE)-1,2]*1)/3
    RNE_normalise_prod <- (exercice[length(exercice$RNE_norm),3]*2+exercice[length(exercice$RNE_norm)-1,3]*1)/3
  } else if (as.numeric(length(exercice$id)) == 1){
    RNE_normalise <- exercice[length(exercice$RNE),2]
    RNE_normalise_prod <- exercice[length(exercice$RNE_norm),3]
  } else if (as.numeric(length(exercice$id)) == 0){
    RNE_normalise <- 0
    RNE_normalise_prod <- 0
    }
  exercice$EBE <- as.numeric(exercice$EBE)
  exercice$EBE_norm <- as.numeric(exercice$EBE_norm)
  exercice <- exercice[exercice$EBE!=0,]
  if (as.numeric(length(exercice$id)) >=3) {
    EBE_normalise <- (exercice[length(exercice$EBE),4]*3+exercice[length(exercice$EBE)-1,4]*2+exercice[length(exercice$EBE),4]*1)/6
    EBE_normalise_prod <- (exercice[length(exercice$EBE_norm),5]*3+exercice[length(exercice$EBE_norm)-1,5]*2+exercice[length(exercice$EBE_norm)-2,5]*1)/6
   } else if (as.numeric(length(exercice$id)) == 2) {
    EBE_normalise <- (exercice[length(exercice$EBE),4]*2+exercice[length(exercice$EBE)-1,4]*1)/3
    EBE_normalise_prod <- (exercice[length(exercice$EBE_norm),5]*2+exercice[length(exercice$EBE_norm)-1,5]*1)/3
  } else if (as.numeric(length(exercice$id)) == 1){
    EBE_normalise <- exercice[length(exercice$EBE),4]
    EBE_normalise_prod <- exercice[length(exercice$EBE_norm),5]
  } else if (as.numeric(length(exercice$id)) == 0){
    EBE_normalise <- 0
    EBE_normalise_prod <- 0
  }
  exercice$EBIT <- as.numeric(exercice$EBIT)
  exercice$EBIT_norm <- as.numeric(exercice$EBIT_norm)
  exercice <- exercice[exercice$EBIT!=0,]
  if (as.numeric(length(exercice$id)) >=3) {
    EBIT_normalise <- (exercice[length(exercice$EBIT),6]*3+exercice[length(exercice$EBIT)-1,6]*2+exercice[length(exercice$EBIT),6]*1)/6
    EBIT_normalise_prod <- (exercice[length(exercice$EBIT_norm),7]*3+exercice[length(exercice$EBIT_norm)-1,7]*2+exercice[length(exercice$EBIT_norm)-2,7]*1)/6
  } else if (as.numeric(length(exercice$id)) == 2) {
    EBIT_normalise <- (exercice[length(exercice$EBIT),6]*2+exercice[length(exercice$EBIT)-1,6]*1)/3
    EBIT_normalise_prod <- (exercice[length(exercice$EBIT_norm),7]*2+exercice[length(exercice$EBIT_norm)-1,7]*1)/3
  } else if (as.numeric(length(exercice$id)) == 1){
    EBIT_normalise <- exercice[length(exercice$EBIT),6]
    EBIT_normalise_prod <- exercice[length(exercice$EBIT_norm),7]
  } else if (as.numeric(length(exercice$id)) == 0){
    EBIT_normalise <- 0
    EBIT_normalise_prod <- 0
  }
  GRDF_fusion_ratios_fi_BDD_par_ex$RNE_normalise[GRDF_fusion_ratios_fi_BDD_par_ex$siren == siren] <- RNE_normalise_prod
  GRDF_fusion_ratios_fi_BDD_par_ex$RNE_parK[GRDF_fusion_ratios_fi_BDD_par_ex$siren == siren] <- RNE_normalise/GRDF_fusion_ratios_fi_BDD_par_ex$`montant capital social`[GRDF_fusion_ratios_fi_BDD_par_ex$siren == siren]
  GRDF_fusion_ratios_fi_BDD_par_ex$EBE_normalise[GRDF_fusion_ratios_fi_BDD_par_ex$siren == siren] <- EBE_normalise_prod
  GRDF_fusion_ratios_fi_BDD_par_ex$EBE_parK[GRDF_fusion_ratios_fi_BDD_par_ex$siren == siren] <- EBE_normalise/GRDF_fusion_ratios_fi_BDD_par_ex$`montant capital social`[GRDF_fusion_ratios_fi_BDD_par_ex$siren == siren]
  GRDF_fusion_ratios_fi_BDD_par_ex$EBIT_normalise[GRDF_fusion_ratios_fi_BDD_par_ex$siren == siren] <- EBIT_normalise_prod
  GRDF_fusion_ratios_fi_BDD_par_ex$EBIT_parK[GRDF_fusion_ratios_fi_BDD_par_ex$siren == siren] <- EBIT_normalise/GRDF_fusion_ratios_fi_BDD_par_ex$`montant capital social`[GRDF_fusion_ratios_fi_BDD_par_ex$siren == siren]
  }

#dernière capacité de production 
GRDF_fusion_ratios_fi_BDD_par_ex$derniere_cap_prod <- ""
GRDF_fusion_ratios_fi_BDD_par_ex$evolution_cap_prod <- ""
for (siren in GRDF_fusion_ratios_fi_BDD_par_ex$siren) {
  exercice <- data.frame(cap_prod = GRDF_fusion_ratios_fi_BDD_tt_ex$`Capacité d’injection au 31/12 (en Nm3/h)`[GRDF_fusion_ratios_fi_BDD_tt_ex$siren == siren], date= GRDF_fusion_ratios_fi_BDD_tt_ex$annee[GRDF_fusion_ratios_fi_BDD_tt_ex$siren == siren])
  exercice <- exercice[order(exercice$date),]
  augmentation <- exercice$cap_prod[nrow(exercice)]/exercice$cap_prod[1]
  derniere_cap_prod <- exercice$cap_prod[nrow(exercice)]
  GRDF_fusion_ratios_fi_BDD_par_ex$derniere_cap_prod[GRDF_fusion_ratios_fi_BDD_par_ex$siren == siren] <- derniere_cap_prod
  GRDF_fusion_ratios_fi_BDD_par_ex$evolution_cap_prod[GRDF_fusion_ratios_fi_BDD_par_ex$siren == siren] <- augmentation
}

#write.csv2(GRDF_fusion_ratios_fi_BDD_par_ex, "GRDF ratios fi normalisés.csv", row.names = FALSE)
library(writexl)  # For exporting Excel files
write_xlsx(GRDF_fusion_ratios_fi_BDD_par_ex,"Memoire/V1 après première recherche avec R/version pour PCA/GRDF ratios fi normalisés_v23_03.xlsx")
