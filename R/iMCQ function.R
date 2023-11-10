#@iMCQ
#All costprices following the costing manual excluding medication

func_iMCQ <- function(dat, reference_year){

#Import required libraries
  require("dplyr")
  require("cbsodataR")
  require("tidyverse")
  require("here")

#Import datafile reference prices

  df_ref_prices <- data.frame(openxlsx::read.xlsx(xlsxFile = here("data/Referentieprijzen hoofdstuk 4.xlsx"), sheet = "tab_iMCQ"))

#Download inflation index

  cbs_inflation <-  cbsodataR::cbs_get_data("83131ned") %>%
    cbsodataR::cbs_add_date_column() %>%
    cbsodataR::cbs_add_label_columns() %>%
    filter(Perioden_freq == "Y",
           Bestedingscategorieen_label == "000000 Alle bestedingen") %>%
    select(Perioden_label, CPI_1) %>%
    mutate(Perioden_label = as.numeric(as.character(Perioden_label)))

#Inflation index 2022

  cbs_inflation_2022 <- cbs_inflation %>%
    filter(Perioden_label == 2022) %>%
    pull(CPI_1)

#Inflation index new

  cbs_inflation_new <- cbs_inflation %>%
    filter(Perioden_label == reference_year) %>%
    pull(CPI_1)

#Mutate inflation index across reference prices

  df_ref_prices <- df_ref_prices %>%
    mutate(Referentieprijs = (((cbs_inflation_new - cbs_inflation_2022) / cbs_inflation_2022) + 1) * Referentieprijs)

#Names of columns that have to be included in data file (dat)

  col_names <-  c("n_GP",       #Amount of appointments with GP
                  "n_SW",       #Amount of appointments with social worker (SW)
                  "n_FYSIO",    #Amount of appointments with fysiotherapist (FYSIO)
                  "n_ERGOT",    #Amount of appointments with ergotherapist (ERGOT)
                  "n_LOGOP",    #Amount of appointments with logopedist (LOGOP)
                  "n_DIETI",    #Amount of appointments with dietician (DIETI)
                  "n_HOMEO",    #Amount of appointments with homeopathist (HOMEO)
                  "n_PSYCH",    #Amount of appointments with psychologist (PSYCH)
                  "n_OCCUP",    #Amount of appointments with occupational physician (OCCUP)
                  "n_DOMES",    #Amount of weeks of domestic care help (homecare) (DOMES)
                  "h_DOMES",    #Amount of hours of domestic care help (homecare (DOMES)
                  "n_CAREH",    #Amount of weeks of care at home (homecare) (CAREH)
                  "h_CAREH",    #Amount of hours of care at home (homecare) (CAREH)
                  "n_NURSEH",   #Amount of weeks of nursing at home (homecare) (NURSEH)
                  "h_NURSEH",   #Amount of hours of nursing at home (homecare) (NURSEH)
                  "n_EMERG",    #Amount of emergency care visits (EMERG)
                  "n_AMBUL",    #Amount of ambulance usage (AMBUL)
                  "n_POLI",     #Amount of policlinic visits (POLI)
                  "n_DAYC_HOSP",#Amount of daycare treatments in hospital (DAYC_HOSP)
                  "n_DAYC_CARE",#Amount of daycare treatments are carecenters (DAYC_CARE)
                  "n_DAYC_REVA",#Amount of daycare treatments in revalidation centers (DAYC_REVA)
                  "n_DAYC_PSYC",#Amount of daycare in psychiatric institutions (DAYC_PSYC)
                  "n_ADM_HOSP", #Amount of days admitted to hospital (ADM_HOSP)
                  "n_ADM_CARE", #Amount of days admitted to care centers (ADM_CARE)
                  "n_ADM_REVA", #Amount of days admitted to revalidation centers (ADM_REVA)
                  "n_ADM_PSYC", #Amount of days admitted to psychiatric institutions (ADM_PSYC)
                  "n_INF_CARE", #Amount of weeks of informal care (INF_CARE)
                  "h_INF_CARE" #Amount of hours of informal care (INF_CARE)
  )

#Check for the presence of required columns in the input dataset

  if(length(setdiff(col_names, names(dat))) > 0) stop(cat(
    "All iMCQ columns need to be present in dat. The following are missing:",
    setdiff(col_names, names(dat))))

#Generate new column names for the general variables by replacing n with k an exclude the special variables

  general_col_names <- col_names[!startsWith(col_names, "h") &
                                   col_names != "n_DOMES" &
                                   col_names != "n_CAREH" &
                                   col_names != "n_EMERG" &
                                   col_names != "n_INF_CARE"]

  new_general_col_names <- gsub("n", "k", general_col_names)
  spec_col_names <- c("n_DOMES, h_DOMES, n_CAREH, h_CAREH, n_EMERG, h_EMERG, n_INF_CARE, h_INF_CARE")

#Define general costprices

  general_names <- c("Huisarts, visite gemiddeld",
                     "Contact maatschappelijk werk",
                     "Fysiotherapie (per zitting)",
                     "Ergotherapie (per zitting)",
                     "Logopedie (per zitting)",
                     "Dieetadvisering (per zitting)",
                     "Contact vrijgevestigd zorgverlener in de basis GGZ",
                     "Contact zorgverlener in de generalistische basis GGZ-instellingen",
                     "Bedrijfsarts, visite gemiddeld",
                     "Spoedeisende hulp",
                     "Ambulancerit, gewogen gemiddelde",
                     "Polikliniekbezoek, ziekenhuis",
                     "Dagbehandeling, ziekenhuis",
                     "Dagbesteding woon/zorgcentrum, per dagdeel",
                     "Revalidatie behandelconsult, volwassenen",
                     "Contact zorgverlener in de specialistische GGZ-instellingen",
                     "Verpleegdag inclusief kosten personeel, ziekenhuis",
                     "Verpleging & verzorging, incl. dagbesteding, per dag",
                     "Verpleegdag revalidatiecentrum, volwassenen (incl. revalidatie behandeluren)",
                     "Verpleegdag, psychiatrische instelling")

  general_costprices <- df_ref_prices %>%
    filter(df_ref_prices$Eenheid %in% general_names) %>%
    pivot_wider(names_from = Eenheid, values_from = Referentieprijs)

#Calculate costs for the general outcomes

  costs_general <- as.data.frame(mapply(`*`, dat[general_col_names], general_costprices))

  colnames(costs_general) <- new_general_col_names


#Cbind with dat

  dat <- cbind(dat,costs_general)

#Calculate costs for the specific outcomes

  dat$k_DOMES <- df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Huishoudelijke hulp thuis"] * dat$n_DOMES *dat$h_DOMES + dat$n_DOMES * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Reiskosten, per bezoek"]
  dat$k_CAREH <- df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Persoonlijke verzorging thuis"] * dat$n_CAREH * dat$h_CAREH + dat$n_CAREH * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Reiskosten, per bezoek"]
  dat$k_NURSEH <- df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Verpleging thuis, per uur"] * dat$n_NURSEH * dat$h_NURSEH + dat$n_NURSEH * df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Reiskosten, per bezoek"]
  dat$k_INF_CARE <- df_ref_prices$Referentieprijs[df_ref_prices$Eenheid == "Vervangingskosten voor huishoudelijk werk"] * dat$n_INF_CARE * dat$h_INF_CARE

#Calculate the cost categoreies
#Direct medical costs without medication
  Direct_med_costs_cols <- c(new_general_col_names, "k_DOMES", "k_CAREH", "k_NURSEH")

  dat <- dat %>%
    rowwise() %>%
    mutate(direct_medical_costs_no_medication = sum(across(Direct_med_costs_cols)))


#Informal care costs

  dat$informal_care_costs <- dat$k_INF_CARE


#Return the dataset

  dat
}
