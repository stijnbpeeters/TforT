#@iPCQ
#All costprices following the costing manual

func_iPCQ <- function(dat, referentieprijs){

#Import required packages
  require("dplyr")
  require("cbsodataR")
  require("tidyverse")
  require("here")

#Import datafile reference prices


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
    filter(Perioden_label == referentiejaar) %>%
    pull(CPI_1)

#Mutate inflation index across reference prices

  df_ref_prices <- df_ref_prices %>%
    mutate(Referentieprijs = (((cbs_inflation_new - cbs_inflation_2022) / cbs_inflation_2022) + 1) * Referentieprijs)


}
