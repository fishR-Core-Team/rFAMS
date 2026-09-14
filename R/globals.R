# Silence R CMD check NOTES for dplyr NSE variables
utils::globalVariables(
  c(
    "L_t", "Fec_t", "percF_t", "percFSpawn_t",
    "E_t", "F_t", "M_t", "Z_t",
    "S_noexp_t", "S_exp_t",
    "S_noexp_t1", "S_exp_t1",
    "Eggs_exp", "Eggs_noexp", "M", "P_a", "S_exp", "S_noexp", "Z", "age", "nstart",
    "totaleggs_exp", "totaleggs_noexp", "wtSPR", "yc", "year","ss_1"
  )
)
