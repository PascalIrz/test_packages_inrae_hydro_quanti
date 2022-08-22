# station Couesnon
QMNA <- hydroportail::get_stats_hydro(code = "J012151001",
                                      stat = "QMNA")

nom_station <- QMNA$meta$entitylabel
bv_station <- QMNA$meta$hydrowatershedarea

mois_debut_annee_hydro <- QMNA$meta$firsthydromonth %>% 
  as.integer()

QmM <- get_ts_hydro(code = "J012151001",
                    metric = "QmM", # average monthly steamflow
                    date_start = min(QMNA$seasons$start),
                    date_end   = max(QMNA$seasons$start)) %>% 
  mutate(year = lubridate::year(t),
         month = lubridate::month(t),
         hydro_year = ifelse(month >= mois_debut_annee_hydro,
                             year + 1,
                             year))

# QmM$hydro_year <- QmM$year + cumsum(QmM$month == QMNA$meta$firsthydromonth) - 1 # annee hydrologique

QM_N <- QMNA$seasons

plot(QMNA)
# plot(QmM)

max_qmna <- max(QM_N$value, na.rm = TRUE)
min_qmna <- min(QM_N$value, na.rm = TRUE)

annees_a_exclure <- QMNA$meta$automaticallyexcludedseasons %>% 
  as.numeric()

QmM <- QmM %>% 
  filter(!hydro_year %in% annees_a_exclure)

QM_N <- QM_N %>% 
  filter(!year %in% annees_a_exclure)

QM_N %>% 
  filter(year != 1976) %>% 
  pull(value) %>% 
  quantile(probs = c(0.05, 0.1, 0.2, 0.5, 0.8, 0.9, 0.95))

# Ca c'est bon
qmna5 <- QMNA$result$tabs$quantile %>% 
  filter(p == 0.2) %>% 
  pull(q)


# module
nb_mois_par_an <- QmM %>% 
  group_by(year) %>% 
  tally() %>% 
  filter(n == 12)

debit_moy_an <- QmM %>% 
  filter(year %in% nb_mois_par_an$year) %>% 
  group_by(year) %>% 
    summarise(qa = mean(v, na.rm = TRUE))

debit_moy_annuel_quinq_sec <- quantile(debit_moy_an$qa,
                                       probs = 0.2)


module <-  QmM %>% 
  ungroup() %>% 
  summarise(qa = mean(qa)) %>% 
  pull(qa)
  



# stats mensuelles
stats_debit_mensuel <- QmM %>% 
  group_by(month) %>% 
  summarise(q20 = quantile(v, probs = 0.2),
            qmoy = mean(v, na.rm = T),
            n = n())

gg <- stats_debit_mensuel %>% 
  select(-n) %>% 
  pivot_longer(q20:qmoy) %>% 
  mutate(name = ifelse(name == "q20",
                       "Débit moyen mensuel quinquennal sec*",
                       "Débit moyen mensuel interannuel")) %>% 
  ggplot(aes(x = as.factor(month),
             y = value,
             fill = name)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  labs(x = "Mois",
       y = "Débit en l/s",
       fill = "",
       caption = "*Quantile 20% non ajusté statistiquement") +
  scale_y_continuous(labels = function(x) format(x,
                                                 big.mark = " ",
                                                 scientific = FALSE)) +
  scale_fill_manual(values=c("#56B4E9", "#E69F00")) +
  templatesOFB::theme_ofb() +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = module,
             linetype = "dotted") +
  geom_hline(yintercept = qmna5,
             linetype = "dashed") +
  geom_hline(yintercept = debit_moy_annuel_quinq_sec,
             linetype = "dotdash")
  

gg

# -----------------------------------------
# débits journaliers
# -----------------------------------------
qj <- hydroportail::get_stats_hydro(code = "J012151001",
                                      stat = "CRUCAL_J")

# Ca c'est bon
qja5 <- qja$result$tabs$quantile %>% 
  filter(p == 0.2) %>% 
  select(-T,
         -p,
         -u,
         -quality)

debit_crue_journaliere_biennale <- qj$result$tabs$quantile %>% 
  filter(p == 0.5) %>% 
  select(-T,
         -p,
         -u,
         -quality)

# -----------------------------------------
# débits journaliers classés
# -----------------------------------------

debits_classes <- qja$result$tabs$quantile %>% 
  select(-`T`,
         -u,
         -quality) %>% 
  rename(debit = q)

debits_classes

debits_classes %>% 
  ggplot(aes(x = (1 - p),
             y = q)) +
  geom_line()

# -----------------------------------------
# pluie
# -----------------------------------------
s_29250001 <- get_ts_meteo(code = "29250001",
              metric = "RR")

s_35067001 <- get_ts_meteo(code = "35067001",
                           metric = "RR")

class(s_35067001)

plot(s_35067001, show_period = "q")

#################################################

#♦ test hubeau hydrometrie

library(hubeau)
code_station <- "J471201002"

q_mensuels <- get_hydrometrie_obs_elab(list(code_entite = code_station,
                                       grandeur_hydro_elab = "QmM"))

q_journaliers <- get_hydrometrie_obs_elab(
  list(code_entite = code_station,
    #   date_debut_obs_elab = format(Sys.Date() -30, "%Y-%m-%d"),
       grandeur_hydro_elab = "QmJ"))




