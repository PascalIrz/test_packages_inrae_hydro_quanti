remotes::install_gitlab("HYCAR-Hydro/hydroportail",
                        auth_token = "VK3hLxf3sUHhQVQPoxZ7",
                        host = "gitlab.irstea.fr",
                        dependencies = TRUE,
                        build_vignettes = TRUE, 
                        force = TRUE)
library(hydroportail)

# remotes::install_gitlab("olivier.delaigue/hydroportailstats@1-replace-the-use-of-the-api",
#                         auth_token = "65oy2WSu9EfpASW_oGTx",
#                         host = "gitlab.irstea.fr",
#                         dependencies = FALSE, 
#                         force = TRUE)
# library(hydroportailStats)



library(tidyverse)

rm(list=ls())

?hydroportailStats::get_stats_hydro

stat_flow <- hydroportail::get_stats_hydro(code = "Y430651001",
                                           stat = "QMNA")

# code_stat <- "J860241001" # l'Aff à Paimpont
# code_stat <- "J910000101"
# stat_flow <- get_stats_hydro(code = code_stat, stat = "QMNA")


stat_flow$result$tabs$quantile[25, ]


qmna5 <- stat_flow$result$tabs$quantile %>% 
  filter(p == 0.2) %>% 
  pull(q)



# str(data, max.level = 2)
# plot(data)

# chronique de debits mensuels sur la période consideree
ts_flow <- get_ts_hydro(code = "Y430651001",
                        metric = "QmM", # average monthly steamflow
                        date_start = min(stat_flow$seasons$start),
                        date_end   = max(stat_flow$seasons$start)) %>% 
  mutate(year = lubridate::year(t),
         month = lubridate::month(t),
         hydro_year = year + cumsum(month == stat_flow$meta$firsthydromonth) - 1)

# ts_flow$year <- ts_flow$year + cumsum(ts_flow$month == stat_flow$meta$firsthydromonth) - 1 # annee hydrologique

# ts_flow <- ts_flow[!ts_flow$year %in% as.numeric(stat_flow$meta$automaticallyexcludedseasons), ] # retrait des annees a exclure

annees_a_exclure <- stat_flow$meta$automaticallyexcludedseasons %>% 
  as.numeric()

ts_flow <- ts_flow %>% 
  filter(!hydro_year %in% annees_a_exclure)

quantile(ts_flow$v,
         probs = c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 1))


plot(stat_flow)










#####################################
# débits journaliers

qj <- hydroportailStats::get_stats_hydro(code = "Y430651001",
                                           stat = "QJ_ANNUAL")

qj$result$tabs$parameters



