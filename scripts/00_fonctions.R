# mise en forme des chiffres avec séparateur de millier
formater_grand_nb <- function(x) format(x,
                   big.mark = " ",
                   scientific = FALSE)

# mise en forme des dates
formater_date <- function(x) format(x, format = "%d/%m/%Y")

# mise en forme des dates
formater_date_heure <- function(x) format(x, format = "%d/%m/%Y %H:%M:%S")

# MISE EN FORME FLEXTABLE DE BASE
formater_flex <- function(df)
  
{
  df %>%
    flextable() %>%
    autofit() %>%
    align(align = "center", part = "all")
}

###################################
get_module <- function(code)
  
{
  module <- hydroportail::get_stats_hydro(
    code = code,
    stat = "QJ_ANNUAL") %>% 
    .$descriptivestats %>% 
    .$mean
  
  module
  
}

get_qmna5 <- function(code)
  
  {
  qmna5 <- hydroportail::get_stats_hydro(code = code,
                                         stat = "QMNA") %>% 
    .$result %>% 
    .$tabs %>% 
    .$quantile %>% 
    filter(p == 0.2) %>% 
    pull(q)
  
  qmna5
  
  }

get_qmaqs <- function(code)
  
{
  qmaqs <- hydroportail::get_stats_hydro(
    code = code,
    stat = "QJ_ANNUAL") %>% 
    .$result %>% 
    .$tabs %>% 
    .$quantile %>% 
    filter(p == 0.2) %>% 
    pull(q)
  
  qmaqs
}

