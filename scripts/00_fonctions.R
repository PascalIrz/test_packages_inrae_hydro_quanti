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

