library(tidyverse)
library(ggplot2)

generate_plot_lines <- function(sciezka_do_pliku, tytul_wykresu){

  df <- read.csv(sciezka_do_pliku, sep = ";", dec = ".", stringsAsFactors = FALSE)
  
  # Zmiana dist na wartość numeryczną
  df$dist <- as.numeric(df$dist)
  
  # Sortowanie po ID
  df <- df %>% arrange(ID)
  
  # Obliczenie skumulowanej odległości
  df <- df %>%
    mutate(dist_cum = cumsum(dist))
  
  # ustalenie ile podpisów do punktów będzie wyświetlane
  if(nrow(df)>20){
    ind <- c(1, seq(10, nrow(df), by = 10))
  } else {
    ind <- seq(1, nrow(df))
  }
  
  # Dane w formacie long
  df_long <- df %>%
    pivot_longer(
      cols = c(W1, W1_3, W1_5),
      names_to = "wariant",
      values_to = "wartosc"
    )
  
  # Nazwy do legendy
  df_long$wariant <- factor(
    df_long$wariant,
    levels = c("W1", "W1_3", "W1_5"),
    labels = c("Stan 1", "Stan 1-3", "Stan 1-5")
  )
  
  # Nowa spójna paleta (fiolet – granat – turkus)
  kolory <- c(
    "Stan 1"  = "#003f5c",   # fiolet
    "Stan 1-3" = "#bc5090",  # granat
    "Stan 1-5" = "#ffa600"   # turkus
  )

  
  # Wykres
  p <- ggplot(df_long, aes(x = dist_cum, y = wartosc, color = wariant)) +
    geom_line(size = 0.9) +
    geom_point(size = 2.8) +
    scale_color_manual(values = kolory) +
    
    # Oś X: etykiety ID + przeniesienie na górę + proste etykiety
    scale_x_continuous(
      breaks = df$dist_cum[ind],
      labels = df$ID[ind],
      position = "top"
    ) +
    
    labs(
      title = tytul_wykresu,
      x = "Numer punktu",
      y = "Osiadania W [m]",
      color = "Legenda"
    ) +
    
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "right",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 12),
      
      axis.text.x = element_text(angle = 0, hjust = 0.5, margin = margin(t = 1000)),
      axis.title.x = element_text(size = 13, margin = margin(b = 1000)),
      axis.title.y = element_text(size = 13),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  return(p)

}