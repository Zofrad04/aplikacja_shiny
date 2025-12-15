library(tidyverse)
library(plotly)

generate_subsidence_animation <- function(sciezka_do_pliku, tytul_wykresu, sep, zaokragl) {
  
  # 1. Wczytanie danych ze zmiennej 'sciezka_do_pliku'
  if (!file.exists(sciezka_do_pliku)) {
    stop("Podany plik nie istnieje! Sprawdź ścieżkę.")
  }
  
  df <- read.csv(sciezka_do_pliku, sep = sep, stringsAsFactors = FALSE)
  
  # 2. Poprawa nazw kolumn
  # Zakładamy, że struktura pliku jest zawsze taka sama
  df <- df %>%
    rename(
      S1_3_w = S1.3_w,
      S1_5_w = S1.5_w
    )
  
  # 3. Konwersja danych
  df$S1_w <- as.numeric(ifelse(df$S1_w %in% c("fałsz", "FALSE"), 0, df$S1_w))
  df$S1_3_w <- as.numeric(df$S1_3_w)
  df$S1_5_w <- as.numeric(df$S1_5_w)
  
  # 4. Tworzenie formatu LONG
  df_long <- df %>%
    pivot_longer(cols = c(S1_w, S1_3_w, S1_5_w),
                 names_to = "stan_raw", 
                 values_to = "osiadanie")
  
  
  # 5. Definicja kolejności (Faktor) i Sortowanie
  # Używamy wersji z prefiksami (1_, 2_, 3_) dla pewności sortowania
  df_long$stan <- factor(df_long$stan_raw,
                         levels = c("S1_w", "S1_3_w", "S1_5_w"),
                         labels = c("1_Stan 1", "2_Stan 1-3", "3_Stan 1-5"))
  
  # Fizyczne sortowanie (kluczowe dla animacji linii)
  df_long <- df_long %>%
    arrange(stan, vertex_ind)
  
  df_long$osiadanie_flipped <- df_long$osiadanie * -1
  
  # 6. Ustalenie zakresu osi Z
  z_min <- min(df_long$osiadanie, na.rm = TRUE)
  # Zabezpieczenie na wypadek gdyby z_min było NA lub dodatnie (choć przy osiadaniu zazwyczaj ujemne)
  if(is.infinite(z_min)) z_min <- 0 
  z_range <- c(z_min * 1.1, 0.01)
  
  # 7. Generowanie wykresu
  p <- plot_ly(df_long,
               x = ~xcoord, 
               y = ~ycoord, 
               z = ~osiadanie, 
               color = ~osiadanie_flipped, 
               colors = c("blue", "red"),
               frame = ~stan,
               type = "scatter3d", 
               mode = "lines+markers", 
               line = list(width = 4, color = 'grey'), 
               marker = list(size = 5)) %>%
    layout(
      title = tytul_wykresu,  # Tutaj wstawiamy tytuł z argumentu funkcji
      scene = list(
        xaxis = list(title = "X", showticklabels = FALSE),
        yaxis = list(title = "Y", showticklabels = FALSE),
        zaxis = list(
          title = "Osiadanie [m]", 
          range = z_range,
          tickformat = ".3f"
        ),
        aspectmode = "manual",
        aspectratio = list(x=1, y=1, z=0.5)
      )
    ) %>%
    colorbar(
      title = "Osiadanie [m]",
      tickprefix = "-", 
      tickformat = ".{zaokragl}f"
    ) %>%
    animation_opts(frame = 1500, transition = 1000, redraw = TRUE) %>%
    animation_slider(currentvalue = list(prefix = "Etap: ", 
                                         font = list(size = 12)))
  
  return(p)
}