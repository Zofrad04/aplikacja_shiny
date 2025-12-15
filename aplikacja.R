library(shiny)
library(tidyverse)
library(ggplot2)
source("generate_plot_lines.R")
source("generate_subsidence_animation.R")

ui <- fluidPage(
  titlePanel("Osiadania terenu - reprezentacja graficzna"),
  tabsetPanel(
    tabPanel("Animacje",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "wybor_animacji",
                   "Wybierz animację:",
                   choices = c(
                     "Kościół" = "kosciol",
                     "Szkoła" = "szkola",
                     "Straż pożarna" = "straz",
                     "Zalewisko" = "jezioro"
                   )
                 )
               ),
               mainPanel(
                 plotlyOutput("animacja_wybor")
               )
             )
    ),

    tabPanel("Wykresy liniowe",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "wybor_wykresu",
                   "Wybierz wykres liniowy:",
                   choices = c(
                     "Ciek" = "ciek",
                     "Tor" = "tor",
                     "Słupy" = "slupy"
                   )
                 )
               ),
               mainPanel(
                 plotOutput("wykres_wybor")
               )
             )
    )
  )
)

server <- function(input, output){
  
  output$animacja_wybor <- renderPlotly({
    switch(input$wybor_animacji,
          "kosciol" = generate_subsidence_animation("kosciol_w_cum.csv", 
                                                    "Osiadanie Kościoła pw. Matki Bożej Królowej Świata w czasie", 
                                                    ";", 2),
          "szkola" = generate_subsidence_animation("szkola_w_cum.csv", 
                                                   "Osiadanie Szkoły Podstawowej im. Jana i Kazimierza Bogdanowiczów w Nadrybiu-Dworze w czasie", 
                                                   ",", 1),
          "straz" = generate_subsidence_animation("straz_pozarna_w_cum.csv", 
                                                  "Osiadanie OSP w Nadrybiu w czasie", 
                                                  ",", 1),
          "jezioro" = generate_subsidence_animation("jezioro_w_cum.csv", 
                                                    "Osiadanie zalewiska Szczecin w czasie", 
                                                    ",", 0)
    
  )})
  
  output$wykres_wybor <- renderPlot(
    switch(input$wybor_wykresu,
            "ciek" = generate_plot_lines("w_dist_ciek.csv",
                                         "Wartość osiadania wzdłuż cieku dla poszczególnych punktów"),
            "tor" = generate_plot_lines("w_dist_tor.csv",
                                        "Wartość osiadania wzdłuż toru dla poszczególnych punktów"),
            "slupy" = generate_plot_lines("w_dist_slupy.csv",
                                          "Wartość osiadania wzdłuż linii energetycznej dla poszczególnych słupów")
  ))
    
}

shinyApp(ui, server)