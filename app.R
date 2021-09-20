library(shiny)
library(tidyverse)
library(plotly)
library(tidyr)
library(dplyr)
# devtools::install_github("jaredhuling/jcolors")
library(jcolors)

medidas_final <- readRDS("medidas_final.rds")

medidas_final$quantil <- as.character(medidas_final$quantil)

ui <- fluidPage(
  titlePanel("Curvas"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
      width = 3,
      selectInput(
        "sexo",
        label = "Sexo Fetal",
        choices = c("masculino" = "sexom",
                    "feminino"),
        selected = "masculino",
      ),
      selectInput(
        "imc",
        label = "IMC",
        choices = c("normal",
                    "sobrepeso" = "imc_catsobrepeso", 
                    "subpeso" = "imc_catsubpeso", 
                    "obesidade" = "imc_catobesidade"),
        selected = "normal"
      ),
      selectInput(
        "cor",
        label = "Cor da Gestante",
        choices = c("branca" = "brancasn1",
                    "negra", 
                    "parda",
                    "outra"),
        selected = "outra"
      ),
      selectInput(
        "idade",
        label = "Idade da Gestante",
        choices = c("Menos de 20 anos",
                    "20 a 34 anos" = "idade_cat20-34",
                    "Mais de 34 anos" = "idade_cat35+"),
        selected = "Menos de 20 anos"
      ),
      selectInput(
        "medida_escolha",
        label = "Medida Ultrassonográfica",
        choices = c("Circunferência Abdominal" = "ca",
                    "Circunferência Cefálica" = "cc",
                    "Diâmetro Biparietal" = "dbp",
                    "Fêmur" = "femur"),
        selected = "ca"
      ),
      selectInput(
        "quantis",
        label = "Quantis",
        choices = c("0.05", "0.1", "0.2",  "0.3", "0.4", "0.5", "0.6",
                    "0.7", "0.8", "0.9", "0.95"),
        selected = c("0.1", "0.5", "0.9"),
        multiple = TRUE
        )
    ),
    mainPanel(width = 9, 
              plotlyOutput("plot"),
              verbatimTextOutput("print")) 
  )
  )
  )

server <- function(input, output, session) {
  
  output$print <- renderPrint({
    print(c(input$sexo,
            input$imc,
            input$idade,
            input$cor,
            input$quantis,
            input$medida_escolha))
  })
  
  output$plot <- renderPlotly({
    
    coeficientes <- filter(medidas_final,
                           medida == input$medida_escolha,
                           var %in% c("(Intercept)",
                                      "igaval",
                                      input$sexo,
                                      input$imc,
                                      input$idade,
                                      input$cor),
                           quantil %in% input$quantis)
    
    medidas_todas <- tapply(
      coeficientes$media[coeficientes$var != "igaval"],
      coeficientes$quantil[coeficientes$var != "igaval"],
      sum) 
    
    medidas <- data.frame(
      ig_aval = round(12 + (0:210)/7, 4)
    )
    
    func1 <- function(q1){
      out <- sum(coeficientes$media[coeficientes$var != "igaval" &
                                      coeficientes$quantil == q1]) +
        coeficientes$media[coeficientes$var == "igaval" & coeficientes$quantil == q1] * medidas$ig_aval
    }
    
    for (i in 1:length(input$quantis)){
      medidas[[paste("q",input$quantis[i],sep="")]] <- func1(input$quantis[i])
    }
    
    titulo <- ifelse(input$medida_escolha == "ca", "Circunferência Abdominal",
                     ifelse(input$medida_escolha == "cc", "Circunferência Cefálica",
                            ifelse(input$medida_escolha == "dbp", "Diâmetro Biparietal",
                                   "Fêmur")))
    
    quantis_nomes <- names(medidas)[-1]
    
    dados_longer <- medidas %>%
      pivot_longer(
        cols = quantis_nomes, 
        names_to = "q", 
        values_to = "valor"
      ) 
    
    g <- ggplot(dados_longer, aes(x=ig_aval, y=valor,group=q, shape=q, colour=q)) +
      geom_line(aes(linetype=q), size=1.2) + 
      theme(text = element_text(size=14))+
      labs(y = "",x="Idade Gestacional", title = titulo) +
      scale_x_continuous(limits = c(12, 42),
                         breaks=seq(12, 42, 3))+
      theme(legend.position="bottom") +
      theme(legend.title=element_blank())+
      theme(panel.background =   element_rect(),
            legend.background = element_rect(),
            axis.text.x = element_text(colour = "black"), 
            axis.text.y = element_text(colour = "black"))
    
    g <- g + scale_color_jcolors(palette = "pal12")
    
    ggplotly(g,
             tooltip = c("x", "y"))
    
    })
}

shinyApp(ui, server)