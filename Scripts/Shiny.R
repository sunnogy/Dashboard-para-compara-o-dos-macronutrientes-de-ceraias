library(ggplot2)
library(plotly)
library(RCurl)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)

source("script.R")

cereal <- read.csv("../Data/cereal.csv")

# Define UI
ui <- navbarPage(
  selected = "cereal", theme = shinytheme("paper"),
  title = div("", img(src = "menu.png", id = "simulation", height = "60px",width = "1000px",style = "position: relative; margin:-15px 0px; display:right-align;")),
  tabPanel("Sobre",
           fluidRow(
             column(width = 10 , offset = 1, align = "center", 
                    div(
                      img(src = "sobre.png", width = "1000px", height = "3000px")
                    ),
                    tags$a(href = "https://github.com/Mordecai570/AtividadesME918/tree/main/Trabalho_Final", target = "_blank",
                           tags$i(class = "fab fa-github fa-3x"),  #fonte de letra
                           " GitHub"
                    ),
                    style = "background-color: #cef3c4; padding: 40px; border-radius: 5px; box-shadow: 0px 0px 5px 0px #ccc;"
             ), 
           )),
  tabPanel("Cadastro",
           fluidPage(
             titlePanel(""),
             sidebarLayout(
               sidebarPanel(
                 div(
                   img(src = "cadastro.png", width = "400px", height = "300px")
                 ),
                 textInput("name", "Name"),
                 numericInput("calories", "Calories", value = 0),
                 numericInput("protein", "Protein", value = 0),
                 numericInput("fat", "Fat", value = 0),
                 numericInput("sodium", "Sodium", value = 0),
                 numericInput("fiber", "Fiber", value = 0),
                 numericInput("carbo", "Carbohydrates", value = 0),
                 numericInput("sugars", "Sugars", value = 0),
                 numericInput("potass", "Potassium", value = 0),
                 numericInput("vitamins", "Vitamins", value = 0),
                 actionButton("submit", "Registrar"),
                 uiOutput("predictedRating"),
                 div(
                   img(src = "imgtabela.png", width = "400px", height = "1800px")
                 ),
                 style = "background-color: #cef3c4; padding: 20px; border-radius: 5px; box-shadow: 0px 0px 5px 0px #ccc;"
               ),
               mainPanel(
                 tableOutput("dataframeTable"),
                 downloadButton(outputId = "downloadData", label = "Download banco de dados"),
                 # Usa CSS para deixar bonito
                 style = "background-color: #cef3c4; padding: 20px; border-radius: 5px; box-shadow: 0px 0px 5px 0px #ccc;"
               )
             )
           )
  ),
  tabPanel("Estatísticas",
           fluidRow(
             column(6,plotOutput("caloriesPlot")),
             column(6,plotOutput("proteinPlot")),
             column(6,plotOutput("fatPlot")),
             column(6, plotOutput("sodiumPlot")),
             column(6, plotOutput("fiberPlot")),
             column(6, plotOutput("carboPlot")),
             column(6,plotOutput("sugarsPlot")),
             column(6,plotOutput("vitaminsPlot")),
           )
  ),
  tabPanel("Comparações",
           fluidPage(
             titlePanel(""),
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "searchName", label = "Escolha um cereal e compare ele com todos os outros!", choices = cereal$name, selected = "All", multiple = FALSE),
                 actionButton("searchButton", "Procurar"),
                 div(
                   img(src = "compara.png", width = "400px", height = "300px")
                 ),
                 # Usa CSS para deixar bonito
                 style = "padding: 20px; background-color: #cef3c4; border-radius: 5px; box-shadow: 0px 0px 5px 0px #ccc;"
               ),
               mainPanel(
                 textOutput("searchResult"),
                 uiOutput("caloriesComparison"),
                 uiOutput("sugarsComparison"),
                 uiOutput("proteinComparison"),
                 uiOutput("fatComparison"),
                 # Usa CSS para deixar bonito
                 style = "padding: 20px; background-color: #cef3c4; border-radius: 5px; box-shadow: 0px 0px 5px 0px #ccc;"
               )
             )
           )
  ),
  tags$style(HTML(".navbar-default { background-color: #e5c1c1; }")),
  
  
  
  
  
  tags$script('
      $(document).on("shiny:inputchanged", function(event) {
        if (event.name.endsWith("_color")) {
          var columnName = event.name.replace("_color", "");
          $("#" + columnName + "-stat-box").css("background-color", event.value);
        }
      });
    ')
  
  
)


# Servidor
server <- function(input, output, session) {
  # Valores reativos 
  cereal_reactive <- reactiveVal(cereal)
  
  # Função para inserir valores no cereal
  observeEvent(input$submit, {
    # Pega o df atual
    current_cereal <- cereal_reactive()
    
    # Novo registro
    new_row <- data.frame(
      name = input$name,
      calories = input$calories,
      protein = input$protein,
      fat = input$fat,
      sodium = input$sodium,
      fiber = input$fiber,
      carbo = input$carbo,
      sugars = input$sugars,
      potass = input$potass,
      vitamins = input$vitamins,
      rating = NA,
      Nationality = "BR"
    )
    
    # Predicao
    
    train_and_predict(current_cereal)
    new_row$rating = predict_rating(new_row)
    
    # checa se o df esta vazio
    cereal_reactive(rbind(current_cereal, new_row))
    
    # Adiciona o registro
    write.csv(cereal_reactive(), "../Data/cereal.csv", row.names = FALSE)
    
    output$predictedRating <- renderText({
      rating <- round(predict_rating(new_row),2)
      paste("A nota do seu cereal é ", rating)
    })
  })
  
  # Mostra o dataframe atualizado
  output$dataframeTable <- renderTable({
    cereal_reactive()
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("cereal_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(cereal, file)
    }
  )
  # GRÁFICO:
  fill_colors <- c("US" = "#B22234", "BR" = "#009c3b")
  output$caloriesPlot <- renderPlot({
    ggplot(cereal, aes(x = calories, fill = Nationality)) +
      geom_density(alpha = 0.5, position = "identity", color = "black") +
      labs(x = "Quantidade de Calorias", y = "") +
      scale_fill_manual(values = fill_colors) +
      theme_minimal() +
      labs(title = "Calorias") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$proteinPlot <- renderPlot({
    ggplot(cereal, aes(x = protein, fill = Nationality)) +
      geom_density(alpha = 0.5, position = "identity", color = "black") +
      labs(x = "Gramas de Proteínas", y = "") +
      scale_fill_manual(values = fill_colors) +
      theme_minimal() +
      labs(title = "Proteínas") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$fatPlot <- renderPlot({
    ggplot(cereal, aes(x = fat, fill = Nationality)) +
      geom_density(alpha = 0.5, position = "identity", color = "black") +
      labs(x = "Gramas de Gorduras", y = "") +
      scale_fill_manual(values = fill_colors) +
      theme_minimal() +
      labs(title = "Gorduras") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  
  output$sodiumPlot <- renderPlot({
    ggplot(cereal, aes(x = sodium, fill = Nationality)) +
      geom_density(alpha = 0.5, position = "identity", color = "black") +
      labs(x = "Miligramas de Sódio", y = "") +
      scale_fill_manual(values = fill_colors) +
      theme_minimal() +
      labs(title = "Sódio") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$fiberPlot <- renderPlot({
    ggplot(cereal, aes(x = fiber, fill = Nationality)) +
      geom_density(alpha = 0.5, position = "identity", color = "black") +
      labs(x = "Gramas de Fibras", y = "") +
      scale_fill_manual(values = fill_colors) +
      theme_minimal() +
      labs(title = "Fibras") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$carboPlot <- renderPlot({
    
    ggplot(cereal, aes(x = carbo, fill = Nationality)) +
      geom_density(alpha = 0.5, position = "identity", color = "black") +
      labs(x = "Gramas de Carboidratos Complexos", y = "") +
      scale_fill_manual(values = fill_colors) +
      theme_minimal() +
      labs(title = "Carboidratos") +
      theme(plot.title = element_text(hjust = 0.5))
    
  })
  
  output$sugarsPlot <- renderPlot({
    
    ggplot(cereal, aes(x = sugars, fill = Nationality)) +
      geom_density(alpha = 0.5, position = "identity", color = "black") +
      labs(x = "Gramas de Açúcares", y = "") +
      theme_minimal() +
      labs(title = "Açúcar") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_fill_manual(values = fill_colors) 
  })
  
  output$vitaminsPlot <- renderPlot({
    ggplot(cereal, aes(x = vitamins, fill = Nationality)) +
      geom_density(alpha = 0.5, position = "identity", color = "black") +
      labs(x = "Quantidades de Vitaminas de Minerais em % quando comparado ao valor recomendado pela FDA", y = "") +
      scale_fill_manual(values = fill_colors) +
      theme_minimal() +
      labs(title = "Vitaminas") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Compara
  output$searchResult <- renderText({
    if (input$searchName %in% cereal$name) {
      search_name <- input$searchName
      result <- ""
      selected_row <- cereal_reactive()[cereal_reactive()$name %in% search_name, ]
      result
    }
  })
  
  output$caloriesComparison <- renderUI({
    createStatBox2("Calories", "calories", cereal_reactive())
  })
  
  output$proteinComparison <- renderUI({
    createStatBox("Protein", "protein", cereal_reactive())
  })
  
  output$fatComparison <- renderUI({
    createStatBox2("Fat", "fat", cereal_reactive())
  })
  
  output$sugarsComparison <- renderUI({
    createStatBox2("Sugars", "sugars", cereal_reactive())
  })
  
  #funcao para as proteinas porque elas fazem bem 
  createStatBox <- function(label, column, data) {
    if (!is.null(input$searchButton) && input$searchButton > 0) {
      search_name <- input$searchName
      selected_row <- data[data$name == search_name, ]
      
      if (nrow(selected_row) > 0) {
        # compara
        comparison_data <- data[data$name != search_name, ]
        
        # calcula diferenca em porcentagem 
        mean_stat <- mean(comparison_data[[column]])
        stat_difference <- ((selected_row[[column]] - mean_stat) / mean_stat) * 100
        
        
        # Muda a cor 
        if (stat_difference < 0) {
          intensity <- 190*abs(stat_difference)
          color <-  paste0("rgb(", intensity*0.5, ",50 ,97)") # ruim 
        } else {
          intensity <- 210*abs(stat_difference)
          color <- paste0("rgb(167,171 ,", intensity*1, ")") # bom 
        }
        
        HTML(
          paste0(
            '<div id="', column, '-stat-box" style = "background-color: ', color,'; border: 2px solid #808080; border-radius: 10px; padding: 10px;">',
            '<center>',
            '<font style = "color: black; font-size: 24px;">', round(stat_difference, 2), '%</font>',
            '<br>',
            '<b>', label, '</b>',
            '</center>',
            '</div>'
          )
        )
      } else {
        HTML('<div>No data found for the specified name.</div>')
      }
    }
  }
  #funcao para o resto porque faz mal
  createStatBox2 <- function(label, column, data) {
    if (!is.null(input$searchButton) && input$searchButton > 0) {
      search_name <- input$searchName
      selected_row <- data[data$name == search_name, ]
      
      if (nrow(selected_row) > 0) {
        # Compara 
        comparison_data <- data[data$name != search_name, ]
        
        # calcula diferenca em porcentagem 
        mean_stat <- mean(comparison_data[[column]])
        stat_difference <- ((selected_row[[column]] - mean_stat) / mean_stat) * 100
        
        # muda a cor 
        if (stat_difference < 0) {
          intensity <- 210*abs(stat_difference)
          color <- paste0("rgb(167,171 ,", intensity*0.5, ")") # bom 
        } else {
          intensity <- 190*abs(stat_difference)
          color <-  paste0("rgb(", intensity*1, ",50 ,97)") # ruim 
        }
        
        HTML(
          paste0(
            '<div id="', column, '-stat-box" style = "background-color: ', color,'; border: 2px solid #808080; border-radius: 10px; padding: 10px;">',
            '<center>',
            '<font style = "color: black; font-size: 24px;">', round(stat_difference, 2), '%</font>',
            '<br>',
            '<b>', label, '</b>',
            '</center>',
            '</div>'
          )
        )
      } else {
        HTML('<div>No data found for the specified name.</div>')
      }
    }
  }
}
# Roda 
shinyApp(ui, server)
