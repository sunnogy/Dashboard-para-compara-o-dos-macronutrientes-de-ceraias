# Install and load necessary libraries
#install.packages("tidyverse")
#install.packages("caret")
library(tidyverse)
library(caret)

train_and_predict <- function(cereal_data) {

  # Remove columns "name" and "Nationality"
  cols_to_remove <- c("name", "Nationality")
  cereal_data <- cereal_data[, !colnames(cereal_data) %in% cols_to_remove]
  
  # Divide os dados entre teste e treino
  set.seed(123)
  split_index <- createDataPartition(cereal_data$rating, p = 0.8, list = FALSE)
  train_data <- cereal_data[split_index, ]
  test_data <- cereal_data[-split_index, ]
  
  # Modelo de regressão linear 
  lm_model <- lm(rating ~ ., data = train_data)
  
  # Faz previsões com base no modelo 
  predictions <- predict(lm_model, newdata = test_data)
  
  # Avaliação do modelo 
  mse <- mean((test_data$rating - predictions)^2)
  rmse <- sqrt(mse)
  cat("RMSE:", rmse, "\n")
  
  # Printando os coeficientes 
  cat("Coefficients:\n")
  print(coef(lm_model))
  
  # Salvando o modelo na pasta Models
  saveRDS(lm_model, "../Models/modelo_atual.rds")
}

# Como usar:
# Basta rodar o comando abaixo para gerar e salvar o modelo :)
# train_and_predict(df)

# Função que aplica o modelo gerado anteriormente para gerar um único resultado 
predict_rating <- function(new_observation) {
  model <- readRDS("../Models/modelo_atual.rds")
  prediction <- predict(model, newdata = new_observation)
  
  # Extract the numeric value from the named vector
  numeric_prediction <- as.numeric(prediction[1])
  
  return(numeric_prediction)
}


# A entrada da função deve ser do seguinte formato: 
# estamos usando por padrão a estrutura de dataframe
# new_observation <- data.frame(
#   name = "NewBrand",
#   calories = 100,
#   protein = 3,
#   fat = 1,
#   sodium = 200,
#   fiber = 5,
#   carbo = 15,
#   sugars = 5,
#   potass = 200,
#   vitamins = 25,
#   shelf = 3
# )
# 
# #Chamada da função usando o objeto criado
# predicted_rating <- predict_rating(new_observation)
# #printando o resultado da função
# cat("Predicted Rating:", predicted_rating, "\n")
