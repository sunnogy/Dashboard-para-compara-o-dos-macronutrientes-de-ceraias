dataset_path <- "../Data/cereal_original.csv"

df <- read.csv(dataset_path)

# Dropa as colunas desnecessarias
df <- df[, !(names(df) %in% c("shelf", "type", "mfr", "cups"))]

# Converte ounces em gramas
df$grams <- df$weight * 28.3495  # 1 ounce = 28.3495 grams

# Normaliza dividindo pelo peso e multiplicando por 100
df$normalization_factor <- 100 / df$grams

colunas_p_normalizar = c("calories", "protein", "fat", "sodium", "fiber", "carbo", "sugars", "potass", "vitamins")

# Multiplica pelo fator de normalizacao
df[, colunas_p_normalizar] <- df[, colunas_p_normalizar] * df$normalization_factor

# Dropa as colunas originais
df <- df[, !(names(df) %in% c("cups", "grams", "weight", "normalization_factor"))]

df$Nationality <- "US"

write.csv(df, "../Data/cereal.csv", row.names = FALSE)
