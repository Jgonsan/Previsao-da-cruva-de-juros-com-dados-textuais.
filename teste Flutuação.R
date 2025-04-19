library(murphydiagram)
library(readr)
library(patchwork)
library(ggplot2)
library(zoo)        # para as.yearmon
library(patchwork)  # para combinar os gráficos
erros_modelos_h1 <- read_csv("C:/Users/jhona/OneDrive/Desktop/Resultados/Codigo/erros_selecionados_h1.csv")
erros_modelos_h12 <- read_csv("C:/Users/jhona/OneDrive/Desktop/Resultados/Codigo/erros_selecionados_h12.csv")
setwd("C:/Users/jhona/OneDrive/Desktop/Resultados")
getwd()

# M1
loss_rw_M1 <- as.numeric(erros_modelos_h1$rw_M1)
loss_only_M1 <- as.numeric(erros_modelos_h1$only_M1)
loss_tone_copom_M1 <- as.numeric(erros_modelos_h1$tone_copom_M1)
loss_tone_fomc_M1 <- as.numeric(erros_modelos_h1$tone_fomc_M1)
loss_polaridade_copom_M1 <- as.numeric(erros_modelos_h1$polaridade_copom_M1)
loss_polaridade_fomc_M1 <- as.numeric(erros_modelos_h1$polaridade_fomc_M1)

# M4
loss_rw_M4 <- as.numeric(erros_modelos_h1$rw_M4)
loss_only_M4 <- as.numeric(erros_modelos_h1$only_M4)
loss_tone_copom_M4 <- as.numeric(erros_modelos_h1$tone_copom_M4)
loss_tone_fomc_M4 <- as.numeric(erros_modelos_h1$tone_fomc_M4)
loss_polaridade_copom_M4 <- as.numeric(erros_modelos_h1$polaridade_copom_M4)
loss_polaridade_fomc_M4 <- as.numeric(erros_modelos_h1$polaridade_fomc_M4)

# M10 
loss_rw_M10 <- as.numeric(erros_modelos_h1$rw_M10)
loss_only_M10 <- as.numeric(erros_modelos_h1$only_M10)
loss_tone_copom_M10 <- as.numeric(erros_modelos_h1$tone_copom_M10)
loss_tone_fomc_M10 <- as.numeric(erros_modelos_h1$tone_fomc_M10)
loss_polaridade_copom_M10 <- as.numeric(erros_modelos_h1$polaridade_copom_M10)
loss_polaridade_fomc_M10 <- as.numeric(erros_modelos_h1$polaridade_fomc_M10)



# M13
loss_rw_M13 <- as.numeric(erros_modelos_h1$rw_M13)
loss_only_M13 <- as.numeric(erros_modelos_h1$only_M13)
loss_tone_copom_M13 <- as.numeric(erros_modelos_h1$tone_copom_M13)
loss_tone_fomc_M13 <- as.numeric(erros_modelos_h1$tone_fomc_M13)
loss_polaridade_copom_M13 <- as.numeric(erros_modelos_h1$polaridade_copom_M13)
loss_polaridade_fomc_M13 <- as.numeric(erros_modelos_h1$polaridade_fomc_M13)



# Vetores fornecidos
ano <- c(2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,2020,
         2021,2021,2021,2021,2021,2021,2021,2021,2021,2021,2021,2021,
         2022,2022,2022,2022,2022,2022,2022,2022,2022,2022,2022,2022,
         2023,2023,2023,2023,2023,2023,2023,2023,2023,2023,2023,2023,
         2024)

mes <- c(2,3,4,5,6,7,8,9,10,11,12,1,2,3,4,5,6,7,8,9,10,11,12,
         1,2,3,4,5,6,7,8,9,10,11,12,
         1,2,3,4,5,6,7,8,9,10,11,12,
         1,2,3,4,5,6,7,8,9,10,11,12,
         1)


# Criar uma string com primeiro dia do mês
datas <- as.Date(paste(ano, mes, "01", sep = "-"))

# Obter último dia do mês
library(lubridate)
time_labels <- ceiling_date(datas, "month") - days(1)

# Ver resultado
time_labels <- format(time_labels, "%b/%Y")  # ou "%m/%Y"



# Teste de flutuação
fluct_test <- fluctuation_test(
  loss_rw_M1, 
  loss_only_M1,
  mu = 0.1,
  dmv_fullsample = TRUE,
  lag_truncate = 0,
  conf_level = 0.1,
  time_labels = time_labels
)






############################ Teste para o modelo Sem dados textuais para horizonte 1



# Vetores com os nomes das colunas para comparação
colunas_para_comparar <- c("only_M1",
                           "only_M4",
                           "only_M10",
                           "only_M13")


# Cria uma lista vazia para armazenar os resultados
resultados_fluct <- list()

# Loop para aplicar o teste em cada modelo comparado
for (coluna in colunas_para_comparar) {
  
  # Extrai os erros para a coluna atual
  loss_comparacao <- as.numeric(erros_modelos_h1[[coluna]])
  
  # Aplica o teste de flutuação
  fluct_test_result <- fluctuation_test(
    loss_rw_M1, 
    loss_comparacao,
    mu = 0.1,
    dmv_fullsample = TRUE,
    lag_truncate = 0,
    conf_level = 0.1,
    time_labels = time_labels
  )
  
  
  # Armazena o resultado na lista com o nome da coluna
  resultados_fluct[[coluna]] <- fluct_test_result
}

# Alterando as chaves do objeto 'resultados_fluct'
names(resultados_fluct) <- c("Horizonte de previsão: 1 mês| Maturidade: 3M", "Horizonte de previsão: 1 mês| Maturidade: 12M", "Horizonte de previsão: 1 mês| Maturidade: 36M", "Horizonte de previsão: 1 mês| Maturidade: 60M")


# Lista para armazenar os gráficos
graficos <- list()

# Loop para aplicar o teste e guardar os gráficos
for (coluna in names(resultados_fluct)) {  # Usar names para percorrer os elementos de resultados_fluct
  # Extrai os erros do resultado atual
  modelo <- resultados_fluct[[coluna]]  # Acessa diretamente o item da lista pelo nome
  
  # Prepara o dataframe para o gráfico
  df_result <- modelo$df  # Acesso ao dataframe dentro do modelo
  df_result$time <- as.yearmon(df_result$time, format = "%b/%Y")
  cv <- modelo$CV  # Acesso ao vetor CV
  
  # Cria o gráfico individual
  p <- ggplot(df_result, aes(x = time, y = dmstat, group = 1)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = cv, linetype = "dashed", color = "red") +  # Use o primeiro valor de CV para linha vermelha
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(
      title = paste("Modelo:", coluna),
      x = "Tempo",
      y = "Estatística de Flutuação (DM)"
    ) +
    theme_minimal() +
    scale_x_yearmon(format = "%b/%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          plot.title = element_text(hjust = 0.5)  # Centraliza o título do gráfico individual
    )
  # Armazenando o gráfico na lista
  graficos[[coluna]] <- p
}

# Combinar todos os gráficos com título geral
final_plot <- wrap_plots(graficos, ncol = 2) + 
  plot_annotation(title = "Teste de flutuação para o modelo: Sem dados textuais") &
  theme(plot.title = element_text(hjust = 0.5))  # Centraliza o título geral


# Exibir o gráfico final
final_plot




############################ Teste para o modelo Tom do Copom para horizonte 1



# Vetores com os nomes das colunas para comparação
colunas_para_comparar <- c(
  "tone_copom_M1",
  "tone_copom_M4",
  "tone_copom_M10",
  "tone_copom_M13"
)


# Cria uma lista vazia para armazenar os resultados
resultados_fluct <- list()

# Loop para aplicar o teste em cada modelo comparado
for (coluna in colunas_para_comparar) {
  
  # Extrai os erros para a coluna atual
  loss_comparacao <- as.numeric(erros_modelos_h1[[coluna]])
  
  # Aplica o teste de flutuação
  fluct_test_result <- fluctuation_test(
    loss_rw_M1, 
    loss_comparacao,
    mu = 0.1,
    dmv_fullsample = TRUE,
    lag_truncate = 0,
    conf_level = 0.1,
    time_labels = time_labels
  )
  
  
  # Armazena o resultado na lista com o nome da coluna
  resultados_fluct[[coluna]] <- fluct_test_result
}

# Alterando as chaves do objeto 'resultados_fluct'
names(resultados_fluct) <- c("Horizonte de previsão: 1 mês| Maturidade: 3M", "Horizonte de previsão: 1 mês| Maturidade: 12M", "Horizonte de previsão: 1 mês| Maturidade: 36M", "Horizonte de previsão: 1 mês| Maturidade: 60M")


# Lista para armazenar os gráficos
graficos <- list()

# Loop para aplicar o teste e guardar os gráficos
for (coluna in names(resultados_fluct)) {  # Usar names para percorrer os elementos de resultados_fluct
  # Extrai os erros do resultado atual
  modelo <- resultados_fluct[[coluna]]  # Acessa diretamente o item da lista pelo nome
  
  # Prepara o dataframe para o gráfico
  df_result <- modelo$df  # Acesso ao dataframe dentro do modelo
  df_result$time <- as.yearmon(df_result$time, format = "%b/%Y")
  cv <- modelo$CV  # Acesso ao vetor CV
  
  # Cria o gráfico individual
  p <- ggplot(df_result, aes(x = time, y = dmstat, group = 1)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = cv, linetype = "dashed", color = "red") +  # Use o primeiro valor de CV para linha vermelha
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(
      title = paste("Modelo:", coluna),
      x = "Tempo",
      y = "Estatística de Flutuação (DM)"
    ) +
    theme_minimal() +
    scale_x_yearmon(format = "%b/%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          plot.title = element_text(hjust = 0.5)  # Centraliza o título do gráfico individual
    )
  # Armazenando o gráfico na lista
  graficos[[coluna]] <- p
}

# Combinar todos os gráficos com título geral
final_plot <- wrap_plots(graficos, ncol = 2) + 
  plot_annotation(title = "Teste de flutuação para o modelo: Tom do Copom") &
  theme(plot.title = element_text(hjust = 0.5))  # Centraliza o título geral


# Exibir o gráfico final
final_plot




############################ Teste para o modelo Tom do FOMC para horizonte 1



# Vetores com os nomes das colunas para comparação
colunas_para_comparar <- c(
  "tone_fomc_M1",
  "tone_fomc_M4",
  "tone_fomc_M10",
  "tone_fomc_M13"
)


# Cria uma lista vazia para armazenar os resultados
resultados_fluct <- list()

# Loop para aplicar o teste em cada modelo comparado
for (coluna in colunas_para_comparar) {
  
  # Extrai os erros para a coluna atual
  loss_comparacao <- as.numeric(erros_modelos_h1[[coluna]])
  
  # Aplica o teste de flutuação
  fluct_test_result <- fluctuation_test(
    loss_rw_M1, 
    loss_comparacao,
    mu = 0.1,
    dmv_fullsample = TRUE,
    lag_truncate = 0,
    conf_level = 0.1,
    time_labels = time_labels
  )
  
  
  # Armazena o resultado na lista com o nome da coluna
  resultados_fluct[[coluna]] <- fluct_test_result
}

# Alterando as chaves do objeto 'resultados_fluct'
names(resultados_fluct) <- c("Horizonte de previsão: 1 mês| Maturidade: 3M", "Horizonte de previsão: 1 mês| Maturidade: 12M", "Horizonte de previsão: 1 mês| Maturidade: 36M", "Horizonte de previsão: 1 mês| Maturidade: 60M")


# Lista para armazenar os gráficos
graficos <- list()

# Loop para aplicar o teste e guardar os gráficos
for (coluna in names(resultados_fluct)) {  # Usar names para percorrer os elementos de resultados_fluct
  # Extrai os erros do resultado atual
  modelo <- resultados_fluct[[coluna]]  # Acessa diretamente o item da lista pelo nome
  
  # Prepara o dataframe para o gráfico
  df_result <- modelo$df  # Acesso ao dataframe dentro do modelo
  df_result$time <- as.yearmon(df_result$time, format = "%b/%Y")
  cv <- modelo$CV  # Acesso ao vetor CV
  
  # Cria o gráfico individual
  p <- ggplot(df_result, aes(x = time, y = dmstat, group = 1)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = cv, linetype = "dashed", color = "red") +  # Use o primeiro valor de CV para linha vermelha
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(
      title = paste("Modelo:", coluna),
      x = "Tempo",
      y = "Estatística de Flutuação (DM)"
    ) +
    theme_minimal() +
    scale_x_yearmon(format = "%b/%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          plot.title = element_text(hjust = 0.5)  # Centraliza o título do gráfico individual
    )
  # Armazenando o gráfico na lista
  graficos[[coluna]] <- p
}

# Combinar todos os gráficos com título geral
final_plot <- wrap_plots(graficos, ncol = 2) + 
  plot_annotation(title = "Teste de flutuação para o modelo: Tom do FOMC") &
  theme(plot.title = element_text(hjust = 0.5))  # Centraliza o título geral


# Exibir o gráfico final
final_plot



############################ Teste para o modelo Polaridade do Copom  para horizonte 1



# Vetores com os nomes das colunas para comparação
colunas_para_comparar <- c(
  "polaridade_copom_M1",
  "polaridade_copom_M4",
  "polaridade_copom_M10",
  "polaridade_copom_M13"
)


# Cria uma lista vazia para armazenar os resultados
resultados_fluct <- list()

# Loop para aplicar o teste em cada modelo comparado
for (coluna in colunas_para_comparar) {
  
  # Extrai os erros para a coluna atual
  loss_comparacao <- as.numeric(erros_modelos_h1[[coluna]])
  
  # Aplica o teste de flutuação
  fluct_test_result <- fluctuation_test(
    loss_rw_M1, 
    loss_comparacao,
    mu = 0.1,
    dmv_fullsample = TRUE,
    lag_truncate = 0,
    conf_level = 0.1,
    time_labels = time_labels
  )
  
  
  # Armazena o resultado na lista com o nome da coluna
  resultados_fluct[[coluna]] <- fluct_test_result
}

# Alterando as chaves do objeto 'resultados_fluct'
names(resultados_fluct) <- c("Horizonte de previsão: 1 mês| Maturidade: 3M", "Horizonte de previsão: 1 mês| Maturidade: 12M", "Horizonte de previsão: 1 mês| Maturidade: 36M", "Horizonte de previsão: 1 mês| Maturidade: 60M")


# Lista para armazenar os gráficos
graficos <- list()

# Loop para aplicar o teste e guardar os gráficos
for (coluna in names(resultados_fluct)) {  # Usar names para percorrer os elementos de resultados_fluct
  # Extrai os erros do resultado atual
  modelo <- resultados_fluct[[coluna]]  # Acessa diretamente o item da lista pelo nome
  
  # Prepara o dataframe para o gráfico
  df_result <- modelo$df  # Acesso ao dataframe dentro do modelo
  df_result$time <- as.yearmon(df_result$time, format = "%b/%Y")
  cv <- modelo$CV  # Acesso ao vetor CV
  
  # Cria o gráfico individual
  p <- ggplot(df_result, aes(x = time, y = dmstat, group = 1)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = cv, linetype = "dashed", color = "red") +  # Use o primeiro valor de CV para linha vermelha
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(
      title = paste("Modelo:", coluna),
      x = "Tempo",
      y = "Estatística de Flutuação (DM)"
    ) +
    theme_minimal() +
    scale_x_yearmon(format = "%b/%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          plot.title = element_text(hjust = 0.5)  # Centraliza o título do gráfico individual
    )
  # Armazenando o gráfico na lista
  graficos[[coluna]] <- p
}

# Combinar todos os gráficos com título geral
final_plot <- wrap_plots(graficos, ncol = 2) + 
  plot_annotation(title = "Teste de flutuação para o modelo: Polaridade do Copom") &
  theme(plot.title = element_text(hjust = 0.5))  # Centraliza o título geral


# Exibir o gráfico final
final_plot

############################ Teste para o modelo Polaridade do FOMC  para horizonte 1



# Vetores com os nomes das colunas para comparação
colunas_para_comparar <- c(
  "polaridade_fomc_M1",
  "polaridade_fomc_M4",
  "polaridade_fomc_M10",
  "polaridade_fomc_M13"
)


# Cria uma lista vazia para armazenar os resultados
resultados_fluct <- list()

# Loop para aplicar o teste em cada modelo comparado
for (coluna in colunas_para_comparar) {
  
  # Extrai os erros para a coluna atual
  loss_comparacao <- as.numeric(erros_modelos_h1[[coluna]])
  
  # Aplica o teste de flutuação
  fluct_test_result <- fluctuation_test(
    loss_rw_M1, 
    loss_comparacao,
    mu = 0.1,
    dmv_fullsample = TRUE,
    lag_truncate = 0,
    conf_level = 0.1,
    time_labels = time_labels
  )
  
  
  # Armazena o resultado na lista com o nome da coluna
  resultados_fluct[[coluna]] <- fluct_test_result
}

# Alterando as chaves do objeto 'resultados_fluct'
names(resultados_fluct) <- c("Horizonte de previsão: 1 mês| Maturidade: 3M", "Horizonte de previsão: 1 mês| Maturidade: 12M", "Horizonte de previsão: 1 mês| Maturidade: 36M", "Horizonte de previsão: 1 mês| Maturidade: 60M")


# Lista para armazenar os gráficos
graficos <- list()

# Loop para aplicar o teste e guardar os gráficos
for (coluna in names(resultados_fluct)) {  # Usar names para percorrer os elementos de resultados_fluct
  # Extrai os erros do resultado atual
  modelo <- resultados_fluct[[coluna]]  # Acessa diretamente o item da lista pelo nome
  
  # Prepara o dataframe para o gráfico
  df_result <- modelo$df  # Acesso ao dataframe dentro do modelo
  df_result$time <- as.yearmon(df_result$time, format = "%b/%Y")
  cv <- modelo$CV  # Acesso ao vetor CV
  
  # Cria o gráfico individual
  p <- ggplot(df_result, aes(x = time, y = dmstat, group = 1)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = cv, linetype = "dashed", color = "red") +  # Use o primeiro valor de CV para linha vermelha
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(
      title = paste("Modelo:", coluna),
      x = "Tempo",
      y = "Estatística de Flutuação (DM)"
    ) +
    theme_minimal() +
    scale_x_yearmon(format = "%b/%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          plot.title = element_text(hjust = 0.5)  # Centraliza o título do gráfico individual
    )
  # Armazenando o gráfico na lista
  graficos[[coluna]] <- p
}

# Combinar todos os gráficos com título geral
final_plot <- wrap_plots(graficos, ncol = 2) + 
  plot_annotation(title = "Teste de flutuação para o modelo: Polaridade do FOMC") &
  theme(plot.title = element_text(hjust = 0.5))  # Centraliza o título geral


# Exibir o gráfico final
final_plot


































































# Vetores com os nomes das colunas para comparação
colunas_para_comparar <- c("only_M1", "tone_copom_M1", "tone_fomc_M1", "polaridade_copom_M1", "polaridade_fomc_M1")


# Dicionário com nomes mais amigáveis
nomes_bonitos <- c(
  only_M1 = "Sem dados textuais",
  tone_copom_M1 = "Tom Copom",
  tone_fomc_M1 = "Tom FOMC",
  polaridade_copom_M1 = "Polaridade Copom",
  polaridade_fomc_M1 = "Polaridade FOMC"
)



# Cria uma lista vazia para armazenar os resultados
resultados_fluct <- list()

# Loop para aplicar o teste em cada modelo comparado
for (coluna in colunas_para_comparar) {
  
  # Extrai os erros para a coluna atual
  loss_comparacao <- as.numeric(erros_modelos_h1[[coluna]])
  
  # Aplica o teste de flutuação
  fluct_test_result <- fluctuation_test(
    loss_rw_M1, 
    loss_comparacao,
    mu = 0.1,
    dmv_fullsample = TRUE,
    lag_truncate = 0,
    conf_level = 0.05,
    time_labels = time_labels
  )
  
  # Armazena o resultado na lista com o nome da coluna
  resultados_fluct[[coluna]] <- fluct_test_result
}

# Alterando as chaves do objeto 'resultados_fluct'
names(resultados_fluct) <- c("Sem dados textuais, Maturidade: 3M", "Tom Copom, Maturidade: 3M", "Tom FOMC, Maturidade: 3M", "Polaridade Copom, Maturidade: 3M", "Polaridade FOMC, Maturidade: 3M")


# Lista para armazenar os gráficos
graficos <- list()

# Loop para aplicar o teste e guardar os gráficos
for (coluna in names(resultados_fluct)) {  # Usar names para percorrer os elementos de resultados_fluct
  # Extrai os erros do resultado atual
  modelo <- resultados_fluct[[coluna]]  # Acessa diretamente o item da lista pelo nome
  
  # Prepara o dataframe para o gráfico
  df_result <- modelo$df  # Acesso ao dataframe dentro do modelo
  df_result$time <- as.yearmon(df_result$time, format = "%b/%Y")
  cv <- modelo$CV  # Acesso ao vetor CV
  
  # Cria o gráfico individual
  p <- ggplot(df_result, aes(x = time, y = dmstat, group = 1)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = cv[1], linetype = "dashed", color = "red") +  # Use o primeiro valor de CV para linha vermelha
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(
      title = paste("Modelo:", coluna),
      x = "Data",
      y = "Estatística de Flutuação (DM)"
    ) +
    theme_minimal() +
    scale_x_yearmon(format = "%b/%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  # Armazenando o gráfico na lista
  graficos[[coluna]] <- p
}

# Combinar todos os gráficos
final_plot <- wrap_plots(graficos, ncol = 2)  # Ajuste o número de colunas conforme necessário

# Exibir o gráfico final
final_plot
  






# Vetores com os nomes das colunas para comparação
colunas_para_comparar <- c("only_M4", "tone_copom_M4", "tone_fomc_M4", "polaridade_copom_M4", "polaridade_fomc_M4")


# Cria uma lista vazia para armazenar os resultados
resultados_fluct <- list()

# Loop para aplicar o teste em cada modelo comparado
for (coluna in colunas_para_comparar) {
  
  # Extrai os erros para a coluna atual
  loss_comparacao <- as.numeric(erros_modelos_h1[[coluna]])
  
  # Aplica o teste de flutuação
  fluct_test_result <- fluctuation_test(
    loss_rw_M1, 
    loss_comparacao,
    mu = 0.1,
    dmv_fullsample = TRUE,
    lag_truncate = 0,
    conf_level = 0.05,
    time_labels = time_labels
  )
  
  # Armazena o resultado na lista com o nome da coluna
  resultados_fluct[[coluna]] <- fluct_test_result
}

# Alterando as chaves do objeto 'resultados_fluct'
names(resultados_fluct) <- c("Sem dados textuais, Maturidade: 12M", "Tom Copom, Maturidade: 12M", "Tom FOMC, Maturidade: 12M", "Polaridade Copom, Maturidade: 12M", "Polaridade FOMC, Maturidade: 12M")


# Lista para armazenar os gráficos
graficos <- list()

# Loop para aplicar o teste e guardar os gráficos
for (coluna in names(resultados_fluct)) {  # Usar names para percorrer os elementos de resultados_fluct
  # Extrai os erros do resultado atual
  modelo <- resultados_fluct[[coluna]]  # Acessa diretamente o item da lista pelo nome
  
  # Prepara o dataframe para o gráfico
  df_result <- modelo$df  # Acesso ao dataframe dentro do modelo
  df_result$time <- as.yearmon(df_result$time, format = "%b/%Y")
  cv <- modelo$CV  # Acesso ao vetor CV
  
  # Cria o gráfico individual
  p <- ggplot(df_result, aes(x = time, y = dmstat, group = 1)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = cv[1], linetype = "dashed", color = "red") +  # Use o primeiro valor de CV para linha vermelha
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(
      title = paste("Modelo:", coluna),
      x = "Tempo",
      y = "Estatística de Flutuação (DM)"
    ) +
    theme_minimal() +
    scale_x_yearmon(format = "%b/%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  # Armazenando o gráfico na lista
  graficos[[coluna]] <- p
}

# Combinar todos os gráficos
final_plot2 <- wrap_plots(graficos, ncol = 2)  # Ajuste o número de colunas conforme necessário

# Exibir o gráfico final
final_plot2



# Vetores com os nomes das colunas para comparação
colunas_para_comparar <- c("only_M8", "tone_copom_M8", "tone_fomc_M8", "polaridade_copom_M8", "polaridade_fomc_M8")


# Cria uma lista vazia para armazenar os resultados
resultados_fluct <- list()

# Loop para aplicar o teste em cada modelo comparado
for (coluna in colunas_para_comparar) {
  
  # Extrai os erros para a coluna atual
  loss_comparacao <- as.numeric(erros_modelos_h1[[coluna]])
  
  # Aplica o teste de flutuação
  fluct_test_result <- fluctuation_test(
    loss_rw_M1, 
    loss_comparacao,
    mu = 0.1,
    dmv_fullsample = TRUE,
    lag_truncate = 0,
    conf_level = 0.05,
    time_labels = time_labels
  )
  
  # Armazena o resultado na lista com o nome da coluna
  resultados_fluct[[coluna]] <- fluct_test_result
}

# Alterando as chaves do objeto 'resultados_fluct'
names(resultados_fluct) <- c("Sem dados textuais, Maturidade: 24M", "Tom Copom, Maturidade: 24M", "Tom FOMC, Maturidade: 24M", "Polaridade Copom, Maturidade: 24M", "Polaridade FOMC, Maturidade: 24M")


# Lista para armazenar os gráficos
graficos <- list()

# Loop para aplicar o teste e guardar os gráficos
for (coluna in names(resultados_fluct)) {  # Usar names para percorrer os elementos de resultados_fluct
  # Extrai os erros do resultado atual
  modelo <- resultados_fluct[[coluna]]  # Acessa diretamente o item da lista pelo nome
  
  # Prepara o dataframe para o gráfico
  df_result <- modelo$df  # Acesso ao dataframe dentro do modelo
  df_result$time <- as.yearmon(df_result$time, format = "%b/%Y")
  cv <- modelo$CV  # Acesso ao vetor CV
  
  # Cria o gráfico individual
  p <- ggplot(df_result, aes(x = time, y = dmstat, group = 1)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = cv[1], linetype = "dashed", color = "red") +  # Use o primeiro valor de CV para linha vermelha
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(
      title = paste("Modelo:", coluna),
      x = "Tempo",
      y = "Estatística de Flutuação (DM)"
    ) +
    theme_minimal() +
    scale_x_yearmon(format = "%b/%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  # Armazenando o gráfico na lista
  graficos[[coluna]] <- p
}

# Combinar todos os gráficos
final_plot2 <- wrap_plots(graficos, ncol = 2)  # Ajuste o número de colunas conforme necessário

# Exibir o gráfico final
final_plot2





'rw_M1', 'only_M1', 'tone_copom_M1', 'tone_fomc_M1', 'polaridade_copom_M1', 'polaridade_fomc_M1', ...
'rw_M4', 'only_M4', 'tone_copom_M4', 'tone_fomc_M4', 'polaridade_copom_M4', 'polaridade_fomc_M4', ...
'rw_M8', 'only_M8', 'tone_copom_M8', 'tone_fomc_M8', 'polaridade_copom_M8', 'polaridade_fomc_M8', ...
'rw_M12', 'only_M12', 'tone_copom_M12', 'tone_fomc_M12', 'polaridade_copom_M12', 'polaridade_fomc_M12', ...
'rw_M13', 'only_M13', 'tone_copom_M13', 'tone_fomc_M13', 'polaridade_copom_M13', 'polaridade_fomc_M13' ...
});































# Vetores com os nomes das colunas para comparação
colunas_para_comparar <- c("only_M13", "tone_copom_M13", "tone_fomc_M13", "polaridade_copom_M13", "polaridade_fomc_M13")


# Cria uma lista vazia para armazenar os resultados
resultados_fluct <- list()

# Loop para aplicar o teste em cada modelo comparado
for (coluna in colunas_para_comparar) {
  
  # Extrai os erros para a coluna atual
  loss_comparacao <- as.numeric(erros_modelos_h1[[coluna]])
  
  # Aplica o teste de flutuação
  fluct_test_result <- fluctuation_test(
    loss_rw_M1, 
    loss_comparacao,
    mu = 0.1,
    dmv_fullsample = TRUE,
    lag_truncate = 0,
    conf_level = 0.1,
    time_labels = time_labels
  )
  
  # Armazena o resultado na lista com o nome da coluna
  resultados_fluct[[coluna]] <- fluct_test_result
}

# Alterando as chaves do objeto 'resultados_fluct'
names(resultados_fluct) <- c("Sem dados textuais, Maturidade: 60M", "Tom Copom, Maturidade: 60M", "Tom FOMC, Maturidade: 60M", "Polaridade Copom, Maturidade: 60M", "Polaridade FOMC, Maturidade: 60M")


# Lista para armazenar os gráficos
graficos <- list()

# Loop para aplicar o teste e guardar os gráficos
for (coluna in names(resultados_fluct)) {  # Usar names para percorrer os elementos de resultados_fluct
  # Extrai os erros do resultado atual
  modelo <- resultados_fluct[[coluna]]  # Acessa diretamente o item da lista pelo nome
  
  # Prepara o dataframe para o gráfico
  df_result <- modelo$df  # Acesso ao dataframe dentro do modelo
  df_result$time <- as.yearmon(df_result$time, format = "%b/%Y")
  cv <- modelo$CV  # Acesso ao vetor CV
  
  # Cria o gráfico individual
  p <- ggplot(df_result, aes(x = time, y = dmstat, group = 1)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = cv, linetype = "dashed", color = "red") +  # Use o primeiro valor de CV para linha vermelha
    geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
    labs(
      title = paste("Modelo:", coluna),
      x = "Tempo",
      y = "Estatística de Flutuação (DM)"
    ) +
    theme_minimal() +
    scale_x_yearmon(format = "%b/%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5)  # Centraliza o título do gráfico individual
    )
  # Armazenando o gráfico na lista
  graficos[[coluna]] <- p
}

# Combinar todos os gráficos com título geral
final_plot <- wrap_plots(graficos, ncol = 2) + 
  plot_annotation(title = "Análise de Estatísticas de Flutuação (DM) dos Modelos") &
  theme(plot.title = element_text(hjust = 0.5))  # Centraliza o título geral


# Exibir o gráfico final
final_plot



