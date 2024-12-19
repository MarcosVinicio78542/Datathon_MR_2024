# Carrega as bibliotecas necessárias
library(tidyverse)

# Importa os dados de um arquivo CSV selecionado manualmente
dados <- read.csv(choose.files(), sep = ';')

# Substitui valores "Ignorado, não informado" na coluna SEXO por NA
dados[dados$SEXO == "Ignorado, não informado", "SEXO"] <- NA

# Remove as colunas SEMAGESTAC e CONSPRENAT pois já tem essas mesmas colunas como texto
dados <- dados %>%
  select(-SEMAGESTAC, -CONSPRENAT)

# Converte valores "Ignorado" para NA em todas as colunas de texto
dados <- dados %>%
  mutate(across(where(~ is.character(.)), ~ na_if(., "Ignorado")))

# Define as colunas nominais para tratamento futuro
nominais <- c(
  "LOCNASC", "ESTCIVMAE", "PARTO", "SEXO", "RACACOR",
  "IDANOMAL", "CODUFNATU", "RACACORMAE", "STTRABPART", "STCESPARTO", "TPNASCASSI","MES.NASC"
)

# Converte colunas com varaveis orginais para fatores ordenado, respeitando
# a natureza da variavel
# Gestação
dados <- dados %>%
  mutate(GESTACAO = factor(GESTACAO, levels = c(
    "Menos de 22 semanas",
    "22 a 27 semanas",
    "28 a 31 semanas",
    "32 a 36 semanas",
    "37 a 41 semanas",
    "42 semanas e mais"
  ), ordered = TRUE))

# Gravidez
dados <- dados %>%
  mutate(GRAVIDEZ = factor(GRAVIDEZ, levels = c(
    "Única",
    "Dupla",
    "Tripla e mais"
  ), ordered = TRUE))

# Consultas pré-natais
dados <- dados %>%
  mutate(CONSULTAS = factor(CONSULTAS, levels = c(
    "Nenhuma",
    "de 1 a 3",
    "de 4 a 6",
    "7 e mais"
  ), ordered = TRUE))

# Escolaridade da mãe
dados <- dados %>%
  mutate(ESCMAE2010 = factor(ESCMAE2010, levels = c(
    "Sem escolaridade",
    "Fundamental I (1a a 4a série)",
    "Fundamental II (5ª a 8ª série)",
    "Médio (antigo 2º Grau)",
    "Superior incompleto",
    "Superior completo"
  ), ordered = TRUE))

#Categorias SEMAGESTAC_cat
dados <- dados %>%
  mutate(SEMAGESTAC_cat = factor(SEMAGESTAC_cat, levels = c(
    "Menos de 22 semanas",
    "22 a 27 semanas",
    "28 a 31 semanas",
    "32 a 36 semanas",
    "37 a 41 semanas",
    "42 semanas e mais"
  ), ordered = TRUE))

#Categorias CONSPRENAT_cat
dados <- dados %>%
  mutate(CONSPRENAT_cat = factor(CONSPRENAT_cat, levels = c(
    "Nenhuma",
    "1 a 3",
    "4 a 6",
    "7 ou mais"
  ), ordered = TRUE))

# Converte colunas nominais para fatores não ordenados
dados <- dados %>%
  mutate(across(all_of(nominais), as.factor))

# Converte a coluna DTNASC para formato de data
dados$DTNASC <- as.Date(dados$DTNASC)

# Remove linhas com valores NA
dados <- drop_na(dados)

# Exibe a estrutura do conjunto de dados
str(dados)

# Modelo de regressão por componentes principais
# scale = T para dividir cada variável pelo seu desvio padrão
# validation = "CV" para validação cruzada ser performada
pcr_model <- pls::pcr(PESO ~ ., data = dados, scale = TRUE, validation = "CV")
summary(pcr_model)

# Gráfico de validação do modelo, verificando a estabilização do RMSEP
pls::validationplot(pcr_model, val.type = "RMSEP")
abline(v = 15, col = "red", lty = 2) 
text(x = 15, y = 460, "Number of components = 15", pos = 4, col = "red")

# Comentário: O RMSEP começa em 532.2 sem componentes e diminui até 453.4
# com 15 componentes. A partir do 15º componente, estabiliza, sugerindo
# que adicionar mais componentes não melhora o modelo. Com 15 componentes,
# explica 50.04% da variabilidade dos dados e 27.45% da variável dependente.

# Ajuste do modelo PCR com 15 componentes
pcr_model_final <- pls::pcr(PESO ~ ., data = dados, scale = TRUE, ncomp = 15)

# Resumo do modelo final
summary(pcr_model_final)

# matriz de cargas usada na interpretacao de cada componentes
pcr_model_final$loadings
pcr_model$loadings
# estimacao dos coeficientes do modelo de regressao por componentes principais
betas <- coefficients(pcr_model_final)
print(betas)
