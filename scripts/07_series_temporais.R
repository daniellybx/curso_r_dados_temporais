#instalando e carregando pacotes
#install.packages("pacman")

#carregando pacotes necessários
pacman::p_load(
   read.dbc,      # importando dados
   here,          # alocando arquivos
   tidyverse,     # manipulação de dados e criação de gráficos
   tsibble,       # manipulando séries temporais
   slider,        # calculo de médias móveis
   imputeTS,      # para corrigir dados perdidos
   feasts,        # para composição e autocorrelação de uma série temporal
   forecast,      # ajustando predições temporais
   trending,      # ajuste e avaliação de modelos
   yardstick,     # para avaliar a acurácia do modelo
   surveillance  # para detectar dados errados
)

#importando dados de internação

files = cbind(paste0("dados/sih_df/", list.files("dados/sih_df/", pattern = "\\.dbc$")))

sihdf = NULL

for (i in 1:length(files)) {
  sihdf_temp = read.dbc(files[i])
  sihdf_temp2 = data.frame(DATA = as.Date(sihdf_temp$DT_INTER, "%Y%m%d"))
  sihdf= data.frame(rbind(sihdf, sihdf_temp2))
}

#excluindo data.frames que não serão usados
sihdf_temp = NULL
sihdf_temp2 = NULL

#criando data.frame com internações por data
sihdf$CASOS = 1

#agregando número de casos por dia
sihdf2 = aggregate(CASOS ~ DATA, data = sihdf, sum)

#investigando a amplitude das datas
summary(sihdf2$DATA)

#definindo intervalo de tempo
sihdf2 = subset(sihdf2, sihdf2$DATA >= "2008-01-01" & sihdf2$DATA <= "2021-09-30")

#criando uma variável de semana epidemiológica
sihdf2 <- sihdf2 %>% 
  mutate(SE = yearweek(DATA, week_start = 1))

sihdf3 = aggregate(CASOS ~ SE, data = sihdf2, sum)

#definindo uma série temporal a partir da semana epidemiológica
tssihdf <- tsibble(sihdf3, index = SE)

#plotando a série de internações por semana epidemiológica
ggplot(tssihdf, aes(x = SE, y = CASOS)) + 
  geom_line()+
  theme_minimal()+
  labs(                            # incluindo título do gráfico e dos eixos
    title = "Número de internações por semana epidemiológica no DF, 2008-2021",
    x = "Semana Epidemiológica",
    y = "Internações")

#removendo outliers

##identificando o outlier
ggplot(tssihdf, aes(x = CASOS))+
  geom_boxplot()

summary(tssihdf$CASOS)

tssihdf$CASOS = ifelse(tssihdf$CASOS < 2700 | tssihdf$CASOS > 4700, NA, tssihdf$CASOS)

#plotando a série de internações por semana epidemiológica
ggplot(tssihdf, aes(x = SE, y = CASOS)) + 
  geom_line()+
  theme_minimal()+
  labs(                            # incluindo título do gráfico e dos eixos
    title = "Número de internações por semana epidemiológica no DF, 2008-2021",
    x = "Semana Epidemiológica",
    y = "Internações")

#identificando valores perdidos na série
tssihdf <- tssihdf %>% 
  mutate(MISSING = if_else(
    str_detect(SE, "W51|W52|W53|W01|W02"), 
    NA_real_, 
    CASOS
  ))

#substituindo valores perdidos por interpolação
tssihdf <- tssihdf %>% 
  mutate(CASOS_INT = imputeTS::na_interpolation(MISSING)
  )

##gerando gráfico com os dados interpolados
ggplot_na_imputations(tssihdf$MISSING, tssihdf$CASOS_INT) + 
  theme_classic()

#analise descritiva da série temporal por médias móveis
tssihdf <- tssihdf %>% 
  mutate(MA4SE = slider::slide_dbl(CASOS, 
                                  ~ mean(.x, na.rm = TRUE),
                                  .before = 4))

##gerando um gráfico com a série e com a média móvel
ggplot(tssihdf, aes(x = SE)) + 
  geom_line(aes(y = CASOS), colour = "grey") + 
  geom_line(aes(y = MA4SE), colour = "red")+
  theme_minimal()+
  labs(                           
    title = "Média móvel de internações por semana epidemiológica no DF, 2008-2021",
    x = "Semana Epidemiológica",
    y = "Internações e Média móvel")

#analisando os componentes da série temporal para análise inferencial

##decompondo a série temporal
tssihdf %>% 
  model(classical_decomposition(CASOS_INT, type = "additive")) %>% 
  components() %>% 
  autoplot()

##autocorrelação
tssihdf %>% 
  ACF(CASOS_INT, lag_max = 52) %>% 
  autoplot()

##autocorrelação parcial
tssihdf %>% 
  PACF(CASOS_INT, lag_max = 52) %>% 
  autoplot()

##teste de independencia
Box.test(tssihdf$CASOS_INT, type = "Ljung-Box")

#ajustando uma regressão

##adicionando fourier terms usando semana epidemiológica e casos interpolados
tssihdf$FOURIER <- tssihdf %>% 
  select(CASOS, SE, CASOS_INT) %>% 
  fourier(K = 1)

#modelo binomial negativo
model <- glm_nb_model(CASOS_INT ~ 
                        SE + 
                        FOURIER)

##ajuste o modelo usando o dataset
tsdf = as.data.frame(tssihdf)
fitted_model <- trending::fit(model, tsdf)

## calculate confidence intervals and prediction intervals 
obs <- predict(fitted_model, simulate_pi = FALSE)

## plotando o gráfico da regressão 
ggplot(data = obs, aes(x = SE)) + 
  geom_ribbon(aes(ymin = lower_pi, 
                  ymax = upper_pi), 
              alpha = 0.1) + 
  geom_line(aes(y = CASOS_INT), 
            col = "black") + 
  geom_line(aes(y = estimate),
            col = "Red",
            size = 1) + 
  theme_classic()

#analisando os resíduos
##calculando os resíduos
obs <- obs %>% 
  mutate(RESID = residuals(fitted_model$fitted_model, type = "response"))

##os resíduos são constantes ao longo do tempo?
obs %>%
  ggplot(aes(x = SE, y = RESID)) +
  geom_line() +
  geom_point() + 
  labs(x = "Semana Epidemiológica", y = "Resíduos")

##os resíduos são autocorrelacionados?
obs %>% 
  as_tsibble(index = SE) %>% 
  ACF(RESID, lag_max = 52) %>% 
  autoplot()

##os resíduos apresentam uma distribuição normal?
obs %>%
  ggplot(aes(x = RESID)) +
  geom_histogram(binwidth = 100) +
  geom_rug() +
  labs(y = "count") 

##existe algum padrão nos resíduos?
obs %>%
  ggplot(aes(x = estimate, y = RESID)) +
  geom_point() +
  labs(x = "Ajustado", y = "Resíduos")

##teste de autocorrelação dos resíduos
Box.test(obs$RESID, type = "Ljung-Box")
