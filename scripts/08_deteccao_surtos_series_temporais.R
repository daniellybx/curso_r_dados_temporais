
# rodando o script da aula 7
ll <- parse(file = "scripts/07_series_temporais.R")

for (i in seq_along(ll)) {
  tryCatch(eval(ll[[i]]), 
           error = function(e) message("Oops!  ", as.character(e)))
}

#Detecção de surtos

##definindo data de início e de fim
inicio = min(sihdf2$DATA)

##definindo a semana de corte para a data
corte = yearweek("2019-12-31")

##definindo a semana última data da série
fim = yearweek("2021-12-31")

##definindo número de semanas de interesse
nse = as.numeric(fim - corte)

##ajustando dados até o final do ano
## add in missing weeks till end of year 
tssihdf <- tssihdf %>% 
  group_by_key() %>%
  group_modify(~add_row(.,
                        SE = seq(max(.$SE) + 1, 
                                      fim,
                                      by = 1)))

## define fourier terms (sincos) 
tssihdf <- tssihdf %>% 
  mutate(
    FOURIER2 = rbind(
      fourier(
        filter(tssihdf, 
               SE <= corte), 
        K = 1
      ), 
      fourier(
        filter(tssihdf, 
               SE <= corte),
        K = 1, 
        h = nse
      )
    )
  )

##separando os dados para o treinamento e teste do modelo
dat <- tssihdf %>% 
  group_by(SE <= corte) %>%
  group_split()

##definindo um ajuste de modelo binomial negativo
model <- glm_nb_model(
  CASOS_INT ~
    SE +
    FOURIER2
)

##definindo que data será usada para para o ajuste e qual será usada para a predição
ajuste = pluck(dat, 2) 
ajuste2 = as.data.frame(ajuste)

predicao <- pluck(dat, 1) %>% 
  select(CASOS_INT, SE, FOURIER2)
predicao2 = as.data.frame(predicao)

##ajustando o modelo
modelo_ajustado <- trending::fit(model, ajuste2)

##gerando intervalo de confiança e estimativas para o modelo ajustado
obs <- modelo_ajustado %>% 
  predict(simulate_pi = FALSE)

##gerando predições para o modelo ajustado
pred <- modelo_ajustado %>% 
  predict(predicao2, simulate_pi = FALSE)

#unindo as estimativas e predicoes
obs <- bind_rows(obs, pred)

#gerando o gráfico da regressão
ggplot(data = obs, aes(x = SE)) + 
  geom_line(aes(y = estimate),
            col = "grey") + 
  geom_ribbon(aes(ymin = lower_pi, 
                  ymax = upper_pi), 
              alpha = 0.25) + 
  geom_line(aes(y = CASOS_INT), 
            col = "black") + 
  geom_point(
    data = filter(obs, CASOS_INT > upper_pi), 
    aes(y = CASOS_INT), 
    colour = "red", 
    size = 2) + 
  geom_vline(
    xintercept = as.Date(corte), 
    linetype = "dashed") + 
  annotate(geom = "text", 
           label = "Predição", 
           x = corte, 
           y = max(obs$upper_pi) - 250, 
           angle = 90, 
           vjust = 1
  ) + 
  theme_classic()+
  ylab("Estimativa")+xlab("Semana Epidemiológica")+
  ggtitle("Predição de internações no Distrito Federal, 2008-2021")

#gerando um limiar de alerta
tssihdf$threshold = ifelse(tssihdf$SE >= corte, obs$upper_pi, NA)

#gerando um gráfico com o limiar 
ggplot(tssihdf, aes(x = SE)) + 
  geom_line(aes(y = CASOS_INT, colour = "Observado")) + 
  geom_line(aes(y = threshold, colour = "Alerta"), 
            linetype = "dashed", 
            size = 1) +
  scale_colour_manual(values = c("Observado" = "grey", 
                                 "Alerta" = "red")) +
  geom_vline(
    xintercept = as.Date(corte), 
    linetype = "dashed") + 
  annotate(geom = "text", 
           label = "Predição", 
           x = corte, 
           y = max(obs$upper_pi) - 2000, 
           angle = 90, 
           vjust = 1
  )+
  theme_classic()+ 
  theme(legend.title = element_blank())
