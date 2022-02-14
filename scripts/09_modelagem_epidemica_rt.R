#instalando e carregando pacotes
#install.packages("pacman")

#carregando pacotes necessários
pacman::p_load(
  rio,          # importando dados
  here,         # locando dados
  tidyverse,    # manipulando dados e criando gráficos
  epicontacts,  # analisando redes de transmissão
  EpiEstim,     # estimando Rt
  projections,  # projetando a incidência 
  incidence2,   # manipulando dados de incidência
  epitrix,      # usando funções de epidemiologia
  distcrete     # distribuições discretas de tempo
)

#importando dados
linelist <- import("dados/linelist_cleaned.rds") #disponível em https://epirhandbook.com/en/epidemic-modeling.html

#preparando os dados

##calculando incidência desde a primeira data
cases <- incidence2::incidence(linelist, date_index = date_onset) %>% # gnúmero de casos por dia
  tidyr::complete(date_index = seq.Date(                              # incluindo todas as datas no período considerado
    from = min(date_index, na.rm = T),
    to = max(date_index, na.rm=T),
    by = "day"),
    fill = list(count = 0)) %>%                                       # convertendo NA em 0
  rename(I = count,                                                   # renomeando as colunas para a coincidir com a função estimate_R
         dates = date_index)

##gerando a configuração de tempo com os dados que estão na literatura para a 
##média e o desvio padrão do Ebola -> disponível em https://www.nejm.org/doi/full/10.1056/nejmoa1411100
config_lit <- make_config(
  mean_si = 12.0,
  std_si = 5.2
)

epiestim_res_lit <- estimate_R(
  incid = cases,
  method = "parametric_si",
  config = config_lit
)

##plotando a curva epidêmia, o Rt estimado para o período 
##e o intervalo serial (intervalo entre os sintomas dos casos primários para os secundários)
plot(epiestim_res_lit)

#estimando o intervalo serial a partir dos contatos

##gerando os pares de contatos
contacts <- linelist %>%
  transmute(
    from = infector,
    to = case_id
  ) %>%
  drop_na()

##criando um objeto com os pares de contatos
epic <- make_epicontacts(
  linelist = linelist,
  contacts = contacts, 
  directed = TRUE
)

##estimando o intervalo serial gamma
serial_interval <- fit_disc_gamma(get_pairwise(epic, "date_onset"))

##usando o intervalo serial gamma para criar um novo modelo 
config_emp <- make_config(
  mean_si = serial_interval$mu,
  std_si = serial_interval$sd
)

##rodando o novo modelo de estimativa do Rt
epiestim_res_emp <- estimate_R(
  incid = cases,
  method = "parametric_si",
  config = config_emp
)

##plotando o novo modelo de estimativa Rt
plot(epiestim_res_emp)

#Especificando intervalos de estimação de tempo

##definindo um objeto com as datas iniciando a partir de 1 de junho
start_dates <- seq.Date(
  as.Date("2014-06-01"),
  max(cases$dates) - 7,
  by = 1
) %>%
  `-`(min(cases$dates)) %>%
  as.integer()

##adicionando um intervalo de 6 dias a partir da data de início
end_dates <- start_dates + 6

##configurando a estimativa com base nas datas de ínicio e fim do intervalo proposto
config_partial <- make_config(
  mean_si = 12.0,
  std_si = 5.2,
  t_start = start_dates,
  t_end = end_dates
)

##rodando o novo modelo
epiestim_res_partial <- estimate_R(
  incid = cases,
  method = "parametric_si",
  config = config_partial
)

##plotando os outputs do intervalo
plot(epiestim_res_partial)

#analisando os outputs

##criando um data.frame no formato wide para a mediana
df_wide <- epiestim_res_lit$R %>%
  rename_all(clean_labels) %>%
  rename(
    lower_95_r = quantile_0_025_r,
    lower_90_r = quantile_0_05_r,
    lower_50_r = quantile_0_25_r,
    upper_50_r = quantile_0_75_r,
    upper_90_r = quantile_0_95_r,
    upper_95_r = quantile_0_975_r,
  ) %>%
  mutate(
    dates = epiestim_res_emp$dates[round(map2_dbl(t_start, t_end, median))],
    var = "R[t]"
  ) %>%
  left_join(cases, "dates") %>%
  mutate(
    across(
      lower_95_r:upper_95_r,
      ~ .x*I,
      .names = "{str_replace(.col, '_r', '_risk')}"
    )
  ) %>%
  pivot_longer(
    contains("median"),
    names_to = c(".value", "variable"),
    names_pattern = "(.+)_(.+)"
  ) %>%
  mutate(variable = factor(variable, c("risk", "r")))

##criando um data.frame no formato long para os quantis
df_long <- df_wide %>%
  select(-variable, -median) %>%
  pivot_longer(
    contains(c("lower", "upper")),
    names_to = c(".value", "quantile", "variable"),
    names_pattern = "(.+)_(.+)_(.+)"
  ) %>%
  mutate(variable = factor(variable, c("risk", "r")))

##plotando o gráfico
ggplot() +
  geom_ribbon(
    data = df_long,
    aes(x = dates, ymin = lower, ymax = upper, alpha = quantile),
    color = NA
  ) +
  geom_line(
    data = df_wide,
    aes(x = dates, y = median),
    alpha = 0.2
  ) +
  ## use label_parsed to allow subscript label
  facet_wrap(
    ~ variable,
    ncol = 1,
    scales = "free_y",
    labeller = as_labeller(c(r = "R[t]", risk = "Transmission~potential"), label_parsed),
    strip.position = 'left'
  ) +
  ## manually define quantile transparency
  scale_alpha_manual(
    values = c(`50` = 0.7, `90` = 0.4, `95` = 0.2),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    x = NULL,
    y = NULL,
    alpha = "Credible\ninterval"
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %d\n%Y"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.background = element_blank(),
    strip.placement = 'outside'
  )

#projetando a incidência

##criando um objeto para as datas de incidência de casos
inc <- incidence::incidence(linelist$date_onset)

##extraído valores de r possíveis para as estimativas mais recentes
mean_r <- tail(epiestim_res_emp$R$`Mean(R)`, 1)
sd_r <- tail(epiestim_res_emp$R$`Std(R)`, 1)
shapescale <- gamma_mucv2shapescale(mu = mean_r, cv = sd_r/mean_r)
plausible_r <- rgamma(1000, shape = shapescale$shape, scale = shapescale$scale)

##verificando a distribuição
qplot(x = plausible_r, geom = "histogram", xlab = expression(R[t]), ylab = "Counts")

##criando a projeção
proj <- project(
  x = inc,
  R = plausible_r,
  si = serial_interval$distribution,
  n_days = 21,
  n_sim = 1000
)

##plotando a projeção 
plot(inc[inc$dates > as.Date("2015-03-01")]) %>%
  add_projections(proj)

##convertendo os dados em um data.frame
proj_df <- as.data.frame(proj)
View(proj_df)
