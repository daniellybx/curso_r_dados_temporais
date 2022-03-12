#instalando e carregando pacotes
install.packages("pacman")

#verificando instalação de pacotes e carregando pacotes necessários
pacman::p_load(
  tidyverse,  # manipulação de dados    
  slider,     # manipulando intervalos de tempo    
  tidyquant,  # manipulando médias móveis
  read.dbc    # importando dados em dbc
)

#importando dados de internação

files = cbind(paste0("dados/sih_df_2020/", list.files("dados/sih_df_2020/", pattern = "\\.dbc$")))

sihdf = NULL

for (i in 1:length(files)) {
  sihdf_temp = read.dbc(files[i])
  sihdf= data.frame(rbind(sihdf, sihdf_temp))
}

#visualizando variáveis
glimpse(sihdf)

#separando dados de internação por dengue
sihden = subset(sihdf, sihdf$DIAG_PRINC == "A90"| sihdf$DIAG_PRINC == "A91")
sihden$data = as.Date(sihden$DT_INTER, "%Y%m%d")
sihden = subset(sihden, sihden$data >= "2020-01-01" & sihden$data <= "2020-12-31")

#criando data.frame de internações por dia
intden <- sihden %>% 
  count(data, name = "internacoes")

#cortando dias de internações para um ano
summary(intden$data)

#criando graficos com internacoes por dia
ggplot(data = intden)+
  geom_line(mapping = aes(x = data, y = internacoes), size = 1)+
  ggtitle("Internações por dengue no DF, 2020")+
  theme_bw()+
  ylab("Internações")+
  xlab("Data")

#preenchendo dias que não tiveram internações
pre <- intden %>% 
  mutate(                               
    ma_7dias = slide_index_dbl(
      internacoes,                    # calculando internações
      .i = data,                      # indexando dados 
      .f = ~sum(.x, na.rm = TRUE),    # somando valores e considerando datas perdidas
      .before = days(6))              # criando média movel de 7 dias para dias  
  )

#grágico de médias móveis
ggplot(data = pre)+
  geom_line(mapping = aes(x = data, y = ma_7dias), size = 1)+
  ggtitle("Internações por dengue no DF, 2020")+
  theme_bw()+
  ylab("Internações")+
  xlab("Data")

#calculando média móvel por grupo de idade
sihden = sihden %>% mutate(age = 
                             case_when(
                               IDADE < 10 ~ "Crianças",
                               IDADE >9 & IDADE < 20 ~ "Adolescente",
                               IDADE >19 & IDADE < 60 ~ "Adulto",
                               IDADE >59 ~ "Idoso"
                                         ))

#calculando média móvel por grupo
pretype <- sihden %>% 
  
  count(age, data, name = "internacoes") %>% 
  
  arrange(age, data) %>%           # organizando linhas por grupo de idade
  
  group_by(age) %>%                # agrupando por grupo de idade
  
  mutate(                          # calcualndo média móvel por 7dias 
    ma_7days = slide_index_dbl(
      .x = internacoes,            # contando o número de casos por dia
      .i = data,                   # indexando por data de admissão
      .f = mean,                   # usando a média de valores                   
      .before = days(6)            # criando média movel de 7 dias para dias
    )
  )

#gerando gráficos de média móvel por grupo de idade
ggplot(data = pretype)+
  geom_col(                       # plotando os casos diários em cinza
    mapping = aes(
      x = data,
      y = internacoes),
    fill = "grey",
    width = 1)+
  geom_line(                      # plotando os casos por tgrupo de idade
    mapping = aes(
      x = data,
      y = ma_7days,
      color = age),
    size = 1.2)+
  facet_wrap(~age, ncol = 2)+     # criando mini gráficos por grupo de idade
  theme_classic()+                 # mudando o tema 
  theme(legend.position = "none")+ # removendo legenda
  labs(                            # incluindo título do gráfico e dos eixos
    title = "Média móvel de internações por dengue em cada grupo de idade no DF, 2020",
    x = "Data de admissão",
    y = "Internações")

#calculando média móvel diretamente na criação do gráfico
sihden %>% 
  count(data) %>%                    # contando número de casos por dia
  drop_na(data) %>%               
  ggplot(aes(x = data, y = n))+   
  geom_line(                         
    size = 1,
    alpha = 0.1                      # colocando transparencia na linha
  )+             
  tidyquant::geom_ma(                # plotando média móvel
    n = 7,           
    size = 1,
    color = "blue")+ 
  theme_minimal()+
  labs(                             # incluindo título do gráfico e dos eixos
    title = "Média móvel de internações por dengue no DF, 2020",
    x = "Data de admissão",
    y = "Internações")
