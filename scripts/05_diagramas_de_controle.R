#04 Criando Diagramas de Controle

#instalando pacotes
### install.packages('tidyverse')
### install.packages('data.table')
### install.packages('read.dbc')
### install.packages('lubridate')

#carregando pacotes necessários

require(tidyverse)   # manipulação de dados
require(data.table)  # manipulação de data.frames
require(read.dbc)    # importação de arquivo em .dbc
require(lubridate)   # tratamento de datas

#Importando dados

#tratando e selecionando dados de população
pop = read.csv2('dados/populacao.csv')

colnames(pop) = c("UF", "2014", "2015", "2016", "2017", "2018", "2019")
pop$UF = as.factor(substring(pop$UF, 1, 2))

pop2 =  melt(setDT(pop), id.vars = "UF", variable.name = "ANO")

names(pop2)[names(pop2) == 'UF'] <- 'COD'
names(pop2)[names(pop2) == 'value'] <- 'POPULACAO'

pop_df = subset(pop2, pop2$COD == "53")

#importando dados de dengue
files = cbind(paste0("dados/sinan_dengue_df/", list.files("dados/sinan_dengue_df/", pattern = "\\.dbc$")))

d_fn = NULL

for (i in 1:length(files)) {
  
  d_temp = read.dbc(files[i])
  d_fn= data.frame(rbind(d_fn, d_temp))
}

glimpse(d_fn)

#analisando o tipo da variável de data
class(d_fn$DT_SIN_PRI) 

#analisando a amplitude dos dados
summary(d_fn$DT_SIN_PRI) 
hist(d_fn$DT_SIN_PRI, breaks = 60, 
     main = "Número de casos de dengue por mês no DF, 2015-2019",
     xlab = "Ano", ylab = "Número de casos",
     freq = T,
     border = "White", col = "red") 

#ajustando a variável data
d_fn$DT_SIN_PRI = as.Date(d_fn$DT_SIN_PRI, "%Y-%m-%d")
d_fn = subset(d_fn, d_fn$DT_SIN_PRI > "2014-12-31" & d_fn$CLASSI_FIN != "5")

#analisando a amplitude dos dados após o corte
summary(d_fn$DT_SIN_PRI) 
hist(d_fn$DT_SIN_PRI, breaks = 60, 
     main = "Número de casos de dengue por mês no DF, 2015-2019",
     xlab = "Ano", ylab = "Número de casos",
     freq = T,
     border = "White", col = "red") 

#criando tabela de casos por semana epidemiológica
d_fn$CASO = 1 
d_week = aggregate(CASO ~ SEM_PRI, data = d_fn, sum)

#unindo dados de população
d_week$ANO = substring(d_week$SEM_PRI, 1, 4)
d_week = merge(d_week, pop_df, by = "ANO")

#calculando taxa de incidência por semana epidemiológica
d_week$TX_INC = round(d_week$CASO/d_week$POPULACAO*100000, 2)

ggplot(d_week, aes(x = SEM_PRI, y = TX_INC))+
  geom_bar(stat = 'identity')+
  xlab("Semana dos primeiros sintomas")+
  ylab("Taxa de Incidência")+
  scale_x_discrete(breaks=c("201501","201526","201601","201626", "201701","201726", "201801", "201826", "201901","201926"))+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#calculando média semanal da taxa de incidência
d_week2 = data.frame(ANO = d_week$ANO,
                     SEM_PRI = d_week$SEM_PRI,
                     TX_INC = d_week$TX_INC)
d_week2$SEM_PRI = substring(d_week2$SEM_PRI, 5, 6)

d_week2 = reshape2::dcast(d_week2, SEM_PRI ~ ANO, value.var = "TX_INC")

#removendo ano desconsiderado na análise
d_week2$`2014` = NULL

#gerando data frame para o diagrama de controle
d_week3 = d_week2
d_week3$`2019` = NULL

#gerando data.frame para o teste do controle
d_week4= data.frame(X2019 = d_week2$`2019`)

#gerando estatísticas
row.names(d_week3) = d_week3$SEM_PRI
row.names(d_week4) = d_week3$SEM_PRI
d_week3$SEM_PRI = NULL

d_week3 = d_week3[1:52,1:4]
d_week4 = data.frame(X2019 = d_week4[1:52,1])

d_week3$mean = round(rowMeans(d_week3), 2)
d_week3$sd = round(apply(d_week3[,-5], 1, sd), 2)

#criando os limites superiores e inferiores do diagrama
d_week3$low = round((d_week3$mean - 1.96*(d_week3$sd)), 2)
d_week3$high = round((d_week3$mean + 1.96*(d_week3$sd)), 2)

d_week3$low = ifelse(d_week3$low < 0, 0, d_week3$low)

d_week3$week = 1:52
d_week4$week = 1:52

ggplot()+
  geom_ribbon(data = d_week3, aes(ymin=low, ymax=high, x = week), alpha=0.9 , fill = "grey70")+
  geom_line(data = d_week3, aes(x = week, y= mean), colour = "black", size = 1)+
  ggtitle("Diagrama de controle de dengue, DF, 2019")+
  theme_bw()+
  ylab("Taxa de incidência")+
  xlab("data")+
  geom_line(data = d_week3, aes(x = week, y= low), colour = "grey60", size = 1.3)+
  geom_line(data = d_week3, aes(x = week, y= high), colour = "grey60", size = 1.3)+
  geom_line(data = d_week4, aes(x = week, y= X2019), colour = "red", size = 1)+
  geom_point(data = d_week4, aes(x = week, y= X2019), colour = "red", size = 1.5)

d_week4$simulation = ifelse(d_week4$week > 13, NA, d_week4$X2019)

ggplot()+
  geom_ribbon(data = d_week3, aes(ymin=low, ymax=high, x = week), alpha=0.9 , fill = "grey70")+
  geom_line(data = d_week3, aes(x = week, y= mean), colour = "black", size = 1)+
  ggtitle("Diagrama de controle simulado de dengue, DF, 2019")+
  theme_bw()+
  ylab("Taxa de incidência")+
  xlab("data")+
  geom_line(data = d_week3, aes(x = week, y= low), colour = "grey60", size = 1.3)+
  geom_line(data = d_week3, aes(x = week, y= high), colour = "grey60", size = 1.3)+
  geom_line(data = d_week4, aes(x = week, y= simulation), colour = "red", size = 1)+
  geom_point(data = d_week4, aes(x = week, y= simulation), colour = "red", size = 1.5)
