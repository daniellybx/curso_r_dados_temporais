#03 Calculando taxas de incidência temporais

#instalando pacotes

## install.packages('tidyverse')
## install.packages('data.table')
## install.packages('readxl')

#carregando pacotes necessários

require(tidyverse)   #manipulação de dados
require(data.table)  #manipulação de data.frames
require(readxl)      #importação de arquivo em .xls ou .xlsx

### Preparando os dados ### 

#importando dados
dengue = read.csv2('dados/dengue.csv')
pop = read.csv2('dados/populacao.csv')

#Visualizando data.frames
View(dengue)
View(pop)

#Verificando nome das colunas
colnames(dengue)
colnames(pop)

#modificando nome das colunas de cada data.frame
colnames(dengue) = c("UF", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "Total")
colnames(pop) = c("UF", "2014", "2015", "2016", "2017", "2018", "2019")

#excluindo colunas que não serão usadas na análise
dengue$Total = NULL
dengue$`2021` = NULL
dengue$`2020` = NULL

#separando dados de UF 
dengue$UF = as.factor(substring(dengue$UF, 1, 2))
pop$UF = as.factor(substring(pop$UF, 1, 2))

#juntando com tabela de estados e regiões
regioes = read_excel("dados/regioes.xlsx")

##ajustando os dados para união das tabelas
class(regioes$COD)
class(dengue$UF)

regioes$COD = as.factor(regioes$COD)

##unindo data.frames
dengue = merge(regioes, dengue, by.x = "COD", by.y = "UF")

#reajustando dados do formato wide to long 

##ajustando variaveis
dengue$REGIAO = as.factor(dengue$REGIAO)
dengue$UF = as.factor(dengue$UF)

##mudando o formato dos data.frames
dengue2 =  melt(setDT(dengue), id.vars = c("COD","REGIAO", "UF"), variable.name = "ANO")
pop2 =  melt(setDT(pop), id.vars = "UF", variable.name = "ANO")

##ajustando o nome da variável de valores
names(dengue2)[names(dengue2) == 'value'] <- 'CASOS'
names(pop2)[names(pop2) == 'UF'] <- 'COD'
names(pop2)[names(pop2) == 'value'] <- 'POPULACAO'

#unindo data.frames
dados = dengue2 %>%
  left_join(pop2, by = c("COD", "ANO"))

#calculando taxa de incidência

#por uf
uf = dados
uf$TX_INC = round(uf$CASOS/uf$POPULACAO*100000, 2)

#por regiao

##agregando os dados por regiao
regiao = aggregate(cbind(CASOS, POPULACAO) ~ REGIAO + ANO, data = uf, sum)

##calculando taxa de incidencia por regiao
regiao$TX_INC = round(regiao$CASOS/regiao$POPULACAO*100000, 2)

#de todo o país

##agregando os dados por regiao
brasil = aggregate(cbind(CASOS, POPULACAO) ~ ANO, data = uf, sum)

##calculando taxa de incidencia por regiao
brasil$TX_INC = round(brasil$CASOS/brasil$POPULACAO*100000, 2)

#criando visualizacoes 

##ajustado variável 
brasil$ANO = as.double(as.character(brasil$ANO))

##gerando gráfico de linhas
ggplot(brasil, aes(x = ANO, y = TX_INC))+
  geom_line(color = "blue", size = 1)+
  geom_point(color = "darkblue", size = 2)+
  xlab("Ano") + ylab("Taxa de Incidência")+
  ggtitle("Taxa de incidência de dengue por ano, Brasil, 2014 - 2019")+
  theme_bw()

##ajustado variável 
regiao$ANO = as.double(as.character(regiao$ANO))

##gerando gráfico de área
ggplot(regiao, aes(x = ANO, y = TX_INC, fill = REGIAO))+
  geom_area()+
  xlab("Ano") + ylab("Taxa de Incidência")+
  ggtitle("Taxa de incidência de dengue por ano e região, Brasil, 2014 - 2019")+
  guides(fill=guide_legend(title="Região"))+
  theme_bw()

##ajustado variável 
uf$ANO = as.double(as.character(uf$ANO))

##gerando gráfico de múltiplas linhas
ggplot(uf, aes(x = ANO, y = TX_INC, color = UF))+
  geom_line()+
  xlab("Ano") + ylab("Taxa de Incidência")+
  ggtitle("Taxa de incidência de dengue por ano e UF, Brasil, 2014 - 2019")+
  guides(fill=guide_legend(title="Região"))+
  theme_bw()

##filtrando por regiao
uf_norte = subset(uf, uf$REGIAO == "Centro-oeste")

##gerando gráfico de múltiplas linhas
ggplot(uf_norte, aes(x = ANO, y = TX_INC, color = UF))+
  geom_line(size = 2)+
  geom_point(size = 3)+
  xlab("Ano") + ylab("Taxa de Incidência")+
  ggtitle("Taxa de incidência de dengue por ano e região, Brasil, 2014 - 2019")+
  guides(fill=guide_legend(title="Região"))+
  theme_bw()
