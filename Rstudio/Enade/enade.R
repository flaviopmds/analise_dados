
######### CARREGANDO OS PACOTES UTILIZADOS ###############
library(tidyverse)
library(dplyr)

######### CARREGANDO OS DADOS ###############
setwd("C:/Users/Flávio/Desktop/Analise_Dados/Rstudio/Enade")
enade <- read.csv("enade.csv", head=T, sep=",",dec=".")
##Visualizando as 5 primeiras linhas do banco de dados
head(enade)

######### DESCRIÇÃO DAS VARIÁVEIS ###############
#CO_UF_CURSO - Código da UF de funcionamento do curso
#NU_IDADE - Idade do inscrito em 24/11/2019
#TP_PRES - Tipo de presença no Enade
#CO_MODADELIDADE - Código da Modalidade de Ensino
#NT_GER - Nota bruta da prova - Média ponderada da formação geral (25%) e componente específico (75%). (valor de 0 a 100)
#QE_I17 - Em que tipo de escola você cursou o ensino médio?
#QE_I21 -Alguém em sua família concluiu um curso superior?


######### FILTROS ###############

# Filtrando os candidatos presentes com resultado válido
enade = enade %>% filter(TP_PRES == 555)
# Retirando as linhas que o resultado da Nota bruta da prova era inexistente
enade = enade %>% filter (!is.na(NT_GER))
# Selecionando os candidatos que fizeram o ensino médio apenas no Brasil
enade = enade %>% filter (QE_I17 %in% c("A","B","D","E"))


######### ANÁLISE EXPLORATÓRIA DE DADOS ###############
#Banco de dados com duas colunas: código da UF e  UF
UF <- read.table("UF.txt", head=T, sep=";")
#Criando uma coluna para UF

enade=merge(enade,UF)

# Quantos candidatos por estados fizeram o Enade 2019?
enade %>% group_by(UF) %>% summarise("alunos"=n()) %>% arrange(desc(alunos))

#Gráfico de barra quantidade de candidatos por Estado
enade %>% ggplot( aes(x=UF)) +
  geom_bar(stat="count", fill="#f68060", alpha=.6, width=.4)+
  coord_flip() + xlab("") + ylab("Quandidade de candidatos")+
  labs(title= 'Quandidade de candidatos por Estado' )


#Os 10 estados com as melhores média da nota geral 
enade %>% group_by(UF) %>% summarise(media=mean(NT_GER))%>%
  arrange(desc(media)) %>% top_n(10)




# O candidato mais novo
min(enade$NU_IDADE)
#O candidato mais velho
max(enade$NU_IDADE)
#Idade média dos cantidatos
round(mean(enade$NU_IDADE),2)

#Idade mais frequente
enade %>% group_by(NU_IDADE) %>% summarise("Freq"=n()) %>% arrange(desc(Freq))
#23 anos é idade mais frequente dos alunos que fizeram a prova do Enade
#Histograma da distribuição de idade
enade %>% ggplot(aes(x=NU_IDADE)) +  geom_histogram(aes(y=..count..), binwidth = 5,color = "black",fill="lightblue",alpha=.4) +
  labs(title = "Distribuição das idades",
       x = "Idades dos Alunos",
       y = "Total de alunos") 
#Menor nota
min(enade$NT_GER)
#Maior nota
max(enade$NT_GER)
#Nota Média
round(mean(enade$NT_GER),2)
#Mediana das notas
median(enade$NT_GER)
# Metade dos alunos tiraram notas até 43.8

# Distribuição das notas dos candidatos
enade %>% ggplot(aes(x=NT_GER)) + geom_density(size=1.5,fill="pink",alpha=.4) + 
  geom_histogram(aes(y=..density..), binwidth = 5,color = "black",fill="lightblue",alpha=.4) +
  labs(title = "Distribuição das notas dos candidatos",
       x = "Notas Candidatos",
       y = "Densidade") 


#Proporção da modalidade de ensino dos candidatos
enade$MODALIDADE = factor(enade$CO_MODALIDADE,labels = c("EaD","Presensial"))
prop=enade %>% group_by(MODALIDADE) %>% summarise(freq=n()) %>% mutate(prop = freq/sum(freq)*100,lab.ypos=cumsum(prop)-0.5*prop)
#Gráfico de setores da modalidade de ensino dos candidatos
prop %>% ggplot(aes(x="",y=prop,fill=MODALIDADE)) + geom_bar(width = 1,stat = 'identity') + coord_polar("y",start=0)+
  geom_text(aes(y=lab.ypos-2.3,label=paste(round(prop,0),"%")),color = "black",size=4) + theme_void() + labs(title = "Gráfico de setores da modalidade de ensino dos candidatos")

# Comparação do desempenho dos alunos da modalidade:EaD e Presencial
enade %>% ggplot(aes(x =MODALIDADE,y=NT_GER,fill=MODALIDADE)) + geom_boxplot() + guides(fill=F) +
  theme_bw() + xlab("Modalidade") + ylab("Notas dos candidatos") +labs(title = "Notas dos candidatos por tipo de modalidade de ensino")
# O boxplot é um gráfico que avalia a distribuição. Nele podemos verificar 
# o primeiro, o terceiro quartil e a mediana, além dos outlier. 

# Sendo assim, observamos que a mediana dos alunos da modalidade presencial
# é maior que a dos alunos EaD. 


# Contagem dos candidatos por tipo de escola que cursou o ensino médio
enade %>% group_by(QE_I17) %>% count()
enade$QE_I17=factor(enade$QE_I17,labels = c("Pública","Particular","Pública e Particular","Pública e Particular"))

enade %>% ggplot(aes(x =QE_I17)) + geom_bar(stat="count",fill=c("#10c6d3","#0ea5af","#0b8b93")) + 
  geom_text(stat='count',aes(label=..count..),vjust=1.6, color="black",size=4.5)+
  labs(x="Tipo de escola que cursou no ensino médio",y="Total de alunos por ensino",title = 'Contagem de candidatos por tipo de escola que cursou o ensino médio')

# Comparação do desempenho por tipo de escola que cursou o ensino médio em escola : Pública, particular ou pública e particular
enade %>% ggplot(aes(x =QE_I17,y=NT_GER,fill=QE_I17)) + geom_boxplot() + guides(fill=F) +
  theme_bw()  + labs(x="Tipo de escola que cursou no ensino médio",y="Total de alunos por ensino",title = "Notas dos candidatos por tipo de escola que cursou no ensino médio")
#Observa-se que os candidatos que cursaram o ensino médio em escolas particulares tiveram um desempenho melhor





######### PERGUNTAS ###############

# De qual estado é o candidato mais velho e qual foi a modalidade de ensino?

enade %>% filter(NU_IDADE==86) %>% select(UF,MODALIDADE)

# Qual a modalidade de ensino e o tipo de escola que cursou no ensino médio o candidato com a  maior nota?

enade %>% filter(NU_IDADE==86) %>% select(MODALIDADE,QE_I17)


