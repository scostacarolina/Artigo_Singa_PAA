getwd()
setwd("C:/Users/Carol/Desktop/cadeiras-mestrado/singa")

require(ggplot2)
require(readODS)
require(tidyverse)
#soliicito abertura do arquivo
paa_data <- read_ods(path = "paa_data_brasil.ods", sheet = 1, col_names = TRUE)
#conhecendo odataset
View(paa_data)  

class(paa_data$recursos)
class(paa_data$ANO)
class(paa_data$GOVERNO)
#classificando variavel como binária
paa_data$GOVERNO_dummy <- as.character(paa_data$GOVERNO_dummy)

class(paa_data$GOVERNO_dummy)

#averiguando os dados
summary(paa_data)

#limpano variavel numérica para casa dos milhões
paa_data$recursos <- paa_data$recursos/1000000
paa_data$recursos
paa_data$recursos <- round(paa_data$recursos,2)
# averigando o resultado., variavel pronta para visualização em grafico
paa_data$recursos


#limpando variavel numérica para casa dos milhões
paa_data$produtos_kg <- paa_data$produtos_kg/1000000
paa_data$produtos_kg
paa_data$produtos_kg <- round(paa_data$produtos_kg,2)
# averigando o resultado., variavel pronta para visualização em grafico
paa_data$produtos_kg


#limpando variavel numérica para casa dos milhões
paa_data$atendimentos <- paa_data$atendimentos/1000000
paa_data$atendimentos
paa_data$atendimentos <- round(paa_data$atendimentos,2)
# averigando o resultado., variavel pronta para visualização em grafico
paa_data$atendimentos

#limpando variavel numérica para casa dos mil
paa_data$agr_fornecedores <- paa_data$agr_fornecedores/1000
paa_data$agr_fornecedores
paa_data$agr_fornecedores <- round(paa_data$agr_fornecedores,2)
# averigando o resultado., variavel pronta para visualização em grafico
paa_data$agr_fornecedores

class(paa_data$agr_fornecedores)


# iniciando confecção de graficos
require(RColorBrewer)

#  figura 1 - grafuco de linha recurso X ano X governo 
ggplot(paa_data, aes(x=ANO, y=recursos, colour = GOVERNO)) + 
  geom_line() +  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  scale_color_manual(values = c("blue","red")) +
  labs(title=" Total dos Recursos do PAA", 
       x="Ano", 
       y=" Total de Recursos em Milhões", 
       caption = "Fonte: Nossa Autoria - Dados MDS 2019 ") +
  theme(legend.position="right")

# figura 2 - grafico de linha produtos X ano X governo
ggplot(paa_data, aes(x=ANO, y= produtos_kg, colour = GOVERNO)) + 
  geom_line() +  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  scale_color_manual(values = c("blue","red")) +
  labs(title=" Total de KG de Produtos Adquiridos pelo PAA", 
       x="Ano", 
       y=" Kg de Produtos em Milhões", 
       caption = "Fonte: Nossa Autoria - Dados MDS 2019 ") +
  theme(legend.position="right")

# figura 3 - grafico de linha atendimentos X ano X governo 
ggplot(paa_data, aes(x=ANO, y= atendimentos, colour = GOVERNO)) + 
  geom_line() +  theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
  scale_color_manual(values = c("blue","red")) +
  labs(title=" Total de Atendimentos Realizados pelo PAA", 
       x="Ano", 
       y="Número de Atendimentos em Milhões", 
       caption = "Fonte: Nossa Autoria - Dados MDS 2019 ") +
  theme(legend.position="right")

# figura 4 - grafico de pontos recursosX produtos X governo

sp1 <- ggplot(paa_data, aes(recursos, produtos_kg, colour= GOVERNO,)) + geom_point()
sp1 + geom_text(aes(label=ANO), vjust=0)  + 
  scale_color_manual(values = c("blue","red")) +
  labs(title=" Relação Recursos X Produtos PAA",
       x=" Recursos em Milhões",
       y=" Kg de Produtos em Milhões", 
       caption = "Fonte: Nossa Autoria - Dados MDS 2019 ") +
  theme(legend.position="right")

# figura 4 - grafico de pontos recursos X atendimento X governo

sp2 <- ggplot(paa_data, aes(recursos, atendimentos, colour= GOVERNO,)) + geom_point()
sp2 + geom_text(aes(label=ANO), vjust=1)  + 
  scale_color_manual(values = c("blue","red")) +
  labs(title=" Relação Recursos X Atendimentos PAA",
       x="Total de Recursos em Milhões",
       y=" Nº de Atendimentos em Milhões", 
       caption = "Fonte: Nossa Autoria - Dados MDS 2019 ") +
  theme(legend.position="right")

# figura 4 - grafico de pontos produtos X atendimento X governo
sp3 <- ggplot(paa_data, aes(produtos_kg, atendimentos, colour= GOVERNO,)) + geom_point()
sp3 + geom_text(aes(label=ANO))  + 
  scale_color_manual(values = c("blue","red")) +
  labs(title=" Relação Produtos X Atendimentos PAA",
       x="KG de Produtos em Milhões",
       y=" Nº de Atendimentos em Milhões", 
       caption = "Fonte: Nossa Autoria - Dados MDS 2019 ") +
  theme(legend.position="right")

