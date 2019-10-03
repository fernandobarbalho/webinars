library(ckanr)
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)


#Captura o recurso relativo à tabela Anexo que tem os dados do RTN
tb_ckan<-resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="https://apickan.tesouro.gov.br/ckan")

#Captura a URL do recurso
URL_add <- tb_ckan$url

#Substitui o início da url para evitar problemas de certificado
URL_add<-gsub("https://www.tesourotransparente.gov.br","https://apickan.tesouro.gov.br",URL_add)
tmp = tempfile(fileext = ".xlsx")

#faz download do arquivo
download.file(URL_add,mode = "wb", destfile = tmp)

#lê a planilha do excel relativo ao resutado primário. 
rtn_geral <- read_xlsx(tmp,sheet = 2,skip = 4,n_max = 74)

#altera o nome da primeira coluna
names(rtn_geral)[1]<-"Rubrica"

#Faz transposição de todas as colunas com exceção de rubrica para gerar uma matriz com trêss colunas: Rubrica, Data e Valor
series_temporais_analise<-gather(rtn_geral,Data, Valor,-Rubrica)


#Converte os valores da coluna data para o formato Date
series_temporais_analise$Data<-as.Date(as.numeric(series_temporais_analise$Data), origin="1899-12-30")

#converte os valore da coluna Valor para o formato numeric
series_temporais_analise$Valor <-as.numeric(series_temporais_analise$Valor)

series_temporais_analise%>%
  filter(Rubrica %in% c("III. RECEITA LÍQUIDA  (I-II)",
                        "IV. DESPESA TOTAL",
                        "V. FUNDO SOBERANO DO BRASIL",
                        "VI. PRIMÁRIO GOVERNO CENTRAL")) %>%
  ggplot(aes(x=Data,y=Valor, color=Rubrica))+
  geom_line()+
  scale_x_date() +
  scale_y_continuous(labels = function(n){format(n, scientific = FALSE, big.mark   = ".")})+
  theme_light()+
  theme(panel.border = element_blank())


series_temporais_analise%>%
  filter(Rubrica %in% c("III. RECEITA LÍQUIDA  (I-II)",
                        "IV. DESPESA TOTAL",
                        "V. FUNDO SOBERANO DO BRASIL",
                        "VI. PRIMÁRIO GOVERNO CENTRAL")) %>%
  ggplot(aes(x=Data,y=Valor))+
  geom_line()+
  scale_x_date() +
  scale_y_continuous(labels = function(n){format(n, scientific = FALSE, big.mark   = ".")})+
  theme_light()+
  theme(panel.border = element_blank())+
  facet_grid(Rubrica~., scales = "free_y")