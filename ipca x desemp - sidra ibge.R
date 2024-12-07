
# Pacotes -----------------------------------------------------------------

library(sidrar)
library(dplyr)
library(lubridate)
library(ggplot2)

# Coleta de dados ---------------------------------------------------------

dados_brutos = sidrar::get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202" )

dados_brutos_desemprego = sidrar::get_sidra(api = "/t/6381/n1/all/v/4099/p/all/d/v4099%201")

# Tratamento de dados -----------------------------------------------------

#dados do ipca
dados_tratados = dados_brutos %>%
  dplyr::mutate(
    data = lubridate :: ym(`Mês (Código)`),
    ipca = Valor, 
    .keep = "none"
  ) %>%
  dplyr:: filter(data >= "2004-01-01") %>%
  dplyr::as_tibble()

#dados desemprego
dados_tratados_desemprego = dados_brutos_desemprego %>%
  dplyr::mutate(
    data = lubridate :: ym(`Trimestre Móvel (Código)`),
    desemp = Valor, 
    .keep = "none"
  ) %>%
  dplyr::as_tibble()


#juntar tabelas
dados_cruzados = dplyr::inner_join(
  x = dados_tratados,
  y = dados_tratados_desemprego,
  by = "data"
)
  

# Análise de Dados --------------------------------------------------------

# Como a inflação se comportou no BR? -------------------------------------
ggplot2 :: ggplot(dados_tratados) +
  ggplot2::aes(x=data,y=ipca) +
  ggplot2::geom_line()
  
#Períodos de maior e menor tabela
dados_tratados %>%
  dplyr::arrange(ipca) %>%
  dplyr::slice(c(1,nrow(dados_tratados)))

#valor médio da inflação no brasil 
summary(dados_tratados$ipca)
#ou gerar grafico histograma
ggplot2::ggplot(dados_tratados) +
  ggplot2::aes(x =ipca) +
  ggplot2::geom_histogram()

# o que afeta a inflação? relação com outras variáveis.
#procurar dados da taxa de desemprego

ggplot2::ggplot(dados_cruzados) +
  ggplot2::aes(x = desemp, y = ipca) +
  ggplot2::geom_point()
#ou 

ggplot(dados_cruzados, aes(x = desemp, y = ipca)) +
  geom_point()



#função de philips 
modelo = lm(ipca~desemp, data=dados_cruzados)
summary(modelo)

ggplot2::ggplot(dados_cruzados) +
  ggplot2::aes(x = desemp, y = ipca) +
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = "lm")




