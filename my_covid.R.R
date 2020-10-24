# PACKAGES E LIBRARYS PARA ANALISAR DADOS

##install.packages("data.table")
##install.packages("plotly") - tornar os graficos interativos
##install.packages("ggpubr")
##install.packages("gganimate")
# install.packages("zoo")
library(data.table)
library(dplyr)
library(tibble)
library(ggplot2)
library(grid)
library(plotly)
library(ggpubr)
library(gganimate)
library(zoo)

# PACKAGES E LIBRARYS PARA FAZER GRAFICOS

##install.packages("rgdal")
##install.packages("geojsonio")
##install.packages("geojsonR")
##install.packages("leaflet")
##install.packages("sf")
# install.packages("dichromat")
# install.packages("ggiraph")
library(rgdal)
library(geojsonio)
library(geojsonR)
library(RColorBrewer)
library(leaflet)
library(sf)
library(dichromat)
library(ggiraph)


# FONTE DOS DADOS DO COVID-19
covid.pt <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv")


# FONTE DOS DADOS DOS TESTES DO COVID-19

covid.pt_testes <- fread("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/amostras.csv")


# MAPA PORTUGAL FONTE
mapa_portugal <- geojson_read("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/extra/mapas/portugal.geojson", what = "sp")


# TRATAR A BASE DE DADOS DO COVID

## Houve um lapso na base de dados e os obitos por idade e por sexo para dia 5/10 (linha 223) estao a 0's. Subituimos esses 0's
## por os mesmos valores do dia anterior

covid.pt[223, 65:84] = covid.pt[222, 65:84]

#POR AS DATAS EM FORMATO DATA

covid.pt$data <- as.Date(as.character(covid.pt$data),format = "%d-%m-%Y")

covid.pt_testes$data <- as.Date(as.character(covid.pt_testes$data),format = "%d-%m-%Y")




# SINAIS CLINICOS
## Frequencia relativa dos sinais clinicos (tabela e grafico de barras)

sintomas <- as.data.frame(t(covid.pt[173, 41:46]))

sintomas <- sintomas %>%
  rownames_to_column(var="Sintomas")
names(sintomas)[2] <- "Frequência"

ggplot(sintomas, aes(x=Sintomas, y=Frequência*100)) + 
  geom_col(fill="darksalmon", width = 0.6) +
  scale_x_discrete(labels= c("Cefaleia", "Dificuldade\nrespiratória", "Dores\nmusculares", "Febre", "Fraqueza\ngeneralizada", "Tosse")) +
  theme_classic() +
  labs(y="Frequência (%)", title = "Frequência de sintomas da COVID-19",x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Frequência, digits=4)), vjust=-0.5)


## Evolução da Frequencia relativa dos sinais clinicos

sintomas_tempo <- as.data.frame((covid.pt[8:173,41:46]*covid.pt$confirmados[8:173])-(covid.pt[7:172,41:46]*covid.pt$confirmados[7:172]))/covid.pt$confirmados_novos[8:173]

sintomas_tempo_2 <- rbind(covid.pt[7,41:46], sintomas_tempo)

sintomas_tempo_3 <- cbind(covid.pt$data[7:173], sintomas_tempo_2)
names(sintomas_tempo_3) <- c("Data", "Tosse", "Febre", "Dificuldade respiratória", "Cefaleia", "Dores musculares", "Fraqueza generalizada")

sintomas_tempo_3[3,4] <- 0.11

sintomas_tempo_melt <- melt(sintomas_tempo_3, id.vars="Data")
names(sintomas_tempo_melt)[-1] <- c("Sintomas", "Valores")

ggplot(sintomas_tempo_melt, aes(x = Data, y = Valores*100, color = Sintomas)) +
  geom_line() +
  facet_grid(sintomas_tempo_melt$Sintomas) + ## para separar em varios graficos
  guides(color = FALSE) +
  labs(size = 20) +
  xlab("Mês") +
  ylab("Frequência (%)")+
  labs(title = "Frequência de Sintomas da COVID-19 ao longo do tempo") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 17, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) + 
  theme(strip.text.y = element_text(color = "black", size = 8, angle = 0))




#TESTES

## Evoluçao do numero de testes realizados (diarios) 

testes_diarios <- covid.pt_testes[,-2]
  names(testes_diarios) <- c("Data", "Testes")

grafico_testes_diarios <- ggplot(testes_diarios, aes(x = Data, y = Testes)) +
  geom_point(color = "orchid3") +
  geom_line(size = 0.4, color = "orchid3") +
  xlab("Mês") +
  ylab("Nº de testes")+
  labs(title = "Evolução do número de testes realizados") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 17, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(date_breaks = "months", date_labels = "%b")

ggplotly(grafico_testes_diarios)


## Evoluçao do numero de testes realizados (diarios) - média rolante 7 dias

testes_diarios_mr <- cbind(covid.pt_testes[7:nrow(covid.pt_testes),1], zoo::rollmean(covid.pt_testes[,3], k = 7))
  names(testes_diarios_mr) = c("Data", "Testes")

grafico_testes_diarios_mr <- ggplot(testes_diarios_mr, aes(x = Data, y = Testes)) +
  geom_line(size = 0.4, color = "orchid3") +
  xlab("Mês") +
  ylab("Nº de testes")+
  labs(title = "Evolução do número de testes realizados - Média rolante") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(date_breaks = "months", date_labels = "%b")

ggplotly(grafico_testes_diarios_mr)


## Evoluçao da taxa de testes positivos (diarios)

incidencia_sd <- as.data.frame(covid.pt$confirmados_novos)

incidencia <- cbind(covid.pt$data, incidencia_sd)
names(incidencia) <- c("Data", "Incidência")

t_testes_posivitos <- cbind(testes_diarios$Data, as.data.frame(incidencia[1:nrow(testes_diarios), 2])/testes_diarios$Testes*100)
names(t_testes_posivitos) <- c("Data", "Taxa_positivos")

grafico_t_testes_posivitos <- ggplot(t_testes_posivitos, aes(x = Data, y = Taxa_positivos)) +
  geom_line(size = 0.4, color = "violetred") +
  geom_point(color="violetred") +
  xlab("Mês") +
  ylab("Taxa de positivos (%)")+
  labs(title = "Evolução da Taxa de Testes Positivos") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 17, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(date_breaks = "months", date_labels = "%b")

ggplotly(grafico_t_testes_posivitos)




# CASOS

#Geral

## Evoluçao da Incidência

grafico_incidencia <- ggplot(incidencia, aes(x = Data, y = Incidência)) +
  geom_point(color = "coral3") +
  geom_line(size = 0.4, color = "coral3") +
  xlab("Mês") +
  ylab("Incidência")+
  labs(title = "Evolução da incidência") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
  scale_x_date(date_breaks = "months", date_labels = "%b")

ggplotly(grafico_incidencia)



#ARS

## Nº de casos por ARS (tabela e grafico de barras)

regioes_casos <- as.data.frame(t(as.data.frame(lapply(covid.pt[,4:10], last))))

regioes_casos <- regioes_casos %>% 
  rownames_to_column(var="Regiões")
names(regioes_casos)[2] <- "Nº_casos"

ggplot(regioes_casos, aes(x=Regiões, y=Nº_casos)) + 
  geom_col(fill="plum3", width = 0.5) +
  scale_x_discrete(labels= c("Açores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Norte", "Madeira")) +
  theme_classic() +
  labs(y="Nº de casos", title = "Nº de casos de COVID-19 por Regiões", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=Nº_casos), vjust=-0.5)


## Nº de casos por ARS (mapa) 

regioes_casos_ordem <- regioes_casos[c(4,5,6,2,7,1,3),] # colocar as regioes pela ordem do mapa
regioes_casos_ordem[,1] <- c("Alentejo", "Algarve", "Açores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_casos <- colorBin("Blues",  bins = c(0,300, 2000, 6000, 20000, 30000, Inf)) ## tonalidade das cores consoante os casos

labels_casos <- paste( #tornar o mapa interativo
  "<strong>", regioes_casos_ordem[,1],"</strong><br/>", 
  regioes_casos_ordem[,2], " casos<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa_portugal) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_casos(regioes_casos_ordem$Nº_casos),
              label = labels_casos, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addLegend("bottomleft", pal = pal_casos, values = regioes_casos_ordem$Nº_casos, opacity = 0.5, title = "Nº de casos por ARS") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Satélite", "Dark"), options = layersControlOptions(collapsed = FALSE))


## Evolução da incidencia por ARS

pop_acores <- 242796
pop_alentejo <- 503507
pop_algarve <- 450484
pop_centro <- 2217285 
pop_lvt <- 3631738
pop_norte <- 3575338
pop_madeira <- 253945

pop_regioes <- as.data.frame(c(pop_norte, pop_centro, pop_lvt, pop_alentejo, pop_algarve, pop_acores,pop_madeira))
pop_regioes_rep <- as.data.frame(t(pop_regioes[rep(seq_len(ncol(pop_regioes)), each=nrow(covid.pt))]))
names(pop_regioes_rep) <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

incidencia_regioes_sd <- as.data.frame((covid.pt[,confirmados_arsnorte:confirmados_madeira]- lag(covid.pt[, confirmados_arsnorte:confirmados_madeira])))

incidencia_regioes <- cbind(covid.pt$data, incidencia_regioes_sd)
names(incidencia_regioes) <- c("Data", "Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

incidencia_regioes_melted <- reshape2::melt(incidencia_regioes, id.vars="Data")
names(incidencia_regioes_melted)[-1] <- c("ARS", "Incidência")

grafico_incidencia_regioes <- ggplot(incidencia_regioes_melted, aes(x= Data, y = Incidência, color = ARS)) +
  geom_line(size=0.5) +
  xlab("Mês") +
  ylab("Incidência") +
  labs(title="Evolução da Incidência por ARS") +
  # facet_grid(incidencia_regioes_melted$ARS) +
  guides(color = FALSE) +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(date_breaks = "months", date_labels = "%b")

ggplotly(grafico_incidencia_regioes)


## Evolução da incidencia por ARS - Média rolante 7 dias

medias_rolantes_7_incidencia_regiao <- cbind(incidencia_regioes_melted[7:nrow(incidencia_regioes_melted),1:2], as.data.frame(zoo::rollmean(incidencia_regioes_melted[,3], k = 7)))
names(medias_rolantes_7_incidencia_regiao) = c("Data", "ARS", "Incidência")

grafico_incidencia_tempo_mr_7 <- ggplot(medias_rolantes_7_incidencia_regiao, aes(x = Data, y = Incidência, color = ARS)) +
  geom_line()+
  xlab("Mês") +
  ylab("Incidência")+
  labs(title = "Evolução da Incidência por ARS - Média rolante 7 dias") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
  theme(legend.title = element_blank()) + 
  scale_x_date(date_breaks = "months", date_labels = "%b")

ggplotly(grafico_incidencia_tempo_mr_7) %>% 
  layout(legend=list(x=1, y=0))


## Evolução da incidencia por ARS - Média rolante 14 dias

medias_rolantes_14_incidencia_regiao <- cbind(incidencia_regioes_melted[14:nrow(incidencia_regioes_melted),1:2], as.data.frame(zoo::rollmean(incidencia_regioes_melted[,3], k = 14)))
names(medias_rolantes_14_incidencia_regiao) = c("Data", "ARS", "Incidência")

grafico_incidencia_tempo_mr_14 <- ggplot(medias_rolantes_14_incidencia_regiao, aes(x = Data, y = Incidência, color = ARS)) +
  geom_line()+
  xlab("Mês") +
  ylab("Incidência")+
  labs(title = "Evolução da Incidência por ARS - Média rolante 14 dias") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
  theme(legend.title = element_blank()) + 
  scale_x_date(date_breaks = "months", date_labels = "%b")

ggplotly(grafico_incidencia_tempo_mr_14) %>% 
  layout(legend=list(x=1, y=0))


## Evolução da Taxa de incidencia por ARS

t_incidencia_regioes_sd <- as.data.frame((covid.pt[,confirmados_arsnorte:confirmados_madeira]- lag(covid.pt[, confirmados_arsnorte:confirmados_madeira]))) / (pop_regioes_rep - as.data.frame(covid.pt[, confirmados_arsnorte:confirmados_madeira]))*100

t_incidencia_regioes <- cbind(covid.pt$data, t_incidencia_regioes_sd)
names(t_incidencia_regioes) <- c("Data", "Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

t_incidencia_regioes_melted <- reshape2::melt(t_incidencia_regioes, id.vars="Data")
names(t_incidencia_regioes_melted)[-1] <- c("ARS", "Taxa_Incidência")

ggplot(t_incidencia_regioes_melted, aes(x= Data, y = Taxa_Incidência, color = ARS)) +
  geom_line(size=1) +
  xlab("Mês") +
  ylab("Taxa de Incidência (%)") +
  labs(title="Evolução da Taxa de Incidência por ARS") +
  facet_grid(t_incidencia_regioes_melted$ARS) +
  guides(color = FALSE) +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(date_breaks = "months", date_labels = "%b")


## Taxa de Incidência cumulativa por ARS (tabela e grafico de barras)

rownames(pop_regioes) <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")
colnames(pop_regioes) <- "População"
pop_regioes_invertido <- t(pop_regioes)

prevalencia_regiao <- (as.data.frame(t(as.data.frame((lapply(covid.pt[,confirmados_arsnorte:confirmados_madeira], last)))))*100 / pop_regioes_invertido) %>% 
  rownames_to_column(var="ARS")

colnames(prevalencia_regiao)[2] <- "Prevalência"
prevalencia_regiao[,1] <- c("Norte", "Centro", "LVT", "Alentejo", "Algarve", "Açores", "Madeira")

grafico_prevalencia_regiao <- ggplot(prevalencia_regiao, aes(x=ARS, y=Prevalência)) + 
                              geom_col(fill="palegreen", width = 0.5) +
                              theme_classic() +
                              labs(y="Taxa de Incidência cumulativa (%)", title = "Taxa de Incidência cumulativa por ARS", x="") +
                              theme(plot.title = element_text(size = 20, hjust = 0.5)) +
                              theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
                              scale_y_continuous(expand = c(0, 0)) 
                              # coord_cartesian( ylim = c(0, max(prevalencia_regiao$Prevalência + 1000)))


ggplotly(grafico_prevalencia_regiao)


## Taxa de Incidência cumulativa por ARS (mapa)

prevalencia_regiao_ordem <- prevalencia_regiao[c(4,5,6,2,7,1,3),] # colocar as regioes pela ordem do mapa
prevalencia_regiao_ordem[,1] <- c("Alentejo", "Algarve", "Açores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_prevalencia_regiao <- colorBin("viridis", domain = prevalencia_regiao_ordem$Prevalência, bins = c(0, 0.1, 0.2, 0.3, 0.5, 0.7, 0.8, 1, Inf))

labels_prevalencia_regiao <- paste( 
  "<strong>", prevalencia_regiao_ordem[,1],"</strong><br/>", 
  round(prevalencia_regiao_ordem[,2], digits = 4), "&#37<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa_portugal) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_prevalencia_regiao(prevalencia_regiao_ordem$Prevalência),
              label = labels_prevalencia_regiao, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addLegend("bottomleft", pal = pal_prevalencia_regiao, values = prevalencia_regiao_ordem$Prevalência , opacity = 0.5, title = "Taxa de Incidência cumulativa por ARS") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Satélite"), options = layersControlOptions(collapsed = FALSE))



#Faixa etária e Género

## Nº de casos por faixa etária e por genero (tabela e grafico de barras)

femininos <- as.data.frame(covid.pt %>% 
                             dplyr::select(starts_with("confirmados_") & (ends_with("9_f")| ends_with("plus_f"))))

masculinos <- as.data.frame(covid.pt %>% 
                              dplyr::select((starts_with("confirmados_") & (ends_with("9_m")| ends_with("plus_m")))))

casos_femininos <- as.data.frame(lapply(femininos, last))
casos_masculinos <- as.data.frame(lapply(masculinos, last))

casos_femininos_invertido <- as.data.frame(t(casos_femininos))
casos_femininos_invertido <- casos_femininos_invertido %>% 
  rownames_to_column(var = "Idade")
names(casos_femininos_invertido)[2] <- "Feminino"
casos_femininos_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

casos_masculinos_invertido <- as.data.frame(t(casos_masculinos))
casos_masculinos_invertido <- casos_masculinos_invertido %>% 
  rownames_to_column(var = "Idade")
names(casos_masculinos_invertido)[2] <- "Masculino"
casos_masculinos_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

casos_fem_masc <- merge(casos_femininos_invertido, casos_masculinos_invertido, by = "Idade")

casos_grupo_etario_genero_melted <- reshape2::melt(casos_fem_masc, id.vars = "Idade")
names(casos_grupo_etario_genero_melted)[2:3] <- c("Género","Nº_casos")

grafico_casos_idade_sexo <- ggplot(casos_grupo_etario_genero_melted, aes(x=Idade, y=Nº_casos, fill=Género)) + 
                            geom_col(width = 0.9, position = "dodge") +
                            theme_classic() +
                            labs(y="Nº de casos", title = " Nº de casos por faixa etária e por género", x="") +
                            theme(plot.title = element_text(size = 17, hjust = 0.5)) +
                            theme(axis.title.x = element_text(size = 1, margin = margin(t=10, r=1, b= 2, l=1)), axis.title.y = element_text(size = 15)) +
                            theme(legend.title = element_blank()) + 
                            #geom_text(aes(label=Nº_casos),position = position_dodge(width = 0.8), vjust=-0.5, size=2) +
                            scale_fill_manual(values = c("pink2", "steelblue3")) +
                            scale_y_continuous(expand = c(0,0)) +
                            coord_cartesian(ylim = c(0, max(casos_grupo_etario_genero_melted$Nº_casos)+500))

ggplotly(grafico_casos_idade_sexo)
  

## Evolução da Incidência por faixa etária - grafico de área

femininos_casos_tempo <- femininos-lag(femininos)
masculinos_casos_tempo <- masculinos-lag(masculinos)
total_casos_tempo <- cbind(covid.pt$data, femininos_casos_tempo + masculinos_casos_tempo)
total_casos_tempo[7,2:10] <- femininos[7,] + masculinos[7,]
names(total_casos_tempo) <- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

total_casos_tempo_melted <- reshape2::melt(total_casos_tempo, id.vars="Data")
names(total_casos_tempo_melted)[2:3] <- c("Idade", "Nº_casos")

grafico_casos_idade_tempo <- ggplot(total_casos_tempo_melted, aes(x=Data, y=Nº_casos, fill=Idade)) +
                             geom_area() +
                             labs(title="Evolução da Incidência por faixa etária", y="Incidência", x="Mês")+
                             scale_x_date(date_breaks = "months", date_labels = "%b") +
                             theme(title = element_text(size = 17),
                                   legend.title = element_blank(),
                                   axis.title.x = element_text(size = 12),
                                   axis.title.y = element_text(size = 12),
                                   axis.text.y = element_text(size = 5),
                                   strip.text.y = element_text(size = 3, angle = 0))


ggplotly(grafico_casos_idade_tempo)


## Evolução da Incidência por faixa etária - grafico de linhas com média rolante 7 dias

medias_rolantes_incidencia_idade_tempo <- cbind(total_casos_tempo_melted[7:nrow(total_casos_tempo_melted),1:2], as.data.frame(zoo::rollmean(total_casos_tempo_melted[,3], k = 7)))
  names(medias_rolantes_incidencia_idade_tempo) = c("Data", "Idade", "Incidência")

grafico_incidencia_idade_tempo_mr <- ggplot(medias_rolantes_incidencia_idade_tempo, aes(x=Data, y=Incidência, color=Idade)) +
                                     geom_line() +
                                     labs(title="Evolução da Incidência por faixa etária")+
                                     xlab("Mês") +
                                     ylab("Incidência")+
                                     theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 17, color = "black", hjust = 0.5)) +
                                     theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
                                     theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
                                     theme(legend.title = element_blank()) + 
                                     scale_x_date(date_breaks = "months", date_labels = "%b")

  
  
  ggplotly(grafico_incidencia_idade_tempo_mr) %>% 
    layout(legend=list(x=1, y=0))

  
## Evoluçao da Taxa de Incidencia por faixa etária

pop_0_9 <- 894631
pop_10_19 <- 1056679
pop_20_29 <- 1092080
pop_30_39 <- 1250448
pop_40_49 <- 1575225
pop_50_59 <- 1482121
pop_60_69 <-  1293301
pop_70_79 <- 973123
pop_80_plus <- 668660
  
pop_idade <- as.data.frame(c(pop_0_9, pop_10_19, pop_20_29, pop_30_39, pop_40_49, pop_50_59, pop_60_69, pop_70_79, pop_80_plus))
  
pop_idade <- pop_idade %>% 
    rownames_to_column(var="Idade")
    names(pop_idade)[2] <- "População"
pop_idade[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

###tirar a coluna das idades
pop_idade_pop <- as.data.frame(pop_idade$População)

###repetir a coluna com a populaçao tantas vezes como ha dias, para se consegirem dividir uma tabela pela outra
pop_idade_rep <- as.data.frame(t(pop_idade_pop[rep(seq_len(ncol(pop_idade_pop)), each=nrow(covid.pt))]))
  
confirmados_idade <- femininos + masculinos
total_casos_tempo_sd <- total_casos_tempo[,2:10]

t_incidencia_grupo_etario_tempo <- cbind(covid.pt$data, total_casos_tempo_sd / (pop_idade_rep - confirmados_idade)*100)
colnames(t_incidencia_grupo_etario_tempo)[1] <- "Data"

t_incidencia_grupo_etario_tempo_melted <- reshape2::melt(t_incidencia_grupo_etario_tempo, id.vars="Data")
  names(t_incidencia_grupo_etario_tempo_melted)[2:3] <- c("Idade", "Taxa_Incidência")

ggplot(t_incidencia_grupo_etario_tempo_melted, aes(x = Data, y = Taxa_Incidência, color = Idade)) +
    geom_line(size=1) +
    facet_grid(t_incidencia_grupo_etario_tempo_melted$Idade)+
    xlab("Mês") +
    ylab("Taxa de incidência (%)")+
    labs(title = "Evolução da Taxa de Incidência por faixa etária") +
    guides(color = FALSE) +
    theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 17, color = "black", hjust = 0.5)) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
    theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
    theme(legend.title = element_blank()) + 
    scale_x_date(date_breaks = "months", date_labels = "%b")


## Taxa de Incidência cumulativa por faixa etária (tabela e grafico de barras)

casos_total <- as.data.frame(casos_femininos + casos_masculinos)

casos_total_invertido <- as.data.frame(t(casos_total))
casos_total_invertido <- casos_total_invertido %>%
  rownames_to_column(var = "Idade")
names(casos_total_invertido)[2] <- "Total"
casos_total_invertido[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

prevalencia_idade <- as.data.frame(round((casos_total_invertido$Total/pop_idade$População)*100, digits=4)) %>% 
  rownames_to_column(var="Idade")
names(prevalencia_idade)[2] <- "Prevalência"
prevalencia_idade[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

grafico_prevalencia_idade <- ggplot(prevalencia_idade, aes(x=Idade, y=Prevalência)) + 
                             geom_col(fill="slategray2", width = 0.5) +
                             scale_x_discrete(labels= c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")) +
                             theme_classic() +
                             labs(y="Prevalência (%)", title = "Taxa de Incidência cumulativa por faixa etária", x="") +
                             theme(plot.title = element_text(size = 20, hjust = 0.5)) +
                             theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) 
                             #geom_text(aes(label=Prevalência), vjust=-0.5)


ggplotly(grafico_prevalencia_idade)


## Evolução da Taxa de Incidência cumulativa por faixa etária

casos_total_tempo <- as.data.frame(femininos+masculinos)

prevalencia_idade_tempo_sd <- round((casos_total_tempo/pop_idade_rep)*100, digits = 4)

prevalencia_idade_tempo <- cbind(covid.pt$data, prevalencia_idade_tempo_sd)
colnames(prevalencia_idade_tempo) <- c( "Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+" )

prevalencia_idade_tempo_melted <- reshape2::melt(prevalencia_idade_tempo, id.vars="Data")
names(prevalencia_idade_tempo_melted)[-1] <- c("Idade", "Prevalencia")

grafico_prevalencia_idade_tempo <- ggplot(prevalencia_idade_tempo_melted, aes(x = Data, y = Prevalencia, color = Idade)) +
                                   geom_line() +
                                   xlab("Mês") +
                                   ylab("Taxa de Incidência cumulativa (%)")+
                                   labs(title = "Evolução da Taxa de Incidência cumulativa por faixa etária") +
                                   theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15, color = "black", hjust = 0.5)) +
                                   theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
                                   theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
                                   theme(legend.title = element_blank()) + 
                                   scale_x_date(date_breaks = "months", date_labels = "%b")


ggplotly(grafico_prevalencia_idade_tempo) %>% 
  layout(legend=list(x=1, y=0))


## Evolução da Incidência por género

pop_total <- 10295909
pop_homens <- 4859977
pop_mulheres <- 5435932

incidencia_homens <- as.data.frame(covid.pt$confirmados_m - lag(covid.pt$confirmados_m))
                                   
incidencia_mulheres <-as.data.frame(covid.pt$confirmados_f - lag(covid.pt$confirmados_f))

incidencia_genero <- cbind(covid.pt$data, incidencia_mulheres, incidencia_homens)
  names(incidencia_genero) <- c("Data", "Mulheres", "Homens")

incidencia_melt <- reshape2::melt(incidencia_genero, id.vars="Data")
  names(incidencia_melt)[-1] <- c("Género", "Incidência")
  
grafico_incidencia_genero_tempo <- ggplot(incidencia_melt, aes(x=Data, y=Incidência, color=Género)) +
                                   geom_line(size=0.5) +
                                   labs(title="Evolução da incidência por género", color="") +
                                   xlab("Mês") +
                                   ylab("Incidência") +
                                   theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
                                   theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
                                   theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
                                   scale_x_date(date_breaks = "months", date_labels = "%b")
  
ggplotly(grafico_incidencia_genero_tempo)
  

## Evoluçao da Taxa de incidência por género

t_incidencia_total <- as.data.frame(covid.pt$confirmados_novos/(pop_total - covid.pt$confirmados))

t_incidencia_homens <- as.data.frame((covid.pt$confirmados_m - lag(covid.pt$confirmados_m))/(pop_homens - covid.pt$confirmados_m))
t_incidencia_homens[174:175,] <- NA

t_incidencia_mulheres <- as.data.frame((covid.pt$confirmados_f - lag(covid.pt$confirmados_f))/(pop_mulheres - covid.pt$confirmados_f))
t_incidencia_mulheres[174:175,] <- NA

t_incidencia_nt <- cbind(covid.pt$data, t_incidencia_total, t_incidencia_homens, t_incidencia_mulheres)
names(t_incidencia_nt) <- c("Data", "Total", "Homens", "Mulheres")

t_incidencia_melt <- reshape2::melt(t_incidencia_nt, id.vars="Data")
names(t_incidencia_melt)[-1] <- c("Género", "Valores")

ggplot(t_incidencia_melt, aes(x=Data, y=Valores*100, color=Género)) +
  geom_line(size=0.7) +
  labs(title="Evolução da Taxa de Incidência por género", color="") +
  facet_grid(t_incidencia_melt$Género)+
  guides(color = FALSE) +
  xlab("Mês") +
  ylab("Incidência (%)") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
  scale_x_date(date_breaks = "months", date_labels = "%b")




# NUMERO DE MORTES 

#Mortalidade

## Relação da evolução da incidencia com o numero de mortes diarias

novas_mortes <- as.data.frame(covid.pt$obitos - lag(covid.pt$obitos))
  
incidencia_mortes <- cbind(incidencia, novas_mortes)
  names(incidencia_mortes)[3] <- "Mortes_diárias"
  
  
incidencia_mortes_melted <- reshape2::melt(incidencia_mortes, id.vars="Data")
  names(incidencia_mortes_melted)[-1] <- c("Variável", "Valor")
  
ggplot(incidencia_mortes_melted, aes(x= Data, y = Valor, color = Variável)) +
    geom_line(size=1) +
    xlab("Mês") +
    ylab("") +
    labs(title="Relação da evolução da incidencia com o numero de mortes diarias") +
    facet_grid(incidencia_mortes_melted$Variável, scales = "free_y") +
    guides(color = FALSE) +
    theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
    theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
    scale_x_date(date_breaks = "months", date_labels = "%b") 


## Nº de mortes por faixa etária e por género (tabela e grafico de barras)


femininos_mortes <- as.data.frame(covid.pt %>% 
                                    dplyr::select(starts_with("obitos_") & (ends_with("9_f")| ends_with("plus_f"))))

masculinos_mortes <- as.data.frame(covid.pt %>% 
                                     dplyr::select((starts_with("obitos_") & (ends_with("9_m")| ends_with("plus_m")))))


mortes_grupo_etario_f <- as.data.frame(t(as.data.frame(lapply(femininos_mortes, last))))
mortes_grupo_etario_m <- as.data.frame(t(as.data.frame(lapply(masculinos_mortes, last))))

mortes_grupo_etario_f <- mortes_grupo_etario_f %>%
  rownames_to_column(var="Idade")
  names(mortes_grupo_etario_f)[2] <- "Feminino"
  mortes_grupo_etario_f[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
mortes_grupo_etario_m <- mortes_grupo_etario_m %>%
  rownames_to_column(var="Idade")
  names(mortes_grupo_etario_m)[2] <- "Masculino"
  mortes_grupo_etario_m[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
mortes_grupo_etario_fm <- merge(mortes_grupo_etario_f, mortes_grupo_etario_m, by = "Idade")

mortes_grupo_etario_melted <- reshape2::melt(mortes_grupo_etario_fm, id.vars = "Idade")
  names(mortes_grupo_etario_melted)[2:3] <- c("Género","Mortes")
  
  
grafico_mortes_idades_genero <- ggplot(mortes_grupo_etario_melted, aes(x=Idade, y=Mortes, fill=Género)) +
                                geom_col(width = 0.9, position = "dodge") +
                                theme_classic() +
                                labs(y="Nº de mortes", title = "Nº de mortes por faixa etária e por género", x="") +
                                theme(plot.title = element_text(size = 20, hjust = 0.5)) +
                                theme(axis.title.x = element_text(size = 1), axis.title.y = element_text(size = 15)) +
                                #geom_text(aes(label=Mortes),position = position_dodge(width = 0.9), vjust=-0.5, size=3.5) +
                                scale_fill_manual(values = c("pink2", "steelblue3"))

ggplotly(grafico_mortes_idades_genero)


## Nº de mortes por ARS (tabela e grafico de barras)

regioes_mortes <- as.data.frame(t(as.data.frame(lapply(covid.pt[,obitos_arsnorte:obitos_madeira], last))))

regioes_mortes <- regioes_mortes %>% 
  rownames_to_column(var="Regiões")
names(regioes_mortes)[2] <- "Nº_mortes"

ggplot(regioes_mortes, aes(x=Regiões, y=Nº_mortes)) + 
  geom_col(fill="olivedrab4", width = 0.5) +
  scale_x_discrete(labels= c("Açores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Norte", "Madeira")) +
  theme_classic() +
  labs(y="Nº de mortes", title = "Nº de mortes de COVID-19 por Regiões", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label=Nº_mortes), vjust=-0.5)


## Nº de mortes por ARS (mapa)

regioes_mortes_ordem <- regioes_mortes[c(4,5,6,2,7,1,3),] # colocar as regioes pela ordem do mapa
regioes_mortes_ordem[,1] <- c("Alentejo", "Algarve", "Açores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_mortes <- colorBin("YlOrBr", domain = regioes_mortes_ordem$Nº_mortes, bins = c(0, 50, 100, 500, 800, 1000, Inf)) 

labels_mortes <- paste( 
  "<strong>", regioes_mortes_ordem[,1],"</strong><br/>", 
  regioes_mortes_ordem[,2], " mortes<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa_portugal) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_mortes(regioes_mortes_ordem$Nº_mortes),
              label = labels_mortes, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addLegend("bottomleft", pal = pal_mortes, values = regioes_mortes_ordem$Nº_mortes, opacity = 0.5, title = "Nº de mortes por ARS") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Satélite", "Dark"), options = layersControlOptions(collapsed = FALSE))


## Nº de mortes por genero e total (Tabela e grafico de barras) - (não foi para o relatório)

mortes_total <- last(covid.pt$obitos)
mortes_mulheres <- last(covid.pt$obitos_f)
mortes_homens <- last(covid.pt$obitos_m)

numero_mortes_nt <-cbind(mortes_total, mortes_homens, mortes_mulheres)

numero_mortes <- as.data.frame(t(numero_mortes_nt))
numero_mortes <- numero_mortes %>%
  rownames_to_column(var="Género")
names(numero_mortes)[2] <- "Nº_mortes"

ggplot(numero_mortes, aes(x=Género, y=Nº_mortes)) +
  geom_col(fill="steelblue4", width = 0.5) +
  scale_x_discrete(labels= c("Homens", "Mulheres", "Total")) +
  theme_classic() +
  labs(y="Nº de mortes", title = "Número de mortes por COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = Nº_mortes), vjust=-0.5)


## Evolução do numero de mortes por ARS - (não foi para o relatorio)

regioes_mortes_tempo <- as.data.frame(cbind(covid.pt$data, (covid.pt[,obitos_arsnorte:obitos_madeira]-lag(covid.pt[,obitos_arsnorte:obitos_madeira]))))
names(regioes_mortes_tempo) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Algarve","Açores", "Madeira")

regioes_mortes_tempo_melted <- reshape2::melt(regioes_mortes_tempo, id.vars="Data")
names(regioes_mortes_tempo_melted)[-1] <- c("ARS", "Nº_mortes")

ggplot(regioes_mortes_tempo_melted, aes(x = Data, y = Nº_mortes, color = ARS)) +
  geom_line() +
  xlab("Mês") +
  ylab("Nº de mortes")+
  labs(title = "Evolução do nº de mortes por ARS") +
  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15, color = "black", hjust = 0.5)) +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
  theme(legend.title = element_blank()) + 
  scale_x_date(date_breaks = "months", date_labels = "%b") +
  facet_grid(regioes_mortes_tempo_melted$ARS)  +
  guides(color = FALSE) +
  theme(strip.text.y = element_text(color = "black", size = 8, angle = 0))



#Taxa de mortalidade

## Taxa de mortalidade por género (tabela e grafico de barras)

tm_total <- last(covid.pt$obitos)/pop_total
tm_homens <- last(covid.pt$obitos_m)/ pop_homens
tm_mulheres <- last(covid.pt$obitos_f)/ pop_mulheres

tm_nt <- cbind(tm_total, tm_homens, tm_mulheres)

tm <- as.data.frame(t(tm_nt))
tm <- tm %>% 
  rownames_to_column(var="Género")
names(tm)[2] <- "Taxa_Mortalidade"

ggplot(tm, aes(x=Género, y=Taxa_Mortalidade*100)) + 
  geom_col(fill="snow3", width = 0.5) +
  scale_x_discrete(labels= c("Homens", "Mulheres", "Total")) +
  theme_classic() +
  labs(y="Taxa de mortalidade (%)", title = "Taxa de mortalidade da COVID-19", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Mortalidade, digits=4)), vjust=-0.5)


## Taxa de mortalidade por faixa etária 

mortes_grupo_etario_total <- as.data.frame(mortes_grupo_etario_f + mortes_grupo_etario_m)

mortes_grupo_etario_total <- mortes_grupo_etario_total %>%
  rownames_to_column(var="Idade")
names(mortes_grupo_etario_total)[2] <- "Total"
mortes_grupo_etario_total[,1] <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

t_mortalidade_idade_nt <- as.data.frame(mortes_grupo_etario_total$Total / pop_idade$População)*100

t_mortalidade_idade <- cbind(pop_idade$Idade, t_mortalidade_idade_nt)
names(t_mortalidade_idade) <- c("Idade", "Taxa_mortalidade")

grafico_t_mortalidade_idade <- ggplot(t_mortalidade_idade, aes(x=Idade, y=Taxa_mortalidade)) + 
  geom_col(fill="indianred4", width = 0.5) +
  theme_classic() +
  labs(y="Taxa de mortalidade (%)", title = "Taxa de mortalidade por faixa etária", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) 

ggplotly(grafico_t_mortalidade_idade)


## Taxa de mortalidade por ARS (tabela e grafico de barras)

tm_acores <- last(covid.pt$obitos_acores)/pop_acores *100 
tm_alentejo <- last(covid.pt$obitos_arsalentejo)/pop_alentejo *100
tm_algarve <- last(covid.pt$obitos_arsalgarve)/pop_algarve *100
tm_centro <- last(covid.pt$obitos_arscentro)/pop_centro *100 
tm_lvt <- last(covid.pt$obitos_arslvt)/pop_lvt *100
tm_norte <- last(covid.pt$obitos_arsnorte)/pop_norte *100
tm_madeira <- last(covid.pt$obitos_madeira)/pop_madeira *100

regioes_tm_nt <- cbind(tm_acores, tm_alentejo, tm_algarve, tm_centro, tm_lvt, tm_norte, tm_madeira)

regioes_tm <- as.data.frame(t(regioes_tm_nt))
regioes_tm <- regioes_tm %>% 
  rownames_to_column(var="Região")
names(regioes_tm)[2] <- "Taxa_Mortalidade"
regioes_tm[,1] <- c( "Açores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Norte", "Madeira")

grafico_t_mortalidade_regiao <- ggplot(regioes_tm, aes(x=Região, y=Taxa_Mortalidade)) + 
                                geom_col(fill="indianred4", width = 0.5) +
                                scale_x_discrete(labels= c("Açores", "Alentejo", "Algarve", "Centro", "LVT", "Madeira", "Norte")) +
                                theme_classic() +
                                labs(y="Taxa de mortalidade (%)", title = "Taxa de mortalidade por ARS", x="") +
                                theme(plot.title = element_text(size = 20, hjust = 0.5)) +
                                theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) 
                                # geom_text(aes(label = scales::percent(round(Taxa_Mortalidade, digits=5))), vjust=-0.5)

ggplotly(grafico_t_mortalidade_regiao)


## Taxa de mortalidade por ARS (mapa)

regioes_tm_ordem <- regioes_tm[c(2,3,1,4,7,6,5),] # colocar as regioes pela ordem do mapa
regioes_tm_ordem[,1] <- c("Alentejo", "Algarve", "Açores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_tm <- colorBin("BuPu", domain = regioes_tm_ordem$Taxa_Mortalidade*100, bins = c(0, 0.003, 0.005, 0.007, 0.01, 0.02, Inf)) 

labels_tm <- paste( 
  "<strong>", regioes_tm_ordem[,1],"</strong><br/>", 
  round(regioes_tm_ordem[,2]*100, digits = 4), "&#37<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa_portugal) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_tm(regioes_tm_ordem$Taxa_Mortalidade),
              label = labels_tm, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addLegend("bottomleft", pal = pal_tm, values = regioes_tm_ordem$Taxa_Mortalidade, opacity = 0.5, title = "Taxa de mortalidade por ARS (%)") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Satélite", "Dark"), options = layersControlOptions(collapsed = FALSE))


  
#Taxa de letalidade

## Taxa de letalidade por género (tabela e grafico de barras)

letalidade_total <- last(covid.pt$obitos)/last(covid.pt$confirmados)
letalidade_homens <- last(covid.pt$obitos_m)/last(covid.pt$confirmados_m)
letalidade_mulheres <- last(covid.pt$obitos_f)/last(covid.pt$confirmados_f)

letalidade_nt <- cbind(letalidade_total, letalidade_homens, letalidade_mulheres)

letalidade <- as.data.frame(t(letalidade_nt))
letalidade <- letalidade %>% 
  rownames_to_column(var="Género")
names(letalidade)[2] <- "Taxa_Letalidade"

ggplot(letalidade, aes(x=Género, y=Taxa_Letalidade*100)) + 
  geom_col(fill="lightblue3", width = 0.5) +
  scale_x_discrete(labels= c("Homens", "Mulheres", "Total")) +
  theme_classic() +
  labs(y="Taxa de letalidade (%)", title = "Taxa de letalidade por género", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Letalidade, digits=4)), vjust=-0.5)


## Evolução da taxa de letalidade por genero e total
 
t_letalidade_fem <- as.data.frame(covid.pt$obitos_f/covid.pt$confirmados_f)*100
t_letalidade_masc <- as.data.frame(covid.pt$obitos_m/covid.pt$confirmados_m)*100
t_letalidade_total <- as.data.frame(covid.pt$obitos/covid.pt$confirmados)*100

t_letalidade_fm <- cbind(t_letalidade_fem, t_letalidade_masc)                                   
t_letalidade_genero_sd <- cbind(t_letalidade_fm, t_letalidade_total)
t_letalidade_genero <- cbind(covid.pt$data, t_letalidade_genero_sd)
  names(t_letalidade_genero) <- c("Data", "Feminino", "Masculino", "Total")
                  
t_letalidade_genero_melted <- reshape2::melt(t_letalidade_genero, id.vars="Data")
names(t_letalidade_genero_melted)[-1] <- c("Género", "Taxa_Letalidade")
  
grafico_t_letalidade_genero <- ggplot(t_letalidade_genero_melted, aes(x = Data, y = Taxa_Letalidade, color = Género)) +
                               geom_line(size=1) +
                               xlab("Mês") +
                               ylab("Taxa de Letalidade (%)")+
                               labs(title = "Evolução da Taxa de Letalidade por género") +
                               theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15, color = "black", hjust = 0.5)) +
                               theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
                               theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
                               theme(legend.title = element_blank()) + 
                               scale_x_date(date_breaks = "months", date_labels = "%b")
  
  ggplotly(grafico_t_letalidade_genero) %>%
    layout(legend=list(x=1, y=0))
  

## Taxa de letalidade por faixa etária e por género (x=idade)

letalidade_genero_grupo_etario_f_t <- femininos_let_o/femininos_let_conf
letalidade_genero_grupo_etario_f <- as.data.frame(t(as.data.frame(lapply(letalidade_genero_grupo_etario_f_t, last))))

letalidade_genero_grupo_etario_f <- letalidade_genero_grupo_etario_f %>% 
  rownames_to_column(var = "Idade")
names(letalidade_genero_grupo_etario_f)[2] <- "Feminino"
letalidade_genero_grupo_etario_f[,1] <- c( "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

letalidade_genero_grupo_etario_m_t <-masculinos_let_o/masculinos_let_conf
letalidade_genero_grupo_etario_m <- as.data.frame(t(as.data.frame(lapply(letalidade_genero_grupo_etario_m_t, last))))

letalidade_genero_grupo_etario_m <- letalidade_genero_grupo_etario_m %>% 
  rownames_to_column(var = "Idade")
names(letalidade_genero_grupo_etario_m)[2] <- "Masculino"
letalidade_genero_grupo_etario_m[,1] <- c( "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")


letalidade_genero_grupo_etario_total_t <- (femininos_let_o + masculinos_let_o)/(femininos_let_conf+masculinos_let_conf)
letalidade_genero_grupo_etario_total <- as.data.frame(t(as.data.frame(lapply(letalidade_genero_grupo_etario_total_t, last))))

letalidade_genero_grupo_etario_total <- letalidade_genero_grupo_etario_total %>% 
  rownames_to_column(var = "Idade")
names(letalidade_genero_grupo_etario_total)[2] <- "Total"
letalidade_genero_grupo_etario_total[,1] <- c( "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")


letalidade_genero_grupo_etario_merged_mf <- merge(letalidade_genero_grupo_etario_f, letalidade_genero_grupo_etario_m, by="Idade")
letalidade_genero_grupo_etario_merged <- merge(letalidade_genero_grupo_etario_merged_mf, letalidade_genero_grupo_etario_total, by="Idade")

letalidade_genero_grupo_etario_melted <- reshape2::melt(letalidade_genero_grupo_etario_merged, id.vars= "Idade")
names(letalidade_genero_grupo_etario_melted)[-1] <- c("Género", "Taxa_Letalidade")

grafico_letalidde_genero_idade <- ggplot(letalidade_genero_grupo_etario_melted, aes(x = Idade, y = Taxa_Letalidade*100, color = Género, 
                                                                                    tooltip = round(Taxa_Letalidade*100, digits = 2), data_id = Taxa_Letalidade)) +
                                  geom_point_interactive() +
                                  xlab("Idade") +
                                  ylab("Taxa de Letalidade (%)") +
                                  labs(title="Taxa de Letalidade por faixa etária e por género") +
                                  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 17, color = "black", hjust = 0.5)) +
                                  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
                                  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13))

girafe(code = print(grafico_letalidde_genero_idade),
       options = list(
         opts_zoom(max = 2),
         opts_hover(css = "fill:black;"),
         opts_sizing(rescale = TRUE, width = 0.8)
       ))


## Evolucao da Taxa de Letalidade por faixa etária

femininos_let_o <- as.data.frame(covid.pt %>% 
                                   dplyr::select(starts_with("obitos_") & (ends_with("9_f")| ends_with("plus_f"))))

femininos_let_conf <- as.data.frame(covid.pt %>% 
                                      dplyr::select(starts_with("confirmados_") & (ends_with("9_f")| ends_with("plus_f"))))

masculinos_let_o <- as.data.frame(covid.pt %>% 
                                    dplyr::select((starts_with("obitos_") & (ends_with("9_m")| ends_with("plus_m")))))

masculinos_let_conf <- as.data.frame(covid.pt %>% 
                                       dplyr::select((starts_with("confirmados_") & (ends_with("9_m")| ends_with("plus_m")))))

total_let_o <- femininos_let_o + masculinos_let_o
total_let_conf <- femininos_let_conf + masculinos_let_conf

letalidade_idade_tempo_sd <- total_let_o / total_let_conf *100

letalidade_idade_tempo <- cbind(covid.pt$data, letalidade_idade_tempo_sd)
names(letalidade_idade_tempo) <- c("Data", "0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")

letalidade_idade_tempo_melted <- reshape2::melt(letalidade_idade_tempo, id.vars="Data")
names(letalidade_idade_tempo_melted)[-1] <- c("Idade", "Taxa_Letalidade")

grafico_letalidade_idade_tempo <- ggplot(letalidade_idade_tempo_melted, aes(x = Data, y = Taxa_Letalidade, color = Idade)) +
                                  geom_line(size=0.5) +
                                  xlab("Mês") +
                                  ylab("Taxa de Letalidade (%)") +
                                  labs(title="Evolução da Taxa de Letalidade por faixa etária") +
                                  #facet_grid(letalidade_idade_tempo_melted$Idade)+ 
                                  #guides(color = FALSE) +
                                  theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15, color = "black", hjust = 0.5)) +
                                  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
                                  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
                                  theme(legend.title = element_blank()) + 
                                  scale_x_date(date_breaks = "months", date_labels = "%b")
  
  ggplotly(grafico_letalidade_idade_tempo) %>% 
    layout(legend=list(x=1, y=0))


## Taxa de letalidade por ARS (tabela e grafico de barras)

letalidade_acores <- last(covid.pt$obitos_acores)/last(covid.pt$confirmados_acores)
letalidade_alentejo <- last(covid.pt$obitos_arsalentejo)/last(covid.pt$confirmados_arsalentejo)
letalidade_algarve <- last(covid.pt$obitos_arsalgarve)/last(covid.pt$confirmados_arsalgarve)
letalidade_centro <- last(covid.pt$obitos_arscentro)/last(covid.pt$confirmados_arscentro)
letalidade_lvt <- last(covid.pt$obitos_arslvt)/last(covid.pt$confirmados_arslvt)
letalidade_madeira <- last(covid.pt$obitos_madeira)/last(covid.pt$confirmados_madeira)
letalidade_norte <- last(covid.pt$obitos_arsnorte)/last(covid.pt$confirmados_arsnorte)

regioes_letalidade_nt <- cbind(letalidade_acores, letalidade_alentejo, letalidade_algarve, letalidade_centro, letalidade_lvt, letalidade_madeira, letalidade_norte)

regioes_letalidade <- as.data.frame(t(regioes_letalidade_nt))
regioes_letalidade <- regioes_letalidade %>% 
  rownames_to_column(var="Regiões")
names(regioes_letalidade)[2] <- "Taxa_Letalidade"

ggplot(regioes_letalidade, aes(x=Regiões, y=Taxa_Letalidade*100)) + 
  geom_col(fill="powderblue", width = 0.5) +
  scale_x_discrete(labels= c("Açores", "Alentejo", "Algarve", "Centro", "Lisboa e Vale do Tejo", "Madeira", "Norte")) +
  theme_classic() +
  labs(y="Taxa de letalidade (%)", title = "Taxa de letalidade por ARS", x="") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
  geom_text(aes(label = scales::percent(Taxa_Letalidade, digits=4)), vjust=-0.5)


## Taxa de letalidade por ARS (mapa)

regioes_letalidade_ordem <- regioes_letalidade[c(2,3,1,4,6,7,5),]
regioes_letalidade_ordem[,1] <- c("Alentejo", "Algarve", "Açores", "Centro", "Madeira", "Norte", "Lisboa e Vale do Tejo" )

pal_letalidade <- colorBin("Oranges", domain = regioes_letalidade_ordem$Taxa_Letalidade*100, bins = c(0, 1, 2, 3, 4, 5, 6, Inf)) 

labels_letalidade <- paste( 
  "<strong>", regioes_letalidade_ordem[,1],"</strong><br/>", 
  round(regioes_letalidade_ordem[,2]*100, digits = 4), "&#37<br/>", 
  sep="") %>%
  lapply(htmltools::HTML)

leaflet(mapa_portugal) %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, fillOpacity = 1, color = "black", weight = 1,
              fillColor = ~pal_letalidade(regioes_letalidade_ordem$Taxa_Letalidade*100),
              label = labels_letalidade, 
              labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")) %>% 
  addLegend("bottomleft", pal = pal_letalidade, values = regioes_letalidade_ordem$Taxa_Letalidade, opacity = 0.5, title = "Taxa de letalidade por ARS (%)") %>% 
  addTiles(group ="Original") %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satélite") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
  addLayersControl(baseGroups = c("Original", "Positron", "Satélite", "Dark"), options = layersControlOptions(collapsed = FALSE))


## Evolucao da taxa de Letalidade por ARS

letalidade_regioes_tempo <- cbind(covid.pt$data, as.data.frame(covid.pt[,49:55]/covid.pt[,4:10]*100))
names(letalidade_regioes_tempo) <- c("Data", "Norte", "Centro", "Lisboa e Vale do Tejo", "Alentejo", "Alrgarve", "Açores", "Madeira")

letalidade_regioes_tempo_melt <- reshape2::melt(letalidade_regioes_tempo, id.vars="Data")
  colnames(letalidade_regioes_tempo_melt)[2:3] <- c("ARS", "Taxa_letalidade")

grafico_letalidade_regiao_tempo <- ggplot(letalidade_regioes_tempo_melt, aes(x = Data, y = Taxa_letalidade, color = ARS)) +
                                   geom_line() +
                                   xlab("Mês") +
                                   ylab("Taxa de Letalidade (%)")+
                                   labs(title = "Evolução da Taxa de letalidade por ARS") +
                                   theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15, color = "black", hjust = 0.5)) +
                                   theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
                                   theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
                                   scale_x_date(date_breaks = "months", date_labels = "%b")

ggplotly(grafico_letalidade_regiao_tempo)




# INTERNAMENTOS

## Evolução do número de Internados e Internados UCI (Nº absoluto)

numero_internados <- covid.pt[,c(1,15,16)]
names(numero_internados)[1:3] <- c("Data", "Internados", "Internados UCI")

numero_internados_melted <-  melt(numero_internados, id.vars="Data")
  colnames(numero_internados_melted)[2:3] <- c("Tipo", "Número")


internados_numero_tempo <- ggplot(numero_internados_melted, aes(x = Data, y = Número, color = Tipo)) +
                           geom_line() +
                           xlab("Mês") +
                           ylab("Nº de internados e internados UCI")+
                           labs(title = "Evolução do número de Internados e Internados UCI", color="") +
                           theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15, color = "black", hjust = 0.5)) +
                           theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
                           theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
                           scale_x_date(date_breaks = "months", date_labels = "%b")

ggplotly(internados_numero_tempo)%>% 
  layout(legend=list(x=1, y=0))


## Evolução da Taxa de internamento (Internados e internados UCI) (por Nº de casos confirmados)

percentagem_internados_sd <- as.data.frame((covid.pt[,15:16]/covid.pt$confirmados)*100)
percentagem_internados <- cbind(covid.pt$data, percentagem_internados_sd)
names(percentagem_internados)[1:3] <- c("Data", "Internados", "Internados UCI")

percentagem_internados_melted <- reshape2::melt(percentagem_internados, id.vars="Data")
  colnames(percentagem_internados_melted)[2:3] <- c("Tipo", "Taxa_internamento")

internados_taxa_tempo <- ggplot(percentagem_internados_melted, aes(x = Data, y = Taxa_internamento, color = Tipo)) +
                         geom_line() +
                         xlab("Mês") +
                         #scale_y_continuous(limits= c(0,5)) +
                         ylab("Taxa de internados e internados UCI")+
                         labs(title = "Evolução da Taxa de internados e internados UCI", color="") +
                         theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 17, color = "black", hjust = 0.5)) +
                         theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 13)) +
                         theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 13)) +
                         scale_x_date(date_breaks = "months", date_labels = "%b")

ggplotly(internados_taxa_tempo)%>% 
  layout(legend=list(x=1, y=0)) 


## Rácio entre Internados UCI e Internados

racio_internados <- cbind(covid.pt$data, as.data.frame(covid.pt[,16]/covid.pt[,15]*100))
  names(racio_internados) <- c("Data", "Rácio")
  
grafico_racio_internados <- ggplot(racio_internados, aes(x = Data, y = Rácio)) +
                            geom_line(color="tomato4") +
                            xlab("Mês") +
                            ylab("Rácio (%)") +
                            labs(title="Rácio entre Internados UCI e Internados") +
                            theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
                            theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
                            theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
                            scale_x_date(date_breaks = "months", date_labels = "%b")
  
  
  ggplotly(grafico_racio_internados)%>% 
    layout(legend=list(x=1, y=0)) 
  
  
  
  
# RECUPERADOS
  
## Evolução da Taxa de Recuperados
  
recuperados_sd <-  as.data.frame((covid.pt$recuperados/covid.pt$confirmados)*100)
recuperados <- cbind(covid.pt$data, recuperados_sd)
names(recuperados)[1:2] <- c("Data", "Recuperados")
  
recuperados_taxa <- ggplot(recuperados, aes(x = Data, y = Recuperados)) +
                    geom_line(color = "salmon1", size = 1) +
                    xlab("Mês") +
                    ylab("Taxa de Recuperados (%)") +
                    labs(title="Evolução da Taxa de Recuperados") +
                    theme(plot.title = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 20, color = "black", hjust = 0.5)) +
                    theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 20), size = 15)) +
                    theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 20, l = 0), size = 15)) +
                    scale_x_date(date_breaks = "months", date_labels = "%b")
  
  
ggplotly(recuperados_taxa)  
  
  
  
  
# FUTURO 
  
  ## Taxa de letalidade ajustada à idade (tabela) (eventualmente com a evolucao temporal - ainda nao tem)
  
  percentagem_confirmados_0_9 <- (last(covid.pt$confirmados_0_9_f) + last(covid.pt$confirmados_0_9_m)) / last(covid.pt$confirmados)
  percentagem_confirmados_10_19 <- (last(covid.pt$confirmados_10_19_f) + last(covid.pt$confirmados_10_19_m)) / last(covid.pt$confirmados)
  percentagem_confirmados_20_29 <- (last(covid.pt$confirmados_20_29_f) + last(covid.pt$confirmados_20_29_m)) / last(covid.pt$confirmados)
  percentagem_confirmados_30_39 <- (last(covid.pt$confirmados_30_39_f) + last(covid.pt$confirmados_30_39_m)) / last(covid.pt$confirmados)
  percentagem_confirmados_40_49 <- (last(covid.pt$confirmados_40_49_f) + last(covid.pt$confirmados_40_49_m)) / last(covid.pt$confirmados)
  percentagem_confirmados_50_59 <- (last(covid.pt$confirmados_50_59_f) + last(covid.pt$confirmados_50_59_m)) / last(covid.pt$confirmados)
  percentagem_confirmados_60_69 <- (last(covid.pt$confirmados_60_69_f) + last(covid.pt$confirmados_60_69_m)) / last(covid.pt$confirmados)
  percentagem_confirmados_70_79 <- (last(covid.pt$confirmados_70_79_f) + last(covid.pt$confirmados_70_79_m)) / last(covid.pt$confirmados)
  percentagem_confirmados_80_plus <- (last(covid.pt$confirmados_80_plus_f) + last(covid.pt$confirmados_80_plus_m)) / last(covid.pt$confirmados)
  
  percentagem_confirmados <- as.data.frame(t(cbind(percentagem_confirmados_0_9, percentagem_confirmados_10_19, percentagem_confirmados_20_29, percentagem_confirmados_30_39, percentagem_confirmados_40_49, percentagem_confirmados_50_59, percentagem_confirmados_60_69, percentagem_confirmados_70_79, percentagem_confirmados_80_plus)))
  
  letalidade_aged_ajusted_tabela_1 <- cbind(letalidade_genero_grupo_etario_merged[,4], percentagem_confirmados) 
  row.names(letalidade_aged_ajusted_tabela_1) <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+")
  
  letalidade_aged_ajusted_tabela_1 <- letalidade_aged_ajusted_tabela_1 %>% 
    rownames_to_column(var="Idades")
  colnames(letalidade_aged_ajusted_tabela_1)[2:3] <- c("Taxa_Letalidade", "Percentagem_confirmados")
  
  cross_products_aged_ajusted <- letalidade_aged_ajusted_tabela_1[,2]* letalidade_aged_ajusted_tabela_1[,3]
  
  letalidade_aged_ajusted_tabela_2 <- cbind(letalidade_aged_ajusted_tabela_1, cross_products_aged_ajusted)
  colnames(letalidade_aged_ajusted_tabela_2)[4] <- "Cross_products"
  
  total <- c("Total", letalidade[1,2] , round(sum(letalidade_aged_ajusted_tabela_2[,3]), digits = 1), sum(letalidade_aged_ajusted_tabela_2[,4]))
  
  letalidade_aged_ajusted <- rbind(letalidade_aged_ajusted_tabela_2, total)
  
  
  ## Taxa de letalidade ajustada ao sexo (tabela)
  
  percentagem_confirmados_fem <- last(covid.pt$confirmados_f) / last(covid.pt$confirmados)
  percentagem_confirmados_masc <- last(covid.pt$confirmados_m) / last(covid.pt$confirmados)
  percentagem_confirmados_genero <- as.data.frame(t(cbind(percentagem_confirmados_fem, percentagem_confirmados_masc)))
  rownames(percentagem_confirmados_genero)[1:2] <- c("Feminino", "Masculino")
  colnames(percentagem_confirmados_genero) [1] <- "Percentagem_confirmados"
  
  letalidade_fem <- last(covid.pt$obitos_f) / last(covid.pt$confirmados_f)
  letalidade_masc <- last(covid.pt$obitos_m) / last(covid.pt$confirmados_m)
  letalidade_genero <- as.data.frame(t(cbind(letalidade_fem, letalidade_masc)))
  rownames(letalidade_genero)[1:2] <- c("Feminino", "Masculino")
  colnames(letalidade_genero) [1] <- "Taxa_Letalidade"
  
  letalidade_sex_adjusted_tabela_1 <- cbind(letalidade_genero, percentagem_confirmados_genero)
  
  cross_products_sex_ajusted <- as.data.frame(letalidade_sex_adjusted_tabela_1[,1] * letalidade_sex_adjusted_tabela_1[,2])
  colnames(cross_products_sex_ajusted) <- "Cross_products"
  
  letalidade_sex_adjusted_tabela_2 <- cbind(letalidade_sex_adjusted_tabela_1, cross_products_sex_ajusted)
  
  Total_sex <- c(letalidade[1,2], round(sum(letalidade_sex_adjusted_tabela_2$Percentagem_confirmados), digits = 1), sum(letalidade_sex_adjusted_tabela_2$Cross_products))
  
  letalidade_sex_adjusted <- rbind(letalidade_sex_adjusted_tabela_2, Total_sex)
  row.names(letalidade_sex_adjusted)[3] <- "Total"
  
