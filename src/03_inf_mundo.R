library(ProjectTemplate)
load.project()

### Corrección al índice de inseguridad
load('cache/tab.indices.RData')
inseguridadURL <- "https://en.wikipedia.org/wiki/List_of_countries_by_incarceration_rate"
temp.3 <- inseguridadURL %>% 
  read_html %>%
  html_nodes("table")
inseguridad <- as.data.frame(html_table(temp.3[3]))
inseguridad <- inseguridad[2:nrow(inseguridad),]
names(inseguridad) <- c('Country', 'Incarcelation')
#write.csv(inseguridad,file="doc/legalidad/inseguridad.csv")
inseg <- read.csv("doc/legalidad/inseguridad.csv",stringsAsFactors = FALSE)

### Datos procesados con indicadores imputados de UNODC y Wikipedia
tab.indices.2 <- tab.indices %>% 
  left_join(inseg) %>% 
  mutate(correc = ifelse(Country %in% c('China','Egypt','France','Indonesia','Japan','Malaysia',
                                        'Nigeria','Norway','Philippines','Poland','Saudi Arabia',
                                        'Singapore','South Korea','Thailand','Turkey','Ukraine',
                                        'United Arab Emirates','Vietnam'),0.1,
                         ifelse(Country %in% c('Bangladesh','Cambodia', 'Canada','Chile','Colombia',
                                               'Czech Republic','India','Jamaica','Mexico','Portugal',
                                               'Spain','Costa Rica','Uruguay','Germany','Netherlands',
                                               'United States','Australia'),2,1))) %>% 
  filter(!is.na(Incarcelation)) %>% 
  mutate(legalidad2 = legalidad*correc,
         inseguridad = (homicidios/100+Incarcelation/700)/2)
max.afe <- max(tab.indices.2$afectacion)
max.leg <- max(tab.indices.2$legalidad2)
max.ins <- max(tab.indices.2$inseguridad)
tab.indices.corr <- tab.indices.2 %>% 
  mutate(afe = (afectacion/max.afe)*100,
         leg = (legalidad2/max.leg)*100,
         ins = (inseguridad/max.ins)*100)

### Datos procesados para hacer mapas (coordenadas de polígonos)
tab.map <- map_data(map="world") %>% 
  tbl_df() %>% 
  left_join(tab.indices.corr, by = c("region"="id.ggmap"))
  
### Mapa de índice de afectación por consumo de marihuana
gg.afe <- ggplot() + 
  geom_map(data = tab.map, map = tab.map, 
           aes(map_id=region, x=long, y=lat, fill=afe)) + 
  scale_fill_gradient(guide = "colourbar", 
                      high = '#003366', low = '#99CCFF', 
                      na.value = 'gray90', 
                      name = 'Afectación') + 
  coord_equal()  +
  theme_minimal() + 
  theme(axis.text = element_blank())+
  ylab(NULL) + xlab(NULL)
ggsave(filename = "graphs/indice_afectacion.svg", gg.afe, width = 9, height = 8)

### Top 15 países más afectados por el consumo de marihuana (MX 97 de 114)
top.afec <- tab.indices.corr %>% 
  select(id.ggmap,afe) %>% 
  ungroup() %>% 
  arrange(desc(afe)) %>% 
  data.frame()
  head(15)

### Mapa de índice de legalidad y consumo de marihuana
gg.leg <- ggplot() + 
  geom_map(data = tab.map, map = tab.map, 
           aes(map_id=region, x=long, y=lat, fill=leg)) + 
  scale_fill_gradient(guide = "colourbar", 
                      high = '#003366', low = '#99CCFF', 
                      na.value = 'gray90', 
                      name = 'Tolerancia') + 
  coord_equal()  +
  theme_minimal() + 
  theme(axis.text = element_blank())+
  ylab(NULL) + xlab(NULL)
ggsave(filename = "graphs/indice_legalidad.svg", gg.leg, width = 9, height = 8)

### Top 15 países más tolerantes al consumo de marihuana (MX 55 de 110)
top.leg <- tab.indices.corr %>% 
  select(id.ggmap,leg) %>% 
  ungroup() %>% 
  arrange(desc(leg)) %>% 
  head(15)

### Mapa de inseguridad por tasa de homicidios
gg.ins <- ggplot() + 
  geom_map(data = tab.map, map = tab.map, 
           aes(map_id=region, x=long, y=lat, fill=ins)) + 
  scale_fill_gradient(guide = "colourbar", 
                      high = '#003366', low = '#99CCFF', 
                      na.value = 'gray90', 
                      name = 'Inseguridad') + 
  coord_equal()  +
  theme_minimal() + 
  theme(axis.text = element_blank())+
  ylab(NULL) + xlab(NULL)
ggsave(filename = "graphs/indice_homicidios.svg", gg.ins, width = 9, height = 8)

### Top 15 países más inseguros por tasa de homicidios y población en prisión (MX 22 de 110)
top.ins <- tab.indices.corr %>% 
  select(id.ggmap,ins) %>% 
  ungroup() %>% 
  arrange(desc(ins)) %>%
  head(15)

### Cuadrantes de afectación vs tolerancia
vertical <- mean(log(tab.indices.corr$leg))
limite.ver <- sd(log(tab.indices.corr$leg))
horizontal <- mean(log(tab.indices.corr$afe))
limite.hor <- sd(log(tab.indices.corr$afe))
gg.afe.tol <- ggplot(tab.indices.corr, 
       aes(x = log(leg), y = log(afe), label = id.ggmap, color = Region)) +
  geom_vline(xintercept = vertical,color='red') +
  geom_hline(yintercept = horizontal,color='red') +
  geom_point(alpha = 0.5) + 
  geom_text(check_overlap = T, size = 2.5, hjust = -0.2, vjust = 0.5) +
  xlim(vertical-2.5*limite.ver,vertical+2.5*limite.ver) + 
  ylim(horizontal-2.5*limite.hor,horizontal+2.5*limite.hor) + 
  xlab("Tolerancia al consumo de marihuana") + 
  ylab("Afectación por consumo de marihuana") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  theme(legend.position = "none")# + 
  #geom_smooth(se=F, method = 'lm')
ggsave(filename = "graphs/afectacion_tolerancia.svg", gg.afe.tol, width = 9, height = 8)

### Cuadrantes de inseguridad vs tolerancia
vertical <- mean(log(tab.indices.corr$leg))
limite.ver <- sd(log(tab.indices.corr$leg))
horizontal <- mean(log(tab.indices.corr$ins))
limite.hor <- sd(log(tab.indices.corr$ins))
gg.ins.tol <- ggplot(tab.indices.corr, 
                     aes(x = log(leg), y = log(ins), label = id.ggmap, color = Region)) +
  geom_vline(xintercept = vertical,color='red') +
  geom_hline(yintercept = horizontal,color='red') +
  geom_point(alpha = 0.5) + 
  geom_text(check_overlap = T, size = 2.5, hjust = -0.2, vjust = 0.5) +
  xlim(vertical-2.5*limite.ver,vertical+2.5*limite.ver) + 
  ylim(horizontal-2.5*limite.hor,horizontal+2.5*limite.hor) + 
  xlab("Tolerancia al consumo de marihuana") + 
  ylab("Inseguridad por tasa de homicidios y población en prisión") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  theme(legend.position = "none")# + 
  #geom_smooth(se=F, method = 'lm')
ggsave(filename = "graphs/inseguridad_tolerancia.svg", gg.ins.tol, width = 9, height = 8)

### Cuadrantes de inseguridad vs afectación
vertical <- mean(log(tab.indices.corr$afe))
limite.ver <- sd(log(tab.indices.corr$afe))
horizontal <- mean(log(tab.indices.corr$ins))
limite.hor <- sd(log(tab.indices.corr$ins))
gg.ins.afe <- ggplot(tab.indices.corr, 
                     aes(x = log(afe), y = log(ins), label = id.ggmap, color = Region)) +
  geom_vline(xintercept = vertical,color='red') +
  geom_hline(yintercept = horizontal,color='red') +
  geom_point(alpha = 0.5) + 
  geom_text(check_overlap = T, size = 2.5, hjust = -0.2, vjust = 0.5) +
  xlim(vertical-2.5*limite.ver,vertical+2.5*limite.ver) + 
  ylim(horizontal-2.5*limite.hor,horizontal+2.5*limite.hor) + 
  xlab("Afectación por consumo de marihuana") + 
  ylab("Inseguridad por tasa de homicidios y población en prisión") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  theme(legend.position = "none")# +
  #geom_smooth(se=F, method = 'lm')
ggsave(filename = "graphs/inseguridad_afectacion.svg", gg.ins.afe, width = 9, height = 8)
