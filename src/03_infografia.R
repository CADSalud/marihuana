library(ProjectTemplate)
load.project()

### Corrección al índice de inseguridad
load('cache/tab.indices.corr.RData')
load('cache/tab.map.RData')

### Mapa de índice de afectación por consumo de marihuana
gg.afe <- ggplot() + 
  geom_map(data = tab.map, map = tab.map, 
           aes(map_id=region, x=long, y=lat, fill=log(afe))) + 
  scale_fill_gradient(guide = "colourbar", 
                      high = '#003366', low = '#99CCFF', 
                      na.value = 'gray90', 
                      name = 'Afectación',
                      labels = c("-", " ", "+"),
                      breaks = c(-1,1.5,4)) + 
  coord_equal()  +
  theme_minimal() + 
  theme(axis.text = element_blank())+
  ylab(NULL) + xlab(NULL)
ggsave(filename = "graphs/mapas/indice_afectacion.svg", gg.afe, width = 9, height = 8)
ggsave(filename = "graphs/mapas/indice_afectacion.png", gg.afe, width = 9, height = 8)

### Top 15 países más afectados por el consumo de marihuana (MX 97 de 110)
top.afec <- tab.indices.corr %>% 
  select(id.ggmap,afe) %>% 
  ungroup() %>% 
  arrange(desc(afe)) %>% 
  data.frame() %>% 
head(15)


### Mapa de índice de intolerancia por consumo de marihuana
gg.int <- ggplot() + 
  geom_map(data = tab.map, map = tab.map, 
           aes(map_id=region, x=long, y=lat, fill=-log(leg))) + 
  scale_fill_gradient(guide = "colourbar", 
                      high = '#003366', low = '#99CCFF', 
                      na.value = 'gray90', 
                      name = 'Intolerancia',
                      labels = c("-", " ", "+"),
                      breaks = c(-4,-1,2)) +  
  coord_equal()  +
  theme_minimal() + 
  theme(axis.text = element_blank())+
  ylab(NULL) + xlab(NULL)
ggsave(filename = "graphs/mapas/indice_intolerancia.svg", gg.int, width = 9, height = 8)
ggsave(filename = "graphs/mapas/indice_intolerancia.png", gg.int, width = 9, height = 8)

### Top 15 países más intolerantes por el consumo de marihuana (MX 56 de 110)
top.int <- tab.indices.corr %>% 
  select(id.ggmap,leg) %>% 
  ungroup() %>% 
  arrange(leg) %>% 
  data.frame() %>% 
  head(15)

### Mapa de índice de inseguridad por homicidios
gg.ins <- ggplot() + 
  geom_map(data = tab.map, map = tab.map, 
           aes(map_id=region, x=long, y=lat, fill=log(Rate))) + 
  scale_fill_gradient(guide = "colourbar", 
                      high = '#003366', low = '#99CCFF', 
                      na.value = 'gray90', 
                      name = 'Violencia',
                      labels = c("-", " ", "+"),
                      breaks = c(-0.5,2.5,3.5)) +   
  coord_equal()  +
  theme_minimal() + 
  theme(axis.text = element_blank())+
  ylab(NULL) + xlab(NULL)
ggsave(filename = "graphs/mapas/indice_inseguridad.svg", gg.ins, width = 9, height = 8)
ggsave(filename = "graphs/mapas/indice_inseguridad.png", gg.ins, width = 9, height = 8)

### Top 15 países más inseguros por homicidios (MX 56 de 110)
top.ins <- tab.indices.corr %>% 
  select(id.ggmap,Rate) %>% 
  ungroup() %>% 
  arrange(desc(Rate)) %>% 
  data.frame() %>% 
  head(15)

### Generación de óndices que resumen información de 3 variables
tab.segm <- tab.indices.corr %>% 
  ungroup() %>% 
  mutate(afe.cl = log(afe),
         int.cl = -log(leg),
         ins.cl = log(Rate)) %>% 
  dplyr::select(afe.cl, int.cl, ins.cl)

pca.cuad <- princomp(tab.segm, cor = T)
summary(pca.cuad)
ggbiplot(pca.cuad)
componentes <- data.frame(pca.cuad$scores)
names(componentes) <- c('ind.1','ind.2','ind.3')

### Cluster jerárquico para agrupar países
segm <- hclust(dist(tab.segm))
plot(segm)
segm.cut <- cutree(segm,4)
table(segm.cut)

tab.cuadr <- tab.indices.corr %>% 
  cbind(segm.cut,componentes) %>% 
  dplyr::select(ind.1, ind.2, segm.cut, id.ggmap)

gg.cuadr <- ggplot(tab.cuadr, 
                     aes(x = ind.1, y = ind.2, label = id.ggmap, 
                         color = as.factor(segm.cut))) +
  geom_point(alpha = 0.5) + 
  xlim(-3.5,3.5) +
  ylim(-2.3,2.3) +
  geom_text(check_overlap = T, size = 2.5, hjust = -0.2, vjust = 0.5) +
  geom_vline(xintercept = 0,color='black') +
  geom_hline(yintercept = 0,color='black') +
  xlab(" ") + 
  ylab(" ") + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + 
  theme(legend.position = "none") + 
  annotate("text", x = -3.1, y = 2.0, size = 5,
               label = "Países muy violentos \n No afectados por el consumo \n Intolerantes a la legalización") + 
  annotate("text", x = 3.1, y = 2.0, size = 5,
           label = "Países muy violentos \n Afectados por el consumo \n Tolerantes a la legalización") + 
  annotate("text", x = -3.1, y = -2.0, size = 5,
           label = "Países poco violentos \n No afectados por el consumo \n Intolerantes a la legalización") + 
  annotate("text", x = 3.1, y = -2.0, size = 5,
           label = "Países poco violentos \n Afectados por el consumo \n Tolerantes a la legalización")
ggsave(filename = "graphs/mapas/cuadrantes.svg", gg.cuadr, width = 14, height = 8)
ggsave(filename = "graphs/mapas/cuadrantes.png", gg.cuadr, width = 14, height = 8)

tab.grupos <- tab.indices.corr %>% 
  ungroup() %>% 
  mutate(afe.cl = log(afe)/max(log(afe)),
         int.cl = -log(leg)/max(log(leg)),
         ins.cl = log(homicidios)/max(log(homicidios))) %>% 
  cbind(segm.cut,componentes)

prom.grupos <- tab.grupos %>% 
  group_by(segm.cut) %>% 
  summarise(p.afe = mean(afe.cl), p.int = mean(int.cl), p.ins = mean(ins.cl))