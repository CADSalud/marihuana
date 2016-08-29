# library(ProjectTemplate)
# reload.project()

head(df.general)
tab.g <- df.general %>% 
  filter(!is.na(total)) %>% 
  dplyr::select(country, total, year, source, drug) %>% 
  group_by(country, drug) %>% 
  mutate(maxm = max(year)) %>% 
  filter(year == maxm) %>% 
  group_by(country, year, drug) %>% 
  dplyr::summarise(total.prom = mean(total)) %>% 
  ungroup() %>% 
  dplyr::mutate(
    drug.nom = recode(drug, 
        "'opioids'='opioides';'tranqsed'='tranquilizantes'"),
    periodo.nom = recode(periodo, 
        "'annual'='ultimo año';'lifetime'='alguna vez';'x30.day'='último mes'")
  )
head(tab.g)
tab.g %>% filter(country == 'United States of America')


head(df.youth)
tab.y <- df.youth %>% 
  dplyr::select(country, lifetime:x30.day, year, source, drug) %>% 
  gather(periodo, total, lifetime:x30.day, na.rm = T) %>% 
  group_by(country, drug) %>% 
  dplyr::mutate(maxm = max(year)) %>% 
  filter(year == maxm) %>% 
  group_by(country, year, drug, periodo) %>% 
  dplyr::summarise(total.prom = mean(total)) %>% 
  ungroup() %>% 
  dplyr::mutate(
    drug.nom = recode(drug, 
        "'opioids'='opioides';'tranqsed'='tranquilizantes'"),
    periodo.nom = recode(periodo, 
        "'annual'='ultimo año';'lifetime'='alguna vez';'x30.day'='último mes'")
  )
head(tab.y)
tab.y %>% filter(country == 'Mexico')


# • Mapa
# map.world$name_len <- nchar(map.world$region) + sample(nrow(map.world))
map.world <- map_data(map="world")

# union de países
setdiff(union(unique(tab.y$country), unique(tab$country)),
  unique(map.world$region)) %>%  
  as.data.frame() #%>% 
  # write_csv('doc/recode_regiones.csv')
sort(setdiff(unique(map.world$region), 
        union(unique(tab.y$country), unique(tab$country))
        ))

# Proporción de consumo (mapas)
# tab.sub <- filter(tab.y, drug == 'cannabis' & periodo == 'annual')
rec.reg <- read_csv("doc/recode_regiones.csv")

GGmap <- function(tab.sub, tipoedad){
  tab.map <- tab.sub %>% 
    full_join(rec.reg, by = 'country' ) %>% 
    mutate(region = ifelse(is.na(region), country, region)) %>% 
    full_join(map.world, by = 'region') 
  
  gg <- ggplot() + 
    geom_map(data = tab.map, map = tab.map, 
             aes(map_id=region, x=long, y=lat, fill=total.prom)) + 
    scale_fill_gradient(guide = "colourbar", 
                        high = '#003366', low = '#99CCFF', 
                        na.value = 'gray90', 
                        name = 'Prevalencia') + 
    coord_equal()  +
    ggtitle(paste( unique(tab.map$drug.nom)[1],
                   unique(tab.map$periodo.nom)[1],
                   tipoedad, sep = "\n")
    )+ 
    theme_minimal() + 
    theme(axis.text = element_blank())+
    ylab(NULL) + xlab(NULL)
  
  ggsave(filename = paste0("graphs/mapas/mapa_", 
                          unique(tab.map$drug)[1],
                          "_", 
                          unique(tab.map$periodo)[1], 
                          "_",
                          tipoedad,
                          ".png"), 
         plot = gg, width = 8.5, height = 4) 
  "fin"
}


# jovenes
ddply(tab.y, .(drug, periodo), 
      function(sub){GGmap(sub, "adolescentes")}, 
      .progress = 'text')
