library(ProjectTemplate)
load.project()

legalityofcannabisURL <- "https://en.wikipedia.org/wiki/Legality_of_cannabis_by_country"

temp <- legalityofcannabisURL %>% 
  read_html %>%
  html_nodes("table")

legality <- as.data.frame(html_table(temp[2])) %>% 
  mutate(country = trim(gsub("\\[edit\\]", "", Country.Territory ))) %>% 
  filter(country != "Country/Territory") %>% 
  mutate(poss = ifelse(Possession == "Illegal",1,
                      ifelse(rownames(legalidad) %in% grep("Legal",Possession),3,2)),
         sale = ifelse(Sale == "Illegal",1,
                      ifelse(rownames(legalidad) %in% grep("Legal",Sale),3,2)),
         tran = ifelse(Transport == "Illegal",1,
                      ifelse(rownames(legalidad) %in% grep("Legal",Transport),3,2)),
         cult = ifelse(Cultivation == "Illegal",1,
                      ifelse(rownames(legalidad) %in% grep("Legal",Cultivation),3,2))) %>% 
  rowwise() %>% 
  mutate(ind.leg = sum(poss,sale,tran,cult)/4) %>% 
  dplyr::select(country,poss,sale,tran,cult,ind.leg) %>% 
  ungroup() %>% 
  arrange(desc(ind.leg))

### Categorizar niveles de legalidad
# pos <- legality %>% group_by(Possession) %>% tally
# sal <- legality %>% group_by(Sale) %>% tally
# tra <- legality %>% group_by(Transport) %>% tally
# cul <- legality %>% group_by(Cultivation) %>% tally
# write.csv(pos,file="data/legalidad/pos.csv")
# write.csv(sal,file="data/legalidad/sal.csv")
# write.csv(tra,file="data/legalidad/tra.csv")
# write.csv(cul,file="data/legalidad/cul.csv")

write.csv(legality,file="data/legalidad/legality.csv")

claves <- map_data(map="world") %>% 
  tbl_df() %>% 
  group_by(region) %>% 
  summarise(coords = n())
write.csv(claves,file="data/legalidad/claves.csv")

### Se carga legality.csv con las claves de id.ggmap
legalidad <- read.csv("data/legalidad/legality.csv",stringsAsFactors = FALSE)

# Map world
tab.map <- map_data(map="world") %>% 
  tbl_df() %>% 
  left_join(
    legalidad, by = c("region"="id.ggmap")
  )
gg <- ggplot() + 
  geom_map(data = tab.map, map = tab.map, 
           aes(map_id=region, x=long, y=lat, fill=ind.leg)) + 
  scale_fill_gradient(guide = "colourbar", 
                      high = '#003366', low = '#99CCFF', 
                      na.value = 'gray90', 
                      name = 'Legalidad') + 
  coord_equal()  +
  theme_minimal() + 
  theme(axis.text = element_blank())+
  ylab(NULL) + xlab(NULL)
ggsave(filename = "graphs/mapas_unodc/indice_prom.png", gg, width = 9, height = 8)


