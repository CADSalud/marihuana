library(ProjectTemplate)
reload.project()

#options("scipen"=999)

head(df.deaths)
head(df.general)
head(df.prision)
head(df.youth)


# Tabla por tema y en algunos casos imputación

# Prision: prevalencia de drogas en prision
t.prision <- df.prision %>% 
  tbl_df() %>%
  filter(drug == 'cannabis',
         !is.na(prop)) %>% 
  group_by(country, drug, period) %>% 
  group_by(subregion, country, year, period) %>% 
  summarise(prop = mean(prop, na.rm = T)/100) %>% 
  spread(period, prop)

set.seed(19871002)
dat.mi <- as.data.frame(t.prision[, 4:6])
missing.df <- missing_data.frame(dat.mi)
show(missing.df)
imps.prision <- mi(missing.df, n.iter = 50, n.chains = 1)
plot(imps.prision)
complete(imps.prision)[,1:3] %>% 
  cbind(t.prision[, -1])

t.prision.imp <- t.prision[, 1:3] %>% 
  cbind(complete(imps.prision)[,1:3]) %>% 
  group_by(country) %>% 
  mutate(max.y=max(year)) %>% 
  ungroup %>% 
  filter(max.y == year) 
cache("t.prision.imp")
  
# Deaths: muertes relacionadas a uso de dorgas
t.deaths <- df.deaths %>% 
  tbl_df() %>% 
  filter(!is.na(rate.mill),
         drug == 'cannabis') %>% 
  select(subregion, country, year, death.rate = rate.mill) %>% 
  group_by(country) %>% 
  mutate(max.y = max(year)) %>% 
  ungroup %>% 
  filter(max.y == year) 

# Youth: consumo de drogas por en jovenes
t.youth <- df.youth %>% 
  tbl_df() %>%
  # quita tranqsed por poca información
  filter(drug != 'tranqsed') %>% 
  group_by(subregion, country, year, drug) %>% 
  summarise(prop = (mean(lifetime, na.rm = T)+.0001)/100  ) %>% 
  # nan's se van
  filter(!is.nan(prop),
         year > 2009) %>% 
  spread(drug, prop) %>% 
  group_by(country) %>% 
  mutate(max.y = max(year)) %>% 
  ungroup %>% 
  # información mas actualizada
  filter(max.y == year, 
  # regiones muy pequeñas que meten ruido
         subregion != 'Caribbean', 
         !str_detect(subregion, "Africa")) 

set.seed(19871002)
t.youth$country %>% unique %>% sort
dat.mi <- as.data.frame(t.youth[, c(4,5,6)])
missing.df <- missing_data.frame(dat.mi)
show(missing.df)
imps.youth <- mi(missing.df, n.iter = 50, n.chains = 1)
quartz();plot(imps.youth)
complete(imps.youth)[,1:3] %>% 
  cbind(t.youth[, -1])

t.youth.imp <- t.youth[, 1:3] %>% 
  cbind(complete(imps.youth)[,1:3])
cache('t.youth.imp')

# General: consumo de drogas por en población en general
t.general <- df.general %>% 
  tbl_df() %>%
  filter(drug != 'tranqsed') %>% 
  group_by(subregion, country, year, drug) %>% 
  summarise(prop = mean(total, na.rm = T)/100)  %>% 
  filter(!is.na(year)) %>% 
  spread(drug, prop) %>% 
  group_by(country) %>% 
  mutate(max.y = max(year)) %>% 
  ungroup %>% 
  filter(max.y == year) 

set.seed(19871002)
dat.mi <- as.data.frame(t.general[, c(4,5,6)])
missing.df <- missing_data.frame(dat.mi)
show(missing.df)
imps.general <- mi(missing.df, n.iter = 50, n.chains = 1)
quartz();plot(imps.general)
complete(imps.general)[,1:3] %>% 
  cbind(t.general[, -1])  

t.general.imp <- t.general[, 1:3] %>% 
  cbind(complete(imps.general)[,1:3])
cache('t.general.imp')

# Union de 4 tablas
dim(t.general.imp)
dim(t.deaths)
dim(t.youth.imp)
dim(t.prision.imp)


# Homologar nombres de paises
t.general.imp %>% select(subregion, country) %>% arrange(subregion, country) %>% write.csv("doc/unodc_countryindex.csv")
t.prision.imp %>% select(subregion, country) %>% arrange(subregion, country) %>% write.csv(row.names = F)
t.youth.imp %>% select(subregion, country) %>% arrange(subregion, country) %>% write.csv(row.names = F)
t.deaths %>% select(subregion, country) %>% arrange(subregion, country) %>% write.csv(row.names = F)


# Nombres de países modificados (id país)
# China y UK se selecciona una region por el mapa
aux.country <- read_csv(file = "doc/unodc_countryindex.csv") %>% 
  gather(var.name, var.val, -1:-2, na.rm = T) %>% 
  filter(!is.na(id.ggmap)) %>% 
  separate(var.name, c('lugar', 'base.nom'), sep = '\\.') %>% 
  spread(lugar, var.val) %>% 
  group_by(id.country) 
table(aux.country$base.nom)


# Tablas homologadas
tab.union <- t.general.imp %>%
  tbl_df() %>% 
  select(subregion, country, general.cannabis = cannabis) %>% 
  inner_join(
    filter(aux.country, base.nom == 'general') %>% 
      select(-base.nom), 
    by = c('subregion', 'country')
  ) %>% 
  select(-subregion, -country) %>% 
  full_join(
    t.deaths %>%
      select(-year, -max.y) %>% 
      inner_join(
        filter(aux.country, base.nom == 'deaths') %>% 
          select(-base.nom), 
        by = c('subregion', 'country')
      ) %>% 
      select(-subregion, -country),
    by = c("id.country", "id.ggmap")
  ) %>% 
  full_join(
    t.youth.imp %>%
      tbl_df() %>% 
      select(subregion, country, youth.cannabis = cannabis) %>% 
      inner_join(
        filter(aux.country, base.nom == 'general') %>% 
          select(-base.nom), 
        by = c('subregion', 'country')
      ) %>% 
      select(-subregion, -country),
    by = c("id.country", "id.ggmap")
  ) %>% 
  full_join(
    t.prision.imp %>%
      mutate(subregion = as.character(subregion),
             country = as.character(country)) %>% 
      select(subregion, country, prision.annual = annual) %>% 
      inner_join(
        filter(aux.country, base.nom == 'prision') %>% 
          select(-base.nom), 
        by = c('subregion', 'country')
      ) %>% 
      select(-subregion, -country),
    by = c("id.country", "id.ggmap")
  )


# Imputacion de tabla unida
dat.mi <- tab.union %>% 
  select(general.cannabis, death.rate, youth.cannabis, prision.annual) %>% 
  mutate(death.rate = log(death.rate +.001)) %>% 
  data.frame()

set.seed(160922)  
missing.df <- missing_data.frame(dat.mi)
show(missing.df)
summary(missing.df)
imps.union <- mi(missing.df, n.iter = 30, n.chains = 1)
quartz();plot(imps.union)
cache('imps.union')

complete(imps.union)[,1:4] %>% 
  mutate(death.rate = exp(death.rate)-.001) %>% 
  cbind(tab.union[, c(2, 1,4, 5, 6) ])  %>% 
  data.frame()

tab.union.imps <- complete(imps.union)[,1:4] %>% 
  mutate(death.rate = exp(death.rate)-.001) %>% 
  cbind(tab.union[, 2:3]) 

cache("tab.union.imps")



# Indice
tab.indice <- tab.union.imps %>% 
  filter(id.country != 135) %>% 
  tbl_df() %>% 
  group_by(id.country) %>% 
  mutate(acum.prom = mean( c(general.cannabis, death.rate, 
                          youth.cannabis, prision.annual)),
         acum.geom = geometric.mean(c(general.cannabis, death.rate, 
                          youth.cannabis, prision.annual))
         )%>% 
  ungroup %>% 
  mutate(indice.prom = 100*acum.prom/mean(acum.prom)) %>% 
  arrange(indice.prom)
  
pca.ind <- princomp(tab.indice %>% select(general.cannabis, youth.cannabis, prision.annual), cor = T)
summary(pca.ind)
ggbiplot(pca.ind)

tab.indice %<>% 
  cbind(pca.ind$scores) %>% 
  tbl_df() %>% 
  group_by(id.ggmap) %>% 
  mutate(comp.pond = sum(Comp.1*.57, Comp.2*.43))
  
qplot(Comp.1, acum.prom, data = tab.indice) + 
  geom_text(aes(label = id.ggmap), check_overlap = F)
cache('tab.indice')

arrange(tab.indice, desc(indice.prom))

# Map world
tab.map <- map_data(map="world") %>% 
  tbl_df() %>% 
  left_join(
    tab.indice, by = c("region"="id.ggmap")
  )
cache('tab.map')
gg <- ggplot() + 
  geom_map(data = tab.map, map = tab.map, 
           aes(map_id=region, x=long, y=lat, fill=indice.prom)) + 
  scale_fill_gradient(guide = "colourbar", 
                      high = '#003366', low = '#99CCFF', 
                      na.value = 'gray90', 
                      name = 'Prevalencia') + 
  coord_equal()  +
  theme_minimal() + 
  theme(axis.text = element_blank())+
  ylab(NULL) + xlab(NULL)
ggsave(filename = "graphs/mapas_unodc/indice_prom.png", gg, width = 9, height = 8)

ggplot() + 
  geom_map(data = tab.map, map = tab.map, 
           aes(map_id=region, x=long, y=lat, fill=prision.annual)) + 
  scale_fill_gradient(guide = "colourbar", 
                      high = '#003366', low = '#99CCFF', 
                      na.value = 'gray90') + 
  coord_equal()  +
  theme_minimal() + 
  theme(axis.text = element_blank())+
  ylab(NULL) + xlab(NULL)
