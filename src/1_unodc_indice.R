library(ProjectTemplate)
reload.project()

options("scipen"=999)

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
    by = "id.country"
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
    by = "id.country"
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
    by = "id.country"
  )


# Imputacion de tabla unida
dat.mi <- tab.union %>% 
  select(general.cannabis, death.rate, youth.cannabis, prision.annual) %>% 
  data.frame()
missing.df <- missing_data.frame(dat.mi)
show(missing.df)
summary(missing.df)
imps.union <- mi(missing.df, n.iter = 30, n.chains = 1)
plot(imps.union)

tab.union.imps <- exp(complete(imputations)$`chain:2`[, 1:4]-.0001) %>% 
  cbind(tab.index[, 1:2]) %>% 
  filter(id.country != 43) %>% 
  group_by(id.country) %>% 
  mutate(num = mean(c(cann.general, cann.youth, death.rate, prision.cons))) %>% 
  ungroup %>% 
  mutate(den = mean(num), 
         indice = 100*num/den)


pca.df <- data.frame(tab.preds)
pca.ind <- princomp(pca.df %>% select(cann.general:prision.cons))
summary(pca.ind)


tab.prcomp <- tab.preds %>% 
  cbind(pca.ind$scores)

qplot(indice, Comp.1, data = tab.prcomp) + 
  geom_text(aes(label = country), check_overlap = T)

# Map world
tab.map <- map_data(map="world") %>% 
  left_join(
    read_csv(file = "doc/ena_atrcountrygg_index.csv") %>% 
      select(id.country, region = id.plot) %>% 
      na.omit()
  ) %>% 
  left_join(tab.prcomp)

ggplot() + 
  geom_map(data = tab.map, map = tab.map, 
           aes(map_id=region, x=long, y=lat, fill=Comp.1*-1)) + 
  scale_fill_gradient(guide = "colourbar", 
                      high = '#003366', low = '#99CCFF', 
                      na.value = 'gray90', 
                      name = 'Prevalencia') + 
  coord_equal()  +
  theme_minimal() + 
  theme(axis.text = element_blank())+
  ylab(NULL) + xlab(NULL)
