library(ProjectTemplate)
load.project()

# ≈ Menu ≈
# • 1. Incidencia


print(demos, n = 3, width = Inf)
print(hogar, n = 3, width = Inf)
print(percep, n = 3, width = Inf)
print(percep.legal, n = 3, width = Inf)
print(riesgo, n = 3, width = Inf)
print(tolerancia, n = 3, width = Inf)
print(usos, n = 3, width = Inf)
print(venta, n = 3, width = Inf)



# • 1. Incidencia
print(usos, n = 3, width = Inf)

tab <- usos %>% 
  unite(folio.u, c(folio, pond), remove = F) %>% 
  select(folio, folio.u, pond, 
         starts_with("algvez"), 
         starts_with("ult30"), 
         starts_with("ult12") ) %>% 
  gather(var.lab, var.num, algvez_tabaco:ult12_otras) %>% 
  separate(var.lab, c("periodo", "droga"), sep = "_", 
           remove = F)  %>% 
  mutate(var.num = ifelse(var.num %in% c('Sí', "si"), 1, 0 ),
         var.nump = var.num*pond) %>% 
  left_join( select(demos, folio, 
                    edad:escolar, edad.cut, 
                    ingreso.cod)
             )
head(tab)



# Base de uso mariguana
prevdrogas <- tab %>% 
  select(folio, pond, periodo, droga, var.num) %>% 
  filter( periodo == 'algvez') %>% 
  unique() %>% 
  spread(droga, var.num) %>% 
  mutate(drogas.ilegales = aluciógenos + anfetamina + 
           cocaína + crack + heroína + inhalables + 
           mariguana + otras) %>% 
  select(folio, pond, 
         alcohol, mariguana, drogas.ilegales) %>% 
  left_join(
    usos %>% 
      select(folio, pond, edadinic_mariguana, edadinic_alcohol,
             arrestos_drogas, 
             adiccion_drogas, dependencia_drogas, 
             accidentes_drogas,
             tratamiento_drogas, tratamiento_alcohol)
  )
dim(prevdrogas) # 16249
cache('prevdrogas')
  

# función incidencias
IncPond <- function(sub){
  tab <- sub %>% 
    # filter(!is.na(var.num)) %>%
    group_by(var.lab, var.num) %>% 
    dplyr::summarise(frec.pond = sum(pond)) %>% 
    group_by(var.lab) %>% 
    mutate( porc = frec.pond/sum(frec.pond)) 
}
  

# total
gg.tab.tot <- IncPond(tab) %>% 
  separate(var.lab, c("periodo", "droga"), sep = "_", 
           remove = F) %>% 
  filter(var.num == 1 & droga != 'otras')
gg <- ggplot(filter(gg.tab.tot, periodo == 'algvez'), 
             aes( x= fct_reorder(droga, porc ), y = porc)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  ylab('Prevalencia (%)\nAlguna Vez')  + 
  xlab(NULL) + 
  scale_y_continuous(labels = function(x)round(100*x))
ggsave("graphs/ena_inctot.png", gg, width = 4, height = 4)

gg.tab.tot %>% 
  ungroup %>% 
  select(periodo, droga, porc) %>% 
  spread(periodo, porc) %>% 
  write.table(row.names = F, sep = ",")

# genero
gg.tab <- tab %>% 
  group_by(sexo_h) %>% 
  do(IncPond(sub = .))  %>% 
  filter(var.num == 1) %>%
  select(-var.num, -frec.pond) %>% 
  spread(sexo_h, porc, fill = 0) %>% 
  separate(var.lab, c("periodo", "droga"), sep = "_", 
           remove = F) %>% 
  gather(sexo_h, porc, -var.lab, -periodo, -droga) %>% 
  filter(droga != 'otras') %>% 
  mutate(droga = fct_reorder(droga, porc))

gg <- ggplot( filter(gg.tab, 
                     droga %in% c('mariguana', 'alcohol'),
                     periodo == 'algvez'), 
             aes(x = droga, y = porc, 
                 fill = sexo_h))  + 
  geom_bar(stat = 'identity', position = 'dodge') +
  ylab('Prevalencia (%)\nAlguna Vez')  + 
  xlab(NULL) + 
  scale_y_continuous(labels = function(x)round(100*x)) + 
  guides(fill = guide_legend(title = "Sexo" ))
ggsave("graphs/ena_incgenero.png", gg, width = 5, height = 4)


# edad cut
gg.tab <- tab %>% 
  group_by(edad.cut) %>% 
  do(IncPond(sub = .))  %>% 
  filter(var.num == 1) %>%
  select(-var.num, -frec.pond) %>% 
  spread(edad.cut, porc, fill = 0) %>% 
  separate(var.lab, c("periodo", "droga"), sep = "_", 
           remove = F) %>% 
  gather(edad.cut, porc, -var.lab, -periodo, -droga) %>% 
  left_join(
    select(gg.tab.tot, porc.tot = porc, var.lab)
  ) %>% 
  filter(droga != 'otras') %>% 
  mutate(edad.cut = fct_relevel(edad.cut, "[12,18]"),
         droga = fct_reorder(droga, porc))

gg <- ggplot( filter(gg.tab, 
                     droga %in% c('mariguana', 'alcohol')),
             aes(x = edad.cut, y = porc, 
                 group = periodo, color = periodo))  + 
  geom_smooth(span = 1.8, se = F) + 
  facet_wrap(~ droga, scales = 'free_y') + 
  theme(axis.text.x = element_text(angle = 90), 
        legend.position = 'bottom') +
  ylab('Prevalencia (%)')  + 
  xlab(NULL) + 
  scale_y_continuous(labels = function(x)round(100*x)) 
  
ggsave("graphs/ena_incedad.png", gg, width = 9, height = 5)


GGBiplotEdad <- function(per.selec){
  ggmod <- gg.tab %>% 
    filter(periodo == per.selec & 
             !droga %in% c('otras', 'tabaco', 'alcohol'))
  
  mod.ln <- lm(log(porc + .001) ~ droga * edad.cut,  
               data = ggmod)
  
  tab.fin <- ggmod %>%  
    add_predictions(mod.ln, "preds") %>% 
    mutate(predst = exp(preds) ) %>% 
    group_by(edad.cut) %>% 
    mutate(perf = predst/mean(predst)) %>% 
    group_by(droga) %>% 
    mutate(perfd = 100*perf/mean(perf)) %>% 
    select(droga, edad.cut, perfd)  %>% 
    spread(droga, perfd)
  pc <- princomp(as.matrix(tab.fin[, -1]), cor = T)
  gg <- ggbiplot(pc, scale = .3, labels = tab.fin$edad.cut, 
                 labels.size = 4)+ 
    theme(axis.text = element_blank()) +
    ggtitle(per.selec) 
  ggsave( paste0("graphs/ena_incbipedad_",per.selec, ".png"), 
          gg, width = 6, height = 6)
}

GGBiplotEdad(per.selec = 'algvez')


# ingreso
tab %>% 
  group_by(ingreso.cod) %>% 
  summarise( frec.pond =  sum(pond)) %>% 
  mutate( porc = frec.pond/sum(frec.pond)) 

gg.tab <- tab %>% 
  group_by(ingreso.cod) %>% 
  do(IncPond(sub = .))  %>% 
  filter(var.num == 1, 
         ingreso.cod != 9) %>%
  select(-var.num, -frec.pond) %>% 
  spread(ingreso.cod, porc, fill = 0) %>% 
  separate(var.lab, c("periodo", "droga"), sep = "_", 
           remove = F) %>% 
  gather(ingreso.cod, porc, -var.lab, -periodo, -droga) %>% 
  filter(droga != 'otras') %>% 
  mutate(droga = fct_reorder(droga, porc))

gg <- ggplot( filter(gg.tab, 
                     droga %in% c('mariguana', 'alcohol')),
             aes(x = ingreso.cod, y = porc, 
                 group = periodo, color = periodo))  + 
  geom_smooth(span = 1.3, se = F) + 
  facet_wrap(~ droga, scales = 'free_y') + 
  theme(legend.position = 'bottom') +
  ylab('Prevalencia (%)')  + 
  xlab(NULL) + 
  scale_y_continuous(labels = function(x)round(100*x)) 
ggsave("graphs/ena_incingreso.png", gg, width = 9, height = 5)





# entidad
gg.tab <- tab %>% 
  left_join( select(hogar, folio, ent) ) %>% 
  group_by(ent) %>% 
  do(IncPond(sub = .))  %>% 
  filter(var.num == 1) %>%
  select(-var.num, -frec.pond) %>% 
  spread(ent, porc, fill = 0) %>% 
  separate(var.lab, c("periodo", "droga"), sep = "_", 
           remove = F) %>% 
  gather(ent, porc, -var.lab, -periodo, -droga) %>% 
  filter(droga != 'otras') %>% 
  mutate(droga = fct_reorder(droga, porc), 
         ent = str_trim(ent)) 

df.edos <- hogar %>% 
  select(ent, desc_ent) %>% 
  unique %>% 
  mutate(idorig = as.numeric(as.character(ent)),
         desc_ent = str_sub(str_trim(desc_ent), 4),
         ent = str_trim(ent),
         id = car::recode(idorig, "5 = 7; 6 = 8; 7 = 5; 8 = 6")
                     ) 
library(maptools)
edo <- readShapeSpatial("data/mex_edos/Mex_Edos")
edo@data$id <- rownames(edo@data)
edo_df<- edo %>%
  fortify() %>%
  mutate(id=as.numeric(id)+1)

# periodo.selec <- 'ult12'
GGMapPrev <- function(periodo.selec, tabgral){
  tt <- tabgral %>% 
    filter(periodo == periodo.selec) %>% 
    left_join(df.edos)
  # print(arrange(tt, desc(porc)) %>% data.frame())
  
  edo_subdiag <- left_join(edo_df, tt, by = 'id') 
  
  gg <- ggplot(edo_subdiag, 
               aes(x = long, y = lat, group=group)) + 
    geom_polygon( aes(fill = porc, group = group),
                  color='black', size = .4) + 
    coord_fixed() +
    theme(axis.text = element_blank(), 
          axis.title = element_blank(), 
          axis.ticks = element_blank()
    ) + 
    scale_fill_continuous(low = 'white', high = '#132B43') + 
    guides(fill = guide_legend(title = "Prevalencia")) +
    theme(legend.position = 'bottom') + 
    ggtitle(periodo.selec)
  ggsave( paste0("graphs/ena_incent_", 
                 unique(tt$droga),
                 "_", 
                 periodo.selec, ".png"), 
          gg, width = 7, height = 6)
  "fin"
}

GGMapPrev('ult30', filter(gg.tab, droga == 'mariguana'))
GGMapPrev('ult12', filter(gg.tab, droga == 'mariguana'))
GGMapPrev('algvez', filter(gg.tab, droga == 'mariguana'))
GGMapPrev('ult30', filter(gg.tab, droga == 'alcohol'))
GGMapPrev('ult12', filter(gg.tab, droga == 'alcohol'))
GGMapPrev('algvez', filter(gg.tab, droga == 'alcohol'))
GGMapPrev('ult30', filter(gg.tab, droga == 'inhalables'))
GGMapPrev('ult12', filter(gg.tab, droga == 'inhalables'))
GGMapPrev('algvez', filter(gg.tab, droga == 'inhalables'))



