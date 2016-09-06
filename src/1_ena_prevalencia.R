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
  select(folio, pond, 
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
gg <- ggplot(gg.tab.tot, 
             aes( x= fct_reorder(droga, porc ), y = porc, 
                    fill = periodo)) + 
  geom_bar(stat = 'identity') + 
  facet_wrap(~periodo, scales = 'free_y') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggsave("graphs/ena_inctot.png", gg, width = 8, height = 4)



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

gg <- ggplot(gg.tab, aes(x = sexo_h, y = porc, 
                         group = periodo, fill = periodo))  + 
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_wrap(~ droga, scales = 'free_y', nrow = 2) +
  scale_y_continuous(labels = function(x)round(100*x,1)) + 
  ylab('Incidencia (%)') + 
  xlab('Género')
ggsave("graphs/ena_incgenero.png", gg, width = 12, height = 5)


# edad cut
gg.tab <- tab %>% 
  mutate(edad.cut = cut_width(edad, 10)) %>% 
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
  mutate(edad.cut = fct_relevel(edad.cut, "[5,15]"),
         droga = fct_reorder(droga, porc))

gg <- ggplot(gg.tab, aes(x = edad.cut, y = porc, 
                   group = periodo, color = periodo))  + 
  geom_point(alpha = .1) +
  geom_smooth(span = 1.8, se = F) + 
  facet_wrap(~ droga, scales = 'free_y') + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_y_continuous(labels = function(x)round(100*x,1)) + 
  ylab('Incidencia (%)') + 
  xlab('Edad')
ggsave("graphs/ena_incedad.png", gg, width = 10, height = 7)


GGBiplotEdad <- function(per.selec){
  ggmod <- gg.tab %>% 
    filter(periodo == per.selec & 
             !droga %in% c('otras', 'tabaco', 'alcohol'))
  
  mod.ln <- lm(log(porc + .001) ~ droga * edad.cut,  
               data = ggmod)
  
  tab.fin <- ggmod %>%  
    add_predictions(mod.ln, "preds") %>% 
    mutate(predst = exp(preds) ) %>% 
    select(droga, edad.cut, predst) %>% 
    spread(edad.cut, predst)
  pc <- princomp(Perfiles(as.matrix(tab.fin[, -1]), 'r' ), cor = T)
  gg <- ggbiplot(pc, scale = .3, labels = tab.fin$droga, 
                 labels.size = 4)+ 
    theme(axis.text = element_blank()) +
    ggtitle(per.selec) 
  ggsave( paste0("graphs/ena_incbipedad_",per.selec, ".png"), 
          gg, width = 6, height = 6)
}

GGBiplotEdad(per.selec = 'ult30')
GGBiplotEdad(per.selec = 'ult12')
GGBiplotEdad(per.selec = 'algvez')


# ingreso
gg.tab <- tab %>% 
  group_by(ingreso.cod) %>% 
  do(IncPond(sub = .))  %>% 
  filter(var.num == 1) %>%
  select(-var.num, -frec.pond) %>% 
  spread(ingreso.cod, porc, fill = 0) %>% 
  separate(var.lab, c("periodo", "droga"), sep = "_", 
           remove = F) %>% 
  gather(ingreso.cod, porc, -var.lab, -periodo, -droga) %>% 
  filter(droga != 'otras') %>% 
  mutate(droga = fct_reorder(droga, porc))

gg <- ggplot(gg.tab, aes(x = ingreso.cod, y = porc, 
                         group = periodo, color = periodo))  + 
  geom_point(alpha = .1) +
  geom_smooth(span = 1.3, se = F) + 
  facet_wrap(~ droga, scales = 'free_y') +
  scale_y_continuous(labels = function(x)round(100*x,1)) + 
  ylab('Incidencia (%)') + 
  xlab('Nivel de Ingreso')
ggsave("graphs/ena_incingreso.png", gg, width = 10, height = 8)


#  modelos de ingresos para hacer biplots bonitos
# gg.tab$porc <- 100*gg.tab$porc
GGBiplotIng <- function(per.selec){
  ggmod <- gg.tab %>% 
    filter(periodo == per.selec & 
             !droga %in% c('otras', 'tabaco', 'alcohol')) %>% 
    mutate(ingreso.cod = as.numeric(ingreso.cod))
  
  mod.ln <- lm(log(porc + .01) ~ droga*ns(ingreso.cod,2),  
               data = ggmod)
  
  tab.fin <- ggmod %>%  
    add_predictions(mod.ln, "preds") %>% 
    mutate(predst = exp(preds)-.01) %>% 
    select(droga, ingreso.cod, predst) %>% 
    spread(droga, predst)
  pc <- princomp(Perfiles(as.matrix(tab.fin[, -1]), 'c' ), cor = T)
  gg <- ggbiplot(pc, scale = .4, labels = 1:8, 
                 labels.size = 4)+ 
    theme(axis.text = element_blank()) +
    ggtitle(per.selec) 
  ggsave( paste0("graphs/ena_incbiping_",per.selec, ".png"), 
         gg, width = 6, height = 6)

}
GGBiplotIng(per.selec = 'ult30')
GGBiplotIng(per.selec = 'ult12')
GGBiplotIng(per.selec = 'algvez')



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








