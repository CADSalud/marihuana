library(ProjectTemplate)
load.project()

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
    group_by(var.lab, var.num, add = T) %>% 
    summarise(frec.pond = sum(pond)) %>% 
    group_by(var.lab, add = T) %>% 
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
ggsave("graphs/ena_inctot.png", gg, width = 8, height = 5)


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
  mutate(edad.cut = fct_relevel(edad.cut, "[9,15]"),
         droga = fct_reorder(droga, porc))

gg <- ggplot(gg.tab, aes(x = edad.cut, y = porc, 
                   group = periodo, color = periodo))  + 
  geom_point(alpha = .1) +
  # geom_smooth(span = 3, se = F) + 
  geom_smooth(span = 3, se = F) + 
  facet_wrap(~ droga, scales = 'free_y') + 
  theme(axis.text.x = element_text(angle = 90)) + 
  scale_y_continuous(labels = function(x)round(100*x,1)) + 
  ylab('Incidencia (%)') + 
  xlab('Edad')
ggsave("graphs/ena_incedad.png", gg, width = 10, height = 7)

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


gg.tab$porc <- 100*gg.tab$porc

GGBiplotInc <- function(per.selec){
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
  ggsave( paste0("graphs/ena_incbip_",per.selec, ".png"), 
         gg, width = 6, height = 6)

}
GGBiplotInc(per.selec = 'ult30')
GGBiplotInc(per.selec = 'ult12')
GGBiplotInc(per.selec = 'algvez')



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
  mutate(droga = fct_reorder(droga, porc))







## • Intento de modelo
# mod <- glm(formula = var.num~droga*periodo + droga*edad.cut,
#            data = tab, family = 'binomial')
# mod$family
# summary(mod)
# dfnew <- expand.grid(
#     droga = unique(tab$droga)[-10],
#     periodo = unique(tab$periodo),
#     edad.cut = levels(tab$edad.cut)
#   ) %>% 
#   mutate(prob = predict(mod, newdata = ., type = 'response'))
# 
# ggplot(dfnew, aes(x = edad.cut, y = prob, 
#                    group = periodo, color = periodo))  + 
#   geom_point() +
#   geom_smooth(span = 3, se = F) +
#   facet_wrap(~droga, scales = 'free_y') + 
#   theme(axis.text.x = element_text(angle = 90))
