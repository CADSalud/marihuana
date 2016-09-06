library(ProjectTemplate)
load.project()

#  ultimas preguntas
individ %>% 
  dplyr::select(folio, ccp40, ccp57, ccp61, ccp67, ccp69, ccp72, 
                ccp103, ccp106) %>% 
  gather(var.nom, var.val, ccp40:ccp106) %>% 
  group_by(var.nom, var.val) %>% 
  tally %>%
  group_by(var.nom) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(var.val == 'Sí') %>% data.frame()


# Algunas frecuencias

TabPond <- function(sub){
  sub %>%
    group_by(columna.nom, var.val) %>% 
    summarise(frec = sum(pond),
              frec.raw = n()) %>% 
    group_by(columna.nom) %>% 
    mutate(porc = frec/sum(frec))
}



# Tolerancia y percepcion de riesgo
tab <- tolerancia %>%
  filter(var.val != 'No aplica') %>% 
  do(TabPond(.))
ggplot(tab, aes(x = fct_reorder(columna.nom, porc), 
                y = porc, 
                fill = fct_reorder(var.val, porc), 
                group = var.val)) +
  geom_bar(stat = 'identity')+
  coord_flip() 
  

tab <- tolerancia %>% 
  left_join(demos) %>% 
  mutate(var.val = fct_collapse(facil_drogas, 
                fácil = c('Fácil', 'Muy fácil'),
                difícil = c('Difícil', 'Muy difícil'),
                imposible = 'Imposible',
                ns = "No Sabe"
          ),
         columna.nom = 'facil_drogas') %>% 
  group_by(edad) %>% 
  do(TabPond(.))
ggplot(tab, aes(x = (edad), y = porc, 
                color = fct_reorder(var.val, porc), 
                group= var.val))+ 
  geom_point()+ 
  geom_smooth()


tab <- riesgo %>%
  do(TabPond(.)) %>% 
  group_by(columna.nom) %>% 
  mutate(se = sqrt(porc*(1-porc)/sum(frec.raw)) )
ggplot(tab, aes(x = fct_reorder(columna.nom, porc), 
                y = porc, 
                fill = fct_reorder(var.val, porc), 
                group = var.val)) +
  geom_bar(stat = 'identity')+
  coord_flip() 
ggplot(filter(tab, var.val == 'Es muy peligroso'),
       aes(x = porc, 
           y = fct_reorder(columna.nom, porc))) +
  geom_errorbarh(aes(xmax = porc + 2*se, 
                     xmin = porc - 2* se), 
                 height = .5) +
  geom_point(color = 'red') 
ggplot(tab,
       aes(x = porc, 
           y = fct_reorder(columna.nom, porc))) +
  geom_errorbarh(aes(xmax = porc + 2*se, 
                     xmin = porc - 2* se), 
                 height = .3) +
  geom_point(color = 'red') + 
  facet_wrap(~var.val, scales = 'free_x')
