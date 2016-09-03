library(ProjectTemplate)
load.project()



# • EXPLORANDO VARIABLES
# Durante toda su vida, ¿cuántas veces ha tenido oportunidades para usar cualquier droga?
summary(individ$a054d2) 

#  que tan facil es conseguir drogas
table(individ$a054d3) %>% prop.table()

# ¿Qué tan peligroso considera(s) que es... consumir 
#mariguana heroina coca inhala alcohol
table(individ$a05417a)
individ %>% 
  dplyr::select(folio, a05417a, a05417b, a05417c, a05417d,a05417e) %>% 
  gather(drug, peligroso, -folio) %>% 
  group_by(drug, peligroso) %>% 
  tally %>% 
  group_by(drug) %>% 
  mutate(prop = 100*n/sum(n))


# ¿Qué edad tenia la 1a vez que alcohol, drigas, tranq
individ %>% 
  dplyr::select(folio, a054h,a054i,a054j) %>% 
  gather(drug, edad, -folio) %>% 
  group_by(drug) %>% 
  filter(!edad %in% c(0, 999)) %>% 
  summarise(prom = mean(edad),
            med = median(edad), 
            quant25 = quantile(edad, .25), 
            quant75 = quantile(edad, .75))

# ha fumado mariguana, cocaina, crack, alucin, inhal, heroi, anfet
individ %>% 
  dplyr::select(folio, a067e, a067f, a067g, a067h, a067i, a067j, a067k) %>% 
  gather(drug, res, -folio) %>% 
  filter(!is.na(res)) %>% 
  group_by(drug, res) %>% 
  tally()%>% 
  group_by(drug) %>% 
  mutate(prop = 100*n/sum(n)) %>%  data.frame()


# ha probado mariguana, cocaina, crack, alucin, inhal, heroi, anfet
individ %>% 
  dplyr::select(folio, a067f1,a067g1,a067h1,a067i1,a067j1,a067k1) %>% 
  gather(drug, res, -folio) %>% 
  filter(!is.na(res)) %>%
  group_by(drug, res) %>% 
  tally()%>% 
  group_by(drug) %>% 
  mutate(prop = (100*n/sum(n)) )%>%  data.frame()


# uso de mariguana
# a070e	¿Qué edad tenía cuando usó mariguana por primera vez?
individ$a070e %>% summary
# a071e	¿Cuántas veces en su vida ha usado mariguana?
individ$a071e %>% table %>% prop.table()
# a072e	¿Ha usado mariguana en los últimos 12 meses?
individ$a072e %>% table() %>% prop.table()
# a075e	En los últimos 30 días ¿Ha consumido mariguana?
individ$a075e %>% table() %>% prop.table()


# uso de cocaina
# a070e	¿Qué edad tenía cuando usó coca por primera vez?
individ$a070f %>% summary
# a071e	¿Cuántas veces en su vida ha usado ?
individ$a071f %>% table %>% prop.table()
# a072e	¿Ha usado  en los últimos 12 meses?
individ$a072f %>% table() %>% prop.table()
# a075e	En los últimos 30 días ¿Ha consumido ?
individ$a075f %>% table() %>% prop.table()


# uso de inhalables
# a070e	¿Qué edad tenía cuando usó  por primera vez?
individ$a070i %>% summary
# a071e	¿Cuántas veces en su vida ha usado ?
individ$a071i %>% table %>% prop.table()
# a072e	¿Ha usado  en los últimos 12 meses?
individ$a072i %>% table() %>% prop.table()
# a075e	En los últimos 30 días ¿Ha consumido ?
individ$a075i %>% table() %>% prop.table()


# uso de heroina
# a070e	¿Qué edad tenía cuando usó  por primera vez?
individ$a070j %>% summary
# a071e	¿Cuántas veces en su vida ha usado ?
individ$a071j %>% table %>% prop.table()
# a072e	¿Ha usado  en los últimos 12 meses?
individ$a072j %>% table() %>% prop.table()
# a075e	En los últimos 30 días ¿Ha consumido ?
individ$a075j %>% table() %>% prop.table()


# alcohol
# a103	¿Ha consumido alguna vez cualquier bebida que contenga alcohol?
individ$a103 %>% table() %>% prop.table()
# a103a	¿Me puede decir si ha consumido alguna vez cualquier bebida que contenga alcohol? (Confirmación)
individ$a103a %>% table(useNA = 'always') %>% prop.table()
# a106	¿En los últimos 12 meses tomó alguna bebida que contenga alcohol? (cerveza, vino, ron, etc.)
individ$a106 %>% table(useNA = 'always') %>% prop.table()
# a106a	¿Qué edad tenía la última vez que tomó una bebida alcohólica en su vida?
individ$a106a %>% summary()


# tratamiento
individ %>% 
  dplyr::select(folio, a213a, a213b, a215, a215a, a216, a221) %>% 
  gather(var.nom, var.val, a213a:a216) %>% 
  group_by(var.nom, var.val) %>% 
  tally


# venta en escuelas
table(individ$a320a)  %>% prop.table()

# venta en via publica
table(individ$a320e)  %>% prop.table()

# enferma, debil, indep, egoista, ayuda, delicuente
individ %>% 
  dplyr::select(folio, a326a, a326b, a326c, a326d, a326e, a326f, a326g) %>% 
  gather(var.nom, var.val, a326a:a326g) %>% 
  group_by(var.nom, var.val) %>% 
  tally %>%
  group_by(var.nom) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(var.val == 'Sí')

# ¿Cuál de las siguientes sustancias piensa que producen adicción? 
individ %>% 
  dplyr::select(folio, a327a, a327b, a327c, a327d, a327e, a327f, a327g, a327h) %>% 
  gather(var.nom, var.val, a327a:a327h) %>% 
  group_by(var.nom, var.val) %>% 
  tally %>%
  group_by(var.nom) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(var.val == 'Sí')

# que hacer con adictos
table(individ$a328) %>% prop.table() %>% sort()


individ %>% 
  dplyr::select(folio, a331, a332, a333) %>% 
  gather(var.nom, var.val, a331:a333) %>% 
  group_by(var.nom, var.val) %>% 
  tally %>%
  group_by(var.nom) %>% 
  mutate(prop = n/sum(n)) %>% data.frame()


#  Aumentaría el consumo de drogas
# Se evitaría adulterar las drogas
# Se gastaría más dinero en la atención a los adictos
# Disminuiría la seguridad ciudadana
# Los narcotraficantes perderían poder
# Se terminarían las entre narcotraficantes
# Disminuirían las enfermedades relacionadas con drogas
# Se debilitarían los valores morales y religiosos
# Las drogas serían más baratas
individ %>% 
  dplyr::select(folio, a334, 
                a335, a336, a337, a338, a3381, 
                a339, a340, a341) %>% 
  gather(var.nom, var.val, a334:a341) %>% 
  group_by(var.nom, var.val) %>% 
  tally %>%
  group_by(var.nom) %>% 
  mutate(prop = n/sum(n)) %>% 
  filter(var.val == 'Sí') %>% data.frame()

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



# SEGUROS
individ %>% 
  dplyr::select(folio, ponde_indiv_final, a021, a021hes) %>% 
  mutate(a021hes = str_trim(as.character(a021hes)) ) %>% 
  group_by(a021, a021hes) %>% 
  summarise(nac= sum(ponde_indiv_final)) %>% 
  filter(a021hes != '') %>% 
  group_by(a021) %>% 
  mutate(porc = nac*100/sum(nac)) %>% 
  dplyr::arrange(porc, desc(porc))
  


# TABLAS

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
  

tolerancia %>% 
  mutate(var.val = facil_drogas, 
         columna.nom = 'facil_drogas') %>% 
  do(TabPond(.))
  

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
