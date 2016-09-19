library(ProjectTemplate)
load.project()

print(percep, n = 3, width = Inf)
print(percep.legal, n = 3, width = Inf)

percep.u <- percep %>% 
  left_join(
    tolerancia %>% 
      select(folio:facil_drogas) %>% 
      unique()
  ) %>% 
  left_join( 
    filter(riesgo, columna.nom == 'mariguana') %>% 
      select(-columna.nom, mariguana.riesgo = var.val)
    ) %>%
  left_join(percep.legal) %>% 
  left_join(venta) %>% 
  left_join(prevdrogas) %>% 
  mutate(edadinic_mariguana = parse_number(edadinic_mariguana),
         edadinic_mariguana = ifelse(edadinic_mariguana>100,100, edadinic_mariguana),
         edadinic_alcohol = parse_number(edadinic_alcohol), 
         edadinic_alcohol = ifelse(edadinic_alcohol>100,100, edadinic_alcohol)
         )
dim(percep.u) # 16249
print(percep.u, n=3, width = Inf)


# • Características
# Frecuencias ponderadas 


# drogas en general
percep.u %>% 
  group_by(evolucion) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  group_by(facil_drogas) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  group_by(facil_drogas) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  group_by(mariguana, facil_drogas) %>% 
  summarise(frec = sum(pond)) %>% 
  group_by(mariguana) %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  group_by(venta_escuela) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  group_by(venta_colonia) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  mutate(drogas.ilegales.bin = drogas.ilegales> 0) %>% 
  group_by(drogas.ilegales.bin) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  mutate(drogas.ilegales.bin = drogas.ilegales> 0) %>% 
  # filter(!is.na(dependencia_drogas)) %>%
  group_by(drogas.ilegales.bin, dependencia_drogas) %>% 
  summarise(frec = sum(pond)) %>% 
  group_by(drogas.ilegales.bin) %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  mutate(drogas.ilegales.bin = drogas.ilegales> 0) %>% 
  # filter(drogas.ilegales.bin == T) %>% 
  group_by(drogas.ilegales.bin, tratamiento_drogas) %>% 
  summarise(frec = sum(pond)) %>% 
  group_by(drogas.ilegales.bin) %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

# Arrestos y detenciones
percep.u %>% 
  group_by(arrestos_drogas) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  group_by(detenidos) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

# Mariguana
percep.u %>% 
  group_by(mariguana_legal) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  group_by(mariguana_medicos) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  group_by(mariguana) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

# edad inicial
percep.u %>% 
  select(folio, pond, mariguana, edadinic_mariguana) %>% 
  filter(!is.na(edadinic_mariguana)) %>%
  mutate(edadinic_mariguana = as.numeric(edadinic_mariguana) ) %>% 
  summarise( prom.pond = sum(edadinic_mariguana*pond)/sum(pond), 
             prom = mean(edadinic_mariguana, rm = T),
             median = wtd.quantile(edadinic_mariguana, pond, probs = .5, na.rm = T)
             ) %>% 
  write.table(row.names = F, sep = ",")
qts <- wtd.Ecdf(as.numeric(percep.u$edadinic_mariguana), percep.u$pond)
gg <- ggplot(data.frame(x= qts$x, y= 100*qts$ecdf), 
       aes( x= x, y = y))+ 
  geom_hline(aes(yintercept = 50), alpha = .5, color = 'darkgreen') +
  geom_ribbon( aes(ymin = 25, ymax = 75), alpha = .15, fill = 'darkgreen') + 
  geom_ribbon( aes(ymin = 5, ymax = 95), alpha = .15, fill = 'darkgreen') + 
  geom_line(size = 1) + 
  geom_label(x = 18, y = qts$ecdf[qts$x == 18]*100, 
             label = round(qts$ecdf[qts$x == 18]*100)) +
  scale_x_continuous(breaks = 6*(1:16))  +
  ylab('Porcentaje Acumulado Población') + 
  xlab('Edad de Inicio\nMariguana') 
ggsave(filename = "graphs/graphs_ena/edadecdf_mariguana.png", gg, width = 5.5, height = 4)


percep.u %>% 
  select(folio, pond, mariguana, edadinic_alcohol) %>% 
  filter(!is.na(edadinic_alcohol)) %>%
  summarise( prom.pond = sum(edadinic_alcohol*pond)/sum(pond), 
             prom = mean(edadinic_alcohol, na.rm = T),
             median = wtd.quantile(edadinic_alcohol, pond, probs = .5, na.rm = T)
             ) %>% 
  write.table(row.names = F, sep = ",")
qts <- wtd.Ecdf(as.numeric(percep.u$edadinic_alcohol), percep.u$pond)
gg <- ggplot(data.frame(x= qts$x, y= 100*qts$ecdf), 
             aes( x= x, y = y))+ 
  geom_hline(aes(yintercept = 50), alpha = .5, color = 'salmon') +
  geom_ribbon( aes(ymin = 25, ymax = 75), alpha = .15, fill = 'salmon') + 
  geom_ribbon( aes(ymin = 5, ymax = 95), alpha = .15, fill = 'salmon') + 
  geom_line(size = 1) + 
  geom_label(x = 18, y = qts$ecdf[qts$x == 18]*100, 
             label = round(qts$ecdf[qts$x == 18]*100)) +
  scale_x_continuous(breaks = 6*(1:16))  +
  ylab('Porcentaje Acumulado Población') + 
  xlab('Edad de Inicio\nAlcohol') 
ggsave(filename = "graphs/graphs_ena/edadecdf_alcohol.png", gg, width = 5.5, height = 4)

percep.u  %>% 
  group_by(mariguana, mariguana_legal) %>% 
  summarise(frec = sum(pond)) %>% 
  group_by(mariguana) %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")
  



# perfil imposible
percep.u %>% 
  filter(facil_drogas == 'Imposible') %>% 
  group_by(adiccion_mariguana) %>% 
  summarise(frec = sum(pond)) %>% 
  ungroup() %>% 
  mutate(porc = frec/sum(frec)) %>% 
  write.table(row.names = F, sep = ",")

# facil drogas vs incrementado
tab <- percep.u %>% 
  group_by(evolucion, facil_drogas) %>% 
  summarise(frec = sum(pond)) %>% 
  group_by(evolucion) %>% 
  mutate(perf = frec/mean(frec)) %>% 
  group_by(facil_drogas) %>% 
  mutate(perfd = 100*perf/mean(perf)) %>% 
  select(evolucion, facil_drogas, perfd) %>% 
  spread(evolucion, perfd)
gg <- ggbiplot(princomp(tab[, -1]), scale = .8, 
         labels = tab$facil_drogas) 
ggsave("graphs/ena_evolfacildrogas.png",
       gg, width = 7, height = 6.5)

tab <- percep.u %>% 
  # filter(mariguana_legal != 'No sabe') %>% 
  group_by(mariguana_legal, facil_drogas) %>% 
  summarise(frec = sum(pond)) %>% 
  group_by(mariguana_legal) %>% 
  mutate(porc = frec/sum(frec),
         perf = frec/mean(frec)) %>% 
  group_by(facil_drogas) %>% 
  mutate(perfd = 100*perf/mean(perf)) %>% 
  select(mariguana_legal, facil_drogas, perfd) %>% 
  spread(mariguana_legal, perfd)
gg <- ggbiplot(princomp(tab[, -1]), scale = .8, 
         labels = tab$facil_drogas) 
ggsave("graphs/ena_legalfacildrogas.png",
       gg, width = 7, height = 6.5)



# consecuencia de legalizarla
percep.cons <- percep.u %>% 
  select(folio, pond, starts_with("mariguana_"),
         starts_with("cons_")) 
dim(percep.cons)
print(percep.cons, n=3, width = Inf)


# legal mariguana 
tab <- percep.cons %>% 
  left_join(
    individ %>% select(folio, pond, edad) %>% 
      mutate(edad.cut = cut(edad, c(12, 18, 24, 30, 36, 42, 48, 54, 60, 65), 
                            include.lowest = T)),
    by = c("folio", "pond")
  ) %>% 
  group_by(edad.cut, mariguana_legal) %>% 
  summarise(n = sum(pond)) %>% 
  group_by(edad.cut) %>% 
  mutate(prop = 100*n/sum(n))

gg <- ggplot(tab, aes(x = edad.cut, y = prop, 
                color = mariguana_legal, group = mariguana_legal)) + 
  # geom_line() + 
  geom_point() + 
  geom_smooth(se = F, span = 1) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  facet_wrap(~mariguana_legal, scale = 'free_y') 
ggsave("graphs/graphs_ena/ena_legaledadcut.png", gg, width = 7, height = 3)  


tab <- percep.cons %>% 
  left_join(
    demos %>% select(folio, pond, ingreso.cod),
    by = c("folio", "pond")
  ) %>% 
  group_by(ingreso.cod, mariguana_legal) %>% 
  summarise(n = sum(pond)) %>% 
  filter(ingreso.cod != 9) %>% 
  group_by(ingreso.cod) %>% 
  mutate(prop = 100*n/sum(n))

gg <- ggplot(tab, aes(x = factor(ingreso.cod), y = prop, 
                color = mariguana_legal, group = mariguana_legal)) + 
  # geom_line() + 
  geom_point() + 
  geom_smooth(se = F, span = 1) + 
  facet_wrap(~mariguana_legal, scale = 'free_y') 
ggsave("graphs/graphs_ena/ena_legalingreso.png", gg, width = 7, height = 3)  

  
  
tab <- percep.cons %>% 
  gather(cons, cons.val, -1:-4) %>% 
  group_by(mariguana_legal, cons, cons.val) %>% 
  summarise(n = sum(pond)) %>% 
  group_by(mariguana_legal, cons) %>% 
  mutate(prop = 100*n/sum(n))
tab %>% 
  filter(cons.val == 'Sí') %>% 
  select(-n) %>% 
  spread(mariguana_legal, prop) %>% 
  write.csv(row.names = F)

gg <- ggplot( filter(tab,cons.val == 'Sí'), 
        aes(x  = fct_reorder(cons, prop, mean), 
            y = prop, fill = mariguana_legal))+ 
  geom_bar(stat= 'identity', position = 'dodge') + 
  xlab(NULL) + 
  coord_flip() 
ggsave("graphs/graphs_ena/ena_consecuenciasleg.png", gg, 
       width = 7, height = 5)


modsi <- lm(mariguana_legal == 'Sí' ~ `cons_aumentaría consumo` + 
              `cons_debilitarian valores morales/relig` + 
              `cons_disminuiría seguridad` + `cons_drogas más baratas` + 
              `cons_evitaría adulterar` + `cons_fin disputas narcos` + 
              `cons_mas gasto en adictos` + `cons_menos enfermedades drogas` +
              `cons_narco perdería poder`, data = percep.cons)
coef(modsi)/4*100 

modno <- lm(mariguana_legal == 'No' ~ `cons_aumentaría consumo` + 
              `cons_debilitarian valores morales/relig` + 
              `cons_disminuiría seguridad` + `cons_drogas más baratas` + 
              `cons_evitaría adulterar` + `cons_fin disputas narcos` + 
              `cons_mas gasto en adictos` + `cons_menos enfermedades drogas` +
              `cons_narco perdería poder`, data = percep.cons)
coef(modno)/4*100 

