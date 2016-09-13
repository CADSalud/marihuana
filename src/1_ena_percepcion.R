library(ProjectTemplate)
reload.project()

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
  left_join(prevdrogas)
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

percep.u %>% 
  select(folio, pond, mariguana, edadinic_mariguana) %>% 
  filter(!is.na(edadinic_mariguana)) %>%
  mutate(edadinic_mariguana = as.numeric(edadinic_mariguana) ) %>% 
  summarise( prom.pond = sum(edadinic_mariguana*pond)/sum(pond), 
             prom = mean(edadinic_mariguana, rm = T),
             median = median(edadinic_mariguana, na.rm = T)
             )%>% 
  write.table(row.names = F, sep = ",")

percep.u %>% 
  select(folio, pond, mariguana, edadinic_alcohol) %>% 
  filter(!is.na(edadinic_alcohol)) %>%
  mutate(edadinic_mariguana = as.numeric(edadinic_alcohol) ) %>% 
  summarise( prom.pond = sum(edadinic_alcohol*pond)/sum(pond), 
             prom = mean(edadinic_alcohol, na.rm = T),
             median = median(edadinic_alcohol, na.rm = T)
             ) %>% 
  write.table(row.names = F, sep = ",")
  

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
      mutate(edad.cut = cut(edad, c(12, 18, 24, 30, 36, 42, 48, 54, 60, 65) )),
    by = c("folio", "pond")
  ) %>% 
  group_by(mariguana_legal, edad.cut) %>% 
  summarise(n = sum(pond)) %>% 
  group_by(mariguana_legal) %>% 
  mutate(prop = 100*n/sum(n))

ggplot(tab, aes(x = edad.cut, y = prop)) + 
  

tab <- percep.cons %>% 
  gather(cons, cons.val, -1:-4) %>% 
  group_by(mariguana_legal, cons, cons.val) %>% 
  summarise(n = sum(pond)) %>% 
  group_by(mariguana_legal, cons) %>% 
  mutate(prop = 100*n/sum(n))

ggplot( filter(tab,cons.val == 'Sí'), 
        aes(x  = cons, y = prop, fill = mariguana_legal))+ 
  geom_bar(stat= 'identity', position = 'dodge') + 
  coord_flip() + 
  facet_wrap(~cons.val) 

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

