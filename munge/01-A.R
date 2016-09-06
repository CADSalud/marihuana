
# las tablas que empiezan don df. son de UN
# las demás son de ENA 2012


# ≈≈≈≈≈ #
# # • DATOS UN
# ≈≈≈≈≈ #


library(gdata)
# Youth Prevalence
noms.file <- paste0('data/py_', c('cannabis', 'cocaine', 'opioids', 'tranqsed'), '.xls')
df.youth <- lapply(noms.file, function(file.path){
  wb <- read.xls(xls = file.path, sheet = 1) 
  
  names(wb)[names(wb) == select_vars(names(wb), contains('ever.used'))] <- 'lifetime'
  names(wb)[names(wb) == select_vars(names(wb), contains('past.year'))] <- 'Annual'
  names(wb)[names(wb) == select_vars(names(wb), contains('past.month'))] <- 'X30.day'
  wb <- wb[, !names(wb) %in% c('X', 'Notes', 
      'Notes..cocaine.powder.HCl.unless.otherwise.noted.', 
      'Drug')]
  names(wb) <- tolower(names(wb))
  
  wb <- wb %>% 
    dplyr::rename(year = year.of.estimate) %>% 
    dplyr::mutate(
      drug = str_replace_all(str_split_fixed(file.path, '_', 2)[2], '.xls', ''),
      year = as.numeric(as.character(str_replace(year, '/.*', '') ))
    ) %>% 
    filter(region %in% c('Africa', 'Americas', 'Asia', 'Oceania', 'Europe'))  
  
  print(dim(wb))
  wb
  }) %>% rbind_all()
dim(df.youth) # 269 + 368 + 218 + 147 = 1002
head(df.youth)
df.youth$year %>% summary()



# General Prevalence
noms.file <- paste0('data/gp_', c('cannabis', 'cocaine', 'opioids', 'tranqsed'), '.xls')
df.general <- lapply(noms.file, function(file.path){
  wb <- read.xls(xls = file.path, sheet = 1)
  
  names(wb)[names(wb) == select_vars(names(wb), contains('Best'))] <- 'total'
  wb <- wb[, !names(wb) %in% c('X', 'Method', 'Adjustment', 'Low', 'High')]
  names(wb) <- tolower(names(wb))
  
  wb <- wb %>% 
    dplyr::mutate(
      drug = str_replace_all(str_split_fixed(file.path, '_', 2)[2], '.xls', ''),
      year = as.numeric(str_sub(year, 1, 4))
      ) %>% 
    filter(region %in% c('Africa', 'Americas', 'Asia', 'Oceania', 'Europe'))
  
  print(dim(wb))
  wb
  }) %>% rbind_all()
dim(df.general) # 310 + 292 + 56 + 83 = 741
head(df.general)


# Illicit drug use in prisions
df.prision <- read.xls('data/illicitdruguseinprision.xls') %>% 
  filter(Region %in% c('Africa', 'Americas', 'Asia', 'Oceania', 'Europe')) %>% 
  dplyr::select(-Notes, -Source, year = X, -X.1) %>% 
  gather(var.nom, var.val, -1:-4) %>% 
  mutate(
    drug = tolower(str_replace(var.nom, "[.].*", "")),
    period.num = as.numeric(str_replace(var.nom, ".*[.]", "")), 
    period = car::recode(period.num, 
                         "1='annual';2='month';else = 'lifetime'"
                         ),
    year = as.numeric(str_sub(year, 1, 4))
  ) %>% 
  dplyr::select(-var.nom, -var.val, -period.num)
names(df.prision) <- tolower(names(df.prision))
head(df.prision)


# Drug related deaths
tipo.df <- data.frame(
  drug.raw = c('Cannabis','Opioids','Cocaine',
       'Amphetamine.type..stimulants',
       'Tranquilizers.and.sedatives',
       'Hallucinogens','Solvents.and.inhalants',
       'Fatal.drug.overdoses....'),
  drug = c('Cannabis','Opioids','Cocaine',
       'Amphetamine',
       'Tranquilizers-sedatives',
       'Hallucinogens','Solvents-inhalants',
       'Fatal.drug.overdoses')
)

df.deaths <- read.xls('data/drugrelateddeaths.xls', stringsAsFactors = F) %>% 
  filter(Region %in% c('Africa', 'Americas', 'Asia', 'Oceania', 'Europe')) %>% 
  select(-X.2, -X, -X.1) %>% 
  gather(drug.raw, perc, Cannabis:Fatal.drug.overdoses....) %>% 
  left_join(tipo.df, by = 'drug.raw') %>% 
  dplyr::rename( country = Country...Territory, 
          year = Year.of.Estimate, 
          national.bin = National.estimate...Y.N., 
          deaths.num = Number.of.deaths, 
          ref.popul = Reference.population..15...64.., 
          rate.mill = Rate.per.million.aged.15.64) %>% 
  select(-drug.raw) %>% 
  mutate(perc = as.numeric(str_trim(str_replace(perc, "%", ""))),
         deaths.num = as.numeric(str_replace_all(deaths.num, ',', '')),
         ref.popul = as.numeric(str_replace_all(ref.popul, ',', '')),
         rate.mill = as.numeric(str_replace_all(rate.mill, ',', '')), 
         drug = tolower(drug)
         )
names(df.deaths) <- tolower(names(df.deaths))
head(df.deaths)


rm('tipo.df')



# ≈≈≈≈≈ #
# # • DATOS ENA 2012
# ≈≈≈≈≈ #
library(foreign)

hogar <- read.spss("data/ENA/tbl_hogar_hogar_2012-01-30.sav",
                   to.data.frame = T) %>% 
  tbl_df()
# 
# integrantes <- read.spss("data/ENA/tbl_hogar_integrantes_2012-01-30.sav",
#                          to.data.frame = T)

individ <- read.spss("data/ENA/tbl_individual_seleccionados_2012-01-30.sav",
                         to.data.frame = T) %>% 
  rename(pond = ponde_indiv_final) %>% 
  tbl_df()

# attributes(hogar)$variable.labels %>% 
#   as.data.frame(optional = F) %>% 
#   write.csv("doc/ena_atrs_hogar.csv")
# attributes(integrantes)$variable.labels %>% 
#   data.frame() %>% 
#   write.csv("doc/ena_atrs_integrantes.csv")
# attributes(individ)$variable.labels %>% 
#   data.frame() %>% 
#   write.csv("doc/ena_atrs_individual.csv")






# • TABLAS ENA POR TEMA

# 1. Demográficos generales

# ingreso
df.ingr <- individ %>% 
  dplyr::select(folio, a015a, a015b, a015c) %>% 
  gather(columna.nom, codigo, a015a:a015c) %>% 
  filter(codigo != -999) %>% 
  mutate(codigo = as.numeric(codigo)) %>% 
  left_join(
    read.csv("doc/recode_ingresos_ena.csv") %>% 
      dplyr::select(codigo:ingreso.prom) %>% 
      mutate(codigo = as.numeric(codigo)), 
    by = c('codigo', 'columna.nom')
  ) %>% 
  select(-columna.nom, ingreso.cod = codigo)

# pob ocupada
df.pocup <- individ %>% 
  dplyr::select(folio, a011, a0111, a0112, a0113) %>% 
  gather(columna.nom, codigo, a011:a0113) %>% 
  filter(!is.na(codigo)) %>% 
  mutate(bin = (codigo == "Sí")) %>% 
  group_by(folio) %>% 
  summarise(acum = factor(as.numeric(sum(bin) > 0), 
                          levels = 0:1, 
                          labels = c('no ocupado', 'ocupado'))
  ) %>% 
  rename(pob.ocup = acum)

# migrantes
df.migrac <- individ %>% 
  dplyr::select(folio, 
                algvez_usa = a352, 
                motivo_usa = a358) %>% 
  mutate(motivo_usa = str_replace(motivo_usa, "[?]", ""),
         trabajo_usa = ifelse(motivo_usa == 'trabajo', 'trabajo', NA)
         )
         


# 0. demos
demos <- individ %>% 
  dplyr::select(folio, pond,
                edad, sexo_h, tipo_sel, tipo_ind, 
                a006, a007, a008, a008a, #
                a014a # ocupación
  ) %>% 
  mutate(edad.cut = cut_width(edad, 6)) %>% 
  left_join(
    df.pocup, by = 'folio'
  ) %>% 
  left_join(
    df.ingr, by = 'folio'
  ) %>% 
  left_join(
    df.migrac, by = 'folio'
  ) %>% 
  rename(edo_civil = a006, religion = a007, 
         escolar = a008a, profesion = a014a)
head(demos %>% data.frame())
rm('df.pocup', 'df.ingr', 'df.migrac')


# 8. Tolerancia social
tolerancia <- individ %>% 
  dplyr::select(folio, pond,
                facil_drogas = a054d3,
                a0541:a05416
  ) %>% 
  gather(columna.nom, var.val, a0541:a05416) %>% 
  filter(!is.na(var.val)) %>% 
  mutate(columna.nom = fct_recode(columna.nom,
              "familia_alcohol" = c("a0541", "a0549"),
              "mejoramigo_alcohol" = c("a0542", "a05410"),
              "pareja_alcohol" = c("a0543", "a05411"),
              "maestros_alcohol" = c("a0544", "a05412"),
              "familia_drogas" = c("a0545", "a05413"),
              "mejoramigo_drogas" = c("a0546", "a05414"),
              "pareja_drogas" = c("a0547", "a05415"),
              "maestros_drogas" = c("a0548", "a05416")
              ),
         columna.nom = str_replace(columna.nom, "1|2", "")
         ) 

# 2. Percepción de riesgo 
riesgo <- individ %>% 
  dplyr::select(folio, pond,
              a05417a:a05417e
              ) %>% 
  gather(columna.nom, var.val, a05417a:a05417e) %>% 
  filter(!is.na(var.val)) %>% 
  mutate(columna.nom = fct_recode( str_sub(columna.nom, -1, -1),
             "mariguana" = 'a',
             "heroína" = 'b',
             "cocaína" = 'c',
             "inhalables" = 'd',
             "alcohol frecuentemente" = 'e'
             )
         ) 
  

# 4. Uso, consumo, tratamiento, accidentes y arrestos

usos <- individ %>% 
  dplyr::select(folio, pond, 
      edadinic_tabaco = a031a, edadinic_tabacodiario = a032b, 
      edadinic_alcohol = a054h, edadinic_drogas = a054i, 
      edadinic_tranq = a054j,
      algvez_tabaco = a030, ult12_tabaco = a033, 
      algvez_alcohol = a103, ult12_alcohol = a106, ult30_alcohol = a110, 
      a067e, a067f, a067g, a067h, a067i, a067j, a067k, a067l,
      a070e, a070f, a070g, a070h, a070i, a070j, a070k, a070l,
      a072e, a072f, a072g, a072h, a072i, a072j, a072k, a072l,
      a075e, a075f, a075g, a075h, a075i, a075j, a075k, a075l,
      dependencia_drogas = a094a, adiccion_drogas = a1003a,
      arrestos_drogas = a0981a, 
      accidentes_drogas = a1001a,
      frec_alcohol= a109, prob_policia = a155a,
      accidentesauto_alcohol = a156a, accidentes_alcohol = a157a,
      tratamiento_alcohol = a213a, tratamiento_drogas = a213b
                ) %>% 
  gather(var.val, var.num, edadinic_tabaco:edadinic_tranq) %>% 
  mutate(var.num = ifelse(var.num %in% c(0, 999), NA, var.num)) %>% 
  spread(var.val, var.num) %>% 
  mutate(ult12_tabaco = fct_collapse(ult12_tabaco,
    si = c('Hace 6 meses o más pero menos de 1 año', 
           'Más de 1 mes pero menos de 6 meses',
           'En los últimos 30 días'),
    no = c('Hace 1 año o más pero menos de 3 años',
           'Hace más de 3 años')
  )) %>% 
  gather(var.val, var.num, c(a067e:a067l, a072e:a075l)) %>% 
  mutate(droga = fct_recode(str_sub(var.val, -1, -1),
            "mariguana" = 'e', "cocaína" = 'f',
            "crack" = 'g', "aluciógenos" = 'h', "inhalables" = 'i',
            "heroína" = 'j', "anfetamina" = 'k', "otras" = 'l'
            ),
         var.val = fct_recode(str_sub(var.val, -5, -2),
            "algvez" = "a067", "ult12" = "a072", "ult30" = "a075"
            ),
         var.num = fct_collapse(var.num, 
            si = c("Sí", "Sí 20 días o más", "Sí de 1 a 5 días", "Sí de 6 a 19 días"),
            no = c('No'),
            `ns/nc` = "No sabe/No contesta"
            )
         ) %>% 
  unite(varval, var.val, droga) %>% 
  spread(varval, var.num) %>% 
  gather(var.val, var.num, a070e:a070l) %>% 
  mutate(var.num = ifelse(var.num %in% c(0, 999), NA, var.num),
         droga = fct_recode(str_sub(var.val, -1, -1),
            "mariguana" = 'e', "cocaína" = 'f',
            "crack" = 'g', "aluciógenos" = 'h', "inhalables" = 'i',
            "heroína" = 'j', "anfetamina" = 'k', "otras" = 'l'
            ),
         var.val = fct_recode(str_sub(var.val, -5, -2),
            "edadinic" = "a070"
            )) %>% 
  unite(var.val, var.val, droga) %>% 
  spread(var.val, var.num)



# 3. Percepción de drogas
percep <- individ %>% 
  dplyr::select(folio, pond, 
                evolucion = a325, 
                a326a:a326g,
                a327a:a327h,
                rehabilitacion = a328
                ) %>% 
  gather(var.lab, var.num, a326a:a326g) %>% 
  mutate(var.lab = paste('adicto', fct_recode(var.lab,
          enferma = 'a326a', debil = 'a326b', 
          indep = 'a326c', egoista = 'a326d',
          ayuda = 'a326e',delincuente = 'a326f', 
          otra = 'a326g'
        ), sep = "_"
      )
    ) %>% 
  spread(var.lab, var.num) %>% 
  gather(var.lab, var.num, a327a:a327h) %>% 
  mutate(var.lab = paste('adiccion', fct_recode(var.lab,
          mariguana = 'a327a', alucinógenos = 'a327b', 
          cocaína = 'a327c', heroína = 'a327d',
          inhalables = 'a327e', alochol = 'a327f', 
          tabaco = 'a327g', otra = 'a327h'
        ), sep = "_"
      )
    ) %>% 
  spread(var.lab, var.num)

percep.legal <- individ %>% 
  dplyr::select(folio, pond, 
                drogas_relviolencia = a329,
                drogas_relaccidentes = a330,
                mariguana_medicos = a331,
                mariguana_legal = a332,
                drogas_legal = a333,
                a334:a341
                ) %>% 
  gather(var.lab, var.num, a334:a341) %>% 
  mutate(var.lab = paste('cons', fct_recode(var.lab,
             "aumentaría consumo" = 'a334',
             "evitaría adulterar" = 'a335',
             "mas gasto en adictos" ='a336',
             "disminuiría seguridad" ='a337',
             "narco perdería poder" = 'a338',
             "fin disputas narcos" = 'a3381',
             "menos enfermedades drogas" = 'a339',
             "debilitarian valores morales/relig" = 'a340',
             "drogas más baratas" = 'a341'
          ), sep = "_"
        )
      ) %>% 
  spread(var.lab, var.num)


 


  
  

# 5. Venta drogas 
venta <- individ %>% 
  dplyr::select(folio, pond, 
                venta_escuela = a320a, 
                venta_colonia = a320e, 
                vivir_seguro = a321a,
                vivir_agradable = a321b,
                detenidos = a3211
  )



# Cache de bases
cache('df.deaths')
cache('df.general')
cache('df.prision')
cache('df.youth')

cache('demos')
cache('hogar')
cache('individ')
cache('percep')
cache('percep.legal')
cache('riesgo')
cache('tolerancia')
cache('usos')
cache('venta')
