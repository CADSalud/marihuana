# Example preprocessing script.
library(gdata)

# • DATOS UN

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
prision <- read.xls('data/illicitdruguseinprision.xls') %>% 
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
names(prision) <- tolower(names(prision))
head(prision)


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

deaths <- read.xls('data/drugrelateddeaths.xls', stringsAsFactors = F) %>% 
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
names(deaths) <- tolower(names(deaths))
head(deaths)



# # • DATOS ENA
library(foreign)

hogar <- read.spss("data/ENA/tbl_hogar_hogar_2012-01-30.sav",
                   to.data.frame = T)

integrantes <- read.spss("data/ENA/tbl_hogar_integrantes_2012-01-30.sav",
                         to.data.frame = T)

individ <- read.spss("data/ENA/tbl_individual_seleccionados_2012-01-30.sav",
                         to.data.frame = T)

attributes(hogar)$variable.labels %>% 
  as.data.frame(optional = F) %>% 
  write.csv("doc/ena_atrs_hogar.csv")
attributes(integrantes)$variable.labels %>% 
  data.frame() %>% 
  write.csv("doc/ena_atrs_integrantes.csv")
attributes(individ)$variable.labels %>% 
  data.frame() %>% 
  write.csv("doc/ena_atrs_individual.csv")

