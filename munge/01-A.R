# Example preprocessing script.

# Annual Prevalence of Use as a percentage of the population aged 15-64 Cannabis
dat.gcann <- read.csv("data/generalprevalence.csv", stringsAsFactors = F) %>% 
  mutate(Year = as.numeric(Year))

# Prevalence, amongst young people Cannabis Type
dat.ycann <- read.csv("data/prevalenceyouthcannabis.csv", 
                      stringsAsFactors = F) %>% 
  rename(year = Year.of.Estimate) %>% 
  mutate(year = as.numeric(year)) %>% 
  dplyr::select(-Notes, -Source) %>%
  rename(ever.used = `X..of.young.people.who.ever.used`, 
         alo.last.year = `X..of.young.people.who.used.at.least.once.in.the.past.year`, 
         alo.last.month = `X..of.young.people.who.used.at.least.once.in.the.past.month`) 

# Prevalence, amongst young people Cocaine Type
dat.ycoca <- read.csv("data/prevalenceyouthcocaine.csv",
                      stringsAsFactors = F) %>% 
  rename(year = Year.of.Estimate) %>% 
  mutate(year = as.numeric(year))

# Illicit drug use in prision
dat.prision <- read.csv("data/illicitdruguseprision.csv",
                      stringsAsFactors = F) %>% 
  rename(year = Year.of.estimates) %>% 
  mutate(year = as.numeric(year))

