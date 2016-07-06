library(ProjectTemplate)
reload.project()

head(dat.ycann)


# Heatmaps registros
ggplot(dat.ycann, aes(x = Region, y = factor(year))) + 
  stat_bin2d(aes(fill = ..count..), binwidth = c(3,1)) 
ggplot(dat.ycoca, aes(x = Region, y = factor(year))) + 
  stat_bin2d(aes(fill = ..count..), binwidth = c(3,1)) 
ggplot(dat.prision, aes(x = Region, y = factor(year))) + 
  stat_bin2d(aes(fill = ..count..), binwidth = c(3,1)) 
ggplot(dat.gcann, aes(x = Region, y = factor(Year))) + 
  stat_bin2d(aes(fill = ..count..), binwidth = c(3,1)) 



# restriction per country
least.rest <- c('Argentina', 'Bangladesh', 'Canada', 'Chile', 'Colombia', 
                'Czech Republic', 'Jamaica', 'Peru', 'Spain', 'Uruguay', 
                'Germany', 'Netherlands', 'United States of America', 
                'Australia')
most.rest <- c("China, Hong Kong SAR", "China, Macao SAR", "Taiwan, Province of China", 
               "Egypt", 'France', 'Indonesia', 'Iran', 'Japan', 'Malaysia', 'Nigeria',
               'Poland', 'Thailand', 'Turkey', 'Ukraine')
others <-  c('Mexico', "United Kingdom (England)", "United Kingdom (Scotland)",
             'Switzerland', 'Portugal', 'Italy', 
             'Ecuador', 'Japan', 'Myanmar')


# PREVALENCE: Cannabis
dat <- dat.ycann %>% 
  filter(Country %in% c(least.rest, most.rest, others)) %>% 
  gather(used, prop.val, ever.used:alo.last.month) %>% 
  filter(!is.na(prop.val), 
         !is.na(year)) %>% 
  group_by(Region, Subregion, Country, year, used) %>% 
  summarise(prop.val = max(prop.val, na.rm = T))
  
ggplot(dat, aes(x = factor(year), y = prop.val, 
                color = used, group = used)) + 
  geom_point(size = 2) +
  geom_line() + 
  facet_wrap(~Country)



# PREVALENCE: Cocaine
dat <- dat.ycoca %>% 
  filter(Country %in% c(least.rest, most.rest, others)) %>% 
  gather(used, prop.val, ever.used:alo.last.month) %>% 
  filter(!is.na(prop.val), 
         !is.na(year)) %>% 
  group_by(Region, Subregion, Country, year, used) %>% 
  summarise(prop.val = mean(prop.val, na.rm = T))
  
ggplot(dat, aes(x = factor(year), y = prop.val, 
                color = used, group = used)) + 
  geom_point(size = 2) +
  geom_line() + 
  facet_wrap(~Country)