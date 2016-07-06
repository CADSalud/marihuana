library(ProjectTemplate)
reload.project()

head(dat.ycann)

dat.ycann$Country %>% n_distinct()
dat.ycann$Coverage %>% unique()
dat.ycann$year %>% sort() %>% unique()


ggplot(dat.ycann, aes(x = Region, y = factor(year))) + 
  stat_bin2d(aes(fill = ..count..), binwidth = c(3,1))


