meta <- tibble('author' = c("Didikoglu"),
               'not_tested_smoker' = c(32978),
               'not_tested_never_smoker' = c(189812),
               'tested_smoker' = c(4786),
               'tested_never_smoker' = c(21840),
               'not_tested_former_smoker' = c(118598),
               'tested_former_smoker' = c(16807))

a <-meta %>%
      select(not_tested_never_smoker, tested_never_smoker) %>%
      rename('not tested' = not_tested_never_smoker, 'tested' = tested_never_smoker)
b <- meta %>%
  select(not_tested_smoker, tested_smoker) %>%
  rename('not tested' = not_tested_smoker, 'tested' = tested_smoker)
c <- as.matrix(rbind(a,b))
rownames(c) <- c('never smokers', 'current smokers')
riskratio(c, method = 'wald', conf.level=0.95)

a <-meta %>%
  select(not_tested_never_smoker, tested_never_smoker) %>%
  rename('not tested' = not_tested_never_smoker, 'tested' = tested_never_smoker)
b <- meta %>%
  select(not_tested_former_smoker, tested_former_smoker) %>%
  rename('not tested' = not_tested_former_smoker, 'tested' = tested_former_smoker)
d <- as.matrix(rbind(a,b))
rownames(d) <- c('never smokers', 'former smokers')
riskratio(d, method = 'wald', conf.level=0.95)