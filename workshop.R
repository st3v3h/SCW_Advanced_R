library(tidyverse)

data1 <- read_csv("messy_demographic.csv")
data2 <- read_csv("messy_cognitive.csv")
data3 <- read_csv("messy_genotype.csv")

library(stringr)

data1[data1==""] <- NA
data1[data1=="missing"] <- NA
data1[data1=="9999"] <- NA
data1 <- data1 %>%
  mutate(age = as.numeric(age),
         ethnicity = factor(ethnicity),
         sex = factor(sex, levels = c(0,1), 
                      labels = c("Male", "Female")),
         dx = factor(dx, levels = c(0,1), 
                     labels = c("Control", "Case")))
data2[data2==""] <- NA
data2[data2=="missing"] <- NA
data2[data2=="9999"] <- NA
data2 <- data2 %>%
  mutate(cog1 = as.numeric(cog1),
         cog2 = as.numeric(cog2),
         cog3 = as.numeric(cog3),
         subject_ID = str_replace(subID, "subject", "SUB_")) %>%
  select(subject_ID, cog1:cog3)
data3[data3==""] <- NA
data3[data3=="missing"] <- NA
data3[data3=="9999"] <- NA
data3 <- data3 %>%
  mutate(genotype = factor(genotype,
                           levels=c(0,1,2), 
                           labels=c("AA","AG","GG")),
         subject_ID = str_replace(subID, "subject", "SUB_")) %>%
  select(-subID)
alldata <- data1 %>%
  inner_join(data2, by="subject_ID") %>%
  inner_join(data3, by="subject_ID")

summary(alldata)

library(tableone)

CreateTableOne(alldata, vars=c('age', 'sex', 'genotype', 'ethnicity', 'cog1','cog2','cog3'),
               factorVars = c('sex','genotype','ethnicity'),
               strata = 'dx')

#alternative approach:
CreateTableOne(data = dplyr::select(alldata, -subject_ID),
               factorVars = c('sex','genotype','ethnicity'),
               strata = 'dx')
#t test
my_ttest<-t.test(cog1 ~ dx, data = alldata)
#can call these values in rmarkdown with: `r my_test$p.value`
my_ttest$statistic
my_ttest$parameter
my_ttest$p.value

#BOXPLOT
alldata %>% 
  drop_na(dx) %>% 
  ggplot(aes(x=dx, y=cog1, fill = dx)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center') +
  geom_boxplot(alpha = 0.5) + 
  labs(x = NULL,
       y = "Cog scale 1",
       fill = "Diagnosis") +
  theme_classic()

#SCATTER PLOT
alldata<-alldata %>% 
  mutate(risk_carrier = fct_collapse(genotype,
                                     carrier = c("GG", "AG"),
                                     non_carrier = "AA"))
alldata %>% 
  drop_na(risk_carrier, sex, dx) %>% 
  ggplot(aes(x = age, y = cog1, color = risk_carrier)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(dx~sex, scales="free", ncol=4)
  #facet_grid(dx~sex)
#in rmarkdown you can specify plot size with the little settings gear in your code chunk


#GATHERING
alldata %>% 
  #gather(<new var name -- takes prev col names>, <new value name -- takes values>, <select columns>)
  gather(cognitive_var, cognitive_score, cog1:cog3) %>% 
  drop_na(risk_carrier, sex, dx) %>% 
  ggplot(aes(x = age, y = cognitive_score, color = risk_carrier)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~cognitive_var, scales="free")

#Group_by and summarize
alldata %>% 
  gather(cognitive_var, cognitive_score, starts_with('cog')) %>% 
  group_by(cognitive_var,dx) %>% 
  drop_na(cognitive_score, dx) %>% 
  summarize(n = n(),
            mean_cog = mean(cognitive_score)) %>% 
  knitr::kable()
