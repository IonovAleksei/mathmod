#Ионов Алексей Алексеевич — создайте модель множественной линейной регрессии
#ночных потоков углекислого газа за летний период 2013 года
#по данным измерений методом турбулентной пульсации

rm(list=ls())
library("tidyverse") 
library("readr")     
library("stringr")   
library("dplyr")     
library("ggplot2") 
library("tidyr")
library("stringr")
library("lubridate")

# Считываем файл 
#Пропускаем первую строку,заменяем все
#не числовые значения на NA, игнорируя строчки с символом "["

tbl = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))

#Подготовка к анализу
tbl = tbl[-1,]; tbl
tbl = select(tbl, -(roll))
tbl = tbl %>% mutate_if(is.character, factor)
names(tbl) = names(tbl) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(tbl)
#Отфильтруем данные для своего варианта
tbl = filter(tbl,DOY >= 152 & DOY <= 243)
tbl = filter(tbl, daytime ==FALSE)

#Проведем корелляционный анализ
tbl_numeric = tbl[,sapply(tbl,is.numeric) ]
cor_td = cor(drop_na(tbl_numeric), method = "spearman") %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > 0.2] %>% na.exclude

formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""));formula

row_numbers = 1:length(tbl_numeric$co2_flux)
teach = sample(row_numbers, floor(length(tbl_numeric$co2_flux)*.7))
test = row_numbers[-teach]

teaching_tbl = tbl_numeric[teach,]
testing_tbl = tbl_numeric[test,]

model0=lm(co2_flux ~ H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov + w_div_co2_cov + co2...125 + co2...127, data = teaching_tbl)
summary(model0)
anova(model0)

model1=lm(co2_flux ~ H  + rand_err_co2_flux + co2_molar_density + 
            co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
            w_div_ts_cov + co2...125, data = teaching_tbl)
summary(model1)
anova(model1)
anova(model1, model0)

model2=lm(co2_flux ~ H  + rand_err_co2_flux + co2_molar_density + 
            co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
            co2...125, data = teaching_tbl)
summary(model2)
anova(model2)
anova(model2,model1)

model3=lm(co2_flux ~ H  + rand_err_co2_flux + co2_molar_density + 
            co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux,
          data = teaching_tbl)
summary(model3)
anova(model3)
anova(model3,model2)
