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
#Создадим модели и проведем их анализ для получения наиболее оптимальной
model0=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
            co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
            w_div_ts_cov + w_div_co2_cov + co2...125 + co2...127), data = teaching_tbl)

coef(model0)
resid(model0)
confint(model0)
summary(model0)
anova(model0)

model1=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov + co2...125), data = teaching_tbl)
coef(model1)
resid(model1)
confint(model1)
summary(model1)
anova(model1)
anova(model0, model1)

model2=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov + co2...125)^3, data = teaching_tbl)
coef(model2)
resid(model2)
confint(model2)
summary(model2)
anova(model2)
anova(model1, model2)

model3=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov + co2...125)^2, data = teaching_tbl)
coef(model3)
resid(model3)
confint(model3)
summary(model3)
anova(model3)
anova(model2, model3)

model4=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov)^2 - H:co2_molar_density - H:w_div_ts_cov - rand_err_co2_flux:co2_mole_fraction -
            rand_err_co2_flux:un_H - rand_err_co2_flux:un_co2_flux - rand_err_co2_flux:w_div_ts_cov - 
            co2_molar_density:co2_mole_fraction - co2_molar_density:un_H - co2_molar_density:un_co2_flux - co2_molar_density:w_div_ts_cov - 
            co2_molar_density:co2...125 - co2_mole_fraction:co2_mixing_ratio - co2_mole_fraction:un_H - co2_mole_fraction:un_co2_flux - 
            co2_mole_fraction:w_div_ts_cov - co2_mole_fraction:co2...125 - co2_mixing_ratio:T_star_ - 
            co2_mixing_ratio:un_H - co2_mixing_ratio:un_co2_flux - co2_mixing_ratio:w_div_ts_cov - co2_mixing_ratio:co2...125 - 
            T_star_:co2...125 - un_H:w_div_ts_cov - un_co2_flux:w_div_ts_cov - un_co2_flux:co2...125 - 
            w_div_ts_cov:co2...125, data = teaching_tbl)
coef(model4)
resid(model4)
confint(model4)
summary(model4)
anova(model4)
anova(model3, model4)

model5=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov)^2 - H:co2_molar_density - H:w_div_ts_cov - rand_err_co2_flux:co2_mole_fraction -
            rand_err_co2_flux:un_H - rand_err_co2_flux:un_co2_flux - rand_err_co2_flux:w_div_ts_cov - rand_err_co2_flux:co2_mixing_ratio - 
            co2_molar_density:co2_mole_fraction - co2_molar_density:co2_mixing_ratio - co2_molar_density:un_H - co2_molar_density:un_co2_flux - co2_molar_density:w_div_ts_cov - 
            co2_molar_density:co2...125 - co2_mole_fraction:co2_mixing_ratio - co2_mole_fraction:un_H - co2_mole_fraction:un_co2_flux - 
            co2_mole_fraction:w_div_ts_cov - co2_mole_fraction:T_star_  - co2_mole_fraction:co2...125 - co2_mixing_ratio:T_star_ - T_star_:w_div_ts_cov -
            co2_mixing_ratio:un_H - co2_mixing_ratio:un_co2_flux - co2_mixing_ratio:w_div_ts_cov - co2_mixing_ratio:co2...125 - 
            T_star_:co2...125 - un_H:un_co2_flux - un_H:w_div_ts_cov - un_co2_flux:w_div_ts_cov - un_co2_flux:co2...125 - 
            w_div_ts_cov:co2...125, data = teaching_tbl)
coef(model5)
resid(model5)
confint(model5)
summary(model5)
anova(model5)
anova(model4, model5)


model6=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov)^2 - H:co2_molar_density - H:rand_err_co2_flux - H:w_div_ts_cov - H:co2_mole_fraction -
            H:un_co2_flux - rand_err_co2_flux:co2_mole_fraction -
            rand_err_co2_flux:un_H - rand_err_co2_flux:un_co2_flux - rand_err_co2_flux:w_div_ts_cov - rand_err_co2_flux:co2_mixing_ratio - 
            co2_molar_density:co2_mole_fraction - co2_molar_density:co2_mixing_ratio - co2_molar_density:un_H - co2_molar_density:un_co2_flux - co2_molar_density:w_div_ts_cov - 
            co2_molar_density:co2...125 - co2_mole_fraction:co2_mixing_ratio - co2_mole_fraction:un_H - co2_mole_fraction:un_co2_flux - 
            co2_mole_fraction:w_div_ts_cov - co2_mole_fraction:T_star_  - co2_mole_fraction:co2...125 - co2_mixing_ratio:T_star_ - T_star_:w_div_ts_cov -
            co2_mixing_ratio:un_H - co2_mixing_ratio:un_co2_flux - co2_mixing_ratio:w_div_ts_cov - co2_mixing_ratio:co2...125 - 
            T_star_:co2...125 - un_H:un_co2_flux - un_H:w_div_ts_cov - un_co2_flux:w_div_ts_cov - un_co2_flux:co2...125 - 
            w_div_ts_cov:co2...125, data = teaching_tbl)
coef(model6)
resid(model6)
confint(model6)
summary(model6)
anova(model6)
anova(model5, model6)

model7=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov)^2 - H:co2_molar_density - H:rand_err_co2_flux - H:w_div_ts_cov - H:co2_mole_fraction -
            H:un_co2_flux - H:co2_mixing_ratio - rand_err_co2_flux:co2_mole_fraction -
            rand_err_co2_flux:un_H - rand_err_co2_flux:un_co2_flux - rand_err_co2_flux:w_div_ts_cov - rand_err_co2_flux:co2_mixing_ratio - 
            co2_molar_density:co2_mole_fraction - co2_molar_density:co2_mixing_ratio - co2_molar_density:un_H - co2_molar_density:un_co2_flux - co2_molar_density:w_div_ts_cov - 
            co2_molar_density:co2...125 - co2_mole_fraction:co2_mixing_ratio - co2_mole_fraction:un_H - co2_mole_fraction:un_co2_flux - 
            co2_mole_fraction:w_div_ts_cov - co2_mole_fraction:T_star_  - co2_mole_fraction:co2...125 - co2_mixing_ratio:T_star_ - T_star_:w_div_ts_cov -
            co2_mixing_ratio:un_H - co2_mixing_ratio:un_co2_flux - co2_mixing_ratio:w_div_ts_cov - co2_mixing_ratio:co2...125 - 
            T_star_:co2...125 - un_H:un_co2_flux - un_H:w_div_ts_cov - un_co2_flux:w_div_ts_cov - un_co2_flux:co2...125 - 
            w_div_ts_cov:co2...125, data = teaching_tbl)
coef(model7)
resid(model7)
confint(model7)
summary(model7)
anova(model7)
anova(model6, model7)

model8=lm(co2_flux ~ (H  + rand_err_co2_flux + co2_molar_density + 
                        co2_mole_fraction + co2_mixing_ratio + T_star_ + un_H + un_co2_flux + 
                        w_div_ts_cov)^2 - H:co2_molar_density - H:rand_err_co2_flux - H:w_div_ts_cov - H:co2_mole_fraction -
            H:un_co2_flux - H:co2_mixing_ratio - rand_err_co2_flux:co2_mole_fraction -
            rand_err_co2_flux:un_H - rand_err_co2_flux:un_co2_flux - rand_err_co2_flux:co2_molar_density - rand_err_co2_flux:w_div_ts_cov - rand_err_co2_flux:co2_mixing_ratio - 
            co2_molar_density:co2_mole_fraction - co2_molar_density:co2_mixing_ratio - co2_molar_density:un_H - co2_molar_density:un_co2_flux - co2_molar_density:w_div_ts_cov - 
            co2_molar_density:co2...125 - co2_mole_fraction:co2_mixing_ratio - co2_mole_fraction:un_H - co2_mole_fraction:un_co2_flux - 
            co2_mole_fraction:w_div_ts_cov - co2_mole_fraction:T_star_  - co2_mole_fraction:co2...125 - co2_mixing_ratio:T_star_ - T_star_:w_div_ts_cov -
            co2_mixing_ratio:un_H - co2_mixing_ratio:un_co2_flux - co2_mixing_ratio:w_div_ts_cov - co2_mixing_ratio:co2...125 - 
            T_star_:co2...125 - un_H:un_co2_flux - un_H:w_div_ts_cov - un_co2_flux:w_div_ts_cov - un_co2_flux:co2...125 - 
            w_div_ts_cov:co2...125, data = teaching_tbl)
coef(model8)
resid(model8)
confint(model8)
summary(model8)
anova(model8)
anova(model7, model8)

#модель номер 8 является оптимальной
