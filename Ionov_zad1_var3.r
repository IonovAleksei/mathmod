#Ионов Алексей Алексеевич — для региона 45 рассчитайте урожайность пшеницы в 2013 году,
#взяв для рассчета средние суммы активных температур за предыдущие 25 лет,
#с 2 ближайших метеостанций но рассчитав колонку di самостоятельно, как долю месяца,
#когда среднедневные температуры были выше 7 градусов, но учитывая,
#что посев не может начаться раньше середины апреля, а вегетация составляет 4 месяца

rm(list=ls())
library(rnoaa)
library(dplyr)
library(ggplot2) 
library(tidyverse)
library(lubridate)

station_data = read.csv("station_data.csv")
#После получения всписка всех станций, извлекаем список станций ближайших к городу Курган,
#создав таблицу с именем региона и координатами его столицы
kurgan = data.frame(id = "KURGAN", latitude = 55.45,  longitude = 65.3333)
k_around = meteo_nearby_stations(lat_lon_df = kurgan, station_data = station_data,
                                 limit = 2, var = c("PRCP","TAVG"),
                                 year_min = 1987, year_max = 2013)
#k_around это список единственным элементом которого является таблица, содержащая идентификаторы 
#метеостанций отсортированных по их удалленности от Уфы
#Получаем данные со всех станций
k_id = k_around[["KURGAN"]][["id"]][1]
str(k_around)
all_k_data = meteo_tidy_ghcnd(stationid = k_id)
#Чтобы получить таблицу всех метеостанций вокруг Кургана нужно выбрать целиком первый объект из списка
k_table = k_around[[1]]
summary(k_table)
#Нужно создать цикл, в котором бы скачивались  нужные данные для всех метеостанций из созданного списка
#Создадим промежуточный объект, куда будем скачивать данные с конкретной метеостанции
all_i = data.frame()
#Создадим объект, куда скачаем все данные всех метеостанций
all_k = data.frame()
#Цикл для всех метеостанций
for(i in 1:2) 
{all_i  = meteo_tidy_ghcnd(stationid =  k_around[["KURGAN"]][["id"]][i])
 all_i = all_i[ ,c("id","date","tavg")] 
 print(all_i)
 all_k=rbind(all_k, all_i)}
#Рассчитаем коэффициент di, как долю месяца,
#когда среднедневные температуры были выше 7 градусов, но учитывая,
#что посев не может начаться раньше середины апреля, а вегетация составляет 4 месяца
tdi = all_k %>% mutate(date=ymd(date),
                   year=year(date),
                   month=month(date)) %>%
  group_by(year,month) %>%
  mutate(dm = n(),
         gd = case_when(
           tavg >= 70 ~ T,
           tavg <70 ~ F
           )) %>%  summarise(di = sum(gd)/mean(dm))

tdi = tdi %>% filter(year>1986 & year < 2013) %>% ungroup() %>% group_by(month) %>%
  summarise(di = mean(di))
tdi$di[0:3]=0
tdi = tdi %>% mutate(tdis = cumsum(di))
tdi = tdi %>% mutate(di = case_when(tdis > 4 ~ di - (tdis-4),TRUE ~ di))
tdi = tdi %>% mutate(di = case_when(di < 0 ~ 0,TRUE ~ di))
#Рассчитаем среднюю сумму активных температур для двух метеостанций, для Курганской области за 25 лет
data_k = all_k %>% 
  mutate(date=ymd(date),
         year=year(date),
         month=month(date)) %>%
  mutate(tavg=case_when( tavg<50 ~ 0, TRUE ~ tavg)/10) %>% 
  filter (year>1986 & year < 2013) %>% 
  group_by(id,year,month) %>% 
  summarize(tsum = sum(tavg)) %>% 
  group_by(month) %>% summarize(St = mean(tsum))
#Создадим и считаем фал CSV с коэффициентами и внесем остальные
#коэффициенты для расчета урожайности пшеницы 
coef=read.csv("coef.csv", header = TRUE,
              sep = ";", dec = ",")

y=1.0
Kf=300
Qj=1600
Lj=2.2
Ej=25
#Рассчитаем урожайность по месяцам
data_k= data_k %>% 
  mutate(Fi=(coef$afi)+(coef$bfi)*y*(data_k$St))
data_k= data_k %>% mutate(Yj=(((data_k$Fi)*(tdi$di)*Kf)/(Qj*Lj*(100-Ej))))
#Расчитываем суммарную урожайность как сумму по месяцам
YIELD=sum(data_k$Yj);YIELD
# Ответ: 18,7 ц/га
#По данным аналитики в 2013 году средняя урожайность в регионе 
#составляла  14,9 ц/га полученный ответ вполне адекватен
