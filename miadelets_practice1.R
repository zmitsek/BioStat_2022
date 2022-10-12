# ---
# title: Знакомство с R.
# ---

# This file uses UTF-8 encoding.
# Reopen it if Cyrillic letters are not displayed properly.
# (In RStudio: File -> Reopen with encoding)
# More information you could find in the presentation.

#### Организация рабочего пространства ##################################

# 1. Создайте папку, где будут храниться ВСЕ материалы курса. В эту папку помещайте ВСЕ файлы с кодом (с расширением .R, а в последствии и .Rmd).
# 2. Внутри папки `Statistiks-and-R-programming` создайте папку `data`, где будут храниться все файлы с данными для анализа.
# В итоге у вас должно получиться примерно это (если у вас операционная система Windows):
# C:\Statistiks-and-R-programming\
# C:\Statistiks-and-R-programming\data\


#### Полезные клавиатурные сокращения в RStudio ##################################
#
# - `Ctrl + Shift + C` - закомментировать/раскомментировать
# выделенный фрагмент кода
# - `Ctrl + Enter` - выполнить активную строку
# - `Tab` или `Ctrl + Space` - автоподстановка
# 
#### Настройки окружения ##################################
#  Задать рабочую директорию
#  
#Открыть файл
getwd()
setwd('C:/Statistiks-and-R-programming/data')
path_to_file <- 'mc_donalds_data.csv'
mc_menu_data <- read.csv(path_to_file,
  header = TRUE, sep = ',', encoding = 'UTF-8')

str(mc_menu_data)

#Можно использовать специальный пакет, если открывать файлы в формате Excel
library(readxl)
require(readxl)

mc_menu_data_from_exel <- read_excel('mc_donalds_data.xlsx', sheet = 2, na = 'NA')

#Структура данных
str(mc_menu_data_from_exel)
#Сколько всего наблюдений
nrow(mc_menu_data)

#Сколько признаков
ncol(mc_menu_data) - 1

#Сколько наблюдений в каждой категории и какая категория больше всего представлена
which.max(table(mc_menu_data$Category))

#Проверить на наличие NA и удалить их (что не есть самое лучшее решение)
colSums(is.na(mc_menu_data))
mc_mendu_data_withoutNA <- na.omit(mc_menu_data)

#Отобрать 27 значение 5 столбца
mc_mendu_data_withoutNA[27, 5]

### TASK 1 

#1.1 Отобрать все данные по категории "Snacks & Sides" (2 балла)
snacks_and_sides <- subset(mc_menu_data, Category == 'Snacks & Sides')

#1.2 Отобрать все данные по категориям "Desserts" и "Salads" (2 балла)
desserts_with_salads <- subset(mc_menu_data, Category == 'Desserts' |
                                 Category == "Salads")
#1.3 Отобрат все данные кроме категории "Breakfast" (2 балла)
no_breakfast <- subset(mc_menu_data, Category != "Breakfast")
#1.4 Посчитать число блюд, где калорийность больше чем 500 (2 балла)
?sum(mc_menu_data$Calories > 500, na.rm=TRUE)
#1.5 Найти самое калорийное блюдо (2 балла)
max_calories <- mc_menu_data[which.max(mc_menu_data$Calories), ]

#Посчитать среднее и медиану для каждого столбца
str(mc_mendu_data_withoutNA)
mc_data_withoutNA_and_sting <- mc_mendu_data_withoutNA[, -c(1:3)]
apply(mc_data_withoutNA_and_sting, 2, mean)

#Сохранить файл
write.csv(mc_mendu_data_withoutNA, file = 'mac_data_NA_omit.csv', append = FALSE, row.names = FALSE, sep = ',')
#### Пользовательские функции ##################################

#Правильный и классический вариант (всё внутри функции)
kkal_to_joul <- function(kkal){
  one_joul<-4184
  joul<-kkal * one_joul
  joul
  return('None')
}


#Взаимодействие с глобальным окружением
one_joul<-4184
kkal_to_joul <- function(kkal){
  joul<-kkal * one_joul
  joul
}

#Приоритет локальной переменной перед глобальной
one_joul<-0
kkal_to_joul <- function(kkal){
  one_joul<-4184
  joul<-kkal * one_joul
  joul
}


#### Генерация NA ##################################

#Зерно генератора случайных чисел
set.seed(392408154)

#Выбираем 200 случайных ячеек
N <- prod(dim(mac_data_no_text))
id <- sample(1:N, size = 200)

#Отбираем численную часть датасета
spect <- as.matrix(mac_data_no_text)

#Заполняем случайные ячейки NA
spect[id] <- NA
mac_data_with_NA <- mac_data
mac_data_with_NA[,c(4:16)] <- spect

#Заменить пропущенные значения NA на средние
library(Hmisc)
mac_data_mean_NA_part <- t(apply(X = mac_data_with_NA[,c(4:16)], MARGIN = 1, FUN = impute, fun = mean))
mac_data_mean_NA <- mac_data_with_NA
mac_data_with_NA[,c(4:16)] <- mac_data_mean_NA_part 


####  Продвинутый R ##################################

# # Необходимые пакеты для работы в этом разделе
# ggplot2 и dplyr
# 
# 
# #Tidy data
# 
# Как сделать данные из Mc'Donalds красивыми
library(dplyr)

md <- read.csv('menu.csv')

# Вариант отбора данных с использованием функции subset()

new_md <- md %>% select(-ends_with("Value."))
mac_data <- cbind(new_md, md %>% select(starts_with("Vit")))

colnames(mac_data) <- c("Category", "Item", "Serving_size", "Calories", "Cal_Fat", "Total_Fat", "Satur_Fat", "Trans_Fat", "Cholesterol", "Sodium","Carbohydrates", "Dietary_fiber","Sugars", "Protein", "Vitamin_A_DV", "Vitamin_C_DV")


# # Практика по dplyr

# Отберите только названия блюд с числом калорий больше 360
dplyr_task_1 <- mac_data %>% filter(Calories > 360)

### TASK 2 

#2.1 Найдите число блюд в каждой из категорий, для которых витаминная ценность равна нулю (2 балла)
no_vitamin <- mac_data %>% filter(Vitamin_A_DV == 0 &  Vitamin_C_DV == 0) %>%
  group_by(Category) %>% count(Category)
#2.2 Посчитайте долю калорий, приходящихся на жиры, для блюд из завтраков и округлите значения до 3 знака (2 балла)
fats_in_breakfast <- mac_data %>% filter(Category == 'Breakfast') %>%
  mutate(proportion_of_fat = round(Cal_Fat/Calories, digits = 3))
#2.3 Посчитайте среднее значение, медиану и разницу между ними для холестерола (2 балла)
cholesterol_data <- mac_data %>% summarise('median_Cholesterol' = median(Cholesterol),
                                           'mean_Cholesterol' = mean(Cholesterol),
                                           'diff' = mean(Cholesterol) - median(Cholesterol))
#2.4 Для каждой категории найдите блюдо с самым высоким отношением сахаров к углеводам (2 балла)
max_sugar_per_carbohydrates <-  mac_data %>% group_by(Category) %>%
    filter(Sugars/Carbohydrates == max(Sugars/Carbohydrates))




# # Графика в R

# Базовая графика
data("mtcars")
attach(mtcars)
boxplot(mpg~cyl, 
        xlab="Cylinders", ylab="Miles/(US) gallon", 
        col=topo.colors(3))

legend("bottomleft", inset=.02, title="Number of Cylinders",
       c("4","6","8"), fill=topo.colors(3), horiz=TRUE, cex=0.8)

# #Графика в ggplot2
library(ggplot2)
str(mtcars)
mtcars$cyl <- as.factor(mtcars$cyl)
ggplot(mtcars, aes(cyl, mpg, fill = cyl)) + 
  geom_boxplot()+
  theme_bw() + 
  xlab(label = "Cylinders") + 
  ylab(label = "Miles/(US)gallon") + 
  labs(fill= "Number \nof cylinders") +
  scale_fill_manual(values = c("blue", "green", "yellow")) + 
  theme(text = element_text(size=20)) + 
  theme(legend.position = c(0.2, 0.2))


# # Практика по ggplot2
library(ggplot2)
# Загрузим необходимые пакеты
library(RColorBrewer)
library(DAAG)

# Загрузим данные и немного их отредактируем
data("leafshape")
leafshape$arch <- factor(leafshape$arch, labels = c("Plagiotropic", "Orthotropic"))

# Нарисуем графики, демонстрирующие то, как длина листа взаимосвязана с шириной листа
ggplot(leafshape, aes(x = bladelen, y = bladewid)) + 
  geom_point(color = "blue")

# Цвет по переменной в графике
ggplot(leafshape, aes(bladelen, bladewid, color = location)) + 
  geom_point()

ggplot(leafshape, aes(x = location, y = bladelen, colour = location))+
  geom_boxplot()

# Разделение графика на несколько по переменной
ggplot(leafshape, aes(bladelen, bladewid, color = location)) + 
  geom_point() +
  facet_grid(.~location)

### TASK 3

#3.1 Распределение (boxplot) длины листа, в зависимости от локации и архитектуры (5 баллов)
ggplot(leafshape, aes(x = bladelen , y = location , color = arch )) + 
  geom_boxplot() + 
  labs(x = 'Leaf length', y ='Location', title = 'Leaf parameters',
       color = "Architecture", caption = 'from leafshape dataset') + 
  theme(text = element_text(size=20)) + 
  theme_bw(16)  


# Больше графиков богу графиков
#3.2 Сложный график -- violin plot, с разделением по архитекутре листьев (5 баллов)
ggplot(leafshape, aes (x = location, y = bladelen, color = I('black'), fill = location)) + 
  geom_violin() + 
  facet_grid(.~arch) + 
  labs( x = 'Location/architecture', y = 'Leaf length',
        title = 'Leaf parameters', fill = "Location", caption = 'from leafshape dataset') + 
  theme_bw() + 
  scale_fill_brewer(palette = "Dark2") + 
  theme(text = element_text(size=20)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
