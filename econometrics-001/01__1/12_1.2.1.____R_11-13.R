# if you see KRAKOZYABRY then do 
# File-Reopen with encoding - UTF-8 - (Set as default) - OK

# задаём векторы:
x <- c(23,15,46,NA)
z <- c(5,6,NA,8)

# NA --- пропущенное значение

# среднее арифметическое
mean(x)

# среднее арифметическое с удалением пропущенных значений
mean(x,na.rm = TRUE)
mean(z,na.rm = TRUE)

# сумма всех элементов вектора
sum(x)
sum(x,na.rm = TRUE)

# создаем табличку с данными с именем d
d <- data.frame(rost=x,ves=z)
d


d[4,1] # элемент в 4-ой строке и 1-м столбце
d[3,1]

d[2,] # вся вторая строка
d[,2] # весь второй столбец

d$rost # столбец rost
d$ves # столбец ves

# создаем список из трёх совершенно различных объектов
my_list <- list(a=7,b=10:20,table=d)

# достаем из списка объекты по имени
my_list$a
my_list$b
my_list$table
d$rost

# список немного отличается от вектора 
# выбрать второй элемент списка:
my_list[[2]]

# Ура!


