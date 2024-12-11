

time = 50  
lymbda = 2  # Интенсивность распределения
n = 1000 # Количество реализаций

# вектор для N(t)
event_counts = numeric(n)

for (sim in 1:n) {
  total_time <- 0   # Суммарное время в процессе
  event_count <- 0  # Счетчик событий
  
  # Моделирование пуассоновского процесса
  while (total_time < time) {
    interval <- rexp(1, rate = lymbda)  # Генерация интервала
    total_time <- total_time + interval
    if (total_time < time) event_count <- event_count + 1
  }
  event_counts[sim] <- event_count
}


# Гистограмма 
hist(event_counts, probability = TRUE,main = "Распределение N(t)",
     xlab = "N(t)",ylab="Частота", col = "lightgray")
curve(((lymbda * time)^x / factorial(x)) * exp(-lymbda * time),
                       add = TRUE, col = "darkgreen", lwd = 2)



# Параметры для процесса капитала
initial_capital = 50  # Начальный капитал
claim_rate = 2   # Интенсивность страховых случаев
avg_claim = 1         # Средняя величина страховой выплаты
time_horizon = 100     # Горизонт времени

# Случай  когда условие выполняется
premium_rate_a = 3  # Премии больше выплат
capital_a = c(initial_capital)  # Капитал на каждом шаге
times_a = c(0)  # Времена страховых случаев


while (sum(times_a) < time_horizon) {
  # Генерация времени до следующего события
  time_to_next <- rexp(1, rate = claim_rate)
  times_a <- c(times_a, time_to_next)  # Добавляем время
  
  # Генерация размера страховой выплаты
  claim_amount <- rexp(1, rate = 1 / avg_claim)
  
  # Обновление капитала
  new_capital <- capital_a[length(capital_a)] + premium_rate_a * time_to_next - claim_amount
  capital_a <- c(capital_a, new_capital)
}


plot(cumsum(times_a), capital_a, type = "l", col = "darkblue",
     main = "условие выполняется", xlab = "Время", ylab = "Капитал")


# условие не выполняется
premium_rate_b = 1.5  # Премии меньше выплат
capital_b = c(initial_capital)  # Капитал на каждом шаге
times_b = c(0)  # Времена страховых случаев

# Симуляция процесса капитала для случая (b)
while (sum(times_b) < time_horizon) {
  # Генерация времени до следующего события
  time_to_next <- rexp(1, rate = claim_rate)
  times_b <- c(times_b, time_to_next)  # Добавляем время
  
  # Генерация размера страховой выплаты
  claim_amount <- rexp(1, rate = 1 / avg_claim)
  
  # Обновление капитала
  new_capital <- capital_b[length(capital_b)] + premium_rate_b * time_to_next - claim_amount
  capital_b <- c(capital_b, new_capital)
}

# Построение графика для случая (b)
plot(cumsum(times_b), capital_b, type = "s", col = "red",main = " условие  не выполняется",
     xlab = "Время", ylab = "Капитал")
abline( h = 0, clr = "black",lwd = 1 )

# —-— Задание 4 —---
# Параметры для моделирования вероятности разорения
num_trials = 1000          # Количество реализаций
time_horizon = 1000        # Горизонт времени
initial_capital = 100      # Начальный капитал
premium_rate = 1           # Ставка премий
claim_intensity = 0.3      # Интенсивность страховых случаев
avg_claim = 3             # Средняя выплата

# Инициализация вектора для хранения разорений
ruin_count = 0  # Счетчик разорений

# Цикл для моделирования каждой реализации
for (trial in 1:num_trials) {
  current_time <- 0  # Начальное время
  capital <- initial_capital  # Текущий капитал компании
  
  # Симуляция процесса капитала
  while (current_time < time_horizon && capital >= 0) {
    # Генерация времени до следующего события
    time_to_next <- rexp(1, rate = claim_intensity)
    current_time <- current_time + time_to_next  # Обновляем время
    
    # Проверяем, не вышло ли время за пределы горизонта
    if (current_time >= time_horizon) break
    
    # Генерация размера страховой выплаты
    netto_prem <- rexp(1, rate = 1 / avg_claim)
    
    # Обновление капитала
    capital <- capital + premium_rate * time_to_next - netto_prem
    
    # Проверка разорения
    if (capital < 0) {
      ruin_count <- ruin_count + 1  # Фиксируем разорение
      break
    }
  }
}

# Расчет вероятности разорения
ruin_probability <- ruin_count / num_trials

# Вычисление  вероятности разорения по Лундбергу
p = premium_rate / (claim_intensity * avg_claim) - 1
lundberg = exp(-(1 / avg_claim) * (p / (1 + p)) * initial_capital)

# Вывод результатов
cat("Выборочная вероятность разорения:", ruin_probability, "\n")
cat("Вероятности по Лундбергу:", lundberg, "\n")
