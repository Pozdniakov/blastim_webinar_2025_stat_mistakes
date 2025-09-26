# Circular analysis (круговой анализ)

# Параметры симуляции -----------------------------------------------------

set.seed(0)
n <- 40 #размер нашей "выборки
sd_between_students <- 5 #разброс между студентами
sd_within_students <- 4 #разброс "внутри" студентов, т.е. между пробами
students_sim_n <- 1000 #сколько потянет компьютер, чем больше, тем лучше

# Симулируем --------------------------------------------------------------

students <- tibble(mean_student = rnorm(n, sd = sd_between_students),
       before = rnorm(n, mean = mean_student, sd = sd_within_students),
       after = rnorm(n, mean = mean_student, sd = sd_within_students),
       group = if_else(before < median(before), "lower", "upper"))

# Усредняем (для графика) -------------------------------------------------

students_average <- students %>%
  group_by(group) %>%
  summarise(before = mean(before),
            after = mean(after))

# Рисуем ------------------------------------------------------------------

ggplot(data = students, aes(colour = group)) +
  geom_segment(aes(x = 1, xend = 2, y = before, yend = after)) +
  geom_point(aes(x = 1, y = before)) +
  geom_point(aes(x = 2, y = after)) +
  geom_segment(data = students_average, aes(x = 1, xend = 2, y = before, yend = after), size = 2.5) +
  geom_point(data = students_average, aes(x = 1, y = before), size = 5) +
  geom_point(data = students_average, aes(x = 2, y = after), size = 5) +
  labs(
    title = str_glue("Демонстрация кругового анализа на случайном шуме"),
    subtitle = str_glue(
      "{n} студентов делятся на две группы по медиане, SD между студентами = {sd_between_students}, SD внутри студентов = {sd_within_students}"),
    x = "Время",
    y = "DV"
  ) +
  scale_x_continuous(breaks = 1:2, labels = c("before", "after"), expand = expansion(mult = 0.2, add = 0)) +
  ggsci::scale_color_aaas() +
  theme_linedraw()

# Проводим т-тест на разницу до и после для обоих групп -------------------

students %>%
  group_by(group) %>%
  group_map(~ t.test(.x$before, .x$after, paired = TRUE))

# Повторяем это много-много раз -------------------------------------------


p_matrix <- replicate(students_sim_n,
  {students <- tibble(mean_student = rnorm(n, sd = sd_between_students),
                      before = rnorm(n, mean = mean_student, sd_within_students),
                      after = rnorm(n, mean = mean_student, sd = sd_within_students),
                      group = if_else(before < median(before), "lower", "upper"))
  students %>%
  group_by(group) %>%
  group_map(~ t.test(.x$before, .x$after, paired = TRUE)) %>%
  map("p.value")})


# Как часто мы получаем ложноположительный результат ----------------------

mean(p_matrix < .05)
apply(p_matrix, 1, function(x) mean(x < .05))
