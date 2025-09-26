# Инфляция единиц анализа (нарушение независимости наблюдений)


# Задаем параметры симуляции ----------------------------------------------


set.seed(0)
n_subjects_in_group <- 5 #сколько мышей в одной группе, всего мышей х2
n_trials <- 10 #сколько проб собираем с одной мышки
sd_between_mouse <- 5 #насколько мышки между собой различаются
sd_within_mouse <- 1 #насколько различаются различные пробы у одной мышки
mouse_sim_n <- 1000 #сколько потянет мой компьютер перезапускать симуляцию

# Симулируем --------------------------------------------------------------

mouse <- tibble(subject = rep(1:(n_subjects_in_group * 2), each = n_trials), #айди "испытуемых"
                group = rep(str_glue("Группа {LETTERS[1:2]}"), each = n_subjects_in_group * n_trials), #Разделяем по группам a и b
                mean_mouse = rep(rnorm((n_subjects_in_group * 2), mean = 0, sd = sd_between_mouse), each = n_trials), #симулируем среднее по мышке
                DV = rnorm(n_subjects_in_group * 2 * n_trials, mean = mean_mouse, sd = sd_within_mouse)) #симулируем различные пробы на основе средних


# Тестируем ---------------------------------------------------------------


t.test(DV ~ group, data = mouse, var.equal = TRUE)

# Рисуем ------------------------------------------------------------------


ggplot(data = mouse, aes(x = subject, y = DV)) +
  geom_point(position = position_jitter(width = .1)) +
  facet_wrap(~group, scales = "free_x") +
  labs(title = str_glue("Симуляция двух групп мышей"),
       subtitle = str_glue("{n_subjects_in_group} мышей в группе, {n_trials} проб, SD между мышами = {sd_between_mouse}, SD внутри мышей = {sd_within_mouse}"))+
  theme_linedraw()

# Повторяем симуляцию много-много раз и считаем с каждой p-value ----------

many_p <- replicate(mouse_sim_n, {
  mouse <- tibble(subject = rep(1:(n_subjects_in_group * 2), each = n_trials), #айди "испытуемых"
                  group = rep(letters[1:2], each = n_subjects_in_group * n_trials), #Разделяем по группам a и b
                  mean_mouse = rep(rnorm((n_subjects_in_group * 2), mean = 0, sd = sd_between_mouse), each = n_trials), #симулируем среднее по мышке
                  DV = rnorm(n_subjects_in_group * 2 * n_trials,  mean = mean_mouse, sd = sd_within_mouse)) #симулируем различные пробы на основе средних

  t.test(DV ~ group, data = mouse, var.equal = TRUE)$p.value
})

# Как часто p-value меньше .05?

mean(many_p < .05)


# А как правильно ---------------------------------------------------------

# 1 вариант: усреднить по мышам и провести т-тест

mouse_average <- mouse %>%
  group_by(subject, group) %>%
  summarise(DV_mean = mean(DV), .groups = "drop")

t.test(DV_mean ~ group, data = mouse_average, var.equal = TRUE)

# Симулируем...

many_p <- replicate(mouse_sim_n, {
  mouse <- tibble(subject = rep(1:(n_subjects_in_group * 2), each = n_trials), #айди "испытуемых"
                  group = rep(letters[1:2], each = n_subjects_in_group * n_trials), #Разделяем по группам a и b
                  mean_mouse = rep(rnorm((n_subjects_in_group * 2), mean = 0, sd = sd_between_mouse), each = n_trials), #симулируем среднее по мышке
                  DV = rnorm(n_subjects_in_group * 2 * n_trials,  mean = mean_mouse, sd = sd_within_mouse)) #симулируем различные пробы на основе средних
  mouse_average <- mouse %>%
    group_by(subject, group) %>%
    summarise(DV_mean = mean(DV), .groups = "drop")
  t.test(DV_mean ~ group, data = mouse_average, var.equal = TRUE)$p.value
})

# как часто мы получаем ложно-положительные результаты?

mean(many_p < .05)

# Столько сколько надо!
# 2 вариант: Linear Mixed Effects Modelling - нам не нужно усреднять!
# нужны пакеты lme4 (для модели) и lmerTest (для p-value)

lmer(DV ~ group + (1|subject), data = mouse) %>%
  summary()

