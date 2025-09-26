set.seed(0)

small_n <- 30
big_n <- 3000
small_eff <- 5
big_eff <- 1
iq_sim_n <- 1000


# Маленькая выборка, но большой эффект ------------------------------------

# Симулируем маленькую выборку!
iq_small_sample <- tibble(basic = rnorm(small_n, mean = 100, sd = 15),
       smarter = rnorm(small_n, mean = 100 + small_eff, sd = 15))

# Превращаем в длинный формат для визуализации
iq_small_sample_long <- iq_small_sample %>%
  pivot_longer(cols = basic:smarter, names_to = "group", values_to = "IQ")

# Визуализируем

p_small <- iq_small_sample_long %>%
  ggplot(aes(x = IQ, fill = group)) +
  geom_histogram(bins = 20, position = "identity", alpha = .4, colour = "grey5") +
  geom_vline(data = iq_small_sample_long %>% group_by(group) %>% summarise(IQ = mean(IQ)),
             aes(xintercept = IQ, colour = group), linetype = "dashed", linewidth = 1)+
  ggsci::scale_fill_cosmic() +
  ggsci::scale_colour_cosmic() +
  coord_cartesian(xlim = c(50, 150)) +
  labs(title = str_glue("Симуляция IQ у двух МАЛЕНЬКИХ групп\nРазмер эффекта БОЛЬШОЙ"),
       subtitle = str_glue("{small_n} человек в группе, истинная разница средних = {small_eff}")) +
  theme_minimal()

p_small

# Проводим т-тест

t.test(iq_small_sample$basic, iq_small_sample$smarter, var.equal = TRUE)

# То же самое, но в одну строчку

t.test(rnorm(small_n, mean = 100, sd = 15), rnorm(small_n, mean = 100 + small_eff, sd = 15), var.equal = TRUE)

# А теперь повторяем это много-много раз

iq_small_sample_p <- replicate(iq_sim_n, {
  t.test(
    rnorm(small_n, mean = 100, sd = 15),
    rnorm(small_n, mean = 100 + small_eff, sd = 15),
    var.equal = TRUE
  )$p.value
})

# Как часто мы получаем статистически значимый результат?
mean(iq_small_sample_p < .05)


# Большая выборка, но маленький эффект ------------------------------------

# Симулируем большую выборку

iq_big_sample <- tibble(basic = rnorm(big_n, mean = 100, sd = 15),
                        smarter = rnorm(big_n, mean = 100 + big_eff, sd = 15))

# Превращаем в длинный формат для визуализации

iq_big_sample_long <- iq_big_sample %>%
  pivot_longer(cols = basic:smarter, names_to = "group", values_to = "IQ")

# Рисуем результаты

p_big <- iq_big_sample_long %>%
  ggplot(aes(x = IQ, fill = group)) +
  geom_histogram(bins = 20, position = "identity", alpha = .4, colour = "grey5") +
  geom_vline(data = iq_big_sample_long %>% group_by(group) %>% summarise(IQ = mean(IQ)),
             aes(xintercept = IQ, colour = group), linetype = "dashed", linewidth = 1)+
  ggsci::scale_fill_cosmic() +
  ggsci::scale_colour_cosmic() +
  coord_cartesian(xlim = c(50, 150)) +
  labs(title = str_glue("Симуляция IQ у двух БОЛЬШИХ групп\nРазмер эффекта МАЛЕНЬКИЙ"),
       subtitle = str_glue("{big_n} человек в группе, истинная разница средних = {big_eff}")) +
  theme_minimal()

p_big

# Склеиваем графики вместе!

p_small / p_big # Это из пакет patchwork

# Теперь проводим т-тест на большой выборке с маленьким эффектом

t.test(iq_big_sample$basic, iq_big_sample$smarter, var.equal = TRUE)

# То же самое, в одну строчку

t.test(rnorm(big_n, mean = 100, sd = 15), rnorm(big_n, mean = 100 + big_eff, sd = 15), var.equal = TRUE)

# То же самое, но много-много раз

iq_big_sample_p <- replicate(iq_sim_n, {
  t.test(
    rnorm(big_n, mean = 100, sd = 15),
    rnorm(big_n, mean = 100 + big_eff, sd = 15),
    var.equal = TRUE
  )$p.value
})

# Как часто мы получаем статистически значимый результат?
mean(iq_big_sample_p < .05)

