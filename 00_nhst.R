# Подключаем пакеты! ------------------------------------------------------

library(tidyverse)
library(patchwork)
library(lme4)
library(lmerTest)

# Отключаем научную запись числа ------------------------------------------

options(scipen = 999)

# Задаем параметры симуляции ----------------------------------------------
n <- 40
eff <- .5
sim_n <- 10000

# Эффекта нет -------------------------------------------------------------

set.seed(0)
t.test(rnorm(n), rnorm(n), var.equal = TRUE)

many_p <- replicate(sim_n, t.test(rnorm(n), rnorm(n), var.equal = TRUE)$p.value)
mean(many_p < .05)
ggplot(data.frame(many_p), aes(x = many_p)) +
  geom_histogram(bins = 30, fill = "#F0AB00FF", colour = "#C50084FF", breaks = seq(0, 1, .01)) +
  geom_vline(xintercept = .05, colour = "#2A6EBBFF", size = 1.2) +
  annotate("text", label = str_glue("← p-value = .05"),
           x = .05, y = sim_n/10 * .75, size = 6, colour = "#2A6EBBFF", hjust = -0.05
  ) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  ylim(c(0, sim_n/10)) +
  labs(
    title = str_glue("Распределение p-value, когда эффекта НЕТ"),
    subtitle = str_glue(
      "{sim_n} t-testов на выборках по {n} значений"
    ),
    x = "p-value"
  ) +
  theme_minimal()

# Эффект есть -------------------------------------------------------------

t.test(rnorm(n), rnorm(n, mean = eff), var.equal = TRUE)

many_p_eff <- replicate(sim_n, t.test(rnorm(n), rnorm(n, mean = eff), var.equal = TRUE)$p.value)
mean(many_p_eff < .05)
ggplot(data.frame(many_p_eff), aes(x = many_p_eff)) +
  geom_histogram(bins = 30, fill = "#F0AB00FF", colour = "#C50084FF", breaks = seq(0, 1, .01)) +
  geom_vline(xintercept = .05, colour = "#2A6EBBFF", size = 1.2) +
  annotate("text", label = str_glue("← p-value = .05"),
           x = .05, y = sim_n/2 * .75, size = 6, colour = "#2A6EBBFF", hjust = -0.05
  ) +
  scale_x_continuous(breaks = seq(0, 1, .1)) +
  ylim(c(0, sim_n/2)) +
  labs(
    title = str_glue("Распределение p-value, когда эффект ЕСТЬ"),
    subtitle = str_glue(
      "{sim_n} t-testов на выборках по {n} значений, размер эффекта d = {eff}"
    ),
    x = "p-value"
  ) +
  theme_minimal()
