set.seed(0)
tests_n <- 20
respondents_n <- 100


psych_diploma <- as.data.frame(matrix(rnorm(tests_n * respondents_n), nrow = respondents_n))
cors <- psych::corr.test(psych_diploma, adjust = "none")

((tests_n ^ 2) - tests_n)/2

((tests_n ^ 2) - tests_n)/2 * .05

library(corrplot)
corrplot(cors$r, method = "color", p.mat = cors$p)

(sum(cors$p < .05) - tests_n)/2

false_alarms <- replicate(1000,
          {
            psych_diploma <- as.data.frame(matrix(rnorm(tests_n * respondents_n), nrow = respondents_n))
            cors <- psych::corr.test(psych_diploma, adjust = "none")
            (sum(cors$p < .05) - tests_n)/2
          })

ggplot(data.frame(false_alarms), aes(x = false_alarms)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, colour = "#2A6EBBFF") +
  geom_histogram(binwidth = 1, colour = "#C50084FF", fill = "#F0AB00FF") +
  geom_vline(xintercept = ((tests_n ^ 2) - tests_n)/2 * .05, linewidth = 1.5, colour ="grey5") +
  annotate("text", label = str_glue("← Мы ожидаем {((tests_n ^ 2) - tests_n)/2 * .05}\n   cтатистически значимых\n   результатов в среднем"),
           x = ((tests_n ^ 2) - tests_n)/2 * .05, y = max(table(false_alarms)) * .75, size = 5, colour = "#C50084FF", hjust = -0.05
  ) +
  labs(
    title = str_glue("Симуляция студента-психолога и его множественных сравнений"),
    subtitle = str_glue(
      "{tests_n} тестов → {((tests_n ^ 2) - tests_n)/2} попарных корреляций"
    ),
    x = "Количество гипотез",
    y = "Статистически значимые гипотезы"
  ) +
  theme_minimal()

ggplot(data.frame(false_alarms), aes(x = false_alarms)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 1, colour = "#2A6EBBFF") +
  geom_vline(xintercept = ((tests_n ^ 2) - tests_n)/2, linetype = "dashed", linewidth = 1, colour = "#2A6EBBFF") +
  geom_histogram(binwidth = 1, colour = "#C50084FF", fill = "#F0AB00FF") +
  annotate("text", label = str_glue("← Мы ожидаем {((tests_n ^ 2) - tests_n)/2 * .05} cтатистически\nзначимых результатов в среднем"),
    x = ((tests_n ^ 2) - tests_n)/2 * .05, y = max(table(false_alarms)) * .75, size = 4, colour = "#C50084FF", hjust = -0.05
  ) +
  annotate("text", label = str_glue("Всего проверяется\n {((tests_n ^ 2) - tests_n)/2} гипотез →"),
           x = ((tests_n ^ 2) - tests_n)/2, y = max(table(false_alarms)) * .5, size = 4, colour = "#C50084FF", hjust = 1.05
  ) +
  geom_vline(xintercept = ((tests_n ^ 2) - tests_n)/2 * .05, linewidth = 1.5, colour ="grey5") +
  labs(
    title = str_glue("Симуляция студента-психолога и его множественных сравнений"),
    subtitle = str_glue(
      "{tests_n} тестов → {((tests_n ^ 2) - tests_n)/2} попарных корреляций"
    ),
    x = "Количество гипотез",
    y = "Статистически значимые гипотезы"
  ) +
  theme_minimal()

sum(false_alarms == 0)
