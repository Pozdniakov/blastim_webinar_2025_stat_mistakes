
# Усы ---------------------------------------------------------------------

# Здесь мы используем данные из предыдущего скрипта!
# усредняем и считаем SD, SEM и CI95 для КАЖДОЙ отдельной мышки

mouse_average <- mouse %>%
  group_by(subject, group) %>%
  summarise(DV_mean = mean(DV),
            SD = sd(DV),
            SEM = sd(DV)/(sqrt(n())),
            CI95 = sd(DV)/(sqrt(n())) * qt(0.975, df = n() -1),
            .groups = "drop") %>%
  pivot_longer(cols = SD:CI95, names_to = "errorbar", values_to = "border")

mouse_average

# Рисуем ------------------------------------------------------------------

ggplot(data = mouse, aes(x = subject, y = DV)) +
  geom_point(position = position_jitter(width = .1)) +
  geom_pointrange(data = mouse_average,
                  aes(
                    colour = errorbar,
                    y = DV_mean,
                    ymin = DV_mean - border,
                    ymax = DV_mean + border
                  ),
                  size = .25,
                  linewidth = 1.2,
                  position = position_dodge(width = .6)) +
  geom_point(data = mouse %>% group_by(subject) %>% slice(1),
             aes(y = mean_mouse), colour = "#22AA11", shape = "⬅", size = 6,
             position = position_nudge(x = .5)) +
  facet_wrap( ~ group, scales = "free_x") +
  labs(
    title = str_glue("Симуляция двух групп мышей"),
    subtitle = str_glue(
      "{n_subjects_in_group} мышей в группе, {n_trials} проб, SD между мышами = {sd_between_mouse}, SD внутри мышей = {sd_within_mouse}"
    )
  ) +
  ggsci::scale_color_bmj() +
  theme_linedraw()
