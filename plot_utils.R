require(tidyverse)
require(data.table)

plot_coefplot = function(model_event_study, x_reference) {

  to_plot =
    iplot(model_event_study, only.params = T)$prms %>%
    as.data.frame() %>%
    rename(date = estimate_names) %>%
    mutate(weekday = wday(date)) %>%
    mutate(month_year = year(date) * 100 + month(date)) %>%
    group_by(month_year) %>%
    mutate(last_day = max(day(date))) %>%
    mutate(max_date = max(date)) %>%
    ungroup() %>%
    mutate(weekday_max_date = wday(max_date)) %>%
    mutate(date_string = case_when(
      day(date) == last_day ~ format(date, "<br/>%b<br/>%Y"),
      weekday %in% c(2) ~ format(date, "<i style='font-size:17px;'>%d</i>"),
      TRUE ~ ""
    ))

  to_plot %>%
    ggplot(
      aes(x = factor(date), y = estimate)
    ) +
    geom_errorbar(
      aes(ymin = ci_low, ymax = ci_high), width = 0.5, size = 0.5
    ) +
    geom_point(size = 1.5) +
    ggthemes::theme_clean(base_size = 22) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = scales::pretty_breaks()) +
    scale_x_discrete(breaks = factor(to_plot$date), labels = to_plot$date_string) +
    geom_vline(xintercept = factor(ymd(x_reference - 1)), linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Date",
      y = "Estimate (Date x Exposure to War)"
    ) +
    theme(
      axis.text.x = element_markdown(),
      plot.background = element_blank(),
      aspect.ratio = 1/2,
      axis.line = element_line(colour = 'black', size = 55)
    )

}
