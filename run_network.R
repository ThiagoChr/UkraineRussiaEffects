library(tidyverse)
library(data.table)
library(fixest)
library(lubridate)
library(ggtext)
library(igraph)

# Russia invaded Ukraine on 24 February 2022
RUSSIA_INVASION_DATE = ymd(20220224)

UKRAINE_2DIGIT_ISOCODE = "UA"
RUSSIA_2DIGIT_ISOCODE = "RU"
BELARUS_2DIGIT_ISOCODE = "BY"

WAR_PARTICIPANTS = c(UKRAINE_2DIGIT_ISOCODE, RUSSIA_2DIGIT_ISOCODE)
# WAR_PARTICIPANTS = c(UKRAINE_2DIGIT_ISOCODE)

iso_country_name =
  fread("./data/iso_country_codes.csv")

geo =
  fread("./data/geo.csv") %>%
  mutate(discrete_dist_to_ukr = ntile(dist_to_ukr, 3))

exchange_rate =
  fread("./data/fx.csv") %>%
  filter(iso2 != "US") # anchored in the US

weo =
  fread("./data/weo.csv")

stock_index =
  fread("./data/stock_indexes_adj.csv")

sovereign_bonds =
  fread("./data/sb_adj.csv")

military =
  fread("./data/gfp.csv")

imports =
  fread("./data/imp_seg.csv") %>%
  mutate(
    across(
      .cols = ends_with("_value"),
      .fns = ~ coalesce(., 0)
    )
  ) %>%
  rename(iso2 = origin) %>%
  mutate(date = ymd(sprintf("%s-01", date))) %>%
  group_by(., date, iso2) %>%
  summarize(
    total_imports = sum(imp_value, na.rm = T),
    total_imports_from_war = sum(ifelse(destiny %in% WAR_PARTICIPANTS, imp_value, 0.0), na.rm = T)
  ) %>%
  ungroup() %>%
  bind_rows(
    fread("./data/exp_seg.csv") %>%
      mutate(
        across(
          .cols = ends_with("_value"),
          .fns = ~ coalesce(., 0)
        )
      ) %>%
      rename(iso2 = destiny) %>%
      mutate(date = ymd(sprintf("%s-01", date))) %>%
      group_by(., date, iso2) %>%
      summarize(
        total_imports  = sum(exp_value, na.rm = T),
        total_imports_from_war = sum(ifelse(origin %in% WAR_PARTICIPANTS, exp_value, 0.0), na.rm = T)
      ) %>%
      ungroup()
  ) %>%
  group_by(date, iso2) %>%
  summarize(
    total_imports = sum(total_imports, na.rm = T),
    total_imports_from_war = sum(total_imports_from_war, na.rm = T)
  ) %>%
  ungroup()


exports =
  fread("./data/imp_seg.csv") %>%
  mutate(
    across(
      .cols = ends_with("_value"),
      .fns = ~ coalesce(., 0)
    )
  ) %>%
  rename(iso2 = destiny) %>%
  mutate(date = ymd(sprintf("%s-01", date))) %>%
  group_by(., date, iso2) %>%
  summarize(
    total_exports = sum(imp_value, na.rm = T),
    total_exports_from_war = sum(ifelse(origin %in% WAR_PARTICIPANTS, imp_value, 0.0), na.rm = T)
  ) %>%
  ungroup() %>%
  bind_rows(
    fread("./data/exp_seg.csv") %>%
      mutate(
        across(
          .cols = ends_with("_value"),
          .fns = ~ coalesce(., 0)
        )
      ) %>%
      rename(iso2 = origin) %>%
      mutate(date = ymd(sprintf("%s-01", date))) %>%
      group_by(., date, iso2) %>%
      summarize(
        total_exports  = sum(exp_value, na.rm = T),
        total_exports_from_war = sum(ifelse(destiny %in% WAR_PARTICIPANTS, exp_value, 0.0), na.rm = T)
      ) %>%
      ungroup()
  ) %>%
  group_by(date, iso2) %>%
  summarize(
    total_exports = sum(total_exports, na.rm = T),
    total_exports_from_war = sum(total_exports_from_war, na.rm = T)
  ) %>%
  ungroup()

international_trade =
  imports %>%
  full_join(
    exports
  )

international_trade_2021 =
  international_trade %>%
  filter(year(date) == 2021) %>%
  rename_with(
    .cols = starts_with("total_"),
    .fn = ~ paste0(., "_2021")
  ) %>%
  select(-date) %>%
  group_by(iso2) %>%
  summarize_all(
    .funs = ~ sum(., na.rm = T)
  ) %>%
  mutate(import_exposure_to_war = 100 * total_imports_from_war_2021 / total_imports_2021) %>%
  mutate(export_exposure_to_war = 100 * total_exports_from_war_2021 / total_exports_2021) %>%
  mutate(openness_exposure_to_war = 100 * (total_imports_from_war_2021 + total_exports_from_war_2021) / (total_imports_2021 + total_exports_2021))

international_trade_2021 %>%
  ggplot(aes(x = import_exposure_to_war)) +
  geom_histogram()


imports_network =
  fread("./data/imp_seg.csv") %>%
  mutate(
    across(
      .cols = ends_with("_value"),
      .fns = ~ coalesce(., 0)
    )
  ) %>%
  mutate(date = ymd(sprintf("%s-01", date)))


exports_network =
  fread("./data/exp_seg.csv") %>%
  mutate(
    across(
      .cols = ends_with("_value"),
      .fns = ~ coalesce(., 0)
    )
  ) %>%
  mutate(date = ymd(sprintf("%s-01", date)))

# Merge datasets ----------------------------------------------------------

imports_network_vertex_attributes =
  imports_network %>%
  distinct(iso2 = origin) %>%
  bind_rows(
    imports_network %>%
      distinct(iso2 = destiny)
  ) %>%
  distinct(.keep_all = T) %>%
  left_join(
    international_trade_2021 %>%
      rename_with(
        .cols = everything() & !c("iso2"),
        .fn = ~ paste0("international_trade_", .)
      )
  ) %>%
  mutate(
    across(
      .cols = ends_with("exposure_to_war"),
      .fns = ~ coalesce(., 0.0)
    )
  ) %>%
  left_join(
    geo %>%
      mutate(discrete_distance_to_ukr = ntile(dist_to_ukr, 3))
  ) %>%
  left_join(
    weo %>%
      mutate(discrete_per_capita_gdp = ntile(gdppc, 3)) %>%
      mutate(discrete_pop = ntile(pop, 3)) %>%
      mutate(discrete_gdebt = ntile(gdebt, 3))
  ) %>%
  distinct(.keep_all = TRUE)


exports_network_vertex_attributes =
  exports_network %>%
  distinct(iso2 = origin) %>%
  bind_rows(
    exports_network %>%
      distinct(iso2 = destiny)
  ) %>%
  distinct(.keep_all = T) %>%
  left_join(
    international_trade_2021 %>%
      rename_with(
        .cols = everything() & !c("iso2"),
        .fn = ~ paste0("international_trade_", .)
      )
  ) %>%
  mutate(
    across(
      .cols = ends_with("exposure_to_war"),
      .fns = ~ coalesce(., 0.0)
    )
  ) %>%
  left_join(
    geo %>%
      mutate(discrete_distance_to_ukr = ntile(dist_to_ukr, 3))
  ) %>%
  left_join(
    weo %>%
      mutate(discrete_per_capita_gdp = ntile(gdppc, 3)) %>%
      mutate(discrete_pop = ntile(pop, 3)) %>%
      mutate(discrete_gdebt = ntile(gdebt, 3))
  ) %>%
  distinct(.keep_all = TRUE)


# Create graphs using sliding windows -----------------------------------------------------------


batch_data_via_sliding_windows = function(df, BATCH_SIZE = 6, STRIDE = 1) {

  unique_dates = sort(unique(df$date))

  current_batch = 1
  batched_data = data.frame()
  id = 1
  while(TRUE) {
    end_batch = pmin(current_batch + BATCH_SIZE - 1, length(unique_dates))

    start_date = unique_dates[current_batch]
    end_date   = unique_dates[end_batch]

    futile.logger::flog.info(sprintf("Batching %s - %s...", start_date, end_date))

    batched_data =
      batched_data %>%
      bind_rows(
        df %>%
          filter(dplyr::between(date, start_date, end_date)) %>%
          mutate(batch_id = id)
      )

    if(end_batch == length(unique_dates)) {
      break
    }

    current_batch = current_batch + STRIDE
    id = id + 1

  }

  return(batched_data)

}

batched_imports_data = batch_data_via_sliding_windows(imports_network)
batched_exports_data = batch_data_via_sliding_windows(exports_network)

batch_ids =
  batched_imports_data %>%
  distinct(batch_id, date) %>%
  bind_rows(
    batched_exports_data %>%
      distinct(batch_id, date)
  ) %>%
  distinct(.keep_all = TRUE)


imports_network_descriptors = data.frame()
exports_network_descriptors = data.frame()
for(current_batch in sort(unique(batch_ids$batch_id))) {
  futile.logger::flog.info(sprintf("Processing batch number %d", current_batch))


  batch_window_time_info =
    batch_ids %>%
    filter(batch_id == current_batch) %>%
    summarize(
      min_date = min(date),
      max_date = max(date)
    )

  reduced_imports_data =
    batched_imports_data %>%
    filter(batch_id == current_batch) %>%
    group_by(origin, destiny) %>%
    summarize(imp_value = mean(imp_value, na.rm = T)) %>%
    ungroup()

  reduced_exports_data =
    batched_exports_data %>%
    filter(batch_id == current_batch) %>%
    group_by(origin, destiny) %>%
    summarize(exp_value = mean(exp_value, na.rm = T)) %>%
    ungroup()

  imports_graph =
    igraph::graph_from_data_frame(
      d =
        reduced_imports_data %>%
        rename(edge_weight = imp_value),
      directed = TRUE,
      vertices = imports_network_vertex_attributes
    )

  exports_graph =
    igraph::graph_from_data_frame(
      d =
        reduced_exports_data %>%
        rename(edge_weight = exp_value),
      directed = TRUE,
      vertices = exports_network_vertex_attributes
    )


  evaluate_network_measures = function(graph) {

    network_assortativity = igraph::assortativity.degree(graph, directed = TRUE)
    network_density = igraph::edge_density(graph, loops = FALSE)

    w_page_rank = igraph::page_rank(graph,
                                    directed = TRUE,
                                    weights = 1.0 / (1e-2 + E(graph)$edge_weight))$vector

    results =
      data.table(
        vertex_name = V(graph)$name,
        network_assortativity,
        network_density,
        w_page_rank
      )

  }

  imports_network_descriptors =
    imports_network_descriptors %>%
    bind_rows(
      data.table(
        start_date = batch_window_time_info$min_date,
        end_date   = batch_window_time_info$max_date,
        evaluate_network_measures(imports_graph)
      )
    )

  exports_network_descriptors =
    exports_network_descriptors %>%
    bind_rows(
      data.table(
        start_date = batch_window_time_info$min_date,
        end_date   = batch_window_time_info$max_date,
        evaluate_network_measures(exports_graph)
      )
    )


}



# Plotting ----------------------------------------------------------------

# Assortativity
imports_network_descriptors %>%
  distinct(end_date, network_assortativity) %>%
  ggplot(aes(x = end_date, y = network_assortativity)) +
  geom_line(size = 1.1) +
  ggthemes::theme_clean(base_size = 22) +
  labs(
    x = 'Date',
    y = 'Network Assortativity'
  ) +
  theme(
    plot.background = element_blank()
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y")

# Density
imports_network_descriptors %>%
  distinct(end_date, network_density) %>%
  ggplot(aes(x = end_date, y = network_density)) +
  geom_line(size = 1.1) +
  ggthemes::theme_clean(base_size = 22) +
  labs(
    x = 'Date',
    y = 'Network Density'
  ) +
  theme(
    plot.background = element_blank()
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y")

# Weighted page rank
imports_network_descriptors %>%
  group_by(end_date) %>%
    summarize(w_page_rank = mean(w_page_rank, na.rm = T)) %>%
  ungroup() %>%
  ggplot(aes(x = end_date, y = w_page_rank)) +
  geom_line(size = 1.1) +
  ggthemes::theme_clean(base_size = 22) +
  labs(
    x = 'Date',
    y = 'Weighted Page Rank'
  ) +
  theme(
    plot.background = element_blank()
  ) +
  scale_y_log10() +
  scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y")

