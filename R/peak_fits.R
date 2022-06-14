
# this script is designed to be run by a call to source() from paper.Rmd

# two-peak model

model <- combined %>% 
  filter(
    sample %in% c("3PP", "100SiO2"),
    param == "57Fe",
    date < "2021-12-01"
  ) %>% 
  group_by(date, sample) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(
    sub_long  = map(data, ~ filter(.x, time > 33)),
    # peak 2:
    sub2  = map(data, ~ filter(.x, time > 43.5)),
    pk2 = map(sub2, ~ deconvolve_fff(.x$time, .x$conc, h = 90, mu = 45, s = 4, g = 4,)),
    preds_pk2 = map2(pk2, data, ~ predict(.x$model, newdata = tibble(x = .y$time))),
    coef_pk2 = map(pk2, ~ as_tibble(t(coef(.x$model)))),
    pk2_long = map2(data, coef_pk2, ~ with(.y, fffprocessr:::skew_gaussian(.x$time, h1, mu1, s1, g1))),
    # peak 1:
    sub1  = map(data, ~ filter(.x, time > 33, time < 43)),
    pk1 = map(sub1, ~ deconvolve_fff(.x$time, .x$conc, h = 200, mu = 35, s = 5, g = 4)),
    preds_pk1 = map2(pk1, data, ~ predict(.x$model, newdata = tibble(x = .y$time))),
    coef_pk1 = map(pk1, ~ as_tibble(t(coef(.x$model)))),
    pk1_long = map2(data, coef_pk1, ~ with(.y, fffprocessr:::skew_gaussian(.x$time, h1, mu1, s1, g1))),
    linmod = pmap(
      list(x1 = pk1_long, x2 = pk2_long, y = sub_long, z = data),
      function(x1, x2, y, z) lm(y$conc ~ x1 + x2, data = tibble(x1 = x1[z$time > 33], x2 = x2[z$time > 33]))
    ),
    coef_lm = map(linmod, ~ as_tibble(t(coef(.x)))),
    lm_fit = map(linmod, fitted),
    peak1_fit = map2(coef_lm, pk1_long, ~ .y * .x$x1),
    peak2_fit = map2(coef_lm, pk2_long, ~ .y * .x$x2)
  )

# % of total:

pcent_tot <- model %>%
  mutate(
    pk1 = map2(data, peak1_fit, ~ 100 * sum(.y) / sum(.x$conc[.x$time > 10])),
    pk2 = map2(data, peak2_fit, ~ 100 * sum(.y) / sum(.x$conc[.x$time > 10]))
  ) %>% 
  unnest(c(pk1, pk2)) %>% 
  select(date, sample, pk1, pk2) %>%
  pivot_longer(c(pk1, pk2), names_to = "peak", values_to = "pcent") %>% 
  group_by(sample) %>% 
  mutate(
    rep = as.numeric(factor(date)),
    peak = as.integer(str_extract(peak, "\\d"))
  ) %>% 
  ungroup() %>% 
  select(-date)

# plot fitted values:

# plot <- model %>%
#   ggplot() +
#   facet_grid(
#     rows = vars(date),
#     cols = vars(sample),
#     scales = "free_y"
#   ) +
#   geom_line(
#     data = function(x) {
#       x %>%
#         unnest(c(data, matches("^peak\\d"))) %>% 
#         select(where(~ !is.list(.x))) %>%
#         pivot_longer(c(conc, starts_with("peak")))
#     },
#     aes(time, value, col = name)
#   ) +
#   geom_line(
#     data = function(x) {
#       x %>%
#         unnest(c(sub_long, lm_fit)) %>%
#         select(where(~ !is.list(.x))) %>%
#         pivot_longer(c(lm_fit, starts_with("peak")))
#     },
#     aes(time, value, col = name)
#   )
 


