## ----include = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{
  library(dplyr)
  library(ggplot2)
  library(ggtext)
  library(ggh4x)
  library(nicheROVER) 
  library(nichetools)
  library(patchwork)
  library(purrr)
  library(stringr)
  library(tidyr)
}

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df <- fish %>% 
  janitor::clean_names()

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
nsample <- 1000

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fish_par <- df %>% 
  split(.$species) %>% 
  map(~ select(., d13c, d15n)) %>% 
  map(~ niw.post(nsample = nsample, X = .))

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_mu <- extract_mu(fish_par)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_mu <- df_mu %>%
  mutate(
    element = case_when(
      isotope == "d15n" ~ "N",
      isotope == "d13c" ~ "C",
    ), 
    neutron = case_when(
      isotope == "d15n" ~ 15,
      isotope == "d13c" ~ 13,
    ) 
  )

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_sigma <- extract_sigma(fish_par)

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_sigma_cn <- extract_sigma(fish_par, 
                             data_format = "long") %>%
  filter(id != isotope)

## ----warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
posterior_plots <- df_mu %>%
  split(.$isotope) %>%
  imap(
    ~ ggplot(data = ., aes(x = mu_est)) +
      geom_density(aes(fill = sample_name), alpha = 0.5) +
      scale_fill_viridis_d(begin = 0.25, end = 0.75,
                           option = "D", name = "Species") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.title.x =  element_markdown(),
            axis.title.y =  element_markdown(),
            legend.position = "none",
            legend.background = element_blank()
      ) +
      labs(
        x = paste("\u00b5<sub>\U03B4</sub>", "<sub><sup>",
                  unique(.$neutron), "</sup></sub>",
                  "<sub>",unique(.$element), "</sub>", sep = ""),
        y = paste0("p(\u00b5 <sub>\U03B4</sub>","<sub><sup>",
                   unique(.$neutron), "</sub></sup>",
                   "<sub>",unique(.$element),"</sub>",
                   " | X)"), sep = "")
  )

posterior_plots$d15n +
  theme(legend.position = c(0.18, 0.82)) + 
  posterior_plots$d13c


## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_sigma_cn <- df_sigma_cn %>%
  mutate(
    element_id = case_when(
      id == "d15n" ~ "N",
      id == "d13c" ~ "C",
    ),
    neutron_id = case_when(
      id == "d15n" ~ 15,
      id == "d13c" ~ 13,
    ),
    element_iso = case_when(
      isotope == "d15n" ~ "N",
      isotope == "d13c" ~ "C",
    ),
    neutron_iso = case_when(
      isotope == "d15n" ~ 15,
      isotope == "d13c" ~ 13,
    )
  )

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sigma_plots <- df_sigma_cn %>%
  group_split(id, isotope) %>%
  imap(
    ~ ggplot(data = ., aes(x = post_sample)) +
      geom_density(aes(fill = sample_name), alpha = 0.5) +
      scale_fill_viridis_d(begin = 0.25, end = 0.75,
                           option = "D", name = "Species") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            axis.title.x =  element_markdown(),
            axis.title.y =  element_markdown(),
            legend.position = "none"
      ) +
      labs(
        x = paste("\U03A3","<sub>\U03B4</sub>",
                  "<sub><sup>", unique(.$neutron_id), "</sub></sup>",
                  "<sub>",unique(.$element_id),"</sub>"," ",
                  "<sub>\U03B4</sub>",
                  "<sub><sup>", unique(.$neutron_iso), "</sub></sup>",
                  "<sub>",unique(.$element_iso),"</sub>", sep = ""),
        y = paste("p(", "\U03A3","<sub>\U03B4</sub>",
                  "<sub><sup>", unique(.$neutron_id), "</sub></sup>",
                  "<sub>",unique(.$element_id),"</sub>"," ",
                  "<sub>\U03B4</sub>",
                  "<sub><sup>", unique(.$neutron_iso), "</sub></sup>",
                  "<sub>",unique(.$element_iso),"</sub>", " | X)", sep = ""),
      )
  )

sigma_plots[[1]] + 
  theme(legend.position = c(0.1, 0.82))


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ellipse_df <- niche_ellipse(dat_mu = df_mu, dat_sigma = df_sigma,
                            set_seed = 4)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ellipse_plots <- ggplot() + 
  geom_polygon(data = ellipse_df,
               mapping = aes(x = d13c, y = d15n,
                             group = interaction(sample_number, sample_name),
                             color = sample_name),
               fill = NA,
               linewidth = 0.5) + 
  
  scale_colour_viridis_d(begin = 0.25, end = 0.75, 
                         option = "D", name = "species",
  ) + 
  scale_x_continuous(breaks = rev(seq(-20, -40, -2))) +
  scale_y_continuous(breaks = seq(6, 16, 2)) +
  theme_bw(base_size = 10) +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank(), 
        legend.position = "none", 
        legend.title = element_text(hjust = 0.5),
        legend.background = element_blank()) + 
  labs(x = expression(paste(delta ^ 13, "C")), 
       y = expression(paste(delta ^ 15, "N")))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
iso_long <- df %>%
  pivot_longer(cols = -species,
               names_to = "isotope", 
               values_to = "value") %>% 
  mutate(
    element = case_when(
      isotope == "d15n" ~ "N",
      isotope == "d13c" ~ "C",
    ), 
    neutron = case_when(
      isotope == "d15n" ~ 15,
      isotope == "d13c" ~ 13,
    )
  )

## ----warning=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
iso_density <- iso_long %>% 
  group_split(isotope) %>% 
  imap(
    ~ ggplot(data = .) + 
      geom_density(aes(x = value, 
                       fill = species), 
                   alpha = 0.35, 
                   linewidth = 0.8) +
      scale_fill_viridis_d(begin = 0.25, end = 0.75,
                           option = "D", name = "Species") +
      theme_bw(base_size = 10) +
      theme(axis.text = element_text(colour = "black"),
            panel.grid = element_blank(), 
            legend.position = c(0.15, 0.55), 
            legend.background = element_blank(), 
            axis.title.x = element_markdown(family = "sans")) + 
      labs(x =  paste("\U03B4",
                      "<sup>", unique(.$neutron), "</sup>",unique(.$element), 
                      sep = ""), 
           y = "Density")
  )

d13c_density <- iso_density[[1]] + 
  scale_x_continuous(breaks = rev(seq(-20, -34, -2)),
                     limits = rev(c(-20, -34)))

d15n_density <- iso_density[[2]] +
  scale_x_continuous(breaks = seq(5, 15, 2.5), 
                     limits = c(5, 15)) + 
  theme(
    legend.position = "none"
  )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
iso_biplot <- ggplot() + 
  geom_point(data = df, aes(x = d13c, y = d15n,
                            fill = species),
             shape = 21, colour = "black", 
             stroke = 0.8,
             size = 3, alpha = 0.70) +
  scale_fill_viridis_d(begin = 0.25, end = 0.75,
                       option = "D", name = "species") +
  scale_x_continuous(breaks = rev(seq(-20, -39, -1))) +
  scale_y_continuous(breaks = seq(5, 17, 1)) +
  theme_bw(base_size = 10) +
  theme(axis.text = element_text(colour = "black"),
        panel.grid = element_blank(), 
        legend.position = "none", 
        legend.background = element_blank()) + 
  labs(x = expression(paste(delta ^ 13, "C")), 
       y = expression(paste(delta ^ 15, "N")))

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
d13c_density + ellipse_plots + iso_biplot + d15n_density +
  plot_annotation(tag_levels = "a", 
                  tag_suffix = ")")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
over_stat <- overlap(fish_par, nreps = nsample, nprob = 1000, 
                     alpha = 0.95)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
over_stat_df <- extract_overlap(data = over_stat) %>% 
    mutate(
      niche_overlap_perc = niche_overlap * 100
  )

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
over_sum <- over_stat_df %>% 
  group_by(sample_name_a, sample_name_b) %>% 
  summarise(
    mean_niche_overlap = round(mean(niche_overlap_perc), digits = 2),
    qual_2.5 = round(quantile(niche_overlap_perc, probs = 0.025, na.rm = TRUE), digits = 2), 
    qual_97.5 = round(quantile(niche_overlap_perc, probs = 0.975, na.rm = TRUE), digits = 2)
  ) %>% 
  ungroup() %>% 
  pivot_longer(cols = -c(sample_name_a, sample_name_b, mean_niche_overlap), 
               names_to = "percentage", 
               values_to = "niche_overlap_qual") %>% 
  mutate(
    percentage = as.numeric(str_remove(percentage, "qual_"))
  ) 

## ----warning = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = over_stat_df, aes(x = niche_overlap_perc)) + 
  geom_density(aes(fill = sample_name_a)) + 
  geom_vline(data = over_sum, aes(xintercept = mean_niche_overlap), 
             colour = "black", linewidth = 1) +
  geom_vline(data = over_sum, aes(xintercept = niche_overlap_qual), 
             colour = "black", linewidth = 1, linetype = 6) +
  scale_fill_viridis_d(begin = 0.25, end = 0.75,
                       option = "D", name = "Species", 
                       alpha = 0.35) + 
  facet_grid2(sample_name_a ~ sample_name_b, 
                     independent = "y",
                     scales = "free_y") + 
  theme_bw() + 
  theme(
    panel.grid = element_blank(), 
    axis.text = element_text(colour = "black"), 
    legend.background = element_blank(),
    strip.background = element_blank()
  ) +
  labs(x = paste("Overlap Probability (%)", "\u2013", 
                 "Niche Region Size: 95%"), 
       y = "p(Percent Overlap | X)")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
niche_size <- extract_niche_size(fish_par)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
niche_size_mean <- niche_size %>% 
  group_by(sample_name) %>% 
  summarise(
    mean_niche = round(mean(niche_size), digits = 2), 
    sd_niche = round(sd(niche_size), digits = 2), 
    sem_niche = round(sd(niche_size) / sqrt(n()), digits = 2)
  )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = niche_size) + 
  geom_violin(
    aes(x = sample_name, y = niche_size),
    width = 0.2) + 
  geom_point(data = niche_size_mean, aes(x = sample_name, y = mean_niche)) +
  geom_errorbar(data = niche_size_mean, aes(x = sample_name, 
                                            ymin = mean_niche  - sem_niche, 
                                            ymax = mean_niche  + sem_niche), 
                width = 0.05) +
  theme_bw(base_size = 15) + 
  theme(panel.grid = element_blank(), 
        axis.text = element_text(colour = "black")) + 
  labs(x = "Species", 
       y = "Niche Size") 

