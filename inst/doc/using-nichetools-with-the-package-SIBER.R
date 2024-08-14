## ----include = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{
  library(bayestestR)
  library(dplyr)
  library(ggplot2)
  library(ggdist)
  library(ggtext)
  library(nichetools)
  library(purrr)
  library(SIBER)
  library(tidyr)
  library(viridis)
}

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(demo.siber.data.2)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
demo.siber.data.2 <- demo.siber.data.2 %>%
  mutate(
    group = factor(group), 
    community = factor(community), 
    group_id = as.numeric(group) %>% 
      as.character(),
    community_id = as.numeric(community) %>%
      as.character()
  ) %>% 
  rename(
    group_name = group,
    community_name = community,
    group = group_id,
    community = community_id
  )

glimpse(demo.siber.data.2)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ---- create name with group and community data frame ----
cg_names <- demo.siber.data.2 %>%
  distinct(group,
           community, 
           group_name, 
           community_name) %>%
  
  arrange(community, group)

# ---- create community names data frame ---- 
c_names <- demo.siber.data.2 %>% 
  distinct(community, 
           community_name) %>%
  arrange(community)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = demo.siber.data.2, aes(x = iso1, y = iso2,
                                     colour = group_name)) +
  geom_point() +
  facet_wrap(~ community_name) + 
  scale_colour_viridis_d(option = "A", begin = 0.25, end = 0.85,
                         name = "Groups", alpha = 0.75) + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(), 
    axis.title = element_markdown(),
    legend.position = "inside",
    legend.position.inside = c(0.65, 0.75)
  ) + 
  labs(
    x = paste0("\U03B4","<sup>", 13, "</sup>", "C", " (‰)"),
    y = paste0("\U03B4","<sup>", 15, "</sup>", "N", " (‰)")
  )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
demo_siber_data <- demo.siber.data.2 %>% 
  dplyr::select(iso1, iso2, group, community) %>%
  arrange(community, group) %>% 
  as.data.frame() 

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
siber_example <- createSiberObject(demo_siber_data)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# fit the ellipses which uses an Inverse Wishart prior on the 
# covariance matrix Sigma, and a vague normal prior on the 
# means.
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

## ----message=FALSE, results='hide'------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ellipses_posterior <- siberMVN(siber_example, parms, priors)

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_mu <- extract_mu(ellipses_posterior, pkg = "SIBER", 
                    data_format = "wide") %>%
  separate_wider_delim(sample_name, cols_remove = FALSE,
                       delim = ".", names = c("community",
                                              "group")) %>%
  left_join(cg_names)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_point(data = df_mu, aes(x = d13c, y = d15n,
                               colour = group_name)) +
  geom_point(data = demo.siber.data.2, aes(x = iso1, y = iso2,
                                           colour = group_name)) +
  facet_wrap( ~ community_name) + 
  scale_colour_viridis_d(option = "A", begin = 0.25, end = 0.85,
                         name = "Groups", alpha = 0.75) + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(), 
    axis.title = element_markdown(),
    legend.position = "inside",
    legend.position.inside = c(0.65, 0.75)
  ) + 
  labs(
    x = paste0("\U03B4","<sup>", 13, "</sup>", "C", " (‰)"),
    y = paste0("\U03B4","<sup>", 15, "</sup>", "N", " (‰)")
  )

## ----message=FALSE----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_mu_long <- extract_mu(ellipses_posterior, pkg = "SIBER") %>% 
  separate_wider_delim(sample_name, cols_remove = FALSE,
                       delim = ".", names = c("community",
                                              "group")) %>%
  left_join(cg_names)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_sigma <- extract_sigma(ellipses_posterior, pkg = "SIBER")

## ----message = FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
df_el <- niche_ellipse(dat_mu = df_mu_long,
                       dat_sigma = df_sigma, 
                       set_seed = 4, n = 20) %>%
  separate_wider_delim(sample_name, cols_remove = FALSE,
                       delim = ".", names = c("community",
                                              "group")) %>%
  left_join(cg_names)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = df_el, 
       aes(x = d13c, y = d15n, 
           group = interaction(sample_number, 
                               sample_name), 
           colour = group_name)) + 
  geom_polygon(linewidth = 0.5, fill = NA) + 
  facet_wrap( ~ community_name) + 
  scale_colour_viridis_d(option = "A", begin = 0.25, end = 0.85,
                         name = "Groups", alpha = 0.75) + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(), 
    legend.position = "inside",
    axis.title = element_markdown(),
    legend.background = element_blank(), 
    legend.position.inside = c(0.65, 0.75)
  ) + 
  labs(
    x = paste0("\U03B4","<sup>", 13, "</sup>", "C", " (‰)"),
    y = paste0("\U03B4","<sup>", 15, "</sup>", "N", " (‰)")
  )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sea_b <- siberEllipses(corrected.posteriors = ellipses_posterior)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
seb_convert <- extract_niche_size(data = sea_b,
                                  pkg = "SIBER",
                                  community_df = cg_names)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
group_ml <- groupMetricsML(siber_example)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
group_convert <- extract_group_metrics(data = group_ml,
                                       community_df = cg_names)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
sea_c <- group_convert %>% 
  filter(metric == "SEAc")

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_violin(data = seb_convert, aes(x = community_name,
                                      y = sea,
                                      fill = group_name)) + 
  scale_fill_viridis_d(option = "A", begin = 0.25, end = 0.85,
                       name = "Groups", alpha = 0.75) + 
  geom_point(data = sea_c, aes(x = community_name, 
                               y = est, 
                               group = group_name,
  ), 
  size = 2.5,
  fill = "white",
  shape = 21,
  position = position_dodge(width = 0.9)) +
  theme_bw(
    base_size = 15
  ) + 
  theme(
    strip.background = element_blank(),
    panel.grid = element_blank(), 
    legend.position = "inside",
    legend.background = element_blank(), 
    legend.position.inside = c(0.85, 0.8)
  ) + 
  labs(
    x = "Communities", 
    y = expression(paste("Niche Size p(", "‰"^2, "| x)"))
  )

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mu_post <- extractPosteriorMeans(siber_example, ellipses_posterior)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
layman_b <- bayesianLayman(mu.post = mu_post)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
layman_bc <- extract_layman(layman_b, community_df = c_names)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
viridis_colors <- viridis(2, begin = 0.25, end = 0.85,
                          option = "G",
                          alpha = 0.75
)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(data = layman_bc, aes(x = labels, 
                             y = post_est, 
                             point_fill = community_name)) + 
  stat_pointinterval(
    point_size = 2.5,
    interval_colour = "grey60",
    position = position_dodge(0.4),
    shape = 21
  ) + 
  scale_fill_manual(name = "Community", 
                    aesthetics = "point_fill",
                    values = viridis_colors) + 
  theme_bw(
    base_size = 15
  ) + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_markdown(),
    legend.position = "inside",
    legend.position.inside = c(0.88, 0.85),
  ) +
  labs(
    x = "Community Metrics",
    y = expression(paste("p(", "‰", "|X)"))
  )

