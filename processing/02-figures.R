# ==============================================================================
# Figure Generation Script
# Project: Attitudes toward political violence in Chile (ELSOC 2016–2023)
# Description: Estimates main regression models and produces all figures
#              appearing in the paper. Requires elsoc_final_2.RData produced
#              by 01-proc-data.R.
#
# Output: PNG files saved to output/figures/
# ==============================================================================

# 1. Packages ------------------------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  dplyr,
  tidyr,
  ggplot2,
  patchwork,
  glmmTMB,
  ggeffects,
  here
)

options(scipen = 999)
dir.create(here("output/figures"), recursive = TRUE, showWarnings = FALSE)

# 2. Load and prepare data -----------------------------------------------------

load(here("input/data/proc/elsoc_final_2.RData"))

# Convert education to unordered factor (needed for glmmTMB dummy coding)
# Verify level count before positional relabeling
stopifnot(nlevels(elsoc_final_2$educ_cat_factor) == 5)

elsoc_final_2 <- elsoc_final_2 %>%
  mutate(
    educ_cat_unordered = factor(
      educ_cat_factor,
      ordered = FALSE,
      labels  = c(
        "High School or less",
        "Technical College incomplete",
        "Technical College complete",
        "University incomplete",
        "University complete"
      )
    )
  )

# Consistent protest violence index (workers + students items; available all waves)
elsoc_final_2 <- elsoc_final_2 %>%
  mutate(
    justif_violencia_protesta_consistente = rowMeans(
      dplyr::select(., violencia_trabajadores, violencia_estudiantes),
      na.rm = TRUE
    )
  )

# 3. Estimate models -----------------------------------------------------------
# Models are needed for Figures 4–6 (marginal effects and predicted values).
# Models 1–2: Education × Participation interaction

cat("Estimating models... (this may take a few minutes)\n")

mod_educ_protesta_main <- glmmTMB(
  justif_violencia_protesta_consistente ~
    educ_cat_unordered + protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data   = elsoc_final_2,
  family = gaussian()
)

mod_educ_protesta_int <- glmmTMB(
  justif_violencia_protesta_consistente ~
    educ_cat_unordered * protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data   = elsoc_final_2,
  family = gaussian()
)

mod_educ_estatal_main <- glmmTMB(
  justif_violencia_estatal ~
    educ_cat_unordered + protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data   = elsoc_final_2,
  family = gaussian()
)

mod_educ_estatal_int <- glmmTMB(
  justif_violencia_estatal ~
    educ_cat_unordered * protesta_dummy +
    edad + mujer + ideologia_std + factor(year) +
    (1 | idencuesta),
  data   = elsoc_final_2,
  family = gaussian()
)

cat("Models estimated.\n")

# ==============================================================================
# FIGURE 1: Temporal evolution of protest participation
# ==============================================================================

plot_particip <- elsoc_final_2 %>%
  group_by(year) %>%
  summarise(
    Participation = 100 * mean(protesta_dummy, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = year, y = Participation)) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", alpha = 0.6) +
  geom_line(linewidth = 1.2, color = "#619CFF") +
  geom_point(size = 2.5,    color = "#619CFF") +
  labs(
    title   = "Protest Participation Over Time (ELSOC 2016–2023)",
    x       = "Year",
    y       = "% who Participated",
    caption = "Red dashed line: 2019 social uprising. Source: ELSOC 2016–2023."
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = here("output/figures/fig1_participation_trend.png"),
  plot     = plot_particip,
  width    = 10, height = 6, dpi = 300
)
cat("Figure 1 saved.\n")

# ==============================================================================
# FIGURE 2: Protest vs. state violence justification over time
# ==============================================================================

comparacion_data <- elsoc_final_2 %>%
  group_by(year) %>%
  summarise(
    `Protest Violence` = mean(justif_violencia_protesta_consistente, na.rm = TRUE),
    `State Violence`   = mean(justif_violencia_estatal,              na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(
    cols      = c("Protest Violence", "State Violence"),
    names_to  = "Type",
    values_to = "Justification"
  )

fig2 <- ggplot(comparacion_data,
               aes(x = year, y = Justification, color = Type, group = Type)) +
  geom_line(aes(linetype = Type), linewidth = 1.5) +
  geom_point(aes(shape   = Type), size = 3.5) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", alpha = 0.6) +
  scale_color_manual(
    values = c("Protest Violence" = "#EFC000FF", "State Violence" = "#0073C2FF"),
    name   = "Type of Violence"
  ) +
  scale_linetype_manual(
    values = c("Protest Violence" = "solid",  "State Violence" = "dashed"),
    name   = "Type of Violence"
  ) +
  scale_shape_manual(
    values = c("Protest Violence" = 16, "State Violence" = 17),
    name   = "Type of Violence"
  ) +
  labs(
    title   = "Divergence between Justification of Protest and State Violence (2016–2023)",
    x       = "Year",
    y       = "Justification (scale 1–5)",
    caption = "Red dashed line: 2019 social uprising. Source: ELSOC 2016–2023."
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "bottom",
    plot.caption    = element_text(size = 10, hjust = 0)
  ) +
  ylim(1.4, 2.1)

ggsave(
  filename = here("output/figures/fig2_violence_divergence.png"),
  plot     = fig2,
  width    = 10, height = 6, dpi = 300
)
cat("Figure 2 saved.\n")

# ==============================================================================
# FIGURE 3: Pre/post 2019 comparison by education and participation
# ==============================================================================

calc_means_ci <- function(data, group_var, tipo_violencia) {
  data %>%
    filter(!is.na(year), year != 2019,
           !is.na(!!sym(group_var)), !is.na(protesta_dummy)) %>%
    mutate(
      period = factor(
        ifelse(year < 2019, "Pre-2019\n(2016–2018)", "Post-2019\n(2022–2023)"),
        levels = c("Pre-2019\n(2016–2018)", "Post-2019\n(2022–2023)")
      )
    ) %>%
    group_by(period, !!sym(group_var), protesta_dummy) %>%
    summarise(
      mean   = mean(!!sym(tipo_violencia), na.rm = TRUE),
      se     = sd(!!sym(tipo_violencia), na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      ci_low  = mean - 1.96 * se,
      ci_high = mean + 1.96 * se,
      participation = factor(protesta_dummy,
                             levels = c(0, 1),
                             labels = c("Did not participate", "Participated"))
    )
}

# Panel A: protest violence
data_educ_prot <- calc_means_ci(elsoc_final_2, "educ_cat_unordered",
                                "justif_violencia_protesta_consistente")

p3a <- ggplot(data_educ_prot,
              aes(x = educ_cat_unordered, y = mean,
                  color = participation, group = participation)) +
  geom_point(aes(shape = participation),
             position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.5), width = 0.2, linewidth = 0.8) +
  geom_line(aes(linetype = participation),
            position = position_dodge(width = 0.5), linewidth = 1) +
  facet_wrap(~period, ncol = 2) +
  scale_color_manual(values = c("Did not participate" = "#F8766D",
                                "Participated"        = "#00BFC4"), name = "") +
  scale_linetype_manual(values = c("Did not participate" = "dashed",
                                   "Participated"        = "solid"),  name = "") +
  scale_shape_manual(values  = c("Did not participate" = 17,
                                 "Participated"        = 16),          name = "") +
  labs(title = "Protest Violence",
       x     = "Education Level",
       y     = "Justification (scale 1–5)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        strip.text   = element_text(face = "bold", size = 12)) +
  ylim(1, 3)

# Panel B: state violence
data_educ_est <- calc_means_ci(elsoc_final_2, "educ_cat_unordered",
                               "justif_violencia_estatal")

p3b <- ggplot(data_educ_est,
              aes(x = educ_cat_unordered, y = mean,
                  color = participation, group = participation)) +
  geom_point(aes(shape = participation),
             position = position_dodge(width = 0.5), size = 2.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                position = position_dodge(width = 0.5), width = 0.2, linewidth = 0.8) +
  geom_line(aes(linetype = participation),
            position = position_dodge(width = 0.5), linewidth = 1) +
  facet_wrap(~period, ncol = 2) +
  scale_color_manual(values = c("Did not participate" = "#F8766D",
                                "Participated"        = "#00BFC4"), name = "") +
  scale_linetype_manual(values = c("Did not participate" = "dashed",
                                   "Participated"        = "solid"),  name = "") +
  scale_shape_manual(values  = c("Did not participate" = 17,
                                 "Participated"        = 16),          name = "") +
  labs(title = "State Violence",
       x     = "Education Level",
       y     = "Justification (scale 1–5)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        strip.text   = element_text(face = "bold", size = 12)) +
  ylim(1, 3)

fig3 <- (p3a / p3b) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = 'A',
    caption    = "Means with 95% CI. Periods: 2016–2018 (pre) and 2022–2023 (post). Year 2019 excluded. Source: ELSOC.",
    theme      = theme(plot.caption = element_text(size = 9))
  ) &
  theme(legend.position = "bottom")

ggsave(
  filename = here("output/figures/fig3_prepost_comparison.png"),
  plot     = fig3,
  width    = 12, height = 12, dpi = 300
)
cat("Figure 3 saved.\n")

# ==============================================================================
# FIGURE 4: Marginal effects of participation by education
# ==============================================================================

# Panel A: protest violence
coef_prot  <- fixef(mod_educ_protesta_int)$cond
vcov_prot  <- vcov(mod_educ_protesta_int)$cond

me_base    <- coef_prot["protesta_dummy"]

calc_me_se <- function(coef, vcov_mat, base_name, int_name) {
  me  <- coef[base_name] + (if (is.na(int_name)) 0 else coef[int_name])
  se  <- if (is.na(int_name)) {
    sqrt(vcov_mat[base_name, base_name])
  } else {
    sqrt(vcov_mat[base_name, base_name] +
         vcov_mat[int_name,  int_name]  +
         2 * vcov_mat[base_name, int_name])
  }
  c(me = me, se = se)
}

educ_levels <- c("High School\nor less",
                 "Tech. College\nincomplete",
                 "Tech. College\ncomplete",
                 "University\nincomplete",
                 "University\ncomplete")

int_names_prot <- c(
  NA,
  "educ_cat_unorderedTechnical College incomplete:protesta_dummy",
  "educ_cat_unorderedTechnical College complete:protesta_dummy",
  "educ_cat_unorderedUniversity incomplete:protesta_dummy",
  "educ_cat_unorderedUniversity complete:protesta_dummy"
)

me_df_prot <- do.call(rbind, lapply(int_names_prot, function(nm) {
  calc_me_se(coef_prot, vcov_prot, "protesta_dummy", nm)
}))

me_df_prot <- as.data.frame(me_df_prot)
me_df_prot$education  <- factor(educ_levels, levels = educ_levels)
me_df_prot$ci_low     <- me_df_prot$me - 1.96 * me_df_prot$se
me_df_prot$ci_high    <- me_df_prot$me + 1.96 * me_df_prot$se
me_df_prot$significant <- !(me_df_prot$ci_low < 0 & me_df_prot$ci_high > 0)

panel4a <- ggplot(me_df_prot, aes(x = education, y = me)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_point(aes(color = significant), size = 3.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant),
                width = 0.2, linewidth = 1) +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#2C7BB6"),
                     guide  = "none") +
  labs(title = "Protest Violence",
       x     = "Education Level",
       y     = "Marginal Effect of Participation\n(change in justification)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x     = element_text(hjust = 0.5, size = 11),
        plot.title      = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank())

# Panel B: state violence
coef_est  <- fixef(mod_educ_estatal_int)$cond
vcov_est  <- vcov(mod_educ_estatal_int)$cond

int_names_est <- c(
  NA,
  "educ_cat_unorderedTechnical College incomplete:protesta_dummy",
  "educ_cat_unorderedTechnical College complete:protesta_dummy",
  "educ_cat_unorderedUniversity incomplete:protesta_dummy",
  "educ_cat_unorderedUniversity complete:protesta_dummy"
)

me_df_est <- do.call(rbind, lapply(int_names_est, function(nm) {
  calc_me_se(coef_est, vcov_est, "protesta_dummy", nm)
}))

me_df_est <- as.data.frame(me_df_est)
me_df_est$education   <- factor(educ_levels, levels = educ_levels)
me_df_est$ci_low      <- me_df_est$me - 1.96 * me_df_est$se
me_df_est$ci_high     <- me_df_est$me + 1.96 * me_df_est$se
me_df_est$significant  <- !(me_df_est$ci_low < 0 & me_df_est$ci_high > 0)

panel4b <- ggplot(me_df_est, aes(x = education, y = me)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  geom_point(aes(color = significant), size = 3.5) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high, color = significant),
                width = 0.2, linewidth = 1) +
  scale_color_manual(values = c("FALSE" = "gray50", "TRUE" = "#2C7BB6"),
                     guide  = "none") +
  labs(title = "State Violence",
       x     = "Education Level",
       y     = "Marginal Effect of Participation\n(change in justification)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x     = element_text(hjust = 0.5, size = 11),
        plot.title      = element_text(size = 12, face = "bold"),
        panel.grid.minor = element_blank())

fig4 <- (panel4a | panel4b) +
  plot_annotation(
    tag_levels = 'A',
    caption    = "Marginal effects with 95% CI. Grey points: non-significant at p < .05.\nMarginal effect = expected change in justification from not participating to participating. Source: ELSOC.",
    theme      = theme(plot.caption = element_text(size = 9, hjust = 0))
  )

ggsave(
  filename = here("output/figures/fig4_marginal_effects.png"),
  plot     = fig4,
  width    = 14, height = 7, dpi = 300
)
cat("Figure 4 saved.\n")

# ==============================================================================
# FIGURE 5: Predicted values by education and participation (paradox effect)
# ==============================================================================

pred_prot <- ggpredict(mod_educ_protesta_int,
                       terms = c("educ_cat_unordered", "protesta_dummy")) %>%
  as.data.frame() %>%
  rename(participation = group) %>%
  mutate(participation = factor(participation,
                                levels = c("0", "1"),
                                labels = c("Did not participate", "Participated")))

panel5a <- ggplot(pred_prot,
                  aes(x = x, y = predicted,
                      color = participation, group = participation,
                      fill  = participation)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, color = NA) +
  geom_line(aes(linetype = participation), linewidth = 1) +
  geom_point(aes(shape   = participation), size = 2.5) +
  scale_color_manual(values = c("Did not participate" = "#F8766D",
                                "Participated"        = "#00BFC4"), name = "Participation") +
  scale_fill_manual(values  = c("Did not participate" = "#F8766D",
                                "Participated"        = "#00BFC4"), name = "Participation") +
  scale_linetype_manual(values = c("Did not participate" = "dashed",
                                   "Participated"        = "solid"),  name = "Participation") +
  scale_shape_manual(values   = c("Did not participate" = 17,
                                  "Participated"        = 16),         name = "Participation") +
  labs(title = "Protest Violence",
       x     = "Education Level",
       y     = "Justification (predicted)") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        plot.title      = element_text(size = 13, face = "bold"))

pred_est <- ggpredict(mod_educ_estatal_int,
                      terms = c("educ_cat_unordered", "protesta_dummy")) %>%
  as.data.frame() %>%
  rename(participation = group) %>%
  mutate(participation = factor(participation,
                                levels = c("0", "1"),
                                labels = c("Did not participate", "Participated")))

panel5b <- ggplot(pred_est,
                  aes(x = x, y = predicted,
                      color = participation, group = participation,
                      fill  = participation)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
              alpha = 0.2, color = NA) +
  geom_line(aes(linetype = participation), linewidth = 1) +
  geom_point(aes(shape   = participation), size = 2.5) +
  scale_color_manual(values = c("Did not participate" = "#F8766D",
                                "Participated"        = "#00BFC4"), name = "Participation") +
  scale_fill_manual(values  = c("Did not participate" = "#F8766D",
                                "Participated"        = "#00BFC4"), name = "Participation") +
  scale_linetype_manual(values = c("Did not participate" = "dashed",
                                   "Participated"        = "solid"),  name = "Participation") +
  scale_shape_manual(values   = c("Did not participate" = 17,
                                  "Participated"        = 16),         name = "Participation") +
  labs(title = "State Violence",
       x     = "Education Level",
       y     = "Justification (predicted)") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.position = "bottom",
        plot.title      = element_text(size = 13, face = "bold"))

fig5 <- (panel5a | panel5b) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = 'A',
    caption    = "Predicted values with 95% CI. Other predictors at sample means. Source: ELSOC 2016–2023.",
    theme      = theme(plot.caption = element_text(size = 9, hjust = 0))
  ) &
  theme(legend.position = "bottom")

ggsave(
  filename = here("output/figures/fig5_predicted_values.png"),
  plot     = fig5,
  width    = 12, height = 6, dpi = 300
)
cat("Figure 5 saved.\n")

# ==============================================================================
# FIGURE 6: Temporal evolution of bidirectionality (secondary vs. university)
# ==============================================================================

pred_prot_tiempo <- ggpredict(
  mod_educ_protesta_int,
  terms   = c("year", "protesta_dummy",
              "educ_cat_unordered [High School or less, University complete]"),
  typical = "mean"
)

pred_est_tiempo <- ggpredict(
  mod_educ_estatal_int,
  terms   = c("year", "protesta_dummy",
              "educ_cat_unordered [High School or less, University complete]"),
  typical = "mean"
)

pred_prot_df <- as.data.frame(pred_prot_tiempo)
pred_prot_df$violence_type <- "Protest Violence"
pred_prot_df$participation <- factor(pred_prot_df$group,
                                     labels = c("Did not participate", "Participated"))
pred_prot_df$education     <- pred_prot_df$facet

pred_est_df <- as.data.frame(pred_est_tiempo)
pred_est_df$violence_type  <- "State Violence"
pred_est_df$participation  <- factor(pred_est_df$group,
                                     labels = c("Did not participate", "Participated"))
pred_est_df$education      <- pred_est_df$facet

pred_combined <- rbind(pred_prot_df, pred_est_df)

fig6 <- ggplot(pred_combined,
               aes(x = x, y = predicted,
                   color = violence_type, group = violence_type)) +
  geom_line(aes(linetype = violence_type), linewidth = 1.2) +
  geom_point(aes(shape   = violence_type), size = 2.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = violence_type),
              alpha = 0.15, color = NA) +
  facet_grid(participation ~ education,
             labeller = labeller(
               participation = c("Did not participate" = "Non-Participants",
                                 "Participated"        = "Participants")
             )) +
  scale_color_manual(
    values = c("Protest Violence" = "#EFC000FF", "State Violence" = "#0073C2FF"),
    name   = "Type of Violence"
  ) +
  scale_fill_manual(
    values = c("Protest Violence" = "#EFC000FF", "State Violence" = "#0073C2FF"),
    name   = "Type of Violence"
  ) +
  scale_linetype_manual(
    values = c("Protest Violence" = "solid",  "State Violence" = "dashed"),
    name   = "Type of Violence"
  ) +
  scale_shape_manual(
    values = c("Protest Violence" = 16, "State Violence" = 17),
    name   = "Type of Violence"
  ) +
  geom_vline(xintercept = 2019, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    title   = "Bidirectional Reconfiguration: Secondary vs. University Education (2016–2023)",
    x       = "Year",
    y       = "Predicted Violence Justification",
    caption = "Red dashed line: 2019 social uprising. Shaded areas: 95% CI. Other predictors at means. Source: ELSOC."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    strip.text      = element_text(face = "bold", size = 11),
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 10),
    plot.caption    = element_text(size = 9)
  ) +
  ylim(1.3, 2.4)

ggsave(
  filename = here("output/figures/fig6_temporal_bidirectionality.png"),
  plot     = fig6,
  width    = 12, height = 9, dpi = 300
)
cat("Figure 6 saved.\n")

cat("\nAll figures saved to output/figures/\n")
