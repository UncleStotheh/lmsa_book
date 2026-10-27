# ------------------------------------------------------------------
# Translational Research Continuum (T0 -> T4)
# Reproduces the layout of a "continuum" figure: a coloured top band
# with a rising diagonal, a grey description band, and a coloured
# label band at the bottom. Built with ggplot2 only.
#
# Usage:  source("translational_continuum.R")   # writes PNG + PDF
# ------------------------------------------------------------------

library(ggplot2)

# ---- Content -----------------------------------------------------
stages <- data.frame(
  stage = c("T0", "T1", "T2", "T3", "T4"),
  label = c("Discovery", "Translation to humans", "Translation to practice",
            "Community Implementation", "Population impact"),
  description = c(
    "Fundamental biological discovery",
    "Translation from laboratory findings to initial human testing",
    "Translation from clinical studies to evidence-based practice",
    "Implementation of evidence in health systems and communities",
    "Evaluation of population-level outcomes, equity, and policy impact"
  ),
  fill = c("#ED8A3F", "#D16A45", "#B45A5B", "#A05268", "#8B4A6F"),
  stringsAsFactors = FALSE
)

n          <- nrow(stages)
stages$x0  <- seq_len(n) - 1
stages$x1  <- seq_len(n)
stages$xm  <- (stages$x0 + stages$x1) / 2

# Band geometry (y-axis units)
y_top_hi <- 10;  y_top_lo <- 7.0   # coloured band with diagonal
y_mid_hi <- 7.0; y_mid_lo <- 1.0   # grey description band
y_bot_hi <- 1.0; y_bot_lo <- 0.0   # coloured label band

wrap <- function(x, width = 24) vapply(x, function(s)
  paste(strwrap(s, width = width), collapse = "\n"), character(1))

# ---- Colours -----------------------------------------------------
bg_navy  <- "#2A3F5A"
grey_bnd <- "#E8E8E8"
txt_dark <- "#2B2B2B"

# ---- Plot --------------------------------------------------------
p <- ggplot() +
  # Top coloured band
  geom_rect(data = stages,
            aes(xmin = x0, xmax = x1, ymin = y_top_lo, ymax = y_top_hi, fill = fill)) +
  # Grey description band
  annotate("rect", xmin = 0, xmax = n, ymin = y_mid_lo, ymax = y_mid_hi, fill = grey_bnd) +
  # Bottom coloured label band
  geom_rect(data = stages,
            aes(xmin = x0, xmax = x1, ymin = y_bot_lo, ymax = y_bot_hi, fill = fill)) +
  # Rising diagonal across the top band
  annotate("segment", x = 0, xend = n, y = y_top_lo, yend = y_top_hi,
           colour = "white", linewidth = 1.1) +
  # Tick marks under the top band
  annotate("segment", x = seq(0.5, n - 0.5, by = 0.5), xend = seq(0.5, n - 0.5, by = 0.5),
           y = y_top_lo, yend = y_top_lo - 0.25, colour = txt_dark, linewidth = 0.4) +
  # Stage codes (T0..T4) in grey band
  geom_text(data = stages, aes(x = xm, y = y_mid_hi - 0.9, label = stage),
            colour = txt_dark, fontface = "bold", size = 10) +
  # Descriptions
  geom_text(data = stages, aes(x = xm, y = y_mid_hi - 2.0, label = wrap(description)),
            colour = txt_dark, size = 6, vjust = 1, lineheight = 0.90) +
  # Bottom labels
  geom_text(data = stages, aes(x = xm, y = (y_bot_lo + y_bot_hi) / 2, label = label),
            colour = "white", fontface = "bold", size = 3.6) +
  # Axis-end labels above the band
  annotate("text", x = 0, y = y_top_hi + 0.6, label = "Discovery-centered research",
           hjust = 0, colour = "white", fontface = "bold", size = 6) +
  annotate("text", x = n, y = y_top_hi + 0.6, label = "Population-centered research",
           hjust = 1, colour = "white", fontface = "bold", size = 6) +
  scale_fill_identity() +
  coord_cartesian(xlim = c(-0.05, n + 0.05), ylim = c(-0.1, y_top_hi + 1.2), expand = FALSE) +
  labs(title = "The Translational Research Continuum",
       subtitle = "Continuum, T0 to T4") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = bg_navy, colour = NA),
    panel.background = element_rect(fill = bg_navy, colour = NA),
    plot.title    = element_text(colour = "white", face = "bold", size = 24,
                                 margin = margin(b = 6)),
    plot.subtitle = element_text(colour = "white", size = 15, margin = margin(b = 14)),
    plot.margin   = margin(20, 30, 20, 30)
  )

print(p)

# ---- Export ------------------------------------------------------
ggsave("translational_continuum.png", p, width = 11, height = 6.2, dpi = 300, bg = bg_navy)
ggsave("translational_continuum.pdf", p, width = 11, height = 6.2, bg = bg_navy)