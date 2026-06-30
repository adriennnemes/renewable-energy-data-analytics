
# DYNAMISCHE GRAFIKEN

# Wie verändert der Ausbau erneuerbarer Energien den Strommix
#     und die CO2-Emissionen ausgewählter Länder?

# QUELLEN:
#   https://github.com/owid/energy-data
#   https://github.com/owid/co2-data

# =============================================================================
# 0. PAKETE
# =============================================================================

# install.packages(c(
#   "tidyverse", "gganimate", "gifski",
#   "scales", "ggrepel", "patchwork", "here"
# ))

library(tidyverse)
library(gganimate)
library(gifski)
library(scales)
library(ggrepel)
library(patchwork)
library(here)


# =============================================================================
# 1. DATA LOADING
# =============================================================================

cat("Loading data from Our World in Data...\n")

energy_raw <- read_csv(
  "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv",
  show_col_types = FALSE
)

co2_raw <- read_csv(
  "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv",
  show_col_types = FALSE
)

cat("Energy:", nrow(energy_raw), "rows,", ncol(energy_raw), "cols\n")
cat("CO2:   ", nrow(co2_raw),    "rows,", ncol(co2_raw),    "cols\n\n")


# =============================================================================
# 2. SETTINGS
# =============================================================================

countries <- c("Germany", "France", "Poland", "Spain", "Denmark",
               "China", "Brazil")

# Mapping: English name -> German label (used on plots)
country_labels <- c(
  "Germany" = "Deutschland",
  "France"  = "Frankreich",
  "Poland"  = "Polen",
  "Spain"   = "Spanien",
  "Denmark" = "Dänemark",
  "China"   = "China",
  "Brazil"  = "Brasilien"
)

START <- 1990
END   <- 2024

cat("Data range:", START, "-", END, "\n\n")

caption_src <- paste0(
  "Quelle: Our World in Data | github.com/owid/energy-data & owid/co2-data | ",
  "Abgerufen: ", format(Sys.Date(), "%d.%m.%Y")
)

# Output folder – all plots saved here
# here() anchors every path to the .Rproj root
dir.create(here("output"), showWarnings = FALSE, recursive = TRUE)
cat("Output folder:", here("output"), "\n\n")


# =============================================================================
# 3. DESIGN SYSTEM
# =============================================================================

# Country colors:
country_colors <- c(
  "Deutschland" = "#1D6199",
  "Frankreich"  = "#7A5CA3",
  "Polen"       = "#4D4D4D",
  "Spanien"     = "#B22222",
  "Dänemark"    = "#66AA66",
  "China"       = "#E6AB02",
  "Brasilien"   = "#80CBC4"
)

# Energy source order and colors: semantically coded
# (coal = near black, water = blue, sun = yellow)
source_order <- c("Kohle", "Oel", "Gas", "Kernkraft",
                  "Wasserkraft", "Wind", "Solar", "Biomasse", "Sonstige")

# Display labels for axes/legends (Oel -> Öl for plot output)
source_display <- c(
  "Kohle"       = "Kohle",
  "Oel"         = "Öl",
  "Gas"         = "Gas",
  "Kernkraft"   = "Kernkraft",
  "Wasserkraft" = "Wasserkraft",
  "Wind"        = "Wind",
  "Solar"       = "Solar",
  "Biomasse"    = "Biomasse",
  "Sonstige"    = "Sonstige"
)

source_colors <- c(
  "Kohle"       = "#2C2C2C",
  "Oel"         = "#6B3A2A",
  "Gas"         = "#5f7C84",
  "Kernkraft"   = "#D98A2B",
  "Wasserkraft" = "#2980B9",
  "Wind"        = "#7BAF85",
  "Solar"       = "#E6B800",
  "Biomasse"    = "#964999",
  "Sonstige"    = "#DADADA"
)

# Good-Practice theme 
theme_dyn <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title           = element_text(face = "bold", size = base_size + 4,
                                        margin = margin(b = 6)),
      plot.subtitle        = element_text(size = base_size + 1, color = "grey35",
                                        margin = margin(b = 10)),
      plot.caption         = element_text(size = base_size - 3, color = "grey55",
                                        hjust = 0, margin = margin(t = 8)),
      axis.title           = element_text(face = "bold", size = base_size),
      axis.text            = element_text(size = base_size - 1, color = "#333333"),
      legend.title         = element_text(face = "plain", size = base_size - 1),
      legend.text          = element_text(size = base_size - 2),
      legend.position      = "top",
      legend.justification = "left",
      legend.box.just      = "left",
      legend.margin        = margin(b = -6),
      panel.grid.major.x   = element_blank(),
      panel.grid.minor     = element_blank(),
      panel.grid.major.y   = element_line(color = "#EEEEEE", linewidth = 0.5),
      plot.background      = element_rect(fill = "white", color = NA),
      plot.margin          = margin(15, 40, 15, 15)
    )
}

# Bad-Practice theme 
theme_bad <- function(base_size = 9) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title      = element_text(size = base_size, face = "plain",
                                     color = "grey40"),
      plot.subtitle   = element_text(size = base_size - 1, color = "grey50"),
      axis.title      = element_text(face = "plain", size = base_size - 1),
      axis.text       = element_text(size = base_size - 2, color = "grey40"),
      legend.position = "bottom",
      legend.title    = element_text(size = base_size - 2),
      panel.grid      = element_line(color = "#DDDDDD", linewidth = 0.3),
      plot.margin     = margin(5, 5, 5, 5)
    )
}

# Helper: animate and save GIF into output/
save_gif <- function(plot, filename, n_frames, fps = 12,
                     width = 1000, height = 620) {
  path <- here("output", filename)
  anim_save(
    path,
    animate(plot, nframes = n_frames, fps = fps,
            width = width, height = height,
            renderer = gifski_renderer(), end_pause = 150)
  )
  cat("Saved:", path, "\n")
}


# =============================================================================
# 4. DATA PREPARATION
# =============================================================================

co2_only <- co2_raw %>%
  select(country, year, co2_per_capita)

df <- energy_raw %>%
  filter(country %in% countries, year >= START, year <= END) %>%
  left_join(co2_only, by = c("country", "year")) %>%
  transmute(
    country,
    year,
    label            = country_labels[country],   
    re_share         = renewables_share_elec,      
    co2_per_cap      = co2_per_capita,            
    energy_per_cap   = energy_per_capita,          
    population       = population,                 
    pop_mio          = population / 1e6
  ) %>%
  mutate(label = factor(label, levels = unname(country_labels)))

# Merge check
cat("Merge check (non-NA counts per country):\n")
df %>%
  group_by(label) %>%
  summarise(
    n          = n(),
    re_ok      = sum(!is.na(re_share)),
    co2_ok     = sum(!is.na(co2_per_cap)),
    energy_ok  = sum(!is.na(energy_per_cap)),
    .groups    = "drop"
  ) %>% print()
cat("\n")

# Energy mix – long format ---------------------------------------------------
# Column names verified in owid-energy-data.csv (2024):
#   coal_electricity, oil_electricity, gas_electricity, nuclear_electricity,
#   hydro_electricity, wind_electricity, solar_electricity,
#   biofuel_electricity, other_renewable_electricity

mix_long <- energy_raw %>%
  filter(country %in% countries, year >= START, year <= END) %>%
  transmute(
    country, year,
    label       = country_labels[country],
    Kohle       = coal_electricity,
    Oel         = oil_electricity,
    Gas         = gas_electricity,
    Kernkraft   = nuclear_electricity,
    Wasserkraft = hydro_electricity,
    Wind        = wind_electricity,
    Solar       = solar_electricity,
    Biomasse    = biofuel_electricity,
    Sonstige    = other_renewable_electricity
  ) %>%
  pivot_longer(Kohle:Sonstige, names_to = "source", values_to = "twh") %>%
  mutate(
    source = factor(source, levels = source_order),
    label  = factor(label,  levels = unname(country_labels)),
    twh    = replace_na(twh, 0)
  )

# Shares (makes countries comparable regardless of absolute size)
mix_share <- mix_long %>%
  group_by(country, label, year) %>%
  mutate(
    total = sum(twh, na.rm = TRUE),
    share = if_else(total > 0, twh / total * 100, 0)
  ) %>%
  ungroup()


# =============================================================================
# KAPITEL 1 - DER AUFSTIEG DER ERNEUERBAREN
# Erneuerbare wachsen, aber sieben Länder, sieben Wege.
# =============================================================================

# ---------------------------------------------------------------------------
# G01 - LINE CHART ANIMIERT: EE-Anteil im Zeitverlauf
#
# Story: Sieben Länder, sieben komplett verschiedene Pfade. Dänemark schießt
#        nach oben dank Wind. Brasilien war schon 1990 hoch, aber warum?
#        (Wasserkraft!) Polen bewegt sich fast kaum. Deutschland beschleunigt
#        nach 2010 dramatisch.
# Technik: transition_reveal() - Linien wachsen Jahr für Jahr auf.
# ---------------------------------------------------------------------------

cat("\n=== G01: Line Chart - EE-Anteil animiert ===\n")

d01 <- df %>% filter(!is.na(re_share))

p01 <- ggplot(d01, aes(year, re_share, color = label, group = label)) +
  geom_line(linewidth = 1.0) +
  geom_point(aes(group = interaction(label, year)),
             size = 1.6, alpha = 0.8, shape = 16) +
  scale_color_manual(values = country_colors, name = NULL) + 
  scale_x_continuous(breaks = seq(START, END, 5),
                     limits = c(START, END + 1)) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     limits = c(0, 105)) +
  labs(
    title    = "Der Aufstieg der Erneuerbaren (1990-2024) - sieben verschiedene Wege",
    subtitle = "Jahr: {round(frame_along)} | Anteil erneuerbarer Energien an der Stromproduktion",
    x = NULL, y = "EE-Anteil (%)",
    caption = caption_src
  ) +
  theme_dyn() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.key = element_blank() 
  ) +
  guides(color = guide_legend(nrow = 1, override.aes = list(linetype = 0, size = 5))) +
  transition_reveal(year)

save_gif(p01, "G01_ee_anteil_linechart.gif",
         n_frames = 350, fps = 14, width = 1000, height = 600)


# ---------------------------------------------------------------------------
# G02 - BAD PRACTICE: Linienchart überladen
# Probleme: zu viele Linien, schlechte Graustufen, keine %-Achse, schwacher Titel
# ---------------------------------------------------------------------------

cat("=== G02: Bad Practice - überladener Linienchart ===\n")

d02 <- df %>% filter(!is.na(re_share))

bad_line_colors <- setNames(
  rep(c("#2F2F2F", "#777777", "#BDBDBD", "#66AA66"), length.out = length(levels(d02$label))),
  levels(d02$label)
)

p02 <- ggplot(d02, aes(year, re_share, color = label, linetype = label, group = label)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 0.9) +
  scale_color_manual(values = bad_line_colors) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, END)) +
  scale_y_continuous(limits = c(10, 100)) +
  labs(
    title = "Bad Practice: Entwicklung erneuerbarer Energien",
    x = "Jahr", y = "Wert",
    color = "Land", linetype = "Land"
  ) +
  theme_bad() +
  transition_reveal(year)

save_gif(p02, "G02_bad_linechart.gif",
         n_frames = 160, fps = 12, width = 900, height = 520)


# ---------------------------------------------------------------------------
# G03 - BAR CHART RACE: Wer führt das EE-Ranking?
#
# Story: Das Ranking ist nicht statisch! Dänemark führt konstant - dahinter
#        passiert viel. China macht einen dramatischen Sprung. Polen bleibt
#        am Ende. Spanien überholt Deutschland.
# Technik: transition_states() + Rang-Neuberechnung pro Jahr.
# ---------------------------------------------------------------------------

cat("=== G03: Bar Chart Race - EE-Ranking ===\n")

d03 <- df %>%
  filter(!is.na(re_share)) %>%
  group_by(year) %>%
  mutate(rank = rank(-re_share, ties.method = "first")) %>%
  ungroup() %>%
  mutate(year_fct = factor(year))

p03 <- ggplot(d03, aes(rank, re_share, fill = label, group = label)) +
  geom_col(width = 0.75, alpha = 0.92) +
  geom_text(aes(y = -2, label = label),
            hjust = 1.1, color = "gray35", size = 4.8) +
  coord_flip(clip = "off") +
  scale_x_reverse() +
  scale_y_continuous(limits = c(-15, 105), breaks = seq(0, 100, 20),
                     labels = label_percent(scale = 1)) +
  scale_fill_manual(values = country_colors, guide = "none") +
  labs(
    title = "Erneuerbare Energien im Zeitvergleich: Wer führt den Wandel an?",
    subtitle = "Jahr: {closest_state} | Zeitraum: 1990-2024",
    x = NULL, y = "EE-Anteil an der Stromproduktion (%)",
    caption = caption_src
  ) +
  theme_dyn() +
  theme(panel.grid.major.y = element_blank(),
        axis.text.y        = element_blank(),
        axis.title.x       = element_text(margin = margin(t = 20)),
        plot.margin        = margin(15, 20, 15, 15)) +
  transition_states(year_fct, transition_length = 2,
                    state_length = 1, wrap = FALSE) +
  ease_aes("cubic-in-out")

save_gif(p03, "G03_bar_chart_race.gif",
         n_frames = length(unique(d03$year)) * 10,
         fps = 14, width = 950, height = 580)


# ---------------------------------------------------------------------------
# G04 - BAD PRACTICE: Ranking falsch lesbar
# Probleme: niedrige Werte stehen oben, Achse und Titel sind unklar
# ---------------------------------------------------------------------------

cat("=== G04: Bad Practice - Ranking ===\n")

d04 <- df %>%
  filter(!is.na(re_share)) %>%
  group_by(year) %>%
  mutate(rank_bad = rank(re_share, ties.method = "first")) %>%
  ungroup() %>%
  mutate(year_fct = factor(year))

p04 <- ggplot(d04, aes(rank_bad, re_share, fill = label, group = label)) +
  geom_col(width = 0.75, alpha = 0.85) +
  geom_text(aes(label = label), hjust = -0.05, size = 3.2) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = country_colors, guide = "none") +
  scale_x_reverse() +
  scale_y_continuous(limits = c(0, 105)) +
  labs(
    title = "Bad Practice: Ranking",
    x = NULL, y = "Wert"
  ) +
  theme_bad() +
  transition_states(year_fct, transition_length = 2, state_length = 1, wrap = FALSE) +
  ease_aes("linear")

save_gif(p04, "G04_bad_ranking.gif",
         n_frames = length(unique(d04$year)) * 8,
         fps = 12, width = 900, height = 520)

# ---------------------------------------------------------------------------
# G05 - STACKED AREA FACETS: Alle 7 Länder 
# WIE ÄNDERT SICH DER STROMMIX?
# Story: Der Wandel ist real, aber strukturell sehr verschieden.
# Deutschland ist nicht repräsentativ. Brasilien war immer grün
#        (Wasserkraft). Polen hat sich kaum verändert. Frankreich: wenig EE
#        sichtbar, trotzdem niedrige CO2.
# Technik: facet_wrap, gleiche Farbskala -> alle Länder sofort vergleichbar.
# ---------------------------------------------------------------------------

cat("=== G05: Stacked Area Facets - alle Länder ===\n")

end_label <- paste0("'", END %% 100)

p05 <- mix_share %>%
  ggplot(aes(year, share, fill = source)) +
  geom_area(stat = "identity", alpha = 0.9, color = "white", linewidth = 0.2) +
  facet_wrap(~ label, ncol = 4) +
  scale_fill_manual(values = source_colors, labels = source_display,
                    name = NULL) +
  scale_x_continuous(
    breaks = c(seq(1990, 2020, 10)),
    minor_breaks = seq(1990, 2024, 5),
    labels = c("'90", "'00", "'10", "'20")
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title    = paste0("Strommix 1990-", END, ": Kein Land gleicht dem anderen"),
    subtitle = "Anteile der Energiequellen an der Stromproduktion",
    x = NULL, y = "Anteil (%)",
    caption = caption_src
  ) +
  theme_dyn(base_size = 11) +
  theme(
    legend.position    = "top",
    legend.direction   = "horizontal",
    legend.margin      = margin(b = 20),
    legend.key.size    = unit(0.25, "cm"),
    panel.spacing      = unit(1.0, "lines"),
    panel.grid.major.x = element_blank(),
    axis.title.y       = element_text(margin = margin(r = 10))
  ) +
  guides(fill = guide_legend(nrow = 1))
  
ggsave(here("output", "G05_strommix_alle_laender.png"), p05,
       width = 14, height = 7, dpi = 150, bg = "white")
cat("Saved:", here("output", "G05_strommix_alle_laender.png"), "\n")


# ---------------------------------------------------------------------------
# G06 - BAD PRACTICE: Strommix als unübersichtliche Facets
# Probleme: absolute TWh statt Anteile, unterschiedliche Skalen
# ---------------------------------------------------------------------------

cat("=== G06: Bad Practice - Strommix Facets ===\n")

p06 <- mix_long %>%
  ggplot(aes(year, twh, fill = source)) +
  geom_area(alpha = 0.90, position = "stack") +
  facet_wrap(~ label, ncol = 4, scales = "free_y") +
  scale_fill_manual(values = source_colors, labels = source_display, name = "Quelle") +
  labs(
    title = "Bad Practice: Strommix",
    x = NULL, y = "TWh"
  ) +
  theme_bad() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 9)
  )

ggsave(here("output", "G06_bad_strommix_facets.png"), p06,
       width = 14, height = 8, dpi = 150, bg = "white")


# ---------------------------------------------------------------------------
# G07 - STACKED AREA ANIMIERT: Deutschland - Energiewende live
#
# Story: Die schwarze Kohlefläche schrumpft sichtbar. Wind und Solar wachsen
#        exponentiell erst nach 2010. Man sieht die Subventionspolitik
#        förmlich wirken.
# Technik: transition_reveal() - Flächen wachsen nach rechts auf.
# ---------------------------------------------------------------------------

cat("=== G07: Stacked Area animiert - Deutschland ===\n")

p07 <- mix_share %>%
  filter(country == "Germany") %>%
  ggplot(aes(year, share, fill = source, group = source)) +
  geom_area(stat = "identity", alpha = 0.9, color = "white", linewidth = 0.25) +
  scale_fill_manual(values = source_colors, labels = source_display, name = NULL) +
  scale_x_continuous(breaks = seq(START, END, 5), limits = c(START, END)) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 101)) +
  labs(
    title    = "Deutschlands Energiewende: Kohle weicht Wind und Solar",
    subtitle = "Jahr: {round(frame_along)} | Zeitraum: 1990-2024",
    x = NULL, y = "Anteil an der Stromproduktion (%)",
    caption = caption_src
  ) +
  theme_dyn() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  transition_reveal(year)

save_gif(p07, "G07_strommix_de_animiert.gif",
         n_frames = 350, fps = 14, width = 1000, height = 580)


# ---------------------------------------------------------------------------
# G08 - STACKED AREA ANIMIERT: Kohle bleibt dominierend
#
# Story: Der Kontrast zu Deutschland.
#        Kohle dominiert fast unverändert. Erst durch diesen Kontrast wird
#        die deutsche Energiewende wirklich beeindruckend.
# ---------------------------------------------------------------------------

cat("\n=== G08: Stacked Area animiert - Polen ===\n")

p08 <- mix_share %>%
  filter(country == "Poland") %>%
  ggplot(aes(year, share, fill = source, group = source)) +
  geom_area(stat = "identity", alpha = 0.9, color = "white", linewidth = 0.25) +
  scale_fill_manual(values = source_colors, labels = source_display,
                    name = NULL) +
  scale_x_continuous(breaks = seq(START, END, 5), limits = c(START, END)) +
  scale_y_continuous(labels = label_percent(scale = 1), limits = c(0, 101)) +
  labs(
    title    = "Polens Energiemix: Kohle bleibt dominierend",
    subtitle = "Jahr: {round(frame_along)} | Zeitraum: 1990-2024",
    x = NULL, y = "Anteil an der Stromproduktion (%)",
    caption = caption_src
  ) +
  theme_dyn() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
  ) +
  guides(fill = guide_legend(nrow = 1)) +
  transition_reveal(year)

save_gif(p08, "G08_strommix_pl_animiert.gif",
         n_frames = 360, fps = 14, width = 1000, height = 580)


# =============================================================================
# KAPITEL 2 - FÜHRT MEHR EE ZU WENIGER CO2?
# Tendenz ja, aber es ist komplizierter als gedacht.
# =============================================================================

# ---------------------------------------------------------------------------
# G09 - BUBBLE CHART: Energieverbrauch vs. Emissionen
# 
# Story: Mehr Energieverbrauch pro Kopf geht oft mit höheren CO2-Emissionen
#        einher, aber nicht überall gleich stark. Der Vergleich zeigt, dass
#        Länder unterschiedliche Entwicklungspfade haben: China wächst stark,
#        Brasilien bleibt emissionsarm, während europäische Länder trotz hohem
#        Energieverbrauch sehr unterschiedliche CO2-Werte zeigen.
# Technik: transition_states() für flüssige Bewegung, log. X-Achse.
# ---------------------------------------------------------------------------

cat("=== G09: Energieverbrauch vs. CO2-Emissionen ===\n")

d09 <- df %>% 
  filter(!is.na(energy_per_cap), !is.na(co2_per_cap), !is.na(population)) %>%
  mutate(year_fct = factor(year))

p09 <- ggplot(d09,
              aes(energy_per_cap, co2_per_cap,
                  size = population, color = label, group = label)) +
  geom_point(alpha = 0.82) +
  scale_color_manual(values = country_colors, name = "Land") +
  scale_size_area(max_size = 26, name = "Bevölkerung",
                  breaks = c(50e6, 200e6, 1e9),
                  labels = c("50 Mio.", "200 Mio.", "1 000 Mio.")) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale(), suffix = " kWh"),
                breaks = c(5000, 10000, 20000, 50000, 100000),
                limits = c(3000, 120000)) +
  scale_y_continuous(labels = label_number(suffix = " t"),
                     limits = c(0, 16),
                     breaks = c(0, 5, 10, 15)) +
  labs(
    title    = "Energieverbrauch und CO2-Emissionen im Vergleich",
    subtitle = "Jahr: {closest_state} | Zeitraum: 1990-2024",   
    x = "Primärenergieverbrauch pro Kopf (kWh, log. Skala)",
    y = "CO2-Emissionen pro Kopf (Tonnen)",
    caption = caption_src
  ) +
  theme_dyn(base_size = 13) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(face = "bold", margin = margin(t = 18)),
    plot.caption = element_text(size = 10, color = "grey55",
                                hjust = 0, margin = margin(t = 14)),
    plot.margin = margin(15, 55, 20, 15),
    panel.grid.major.x = element_line(color = "#EEEEEE", linewidth = 0.5)
  ) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(size = 3, alpha = 0.9),
      theme = theme(legend.margin = margin(b = 28))
    ),
    size = guide_legend(
      order = 2,
      override.aes = list(color = "#9A9A9A", fill = "#BDBDBD", alpha = 0.75)
    )
  ) +
  transition_states(year_fct, transition_length = 5,
                    state_length = 2, wrap = FALSE) +
  ease_aes("linear")

save_gif(p09, "G09_energieverbrauch_vs_co2.gif",
         n_frames = length(unique(d09$year)) * 12,
         fps = 12, width = 1060, height = 670)


# ---------------------------------------------------------------------------
# G10 - BUBBLE CHART: EE-Anteil vs. CO2
#
# Story: Tendenziell gilt: mehr EE -> weniger CO2. Aber kein klarer Zusammenhang:
#        Frankreich hat niedrige CO2 trotz mittlerem EE (Kernkraft),
#        China steigert EE, bleibt aber emissionsintensiv.
#
# Technik: transition_states(), Punktgröße = Bevölkerung.
# ---------------------------------------------------------------------------

cat("=== G10: EE-Anteil vs. CO2 - Bubble Chart ===\n")

d10 <- df %>%
  filter(!is.na(re_share), !is.na(co2_per_cap), !is.na(population)) %>%
  mutate(year_fct = factor(year))

p10 <- ggplot(d10,
              aes(re_share, co2_per_cap,
                  size = population, color = label, group = label)) +
  geom_point(alpha = 0.82) +
  scale_color_manual(values = country_colors, name = "Land") +
  scale_size_area(
    max_size = 26,   
    name = "Bevölkerung",
    breaks = c(50e6, 200e6, 1e9),
    labels = c("50 Mio.", "200 Mio.", "1 000 Mio.")
  ) +
  scale_x_continuous(
    labels = label_percent(scale = 1),
    limits = c(0, 100),  
    breaks = seq(0, 100, 25)
  ) +
  scale_y_continuous(
    labels = label_number(suffix = " t"),
    limits = c(0, 16),
    breaks = c(0, 5, 10, 15)
  ) +
  labs(
    title    = "Erneuerbare Energien und CO2-Emissionen im Vergleich",
    subtitle = "Jahr: {closest_state} | Zeitraum: 1990-2024",
    x = "Anteil erneuerbarer Energien (%)",
    y = "CO2-Emissionen pro Kopf (Tonnen)",
    caption = caption_src
  ) +
  theme_dyn(base_size = 13) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(face = "bold", margin = margin(t = 18)),
    plot.caption = element_text(size = 10, color = "grey55",
                                hjust = 0, margin = margin(t = 14)),
    plot.margin = margin(15, 55, 20, 15),   
    panel.grid.major.x = element_line(color = "#EEEEEE", linewidth = 0.5)
  ) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(size = 3, alpha = 0.9),
      theme = theme(legend.margin = margin(b = 28))
    ),
    size = guide_legend(
      order = 2,
      override.aes = list(color = "#9A9A9A", fill = "#BDBDBD", alpha = 0.75)
    )
  ) +
  transition_states(year_fct, transition_length = 5,
                    state_length = 2, wrap = FALSE) +
  ease_aes("linear")

save_gif(p10, "G10_ee_anteil_vs_co2.gif",
         n_frames = length(unique(d10$year)) * 12,
         fps = 12, width = 1060, height = 670)


# ---------------------------------------------------------------------------
# G11 - BAD PRACTICE: Bubble Chart
# Probleme: Farben, Achsen schlecht formatiert, Legende dominiert
# ---------------------------------------------------------------------------

cat("=== G11: Bad Practice - Bubble Chart ===\n")

d11 <- df %>%
  filter(!is.na(energy_per_cap), !is.na(co2_per_cap), !is.na(population)) %>%
  mutate(year_fct = factor(year))

p11 <- ggplot(d11,
              aes(energy_per_cap, co2_per_cap,
                  size = population, color = re_share, label = label)) +
  geom_point(alpha = 0.45) +
  geom_text(size = 3.2, alpha = 0.55) +
  scale_color_gradient(low = "grey80", high = "darkgreen", name = "EE") +
  scale_size_area(max_size = 45, name = "Pop.") +
  scale_x_continuous() +
  scale_y_continuous(limits = c(0, 16)) +
  labs(
    title = "Bad Practice: CO2 und Energieverbrauch",
    x = "Energie",
    y = "CO2"
  ) +
  theme_bad() +
  theme(legend.position = "bottom") +
  transition_states(year_fct, transition_length = 3, state_length = 1, wrap = FALSE) +
  ease_aes("linear")

save_gif(p11, "G11_bad_bubblechart.gif",
         n_frames = length(unique(d11$year)) * 8,
         fps = 12, width = 950, height = 560)


# ---------------------------------------------------------------------------
# G12 - CO2 LINE CHART: CO2 pro Kopf im Zeitverlauf
#
# Story: Hat der EE-Ausbau CO2 wirklich gesenkt? Die direkte Antwort.
#        Deutschland: deutlicher Rückgang nach 2010.
#        China: dramatischer Anstieg bis ~2013, seither Stabilisierung.
#        Frankreich: konsistent niedrig -> Kernkraft-Effekt, nicht EE.
#        Polen: fast kaum Rückgang - strukturell kohlebasiert.
# ---------------------------------------------------------------------------


cat("=== G12: CO2 Line Chart ===\n")

d12 <- df %>% filter(!is.na(co2_per_cap))

p12 <- ggplot(d12, aes(year, co2_per_cap, color = label, group = label)) +
  geom_line(linewidth = 1.0) +
  geom_point(aes(group = interaction(label, year)),
             size = 1.6, alpha = 0.8, shape = 16) +
  scale_color_manual(values = country_colors, name = NULL) +
  scale_x_continuous(breaks = seq(START, END, 5),
                     limits = c(START, END + 1)) +
  scale_y_continuous(labels = label_number(suffix = " t"),
                     limits = c(0, 16),
                     breaks = c(0, 5, 10, 15)) +
  labs(
    title    = "CO2-Emissionen pro Kopf: Entwicklung im Vergleich",
    subtitle = "Jahr: {round(frame_along)} | Zeitraum: 1990-2024",
    x = NULL,
    y = "CO2 pro Kopf (Tonnen)",
    caption = caption_src
  ) +
  theme_dyn() +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.key = element_blank()
  ) +
  guides(color = guide_legend(
    nrow = 1,
    override.aes = list(linetype = 0, size = 5)
  )) +
  transition_reveal(year)

save_gif(p12, "G12_co2_line_chart.gif",
         n_frames = 350, fps = 14, width = 1000, height = 600)


# OUTPUT OVERVIEW
# =============================================================================

cat("\n")
cat("================================================================\n")
cat(paste0("DONE - Data range: ", START, "-", END, "\n"))
cat("================================================================\n\n")
cat("  G01_ee_anteil_linechart.gif          Good: EE-Anteil Line Chart\n")
cat("  G02_bad_linechart.gif                Bad: überladener Linienchart\n\n")
cat("  G03_bar_chart_race.gif               Good: EE-Ranking Race\n")
cat("  G04_bad_ranking.gif                  Bad: Ranking schwer lesbar\n\n")
cat("  G05_strommix_alle_laender.png        Good: Strommix 7 Länder (PNG)\n")
cat("  G06_bad_strommix_facets.gif          Bad: Strommix mit freien Skalen\n\n")
cat("  G07_strommix_de_animiert.gif         Strommix Deutschland\n")
cat("  G08_strommix_pl_animiert.gif         Strommix Polen - Kontrast\n")
cat("  G09_energieverbrauch_vs_co2.gif      Energieverbrauch vs. CO2-Emissionen\n")
cat("  G10_ee_anteil_vs_co2.gif             Good: Bubble EE vs. CO2\n")
cat("  G11_bad_bubblechart.gif              Bad: überladener Bubble Chart\n")
cat("  G12_co2_line_chart.gif               CO2 Line Chart\n")
cat("================================================================\n")
