#' Site-wide ggplot2 themes and palettes.
#'
#' One theme per network (Incomparable, Relay.fm, ATP) plus a neutral theme
#' for the index. Each theme pairs a typography choice with a brand palette
#' derived from the network's own site.
#'
#' Call [register_site_fonts()] once per session before plotting — usually
#' from `_setup.qmd`.

# Brand palettes -----------------------------------------------------------

palette_incomparable <- c(
  navy_dark = "#1f2250",
  navy      = "#2a2e6d",
  navy_lt   = "#393f92",
  warm_dk   = "#393431",
  warm      = "#5d554f",
  warm_lt   = "#978d85",
  cream     = "#e5e2e0",
  orange_dk = "#ee7a06",
  orange    = "#f98e24",
  yellow    = "#fca935"
)

palette_relayfm <- c(
  slate_dk = "#1c2730",
  slate    = "#333f48",
  gray_dk  = "#5a6770",
  gray     = "#75787b",
  gray_lt  = "#a8adb2",
  cream    = "#f2f0eb",
  accent   = "#36667e"
)

palette_atp <- c(
  blue   = "#119AD6",
  purple = "#934196",
  red    = "#E33A41",
  orange = "#F17F21",
  yellow = "#F6B421",
  green  = "#64B648",
  gray   = "#BAC2CC"
)

# Anchor color per network — used on the neutral/index theme to identify a
# network without invoking its full palette. Derived from the Okabe-Ito
# colorblind-safe palette (Wilke ch. 19) rather than each network's exact
# brand hex: Incomparable's navy and Relay's slate are both dark cool blues
# and become hard to discriminate on the same plot. Okabe-Ito tones below
# preserve each network's identity (cool blue, teal-green, orange) while
# remaining perceptually distinct and CVD-safe.
network_colors <- c(
  "The Incomparable" = "#0072B2",  # Okabe-Ito blue   (≈ Incomparable navy_lt)
  "relay.fm"         = "#009E73",  # Okabe-Ito green  (distinct from any blue)
  "ATP"              = "#D55E00"   # Okabe-Ito vermillion (≈ ATP stripe red/orange)
)

# Fonts --------------------------------------------------------------------

#' Register the fonts used by site themes.
#'
#' Downloads Google Fonts via [sysfonts::font_add_google()] and enables
#' [showtext::showtext_auto()] so ggplot2 picks them up. Idempotent.
register_site_fonts <- function() {
  if (!requireNamespace("sysfonts", quietly = TRUE) ||
      !requireNamespace("showtext", quietly = TRUE)) {
    return(invisible(FALSE))
  }
  already <- sysfonts::font_families()
  to_add <- list(
    "Besley"         = "besley",
    "Roboto"         = "roboto",
    "Montserrat"     = "montserrat",
    "Inter"          = "inter",
    "JetBrains Mono" = "jetbrains"
  )
  for (i in seq_along(to_add)) {
    family <- to_add[[i]]
    if (!family %in% already) {
      sysfonts::font_add_google(name = names(to_add)[i], family = family)
    }
  }
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 96)
  invisible(TRUE)
}

# Theme base ---------------------------------------------------------------

# Common foundation: theme_minimal with proportional ink, left-aligned plot
# title, restrained gridlines. Each network theme adjusts typography and
# small accents on top.
.theme_base <- function(base_size = 13,
                        base_family = "",
                        title_family = base_family,
                        caption_family = base_family,
                        fg = "grey15",
                        muted = "grey40",
                        gridline = "grey90") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text             = ggplot2::element_text(color = fg),
      plot.title       = ggplot2::element_text(
        family = title_family, face = "bold",
        size = ggplot2::rel(1.25), margin = ggplot2::margin(b = 6)
      ),
      plot.subtitle    = ggplot2::element_text(
        color = muted, size = ggplot2::rel(0.95),
        margin = ggplot2::margin(b = 12)
      ),
      plot.caption     = ggplot2::element_text(
        family = caption_family, color = muted,
        size = ggplot2::rel(0.8), hjust = 0,
        margin = ggplot2::margin(t = 12)
      ),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin           = ggplot2::margin(8, 8, 8, 8),
      axis.title.x          = ggplot2::element_text(margin = ggplot2::margin(t = 8), color = muted),
      axis.title.y          = ggplot2::element_text(margin = ggplot2::margin(r = 8), color = muted),
      axis.text             = ggplot2::element_text(color = muted),
      panel.grid.major      = ggplot2::element_line(color = gridline, linewidth = 0.3),
      panel.grid.minor      = ggplot2::element_blank(),
      panel.spacing         = ggplot2::unit(1, "lines"),
      legend.position       = "top",
      legend.title          = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.9)),
      legend.text           = ggplot2::element_text(size = ggplot2::rel(0.85)),
      strip.text            = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.9))
    )
}

# Per-network themes -------------------------------------------------------

#' Neutral theme for the index and cross-network pages.
theme_neutral <- function(base_size = 13) {
  .theme_base(
    base_size      = base_size,
    base_family    = "inter",
    title_family   = "inter",
    caption_family = "inter"
  )
}

#' Theme for The Incomparable network pages.
#'
#' Besley (serif) headlines + Roboto body, navy text, warm cream rules.
theme_incomparable <- function(base_size = 13) {
  navy <- palette_incomparable[["navy_dark"]]
  cream <- palette_incomparable[["cream"]]
  .theme_base(
    base_size      = base_size,
    base_family    = "roboto",
    title_family   = "besley",
    caption_family = "roboto",
    fg             = navy,
    muted          = palette_incomparable[["warm"]],
    gridline       = cream
  ) +
    ggplot2::theme(
      axis.line   = ggplot2::element_line(color = navy, linewidth = 0.3),
      plot.title  = ggplot2::element_text(family = "besley", face = "bold",
                                          size = ggplot2::rel(1.35), color = navy)
    )
}

#' Theme for Relay.fm network pages.
#'
#' Montserrat throughout, slate text, restrained gridlines.
theme_relayfm <- function(base_size = 13) {
  slate <- palette_relayfm[["slate_dk"]]
  .theme_base(
    base_size      = base_size,
    base_family    = "montserrat",
    title_family   = "montserrat",
    caption_family = "montserrat",
    fg             = slate,
    muted          = palette_relayfm[["gray_dk"]],
    gridline       = palette_relayfm[["cream"]]
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = "montserrat", face = "bold",
                                         size = ggplot2::rel(1.25), color = slate)
    )
}

#' Theme for ATP pages.
#'
#' Inter (system-UI feel) for body + JetBrains Mono for captions/annotations.
theme_atp <- function(base_size = 13) {
  .theme_base(
    base_size      = base_size,
    base_family    = "inter",
    title_family   = "inter",
    caption_family = "jetbrains",
    fg             = "grey10",
    muted          = "grey40",
    gridline       = "grey92"
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(family = "inter", face = "bold",
                                         size = ggplot2::rel(1.25))
    )
}

# Scales -------------------------------------------------------------------

# Discrete qualitative — anchor per network.
.discrete_palette_factory <- function(colors) {
  function(n) {
    if (n > length(colors)) {
      warning(
        "Discrete palette has ", length(colors), " colors; ", n, " requested. ",
        "Falling back to ggplot2 hue scale for the overflow.",
        call. = FALSE
      )
      c(unname(colors), scales::hue_pal()(n - length(colors)))
    } else {
      unname(colors)[seq_len(n)]
    }
  }
}

.incomparable_seq <- c("navy", "orange", "yellow", "navy_lt", "warm", "orange_dk")
.relayfm_seq     <- c("slate", "accent", "gray_dk", "gray", "gray_lt")
.atp_seq         <- c("blue", "purple", "red", "orange", "yellow", "green")

scale_color_incomparable <- function(...) {
  ggplot2::discrete_scale(
    "colour",
    palette = .discrete_palette_factory(unname(palette_incomparable[.incomparable_seq])),
    ...
  )
}
scale_fill_incomparable <- function(...) {
  ggplot2::discrete_scale(
    "fill",
    palette = .discrete_palette_factory(unname(palette_incomparable[.incomparable_seq])),
    ...
  )
}

scale_color_relayfm <- function(...) {
  ggplot2::discrete_scale(
    "colour",
    palette = .discrete_palette_factory(unname(palette_relayfm[.relayfm_seq])),
    ...
  )
}
scale_fill_relayfm <- function(...) {
  ggplot2::discrete_scale(
    "fill",
    palette = .discrete_palette_factory(unname(palette_relayfm[.relayfm_seq])),
    ...
  )
}

# ATP — the iconic 6-color stripe in display order.
scale_color_atp <- function(...) {
  ggplot2::discrete_scale(
    "colour",
    palette = .discrete_palette_factory(unname(palette_atp[.atp_seq])),
    ...
  )
}
scale_fill_atp <- function(...) {
  ggplot2::discrete_scale(
    "fill",
    palette = .discrete_palette_factory(unname(palette_atp[.atp_seq])),
    ...
  )
}

# Network-anchor scale for plots that compare networks side by side.
scale_color_network <- function(...) {
  ggplot2::scale_color_manual(values = network_colors, ...)
}
scale_fill_network <- function(...) {
  ggplot2::scale_fill_manual(values = network_colors, ...)
}
