

# we use VizPalette to generate a nice divergent palette for the graphs
# https://projects.susielu.com/viz-palette?colors=[%22#1984c5%22,%22#22a7f0%22,%22#63bff0%22,%22#a7d5ed%22,%22#e2e2e2%22,%22#e1a692%22,%22#de6e56%22,%22#e14b31%22,%22#c23728%22]&backgroundColor=%22white%22&fontColor=%22black%22&mode=%22normal%22

ggplot_general_box_theme <- list(
    ggplot2::theme(
        axis.text = ggplot2::element_text(color = "#2D2D2D", size = 10),
        axis.ticks.length = ggplot2::unit(0.25, "cm"),
        strip.background = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        panel.grid = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
            colour = "#2D2D2D", fill = NA, size = 0.75)
    )
)

event_study_layout <- function(lags, leads, tp, binterval = 2, lc = "#003081") {
    options <- list(
        # vertical solid line on the first lag
        ggplot2::geom_vline(xintercept = tp - 1, color = lc, linetype = "solid"),
        # horizontal solid line at zero
        ggplot2::geom_hline(yintercept = 0, color = lc, linetype = "solid"),
        # scale giving first and last year
        ggplot2::scale_x_continuous(
            breaks = seq(tp - lags, tp + leads, binterval)
        ),
        # keep yscale fixed to ten breaks
        ggplot2::scale_y_continuous(n.breaks = 10),
        ggplot2::ylab("Point Estimate / C.I. (Percentage Points)"),
        ggplot_general_box_theme,
        ggplot2::theme(axis.title.x = element_blank())
    )

    return(options)
}

#' @example event_study_soep(data, lags = 9, leads = 7, tp = 2009, cpoints = "#1984C5", cleads = "#003081")
event_study_soep <- function(data, lags, leads, tp, cpoints, cleads) {
    # re-scaling the coefficients to read directly as percentage points
    # data <- data %>%
    #     dplyr::mutate(across(matches("b|ll|ul|se"), ~ .x * 100))
    # aggregated estimates for the lags excluding t-1
    before <- data %>%
        dplyr::filter(wave == 0) %>%
        dplyr::mutate(wave = tp - lags) %>%
        tidyr::complete(wave = seq(tp - lags, tp - 2)) %>%
        tidyr::fill(everything())
    # aggregated estimates for the leads from t0
    after <- data %>%
        dplyr::filter(wave == 1) %>%
        dplyr::mutate(wave = tp) %>%
        tidyr::complete(wave = seq(tp, tp + leads)) %>%
        tidyr::fill(everything())

    data %>%
        dplyr::filter(wave > 1) %>%
        tidyr::complete(wave = seq(tp - lags, tp + leads)) %>%
        replace(is.na(.), 0) %>%
        ggplot2::ggplot() +
        ggplot2::geom_line(
            data = before, aes(x = wave, y = b),
            color = cpoints, size = 1) +
        ggplot2::geom_ribbon(
            data = before, aes(x = wave, ymin = ll, ymax = ul),
            color = cpoints, fill = NA, alpha = .1, size = .4) +
        ggplot2::geom_linerange(
            data = dplyr::filter(before, wave %in% c(tp - lags, tp - 2)),
            aes(ymax = ul, ymin = ll, y = b, x = wave),
            color = cpoints, size = 0.4) +
        ggplot2::geom_line(
            data = after, aes(x = wave, y = b),
            color = cpoints, size = 1) +
        ggplot2::geom_ribbon(
            data = after, aes(x = wave, ymin = ll, ymax = ul),
            color = cpoints, fill = cleads, alpha = .1, size = .4) +
        ggplot2::geom_linerange(
            data = dplyr::filter(after, wave %in% c(tp, tp + leads)),
            aes(ymax = ul, ymin = ll, y = b, x = wave),
            color = cpoints, size = 0.4) +
        ggplot2::geom_point(
            aes(y = b, x = wave),
            color = cpoints, size = 3, shape = 1, stroke = 1) +
        ggplot2::geom_linerange(
            aes(ymax = ul, ymin = ll, y = b, x = wave),
            color = cpoints, size = 0.75
        ) + event_study_layout(lags, leads, tp, lc = cleads, binterval = 1)

}

func_form_linear_plot <- function(data, yt, xt, f = formula(y ~ x), cpoints = "#1984C5", cline = "#e14b31", carea = "#c23728") {
    data %>%
        ggplot2::ggplot(aes(x = exposure, y = diff)) +
        ggplot2::geom_smooth(method = "lm", formula = f, size = 0.4,
                             color = cline, fill = carea, alpha = 0.1) +
        ggplot2::geom_point(color = cpoints,
                            size = 1.2, shape = 1, stroke = 0.7) +
        ggplot2::scale_x_continuous(n.breaks = 10) +
        ggplot2::scale_y_continuous(n.breaks = 10) +
        ggplot2::xlab(xt) +
        ggplot2::ylab(yt) +
        ggplot_general_box_theme
}
