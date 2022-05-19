
#' This script allows to plot all the graphs for the entire project "Credit
#' Shocks and Populism". The data sources are either produced locally (e.g.
#' the measure of county-level Commmerzbank dependence, or the lending stock of
#' German banks, etc.) or they come from the SOEPRemote server after
#' copy-pasting from the output response coming via email as a result of the
#' Stata source code sent.

## -----------------------------------------------------------------------------
## Package Loading
##

# install pacman package manager if it is not already installed
if (!require("pacman")) install.packages("pacman")
# load packages with pacman
pacman::p_load(
    dplyr,     # tidyverse : general data manipulation
    tidyr,     # tidyverse : tidy messy data functions
    lubridate, # tidyverse : date manipulation
    collapse,  # C++ based functions and data manipulation
    ggplot2,   # the famous data visualization package big boy
    readr,     # tidyverse : read triangular data
    haven,     # tidyverse : read foreign statistical packages data
    fst        # fast storage serialization of data.frame(s)
)

## -----------------------------------------------------------------------------
## Data Visualization Functions
##

source(here::here("src", "functions", "datavizFunctions.R"))

## -----------------------------------------------------------------------------
## Figure of ln(lending stock) from Germany
##

shock_data <- haven::read_dta(
    here::here("data", "shock", "lending_for_replication.dta"))

ggplot2::ggplot() +
    # line when Lehman Brothers declares bankrupcy 15-09-2008
    ggplot2::geom_vline(xintercept = 2007.71,
                        color = "#003081", linetype = "dashed") +
    # line when SoFFin Fund comes for help to Commerzbank from Bundestag
    # ggplot2::geom_vline(xintercept = dmy("17-10-2008")) +
    # line end of last quarter 2010 which is the last year of lending cut
    # ggplot2::geom_vline(xintercept = dmy("31-12-2010")) +
    ggplot2::geom_vline(xintercept = 2010,
                        color = "#003081", linetype = "dashed") +
    # line of mid-2011 big repayment to SoFFin for 80%
    # ggplot2::geom_vline(xintercept = dmy("01-06-2011")) +
    annotate("rect", xmin = 2007.71, xmax = 2010,
             ymin = -Inf, ymax = Inf,
             fill = "#1984C5", alpha = .1, size = .4) +
    ggplot2::geom_line(
        data = dplyr::filter(shock_data,
                             bank %in% c("allothers", "commerzbank")),
        aes(x = year, y = lrl_diff_2004, color = bank), linetype = "solid") +
    ggplot2::geom_point(
        data = dplyr::filter(shock_data,
                             bank %in% c("allothers", "commerzbank")),
        aes(x = year, y = lrl_diff_2004, color = bank),
        size = 2, shape = 4, stroke = 0.75) +
    ggplot2::geom_line(
        data = dplyr::filter(shock_data, bank == "privothers"),
        aes(x = year, y = lrl_diff_2004, color = bank),
        linetype = "dashed", alpha = 0.7) +
    ggplot2::geom_point(
        data = dplyr::filter(shock_data, bank == "privothers"),
        aes(x = year, y = lrl_diff_2004, color = bank),
        size = 2, shape = 4, stroke = 0.75, alpha = 0.7) +
    ggplot2::scale_color_manual(
        name = "",
        values = c(
            allothers   = "#499f68",
            commerzbank = "#1984c5",
            privothers  = "#c23728"),
        labels = c(
            allothers   = "All Other Banks",
            commerzbank = "Commerzbank",
            privothers  = "All Other Commercial Banks")
        ) +
    ggplot2::scale_x_continuous(breaks = seq(2004, 2014)) +
    ggplot2::scale_y_continuous(n.breaks = 10) +
    ggplot2::ylab("ln Lending Stock (relative to 2004)") +
    ggplot2::guides(color = guide_legend(nrow = 1, byrow = TRUE)) +
    ggplot2::theme(
        legend.position = "bottom",
        legend.key = element_blank(),
        axis.title.x = element_blank()) +
    ggplot_general_box_theme
ggplot2::ggsave(here::here("out", "graphs", "shock", "huber_lending_cut.pdf"),
                dpi = 600, width = 10, height = 7)

## -----------------------------------------------------------------------------
## Event Study Plots with and without controlling for individual FE
## on the exposure measured using the initial sample of firms (equal weighting).
## We control for time-invariant and time-varying individual and household
## characteristics,county and time fixed effects.

outcome_selection <- c("ps", "pp")
fixed_effects_selection <- c("noife", "ife")
splits_selection <- c("std", "p50", "p75", "p90", "iqr", "1090")

for (out in outcome_selection) {
    for (s in splits_selection) {
        for (i in fixed_effects_selection) {
            # create filename from the wildcards
            # we use the same name of the .csv file for the .pdf plot
            filename <- paste(
                "dynamic_did_cbk_past_mean", out, s, i, sep = "_")
            # read the data copy-pasted from the SOEPRemote output
            readr::read_delim(
                here::here("out", "data", "soep", paste0(filename, ".csv"))) %>%
                # we re-scale the coefficients to have direct number in p.p.
                dplyr::mutate(across(matches("b|ll|ul|se"), ~ .x * 100)) %>%
                # apply our custom function for event study plots from 2000
                # to 2017 with nine lags and eight leads, baseline in t-1
                event_study_soep(
                    lags = 9, leads = 8, tp = 2009,
                    cpoints = "#1984C5", cleads = "#003081")
            ggplot2::ggsave(
                here::here(
                    "out", "graphs", "events", paste0(filename, ".pdf")),
                dpi = 600, width = 10, height = 7)
        }
    }
}

## -----------------------------------------------------------------------------
## Functional Form Plots: we residualise the outcome variable for the fixed
## effects (with or without individual FE), the time-varying observed
## characteristics of individuals and households, and the time-invariant
## characteristics in case we do not include individual FE, we take the mean
## outcome for all periods before and all periods after the shock for each
## level of exposure in its distribution, and we take the difference in mean.

fixed_effects_selection <- c("noife", "ife")

# political support preferences
for (i in fixed_effects_selection) {
    filename <- paste0("ps_functional_form_cbk_past_mean_", i)
    func_form <- readr::read_delim(
        here::here("out", "data", "soep", paste0(filename, ".csv")), na = ".")

    # level-level plot without the outliers
    func_form %>%
        func_form_linear_plot(
            xt = expression("Exposure"[k]),
            yt = expression(
                Delta*" Political Support Preferences Before-After (residualized)"))

    ggplot2::ggsave(
        here::here(
            "out", "graphs", "heterogeneity", paste0(filename, "_levels.pdf")),
        dpi = 600, width = 10, height = 7)

    # log-log plot without outliers
    func_form %>%
        dplyr::mutate(diff = log(diff), exposure = log(exposure)) %>%
        dplyr::filter(diff > -8) %>%
        func_form_linear_plot(
            xt = expression(ln(Exposure[k])),
            yt = expression(
                ln(Delta*" Political Support Preferences "[bar(y)[after] - bar(y)[before]])*" (residualized)")
        )
    ggplot2::ggsave(
        here::here(
            "out", "graphs", "heterogeneity", paste0(filename, "_logs.pdf")),
        dpi = 600, width = 10, height = 7)
}

# populist party preferences
for (i in fixed_effects_selection) {
    filename <- paste0("pp_functional_form_cbk_past_mean_", i)
    func_form <- readr::read_delim(
        here::here("out", "data", "soep", paste0(filename, ".csv")), na = ".")

    # filter the three maximum values
    outliers <- func_form %>%
        dplyr::arrange(desc(diff)) %>%
        head(3) %>% .$diff

    # level-level plot without the outliers
    func_form %>%
        dplyr::filter(!(diff %in% outliers)) %>%
        func_form_linear_plot(
            xt = expression("Exposure"[k]),
            yt = expression(
                Delta*" Populism Preferences Before-After (residualized)"),
            f = formula(y ~ exp(x)))

    ggplot2::ggsave(
        here::here(
            "out", "graphs", "heterogeneity", paste0(filename, "_levels.pdf")),
        dpi = 600, width = 10, height = 7)

    # log-log plot without outliers
    func_form %>%
        dplyr::filter(!(diff %in% outliers)) %>%
        dplyr::mutate(diff = log(diff), exposure = log(exposure)) %>%
        dplyr::filter(diff > -8) %>%
        func_form_linear_plot(
            xt = expression(ln(Exposure[k])),
            yt = expression(
                ln(Delta*" Populist Preferences "[bar(y)[after] - bar(y)[before]])*" (residualized)")
        )
    ggplot2::ggsave(
        here::here(
            "out", "graphs", "heterogeneity", paste0(filename, "_logs.pdf")),
        dpi = 600, width = 10, height = 7)
}


## -----------------------------------------------------------------------------
# trajectories

trajectories <- readr::read_delim(here::here("out", "data", "soep", "pp_mean_outcome_cbk_past_mean_p50_sumcoef.csv"))

before <- trajectories %>%
    dplyr::filter(wave == 0) %>%
    dplyr::mutate(wave = 2000) %>%
    dplyr::group_by(treat) %>%
    tidyr::complete(wave = seq(2000, 2007)) %>%
    tidyr::fill(everything()) %>%
    dplyr::mutate(treat = as.factor(treat))
# aggregated estimates for the leads from t0
after <- trajectories %>%
    dplyr::filter(wave == 1) %>%
    dplyr::mutate(wave = 2009) %>%
    dplyr::group_by(treat) %>%
    tidyr::complete(wave = seq(2009, 2017)) %>%
    tidyr::fill(everything()) %>%
    dplyr::mutate(treat = as.factor(treat))


trajectories %>%
    dplyr::filter(wave != 0 & wave != 1) %>%
    dplyr::mutate(treat = as.factor(treat)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(aes(x = wave, y = b, group = treat, color = treat)) +
    ggplot2::geom_line(aes(x = wave, y = b, group = treat, color = treat)) +
    # ggplot2::geom_line(data = before, aes(x = wave, y = b, group = treat, color = treat)) +
    # ggplot2::geom_line(data = after, aes(x = wave, y = b, group = treat, color = treat)) +
    ggplot2::scale_color_manual(values = c("#1984C5", "#e14b31"), name = "", labels = c("Control", "Treatment")) +
    ggplot2::geom_vline(xintercept = 2008, color = "#003081", linetype = "solid") +
    # horizontal solid line at zero
    ggplot2::geom_hline(yintercept = 0, color = "#003081", linetype = "solid") +
    # scale giving first and last year
    ggplot2::scale_x_continuous(breaks = seq(2000, 2017, 2)) +
    # keep yscale fixed to ten breaks
    ggplot2::scale_y_continuous(n.breaks = 10) +
    ggplot2::ylab("Point Estimate") +
    ggplot_general_box_theme +
    ggplot2::theme(
        legend.position = "bottom",
        legend.key = element_blank(),
        axis.title.x = element_blank())
ggplot2::ggsave(
    here::here(
        "out", "graphs", "events", "pp_mean_outcome_cbk_past_mean_p50_sumcoef.pdf"),
    dpi = 600, width = 10, height = 7)

trajectories <- readr::read_delim(here::here("out", "data", "soep", "pp_mean_outcome_cbk_past_mean_p50_noife_allyears.csv"))
trajectories %>%
    dplyr::mutate(treat = as.factor(treat)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(aes(x = wave, y = b, group = treat, color = treat)) +
    ggplot2::geom_line(aes(x = wave, y = b, group = treat, color = treat)) +
    ggplot2::scale_color_manual(values = c("#1984C5", "#e14b31"), name = "", labels = c("Control", "Treatment")) +
    ggplot2::geom_vline(xintercept = 2008, color = "#003081", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, color = "#003081", linetype = "solid") +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2017, 2)) +
    ggplot2::scale_y_continuous(n.breaks = 10) +
    ggplot2::ylab("Intention to Vote for a Populist Party (residualized)") +
    ggplot_general_box_theme +
    ggplot2::theme(
        legend.position = "bottom",
        legend.key = element_blank(),
        axis.title.x = element_blank())
ggplot2::ggsave(
    here::here(
        "out", "graphs", "events", "pp_mean_outcome_cbk_past_mean_p50_noife_allyears.pdf"),
    dpi = 600, width = 10, height = 7)

trajectories <- readr::read_delim(here::here("out", "data", "soep", "pp_mean_outcome_cbk_past_mean_p50_noife.csv"))
trajectories %>%
    dplyr::group_by(treat) %>%
    tidyr::complete(wave = seq(2000, 2017)) %>%
    replace(is.na(.), 0) %>%
    dplyr::mutate(treat = as.factor(treat)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(aes(x = wave, y = b, group = treat, color = treat)) +
    ggplot2::geom_line(aes(x = wave, y = b, group = treat, color = treat)) +
    ggplot2::geom_ribbon(aes(x = wave, ymin = ll, ymax = ul, group = treat, fill = treat, color = treat), alpha = 0.1, show.legend = FALSE) +
    ggplot2::geom_vline(xintercept = 2008, color = "#003081", linetype = "dashed") +
    ggplot2::geom_hline(yintercept = 0, color = "#003081", linetype = "solid") +
    ggplot2::scale_color_manual(values = c("#1984C5", "#e14b31"), name = "", labels = c("Control", "Treatment")) +
    ggplot2::scale_fill_manual(values = c("#003081", "#c23728")) +
    ggplot2::scale_x_continuous(breaks = seq(2000, 2017, 2)) +
    ggplot2::scale_y_continuous(n.breaks = 10) +
    ggplot2::ylab("Intention to Vote for a Populist Party (residualized)") +
    ggplot_general_box_theme +
    ggplot2::theme(
        legend.position = "bottom",
        legend.key = element_blank(),
        axis.title.x = element_blank())

ggplot2::ggsave(
    here::here(
        "out", "graphs", "events", "pp_mean_outcome_cbk_past_mean_p50_noife.pdf"),
    dpi = 600, width = 10, height = 7)
