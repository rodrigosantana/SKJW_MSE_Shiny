########################################################################
## Description: Bayesian Finite Population Sampling - Shiny App -
## BackEnd
##
## Maintainer: DatenKraft
## Author: Rodrigo Sant'Ana & Paul G. Kinas
## Created: Seg dez 26 14:11:32 2022 (-0300)
## Version:
##
## URL:
## Doc URL:
##
## Database info:
##
### Commentary:
##
### Code:
########################################################################

########################################################################
######@> Carregando pacotes...
library(shiny)
library(ggplot2)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(grid)
library(markdown)
library(ggExtra)
library(dplyr)
library(tidyr)
library(DT)
library(viridis)
library(openMSE)
library(reshape2)
library(ggpmisc)
library(gridExtra)
library(cowplot)
library(tidyverse)
library(viridis)
library(janitor)
library(readxl)
library(patchwork)
library(paletteer)
library(ggrepel)
library(r4ss)
library(gt)
library(plotly)
library(waiter)

######@> ggplot theme...
extrafont::loadfonts(device = "postscript")
rgb01 <- "black"
rgb02 <- "black"
seta <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open")
seta2 <- grid::arrow(length = grid::unit(0.2, "cm"), type = "open",
                     ends = "both")
my_theme <- function(base_size = 20, base_family = "Helvetica") {
    theme_bw(base_size = base_size, base_family = base_family) %+replace%
        theme(axis.ticks = element_line(colour = rgb01),
              axis.line = element_line(colour = rgb01, linewidth = 0.2),
              axis.text = element_text(colour = rgb02, size = 20),
              axis.title = element_text(size = 20),
              strip.text = element_text(size = 20,
                                        margin = ggplot2::margin(0.3,
                                                                 0.3,
                                                                 0.3,
                                                                 0.3,
                                                                 "cm"),
                                        face = "bold"),
              legend.background = element_blank(),
              legend.key = element_blank(),
              legend.text = element_text(size = 18),
              legend.title = element_text(size = 20),
              panel.background = element_blank(),
              panel.grid = element_line(linetype = "solid",
                                        linewidth = 0.2,
                                        colour = "gray90"),
              plot.background = element_blank(),
              complete = TRUE)
}

######@> Padronizando o número de casas após a vírgula...
options(scipen = 10)

######@> Functions to prepare results...

#####@> Function to estimate PGK - Performance Metric...
CalcPGK <- function(DF, Years = 2026:2055) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::mutate(InGreen = SB_SBMSY > 1 & F_FMSY < 1) |>
        dplyr::summarise(PGK = mean(InGreen))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate LRP - Performance Metric...
CalcLRP <- function(DF, Years = 2026:2055, Ref = 0.4) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::mutate(Stat = SB_SBMSY < Ref) |>
        dplyr::summarise(LRP = mean(Stat))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate NLRP - Performance Metric...
CalcNLRP <- function(DF, Years = 2026:2055, Ref = 0.4) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::mutate(Stat = SB_SBMSY > Ref) |>
        dplyr::summarise(NLRP = mean(Stat))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate POF - Performance Metric...
CalcPOF <- function(DF, Years = 2026:2055, Ref = 1) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::mutate(Stat = F_FMSY > Ref) |>
        dplyr::summarise(POF = mean(Stat))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate PNOF - Performance Metric...
CalcPNOF <- function(DF, Years = 2026:2055, Ref = 1) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::mutate(Stat = F_FMSY < Ref) |>
        dplyr::summarise(PNOF = mean(Stat))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate AvC - Performance Metric...
CalcAvC <- function(DF, Stat = Removals, Years = 2026:2055) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::summarise(AvC = median({{ Stat }}))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to estimate VarC - Performance Metric...
CalcVarC <- function(DF, Years = 2026:2055) {
    if (length(Years) < 6) {
        stop("For meaningful triad comparisons, you must supply at least 6 years.")
    }
    yrs <- paste(range(Years), collapse = "-")
    df_filtered <- DF |>
        dplyr::filter(Year %in% Years) |>
        dplyr::mutate(
                   period = floor((Year - min(Years)) / 3) + 1)
    tac_means <- df_filtered |>
        dplyr::group_by(MP, Sim, period) |>
        dplyr::summarise(mean_tac = mean(TAC, na.rm = TRUE),
                         .groups = "drop")
    tac_changes <- tac_means |>
        dplyr::group_by(MP, Sim) |>
        dplyr::arrange(period) |>
        dplyr::mutate(
                   tac_change = ((((mean_tac - dplyr::lag(mean_tac)) /
                       dplyr::lag(mean_tac))^2)^0.5)) |>
        dplyr::filter(!is.na(tac_change)) |>
        dplyr::ungroup()
    summary <- tac_changes |>
        dplyr::group_by(MP) |>
        dplyr::summarise(
                   VarC = mean(tac_change, na.rm = TRUE),
                   ## sd_tac_change = sd(tac_change, na.rm = TRUE),
                   .groups = "drop")
    names(summary)[2] <- paste(names(summary)[2], yrs)
    summary
}

#####@> Function to estimate the TAC1...
CalcTAC1 <- function(DF, Stat = TAC, Years = 2026:2028) {
    yrs <- paste(range(Years), collapse = "-")
    fDF <- DF |> dplyr::filter(Year %in% Years) |>
        dplyr::group_by(MP) |>
        dplyr::summarise(TAC1 = median({{ Stat }}))
    names(fDF)[2] <- paste(names(fDF)[2], yrs)
    fDF
}

#####@> Function to extract data from MSE objects...
GetInfo <- function(MPpath) {
    msefiles <- list.files(MPpath, full.names = TRUE)
    if (length(msefiles) < 1)
        stop("No files found for: ", basename(MPpath))
    OMList <- list()
    for (j in seq_along(msefiles)) {
        mse <- readRDS(msefiles[j])
        t <- strsplit(mse@Name, "_")[[1]][3:4]
        ProjectYears <- seq(mse@OM$CurrentYr[1] + 1,
                            by = 1,
                            length.out = mse@proyears)
        OMList[[j]] <- data.frame(Sim = 1:mse@nsim,
                                  MP = basename(MPpath),
                                  Growth = t[1],
                                  Steepness = t[2],
                                  Year = rep(ProjectYears,
                                             each = mse@nsim),
                                  SB_SBMSY = as.vector(mse@SB_SBMSY),
                                  F_FMSY = as.vector(mse@F_FMSY),
                                  Removals = as.vector(mse@Removals),
                                  TAC = as.vector(mse@TAC),
                                  Landings = as.vector(mse@Catch))
    }
    do.call("rbind", OMList)
}

server <- function(input, output, session) {

######@> Loading state...
    w <- Waiter$new(id = "distPlot", html = spin_fading_circles(),
                    color = "#FFFFFF")

########################################################################
######@> Loading databases...

######@> LRP 0.1 Tunning results...
    load("data/DF.RData")

######@> Loading WSKJ Pre-prepared Data...
    WSKJ_Data <- readRDS("data/WSKJ_Data.rda")

########################################################################
######@> TAB 1 - INTRODUCTION...

########################################################################
######@> TAB 2 - OPERATING MODELS...


########################################################################
######@> TAB 3 - TRAJECTORIS...

######@>----------------------------------------------------------------
######@> Time Series Plot (Trajectories)...

######@> Definitions...
    plotVars <- c("F_FMSY", "SB_SBMSY", "TAC", "Removals", "Landings")
    ribbonCols <- c("darkgray", "black")

######@> Management periods...
    periods <- bind_rows(
        data.frame(Year = 2025:2029,
                   Period = factor(c(rep(1, 5))),
                   y = 1),
        data.frame(Year = 2029:2035,
                   Period = factor(c(rep(2, 7))),
                   y = 1),
        data.frame(Year = 2035:2055,
                   Period = factor(c(rep(3, 21))),
                   y = 1))

######@> All OMs Combined...
    DFplot <- DF |> tidyr::pivot_longer(dplyr::all_of(plotVars)) |>
        dplyr::group_by(Year, MP, name) |>
        dplyr::summarise(Mean = median(value),
                         Lower1 = quantile(value, 0.025),
                         Upper1 = quantile(value, 0.975),
                         Lower2 = quantile(value, 0.1),
                         Upper2 = quantile(value, 0.9))
    DFplot$name <- factor(DFplot$name, levels = plotVars, ordered = TRUE)
    DFplot$MP <- gsub("_2_31", "", DFplot$MP)

#####@> SB_SBMSY...

#####@> Estimating the metrics...
    metrics <- CalcLRP(DF) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "LRP 2026-2055") %>%
        rename("Value" = `LRP 2026-2055`) %>%
        mutate(Value = sprintf("%.3f", Value),
               x = 2025, y = 6) %>%
        select(MP, PM, Value)

####@> Table to be included inside the plot...
    tab <- metrics |>
        group_by(MP) |>
        summarise(x = 2025,
                  y = 6,
                  tb = list(select(cur_data_all(), PM, Value)),
                  .groups = "drop")

####@> Figure...
    output$trajSSB <- renderPlot({
        w$show()
        on.exit(w$hide())
        p00 <- ggplot(filter(DFplot, name %in% c("SB_SBMSY")),
                      aes(x = Year)) +
            ## expand_limits(y = 0) +
            geom_ribbon(aes(ymin = Lower1,
                            ymax = Upper1),
                        alpha = 0.5, fill = ribbonCols[1]) +
            geom_ribbon(aes(ymin = Lower2,
                            ymax = Upper2),
                        alpha = 0.5, fill = ribbonCols[2]) +
            geom_line(aes(y = Mean)) +
            geom_ribbon(data = periods,
                        aes(x = Year, ymin = 0, ymax = y - 0.6,
                            fill = Period, colour = Period),
                        alpha = 0.3) +
            geom_line(data = periods,
                      aes(x = Year, y = y), linetype = "dashed",
                      colour = "coral1") +
            ## geom_hline(yintercept = 0.4, linetype = "dashed",
            ##            colour = "red") +
            geom_table(data = tab, aes(x = x, y = y, label = tb),
                       inherit.aes = FALSE, size = 6) +
            facet_wrap(~ MP, scales = "free", ncol = 4) +
            scale_y_continuous(expand = c(0, 0), breaks = seq(0, 8, 1),
                               limits = c(0, 6)) +
            scale_x_continuous(expand = c(0, 0),
                               limits = c(2025, 2055),
                               breaks = seq(2025, 2055, 5)) +
            labs(x = "Year", y = expression(SSB/SSB[MSY])) +
            my_theme() +
            theme(legend.position = "none",
                  panel.spacing = unit(1.8, "lines"),
                  plot.margin = margin(5, 25, 5, 5, "pt"))
        p00
    })

######@> F_FMSY...

#####@> Estimating the metrics...
    tmp00 <- CalcPGK(DF, Years = 2026:2028) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "PGK 2026-2028") %>%
        rename("Value" = `PGK 2026-2028`) %>%
        mutate(Value = sprintf("%.3f", Value)) %>%
        select(MP, PM, Value)
    tmp01 <- CalcPGK(DF, Years = 2029:2034) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "PGK 2029-2034") %>%
        rename("Value" = `PGK 2029-2034`) %>%
        mutate(Value = sprintf("%.3f", Value)) %>%
        select(MP, PM, Value)
    tmp02 <- CalcPGK(DF, Years = 2035:2055) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "PGK 2035-2055") %>%
        rename("Value" = `PGK 2035-2055`) %>%
        mutate(Value = sprintf("%.3f", Value)) %>%
        select(MP, PM, Value)
    tmp03 <- CalcPGK(DF) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "PGK 2026-2055") %>%
        rename("Value" = `PGK 2026-2055`) %>%
        mutate(Value = sprintf("%.3f", Value)) %>%
        select(MP, PM, Value)
    tmp04 <- CalcPNOF(DF) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "PNOF 2026-2055") %>%
        rename("Value" = `PNOF 2026-2055`) %>%
        mutate(Value = sprintf("%.3f", Value)) %>%
        select(MP, PM, Value)
    metrics <- bind_rows(tmp00, tmp01, tmp02, tmp03, tmp04)

####@> Table to be included inside the plot...
    tab2 <- metrics |>
        group_by(MP) |>
        summarise(x = 2025,
                  y = 5,
                  tb = list(select(cur_data_all(), PM, Value)),
                  .groups = "drop")

####@> Figure...
    output$trajF <- renderPlot({
        w$show()
        on.exit(w$hide())
        p01 <- ggplot(filter(DFplot, name %in% c("F_FMSY")),
                      aes(x = Year)) +
            ## expand_limits(y = 0) +
            geom_ribbon(aes(ymin = Lower1,
                            ymax = Upper1),
                        alpha = 0.5, fill = ribbonCols[1]) +
            geom_ribbon(aes(ymin = Lower2,
                            ymax = Upper2),
                        alpha = 0.5, fill = ribbonCols[2]) +
            geom_line(aes(y = Mean)) +
            geom_ribbon(data = periods,
                        aes(x = Year, ymin = 0, ymax = y - 0.8,
                            fill = Period, colour = Period),
                        alpha = 0.3) +
            geom_line(data = periods,
                      aes(x = Year, y = y), linetype = "dashed",
                      colour = "coral1") +
            geom_table(data = tab2, aes(x = x, y = y, label = tb),
                       inherit.aes = FALSE, size = 6) +
            facet_wrap(~ MP, scales = "free", ncol = 4) +
            scale_y_continuous(expand = c(0, 0), breaks = seq(0, 8, 1),
                               limits = c(0, 7)) +
            scale_x_continuous(expand = c(0, 0),
                               limits = c(2025, 2055),
                               breaks = seq(2025, 2055, 5)) +
            labs(x = "Year", y = expression(F/F[MSY])) +
            coord_cartesian(ylim = c(0, 5)) +
            my_theme() +
            theme(legend.position = "none",
                  panel.spacing = unit(1.8, "lines"),
                  plot.margin = margin(5, 25, 5, 5, "pt"))
        p01
    })

######@> TAC...

#####@> Estimating the metrics...
    tmp00A <- CalcAvC(DF, Years = 2026:2028) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "AvC 2026-2028") %>%
        rename("Value" = `AvC 2026-2028`) %>%
        mutate(Value = sprintf("%.0f", Value)) %>%
        select(MP, PM, Value)
    tmp01A <- CalcAvC(DF, Years = 2029:2034) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "AvC 2029-2034") %>%
        rename("Value" = `AvC 2029-2034`) %>%
        mutate(Value = sprintf("%.0f", Value)) %>%
        select(MP, PM, Value)
    tmp02A <- CalcAvC(DF, Years = 2035:2055) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "AvC 2035-2055") %>%
        rename("Value" = `AvC 2035-2055`) %>%
        mutate(Value = sprintf("%.0f", Value)) %>%
        select(MP, PM, Value)
    tmp03A <- CalcAvC(DF) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "AvC 2026-2055") %>%
        rename("Value" = `AvC 2026-2055`) %>%
        mutate(Value = sprintf("%.0f", Value)) %>%
        select(MP, PM, Value)
    tmp04A <- CalcVarC(DF) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "VarC 2026-2055") %>%
        rename("Value" = `VarC 2026-2055`) %>%
        mutate(Value = sprintf("%.3f", Value)) %>%
        select(MP, PM, Value)
    tmp05A <- CalcTAC1(DF) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(PM = "TAC1 2026-2028") %>%
        rename("Value" = `TAC1 2026-2028`) %>%
        mutate(Value = sprintf("%.0f", Value)) %>%
        select(MP, PM, Value)
    metricsA <- bind_rows(tmp00A, tmp01A, tmp02A, tmp03A, tmp04A, tmp05A)

####@> Table to be included inside the plot...
    tab3 <- metricsA |>
        group_by(MP) |>
        summarise(x = 2025,
                  y = 2505,
                  tb = list(select(cur_data_all(), PM, Value)),
                  .groups = "drop")

####@> References...
    refA <- data.frame(Type = c("10 years", "5 years"),
                       Mean = c(mean(WSKJ_Data@Cat[1, 64:73]),
                                mean(WSKJ_Data@Cat[1, 69:73])))

####@> Figure...
    output$trajTAC <- renderPlot({
        p03 <- ggplot(filter(DFplot, name %in% c("TAC")),
                      aes(x = Year)) +
            ## expand_limits(y = 0) +
            geom_ribbon(aes(ymin = Lower1,
                            ymax = Upper1),
                        alpha = 0.5, fill = ribbonCols[1]) +
            geom_ribbon(aes(ymin = Lower2,
                            ymax = Upper2),
                        alpha = 0.5, fill = ribbonCols[2]) +
            geom_line(aes(y = Mean)) +
            geom_ribbon(data = periods,
                        aes(x = Year, ymin = 0, ymax = y * 2500,
                            fill = Period, colour = Period),
                        alpha = 0.3) +
            geom_hline(yintercept = refA$Mean[2],
                       linetype = "dashed", colour = "coral1") +
            geom_table(data = tab3, aes(x = x, y = y, label = tb),
                       inherit.aes = FALSE, size = 6) +
            facet_wrap(~ MP, scales = "free", ncol = 4) +
            scale_y_continuous(expand = c(0, 0),
                               breaks = seq(0, 70000, 15000),
                               limits = c(0, 70000)) +
            scale_x_continuous(expand = c(0, 0),
                               limits = c(2025, 2055),
                               breaks = seq(2025, 2055, 5)) +
            coord_cartesian(ylim = c(0, 50000)) +
            labs(x = "Year", y = "Total Allowable Catches (in metric tons)") +
            my_theme() +
            theme(legend.position = "none",
                  panel.spacing = unit(1.8, "lines"),
                  plot.margin = margin(5, 25, 5, 5, "pt"))
        p03
    })

########################################################################
######@> TAB 4 - KOBE TIMESERIES...

######@>----------------------------------------------------------------
######@> Kobes Plots...

#####@> Grouping MP by classes...
    df00 <- DF %>%
        select(Sim:F_FMSY) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(Class = case_when(
                   grepl("IR", MP) ~ "Index-based: Index rate",
                   grepl("CE", MP) ~ "Index-based: Exploitation rate",
                   grepl("SP$", MP) ~
                       "Model-based: Surplus Production",
                   grepl("SPAH", MP) ~
                       "Model-based: Surplus Production HCR"))

######@> Estimating Kobe values per year...
    tmpB <- df00 %>%
        mutate(OF = ifelse(F_FMSY < 1, FALSE, TRUE),
               OFD = ifelse(SB_SBMSY < 1, TRUE, FALSE)) %>%
        filter(Year == 2055)

#####@> Proportions by year...
    valdf <- tmpB %>%
        group_by(Year, Class, MP) %>%
        summarise(BL = sum(OF == FALSE & OFD == TRUE)/900 * 100,
                  BR = sum(OF == FALSE & OFD == FALSE)/900 * 100,
                  TL = sum(OF == TRUE & OFD == TRUE)/900 * 100,
                  TR = sum(OF == TRUE & OFD == FALSE)/900 * 100)
    valdf <- valdf %>% tidyr::pivot_longer(., cols = 4:7)
    valdf$x <- -Inf
    valdf$y <- -Inf
    valdf$y[valdf$name == "TL"] <- Inf
    valdf$y[valdf$name == "TR"] <- Inf
    valdf$x[valdf$name == "BR"] <- Inf
    valdf$x[valdf$name == "TR"] <- Inf
    valdf$value <- round(valdf$value, 2)
    valdf$value <- paste0(valdf$value, "%")
    valdf$hjustvar <- -2
    valdf$vjustvar <- -2
    valdf$hjustvar[valdf$name == "TL"] <- -1
    valdf$hjustvar[valdf$name == "TR"] <- 2
    valdf$hjustvar[valdf$name == "BL"] <- -1
    valdf$hjustvar[valdf$name == "BR"] <- 2
    valdf$vjustvar[valdf$name == "TL"] <- 2
    valdf$vjustvar[valdf$name == "TR"] <- 2
    valdf$vjustvar[valdf$name == "BL"] <- -2
    valdf$vjustvar[valdf$name == "BR"] <- -2

#####@> Colors to figure...
    kobe_df <- bind_rows(data.frame(x = c(0, 0, 1, 1),
                                    y = c(0, 1, 1, 0),
                                    fill = "bl"),
                         data.frame(x = c(1, 1, 3, 3),
                                    y = c(0, 1, 1, 0),
                                    fill = "br"),
                         data.frame(x = c(0, 0, 1, 1),
                                    y = c(1, 3, 3, 1),
                                    fill = "tl"),
                         data.frame(x = c(1, 1, 3, 3),
                                    y = c(1, 3, 3, 1),
                                    fill = "tr"))
    kobe_df$alpha <- 0.2

######@> Estimating Kobe values per year...
    tmpD <- df00 %>%
        mutate(OF = ifelse(F_FMSY < 1, FALSE, TRUE),
               OFD = ifelse(SB_SBMSY < 1, TRUE, FALSE))

#####@> Proportions by year...
    valdfB <- tmpD %>%
        group_by(Year, Class, MP) %>%
        summarise(Yellow = sum(OF == FALSE & OFD == TRUE)/900 * 100,
                  Green = sum(OF == FALSE & OFD == FALSE)/900 * 100,
                  Red = sum(OF == TRUE & OFD == TRUE)/900 * 100,
                  Orange = sum(OF == TRUE & OFD == FALSE)/900 * 100) %>%
        pivot_longer(names_to = "Cond", values_to = "Perc", 4:7) %>%
        mutate(Cond = factor(Cond, levels = c("Green", "Yellow", "Orange",
                                              "Red")))

    output$kobeCEtime <- renderPlot({
        p12 <- ggplot() +
            geom_area(data = filter(valdfB, Year %in% 2025:2055,
                                    Class == "Index-based: Exploitation rate"),
                      aes(x = Year, y = Perc, fill = rev(Cond)),
                      stat = "identity", colour = "black") +
            geom_hline(yintercept = 60, lty = 1, colour = "white", alpha = 0.5,
                       linewidth = 1) +
            geom_hline(yintercept = 60, lty = 2, alpha = 0.5) +
            ## facet_grid(Class~MP) +
            scale_fill_manual(values = rev(c("#67C18B", "#F8DC7A", "#FDBD56",
                                             "#D8775D"))) +
            ggnewscale::new_scale_fill() +
            geom_ribbon(data = periods,
                        aes(x = Year, ymin = 0, ymax = y * 3, fill = Period),
                        alpha = 0.8, show.legend = FALSE) +
            geom_vline(xintercept = 2029, lty = 2, alpha = 0.5) +
            geom_vline(xintercept = 2035, lty = 2, alpha = 0.5) +
            labs(x = "Year", y = "%") +
            expand_limits(y = c(0, 100)) +
            scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
            scale_x_continuous(expand = c(0, 0), breaks = seq(2025, 2055, 5),
                               limits = c(2025, 2055)) +
            annotate("rect", xmin = 2025, xmax = 2028, ymin = 0, ymax = 100,
                     fill = "white", alpha = 0.2) +
            annotate("text", x = 2026.5, y = 50, label = "Kobe Matrix Period",
                     alpha = 0.6, size = 4, angle = 90) +
            my_theme() +
            theme(legend.position = "none")
        p12
    })

    output$kobeCE <- renderPlot({
        p08 <- ggplot() +
            geom_polygon(data = kobe_df,
                         aes(x = x, y = y, fill = fill, alpha = alpha)) +
            scale_fill_manual(values = c("#F8DC7A", "#67C18B", "#D8775D",
                                         "#FDBD56")) +
            geom_point(data = filter(tmpB,
                                     Class == "Index-based: Exploitation rate"),
                       aes(x = SB_SBMSY, y = F_FMSY), alpha = 0.2,
                       size = 4) +
            geom_label(data = filter(valdf,
                                     Class == "Index-based: Exploitation rate"),
                       fontface = "bold", size = 4,
                       aes(x = x, y = y, label = value,
                           hjust = hjustvar, vjust = vjustvar),
                       colour = "black") +
            expand_limits(x = c(0, 3), y = c(0, 3)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +
            scale_x_continuous(expand = c(0, 0), limits = c(0, 3)) +
            geom_hline(yintercept = 1, color = "darkgray", linetype = 2) +
            geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
            labs(x = expression(SSB/SSB[MSY]), y = expression(F/F[MSY])) +
            ## facet_grid(Class ~ MP) +
            my_theme() +
            theme(legend.position = "none")
        p08
    })

    output$kobeIRtime <- renderPlot({
        p13 <- ggplot() +
            geom_area(data = filter(valdfB, Year %in% 2025:2055,
                                    Class == "Index-based: Index rate"),
                      aes(x = Year, y = Perc, fill = rev(Cond)),
                      stat = "identity", colour = "black") +
            geom_hline(yintercept = 60, lty = 1, colour = "white", alpha = 0.5,
                       linewidth = 1) +
            geom_hline(yintercept = 60, lty = 2, alpha = 0.5) +
            ## facet_grid(Class~MP) +
            scale_fill_manual(values = rev(c("#67C18B", "#F8DC7A", "#FDBD56",
                                             "#D8775D"))) +
            ggnewscale::new_scale_fill() +
            geom_ribbon(data = periods,
                        aes(x = Year, ymin = 0, ymax = y * 3, fill = Period),
                        alpha = 0.8, show.legend = FALSE) +
            geom_vline(xintercept = 2029, lty = 2, alpha = 0.5) +
            geom_vline(xintercept = 2035, lty = 2, alpha = 0.5) +
            labs(x = "Year", y = "%") +
            expand_limits(y = c(0, 100)) +
            scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
            scale_x_continuous(expand = c(0, 0), breaks = seq(2025, 2055, 5),
                               limits = c(2025, 2055)) +
            annotate("rect", xmin = 2025, xmax = 2028, ymin = 0, ymax = 100,
                     fill = "white", alpha = 0.2) +
            annotate("text", x = 2026.5, y = 50, label = "Kobe Matrix Period",
                     alpha = 0.6, size = 4, angle = 90) +
            my_theme() +
            theme(legend.position = "none")
        p13
    })

    output$kobeIR <- renderPlot({
        p09 <- ggplot() +
            geom_polygon(data = kobe_df,
                         aes(x = x, y = y, fill = fill, alpha = alpha)) +
            scale_fill_manual(values = c("#F8DC7A", "#67C18B", "#D8775D",
                                         "#FDBD56")) +
            geom_point(data = filter(tmpB,
                                     Class == "Index-based: Index rate"),
                       aes(x = SB_SBMSY, y = F_FMSY), alpha = 0.2,
                       size = 4) +
            geom_label(data = filter(valdf,
                                     Class == "Index-based: Index rate"),
                       fontface = "bold", size = 4,
                       aes(x = x, y = y, label = value,
                           hjust = hjustvar, vjust = vjustvar),
                       colour = "black") +
            expand_limits(x = c(0, 3), y = c(0, 3)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +
            scale_x_continuous(expand = c(0, 0), limits = c(0, 3)) +
            geom_hline(yintercept = 1, color = "darkgray", linetype = 2) +
            geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
            labs(x = expression(SSB/SSB[MSY]), y = expression(F/F[MSY])) +
            ## facet_grid(Class ~ MP) +
            my_theme() +
            theme(legend.position = "none")
        p09
    })

    output$kobeSPtime <- renderPlot({
        p14 <- ggplot() +
            geom_area(data = filter(valdfB, Year %in% 2025:2055,
                                    Class == "Model-based: Surplus Production"),
                      aes(x = Year, y = Perc, fill = rev(Cond)),
                      stat = "identity", colour = "black") +
            geom_hline(yintercept = 60, lty = 1, colour = "white", alpha = 0.5,
                       linewidth = 1) +
            geom_hline(yintercept = 60, lty = 2, alpha = 0.5) +
            ## facet_grid(Class~MP) +
            scale_fill_manual(values = rev(c("#67C18B", "#F8DC7A", "#FDBD56",
                                             "#D8775D"))) +
            ggnewscale::new_scale_fill() +
            geom_ribbon(data = periods,
                        aes(x = Year, ymin = 0, ymax = y * 3, fill = Period),
                        alpha = 0.8, show.legend = FALSE) +
            geom_vline(xintercept = 2029, lty = 2, alpha = 0.5) +
            geom_vline(xintercept = 2035, lty = 2, alpha = 0.5) +
            labs(x = "Year", y = "%") +
            expand_limits(y = c(0, 100)) +
            scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
            scale_x_continuous(expand = c(0, 0), breaks = seq(2025, 2055, 5),
                               limits = c(2025, 2055)) +
            annotate("rect", xmin = 2025, xmax = 2028, ymin = 0, ymax = 100,
                     fill = "white", alpha = 0.2) +
            annotate("text", x = 2026.5, y = 50, label = "Kobe Matrix Period",
                     alpha = 0.6, size = 4, angle = 90) +
            my_theme() +
            theme(legend.position = "none")
        p14 
    })

    output$kobeSP <- renderPlot({
        p10 <- ggplot() +
            geom_polygon(data = kobe_df,
                         aes(x = x, y = y, fill = fill, alpha = alpha)) +
            scale_fill_manual(values = c("#F8DC7A", "#67C18B", "#D8775D",
                                         "#FDBD56")) +
            geom_point(data = filter(tmpB,
                                     Class == "Model-based: Surplus Production"),
                       aes(x = SB_SBMSY, y = F_FMSY), alpha = 0.2,
                       size = 4) +
            geom_label(data = filter(valdf,
                                     Class == "Model-based: Surplus Production"),
                       fontface = "bold", size = 4,
                       aes(x = x, y = y, label = value,
                           hjust = hjustvar, vjust = vjustvar),
                       colour = "black") +
            expand_limits(x = c(0, 3), y = c(0, 3)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +
            scale_x_continuous(expand = c(0, 0), limits = c(0, 3)) +
            geom_hline(yintercept = 1, color = "darkgray", linetype = 2) +
            geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
            labs(x = expression(SSB/SSB[MSY]), y = expression(F/F[MSY])) +
            ## facet_grid(Class ~ MP) +
            my_theme() +
            theme(legend.position = "none")
        p10
    })

    output$kobeSPAHtime <- renderPlot({
        p15 <- ggplot() +
            geom_area(data = filter(valdfB, Year %in% 2025:2055,
                                    Class == "Model-based: Surplus Production HCR"),
                      aes(x = Year, y = Perc, fill = rev(Cond)),
                      stat = "identity", colour = "black") +
            geom_hline(yintercept = 60, lty = 1, colour = "white", alpha = 0.5,
                       linewidth = 1) +
            geom_hline(yintercept = 60, lty = 2, alpha = 0.5) +
            ## facet_grid(Class~MP) +
            scale_fill_manual(values = rev(c("#67C18B", "#F8DC7A", "#FDBD56",
                                             "#D8775D"))) +
            ggnewscale::new_scale_fill() +
            geom_ribbon(data = periods,
                        aes(x = Year, ymin = 0, ymax = y * 3, fill = Period),
                        alpha = 0.8, show.legend = FALSE) +
            geom_vline(xintercept = 2029, lty = 2, alpha = 0.5) +
            geom_vline(xintercept = 2035, lty = 2, alpha = 0.5) +
            labs(x = "Year", y = "%") +
            expand_limits(y = c(0, 100)) +
            scale_y_continuous(expand = c(0, 0), breaks = seq(0, 100, 10)) +
            scale_x_continuous(expand = c(0, 0), breaks = seq(2025, 2055, 5),
                               limits = c(2025, 2055)) +
            annotate("rect", xmin = 2025, xmax = 2028, ymin = 0, ymax = 100,
                     fill = "white", alpha = 0.2) +
            annotate("text", x = 2026.5, y = 50, label = "Kobe Matrix Period",
                     alpha = 0.6, size = 4, angle = 90) +
            my_theme() +
            theme(legend.position = "none")
        p15
    })

    output$kobeSPAH <- renderPlot({
        p11 <- ggplot() +
            geom_polygon(data = kobe_df,
                         aes(x = x, y = y, fill = fill, alpha = alpha)) +
            scale_fill_manual(values = c("#F8DC7A", "#67C18B", "#D8775D",
                                         "#FDBD56")) +
            geom_point(data = filter(tmpB,
                                     Class == "Model-based: Surplus Production HCR"),
                       aes(x = SB_SBMSY, y = F_FMSY), alpha = 0.2,
                       size = 4) +
            geom_label(data = filter(valdf,
                                     Class == "Model-based: Surplus Production HCR"),
                       fontface = "bold", size = 4,
                       aes(x = x, y = y, label = value,
                           hjust = hjustvar, vjust = vjustvar),
                       colour = "black") +
            expand_limits(x = c(0, 3), y = c(0, 3)) +
            scale_y_continuous(expand = c(0, 0), limits = c(0, 3)) +
            scale_x_continuous(expand = c(0, 0), limits = c(0, 3)) +
            geom_hline(yintercept = 1, color = "darkgray", linetype = 2) +
            geom_vline(xintercept = 1, color = "darkgray", linetype = 2) +
            labs(x = expression(SSB/SSB[MSY]), y = expression(F/F[MSY])) +
            ## facet_grid(Class ~ MP) +
            my_theme() +
            theme(legend.position = "none")
        p11
    })

########################################################################
######@> TAB 5 - QUILTY PLOT...

######@>----------------------------------------------------------------
######@> Performance metrics...

######@> Consolidating general indicators...
    PropTab <- bind_cols(
        CalcPGK(DF),
        CalcPGK(DF, Years = 2026:2028)[, 2],
        CalcPGK(DF, Years = 2029:2034)[, 2],
        CalcPGK(DF, Years = 2035:2055)[, 2],
        CalcLRP(DF)[, 2],
        CalcLRP(DF, Years = 2026:2028)[, 2],
        CalcLRP(DF, Years = 2029:2034)[, 2],
        CalcLRP(DF, Years = 2035:2055)[, 2],
        CalcPOF(DF)[, 2],
        CalcPNOF(DF)[, 2],
        CalcAvC(DF)[, 2],
        CalcAvC(DF, Years = 2026:2028)[, 2],
        CalcAvC(DF, Years = 2029:2034)[, 2],
        CalcAvC(DF, Years = 2035:2055)[, 2],
        CalcVarC(DF)[, 2],
        CalcVarC(DF, Years = 2029:2034)[, 2],
        CalcVarC(DF, Years = 2035:2055)[, 2]) %>%
        mutate(MP = gsub("_2_31", "", MP)) %>%
        mutate(MP = factor(MP, levels = c("CE", "IR", "SP", "SPAH"))) %>%
        arrange(MP)

#####@> Changing names...
    names(PropTab) <- gsub(" 2026-2055", "", names(PropTab))
    names(PropTab) <- gsub(" 2026-2028", "_short", names(PropTab))
    names(PropTab) <- gsub(" 2029-2034", "_mid", names(PropTab))
    names(PropTab) <- gsub(" 2035-2055", "_long", names(PropTab))

    PropTab <- PropTab %>%
        mutate(across(.cols = c("PGK", "PGK_short", "PGK_mid", "PGK_long",
                                "LRP", "LRP_short", "LRP_mid", "LRP_long",
                                "POF", "PNOF", "AvC", "AvC_short", "AvC_mid",
                                "AvC_long", "VarC", "VarC_mid", "VarC_long"),
                      .fns = \(x) round(x, 2)))


####@> New version...
    output$quilt <- render_gt({
        quiltplot <- PropTab %>%
            gt() %>%
            fmt_number(
                decimals = 2,
                columns = c("PGK", "PGK_short", "PGK_mid", "PGK_long",
                            "LRP", "LRP_short", "LRP_mid", "LRP_long",
                            "POF", "PNOF", "AvC", "AvC_short", "AvC_mid",
                            "AvC_long", "VarC", "VarC_mid", "VarC_long")) %>%
            data_color(
                columns = c("PGK", "PGK_short", "PGK_mid", "PGK_long",
                            "PNOF", "AvC", "AvC_short", "AvC_mid",
                            "AvC_long"),
                colors = c("#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
                alpha = 0.7,
                autocolor_text = FALSE) %>%
            data_color(
                columns = c("LRP", "LRP_short", "LRP_mid", "LRP_long", "POF",
                            "VarC", "VarC_mid", "VarC_long"),
                colors = rev(c("#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab")),
                alpha = 0.7,
                autocolor_text = FALSE) %>%
            tab_style(
                style = list(cell_text(weight = "bold", align = "center")),
                locations = cells_column_labels(everything())) %>%
            tab_style(
                style = cell_text(weight = "bold"),
                locations = cells_body(columns = c(MP))) %>%
            tab_style(
                style = list(
                    cell_borders(sides = c("top", "bottom"), color = "black",
                                 weight = px(3))),
                locations = list(cells_column_labels(columns = gt::everything()))) %>%
            tab_style(
                style = list(
                    cell_borders(sides = c("bottom"), color = "black",
                                 weight = px(3))),
                locations = list(cells_body(rows = 4))) %>%
            tab_options(table.width = pct(100),
                        table.font.size = px(18),
                        heading.title.font.size = px(18),
                        heading.subtitle.font.size = px(14),
                        column_labels.font.size = px(18),
                        data_row.padding = px(4))
        quiltplot
    })

########################################################################
######@> TAB 6 - VIOLIN PLOT...

######@> Violin plot for VarC...
    Years <- 2026:2055
    yrs <- paste(range(Years), collapse = "-")
    df_filtered <- DF |>
        dplyr::filter(Year %in% Years) |>
        dplyr::mutate(
                   period = floor((Year - min(Years)) / 3) + 1)
    tac_means <- df_filtered |>
        dplyr::group_by(MP, Sim, period) |>
        dplyr::summarise(mean_tac = mean(TAC, na.rm = TRUE),
                         .groups = "drop")
    tac_changes <- tac_means |>
        dplyr::group_by(MP, Sim) |>
        dplyr::arrange(period) |>
        dplyr::mutate(
                   tac_change = ((((mean_tac - dplyr::lag(mean_tac)) /
                                   dplyr::lag(mean_tac))^2)^0.5)) |>
        dplyr::filter(!is.na(tac_change)) |>
        dplyr::mutate(MP =  gsub("_2_31", "", MP)) |>
        dplyr::ungroup()

    tabVarC <- tac_changes %>%
        group_by(MP) %>%
        summarise(Min = min(tac_change),
                  Qnt25 = quantile(tac_change)[2],
                  Mean = mean(tac_change),
                  Median = median(tac_change),
                  Qnt75 = quantile(tac_change)[4],
                  Max = max(tac_change))

    tabVarC <- tabVarC %>%
        mutate(across(.cols = c("Min", "Qnt25", "Mean", "Median", "Qnt75", "Max"),
                      .fns = \(x) round(x, 2)))
    
######@> Figure
    output$violVarC <- renderPlot({
        p21 <- ggplot(data = tac_changes,
                      aes(x = MP, y = tac_change * 100, fill = MP,
                          colour = MP)) +
            geom_hline(yintercept = 25, linetype = "dashed", alpha = 0.5) +
            geom_jitter(pch = 21, size = 4, alpha = 0.1) +
            geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), alpha = 0.6,
                        scale = "width") +
            stat_summary(fun = mean, geom = "crossbar", width = 0.2,
                         colour = "darkred") +
            coord_cartesian(ylim = c(0, 100)) +
            labs(x = "Candidate Management Procedures",
                 y = "Absolute change in TAC (%)") +
            my_theme() +
            theme(legend.position = "none")
        p21
    })

    output$violVarC2 <- render_gt({
        tabVarCGT <- tabVarC %>%
            gt() %>%
            fmt_number(
                decimals = 2,
                columns = c("Min", "Qnt25", "Mean", "Median", "Qnt75", "Max")) %>%
            data_color(
                columns = c("Min", "Qnt25", "Mean", "Median", "Qnt75", "Max"),
                colors = c("#e0ecf4", "#9ebcda", "#8c96c6", "#8856a7"),
                alpha = 0.7,
                autocolor_text = FALSE) %>%
            tab_style(
                style = list(cell_text(weight = "bold", align = "center")),
                locations = cells_column_labels(everything())) %>%
            tab_style(
                style = cell_text(weight = "bold"),
                locations = cells_body(columns = c(MP))) %>%
            tab_style(
                style = list(
                    cell_borders(sides = c("top", "bottom"), color = "black",
                                 weight = px(3))),
                locations = list(cells_column_labels(columns = gt::everything()))) %>%
            tab_style(
                style = list(
                    cell_borders(sides = c("bottom"), color = "black",
                                 weight = px(3))),
                locations = list(cells_body(rows = 4))) %>%
            tab_options(table.width = pct(100),
                        table.font.size = px(18),
                        heading.title.font.size = px(18),
                        heading.subtitle.font.size = px(14),
                        column_labels.font.size = px(18),
                        data_row.padding = px(4))
        tabVarCGT
    })

########################################################################
######@> TAB 8 = About...

}

########################################################################
##
##                  Creative Commons License 4.0
##                       (CC BY-NC-SA 4.0)
##
##  This is a humam-readable summary of (and not a substitute for) the
##  license (https://creativecommons.org/licenses/by-nc-nd/4.0/legalcode)
##
##  You are free to:
##
##  Share - copy and redistribute the material in any medium or format.
##
##  The licensor cannot revoke these freedoms as long as you follow the
##  license terms.
##
##  Under the following terms:
##
##  Attribution - You must give appropriate credit, provide a link to
##  license, and indicate if changes were made. You may do so in any
##  reasonable manner, but not in any way that suggests the licensor
##  endorses you or your use.
##
##  NonCommercial - You may not use the material for commercial
##  purposes.
##
##  ShareAlike - If you remix, transform, or build upon the material,
##  you must distributive your contributions under the same license
##  as the  original.
##
##  No additional restrictions — You may not apply legal terms or
##  technological measures that legally restrict others from doing
##  anything the license permits.
##
########################################################################
    
