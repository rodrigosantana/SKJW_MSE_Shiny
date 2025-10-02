########################################################################
## Description: Bayesian Finite Population Sampling - Shiny App -
## FrontEnd
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
######@> Pacotes necessarios
library(shiny)
library(dplyr)
library(plotly)
library(DT)
library(shinydashboard)
library(shinydashboardPlus)
library(waiter)
library(gt)

########################################################################
## Interface do usuario (ui.R)...
### Title:

header <- dashboardHeader(
    title = "ICCAT: SKJ-W MSE"
    ## dropdownMenuOutput("messageMenu")
)

### SideBar:
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Introduction", tabName = "intro",
                 icon = icon("chalkboard-teacher")),
        ## menuItem("Operating Models", tabName = "oms",
        ##          icon = icon("database")),
        menuItem("Trajectories", tabName = "traj",
                 icon = icon("chart-line")),
        menuItem("Kobe timeseries", tabName = "kobe",
                 icon = icon("chart-area")),
        menuItem("Quilty plot", tabName = "quil",
                 icon = icon("calculator")),
        menuItem("Violin plot", tabName = "viol",
                 icon = icon("chart-pie")),
        menuItem("About", tabName = "about",
                 icon = icon("snowman"))
    )
)


### Dashboard:
body <- dashboardBody(
    use_waiter(),
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css",
                  href = "custom.css")
        ),

### Tabintes:
    tabItems(

########################################################################
### TAB 1 - INTRODUCTION...
        tabItem(tabName = "intro",
                fluidPage(
                    box(width = 10, status = "success",
                        shiny::includeMarkdown("INTRO.md"))
                )),

########################################################################
### TAB 2 - OPERATING MODELS...
        tabItem(tabName = "oms",
                
                ),

########################################################################
### TAB 3 - TRAJECTORIS...
      tabItem(tabName = "traj",
              fluidRow(
                  box(width = 12,
                      ## height = 12,
                      title = "Spawning Stock Biomass relative to the biomass that produces the Maximum Sustainable Yield",
                      status = "success", solidHeader = TRUE,
                      plotOutput(outputId = "trajSSB"))
              ),
              fluidRow(
                  box(width = 12,
                      ## height = 12,
                      title = "Fishing mortality relative to the fishing mortality that produces the Maximum Sustainable Yield",
                      status = "success", solidHeader = TRUE,
                      plotOutput(outputId = "trajF"))
              ),
              fluidRow(
                  box(width = 12,
                      ## height = 12,
                      title = "Total allowable catches in metric tons",
                      status = "success", solidHeader = TRUE,
                      plotOutput(outputId = "trajTAC"))
              )),

########################################################################
### TAB 4 - KOBE TIMESERIES...
    tabItem(tabName = "kobe",
            fluidRow(
                box(width = 8,
                    ## height = 20,
                    title =
                        "Kobe timeseries plot - Index-based: Exploitation rate CMP (CE)",
                    status = "success", solidHeader = TRUE,
                    plotOutput(outputId = "kobeCEtime", height = "400px")),
                box(width = 4,
                    ## height = 20,
                    title = "Kobe final year (2055) - Index-based: Exploitation rate CMP (CE)",
                    status = "success", solidHeader = TRUE,
                    plotOutput(outputId = "kobeCE", height = "400px"))
            ),
            fluidRow(
                box(width = 8,
                    ## height = 20,
                    title =
                        "Kobe timeseries plot - Index-based: Index ratio CMP (IR)",
                    status = "success", solidHeader = TRUE,
                    plotOutput(outputId = "kobeIRtime", height = "400px")),
                box(width = 4,
                    ## height = 20,
                    title =
                        "Kobe final year (2055) - Index-based: Index ratio CMP (IR)",
                    status = "success", solidHeader = TRUE,
                    plotOutput(outputId = "kobeIR", height = "400px"))
            ),
            fluidRow(
                box(width = 8,
                    ## height = 20,
                    title =
                        "Kobe timeseries plot - Model-based: Surplus production CMP (SP)",
                    status = "success", solidHeader = TRUE,
                    plotOutput(outputId = "kobeSPtime", height = "400px")),
                box(width = 4,
                    ## height = 20,
                    title =
                        "Kobe final year (2055) - Model-based: Surplus production CMP (SP)",
                    status = "success", solidHeader = TRUE,
                    plotOutput(outputId = "kobeSP", height = "400px"))
            ),
            fluidRow(
                box(width = 8,
                    ## height = 20,
                    title =
                        "Kobe timeseries plot - Model-based: Surplus production HCR CMP (SPAH)",
                    status = "success", solidHeader = TRUE,
                    plotOutput(outputId = "kobeSPAHtime", height = "400px")),
                box(width = 4,
                    ## height = 20,
                    title =
                        "Kobe final year (2055) - Model-based: Surplus production HCR CMP (SPAH)",
                    status = "success", solidHeader = TRUE,
                    plotOutput(outputId = "kobeSPAH", height = "400px"))
            )),

########################################################################
### TAB 5 - QUILTY PLOT...

    tabItem(tabName = "quil",
            fluidRow(
                box(width = 12,
                    ## height = 20,
                    title = "Quiltplot",
                    status = "success", solidHeader = TRUE,
                    gt_output(outputId = "quilt"))
            )),

########################################################################
### TAB 6 - VIOLIN PLOT...

    tabItem(tabName = "viol",
            fluidRow(
                box(width = 12,
                    ## height = 20,
                    title =
                        "Absolute change in Total Allowable Catches across management periods",
                    status = "success", solidHeader = TRUE,
                    plotOutput(outputId = "violVarC",
                               height = "800px"))),
            fluidRow(
                box(width = 12,
                    ## height = 20,
                    title =
                        "Summary table of the Absolute change in Total Allowable Catches across management periods",
                    status = "success", solidHeader = TRUE,
                    gt_output(outputId = "violVarC2"))
            )),

########################################################################
### TAB 8 = About...
    tabItem(tabName = "about",
            fluidPage(
                box(width = 10, status = "success",
                    shiny::includeMarkdown("ABOUT.md"))
            ))
    )
)

ui <- dashboardPage(header, sidebar, body, title = "ICCAT: SKJ-W MSE",
                    skin = "black-light")

######################################################################
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or (at
## your option) any later version.
##
## This program is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
##
######################################################################
