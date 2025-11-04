# ------------------------------------------------------------------------------
# Project: Seoul Bike Interactive EDA (from scratch)
# Author: Shohn Godboldt
# Date: 2025-01-11
# Purpose: Interactive EDA with subsetting (2 categorical + 2 numeric via Apply),
#          tables/summaries, and plots (≥6 total, ≥4 multivariate, ≥1 faceted, ≥1 novel).
# Data: Kaggle "Seoul Bike Sharing Demand Prediction" (SeoulBikeData.csv) in /data.
# ------------------------------------------------------------------------------

# ==== Libraries ================================================================
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(DT)
library(janitor)
library(stringr)
library(purrr)
library(shinycssloaders)
library(scales)
library(readr)
library(rlang)

# ==== Data loader (reads your CSV) ============================================
load_dataset <- function() {
  path <- "data/SeoulBikeData.csv"
  if (!file.exists(path)) stop("Dataset not found: place SeoulBikeData.csv in /data.")
  df <- readr::read_csv(path, show_col_types = FALSE)
  df <- janitor::clean_names(df)
  # Typical columns after clean_names():
  # date, rented_bike_count, hour, temperature_c, humidity, wind_speed_m_s,
  # visibility_10m, dew_point_temperature_c, solar_radiation_mj_m2,
  # rainfall_mm, snowfall_cm, seasons, holiday, functioning_day
  df <- df %>%
    mutate(
      date = suppressWarnings(as.Date(date, format = "%d/%m/%Y")),
      seasons = as.factor(seasons),
      holiday = as.factor(holiday),
      functioning_day = as.factor(functioning_day)
    )
  # Coerce numerics safely if needed
  num_candidates <- c("rented_bike_count","temperature_c","humidity",
                      "wind_speed_m_s","visibility_10m","dew_point_temperature_c",
                      "solar_radiation_mj_m2","rainfall_mm","snowfall_cm","hour")
  for (nm in intersect(names(df), num_candidates)) {
    df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }
  df
}

df_full <- load_dataset()

# Identify types dynamically (works even if column names differ slightly)
is_categorical <- function(x) is.factor(x) || is.character(x) || is.logical(x)
is_numeric     <- function(x) is.numeric(x) || is.integer(x)

cat_cols <- names(df_full)[vapply(df_full, is_categorical, logical(1))]
num_cols <- names(df_full)[vapply(df_full, is_numeric,     logical(1))]

# Smart defaults
default_cat1 <- if ("seasons" %in% cat_cols) "seasons" else (cat_cols[1] %||% NULL)
default_cat2 <- if ("holiday" %in% cat_cols) "holiday" else (cat_cols[2] %||% NULL)
default_num1 <- if ("rented_bike_count" %in% num_cols) "rented_bike_count" else (num_cols[1] %||% NULL)
default_num2 <- if ("temperature_c" %in% num_cols) "temperature_c" else (num_cols[2] %||% NULL)

# ==== UI =======================================================================
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 340,
    h4("Subset Data"),
    helpText("Pick up to 2 categorical and 2 numeric variables. Click 'Apply Filters' to update all tabs."),
    
    # Categorical var 1 + level selector
    selectInput("cat_var1", "Categorical variable 1", choices = cat_cols, selected = default_cat1),
    uiOutput("cat_levels1"),
    
    # Categorical var 2 + level selector
    selectInput("cat_var2", "Categorical variable 2", choices = cat_cols, selected = default_cat2),
    uiOutput("cat_levels2"),
    
    # Numeric var 1 + dynamic range slider
    selectInput("num_var1", "Numeric variable 1", choices = num_cols, selected = default_num1),
    uiOutput("num_range1"),
    
    # Numeric var 2 + dynamic range slider
    selectInput("num_var2", "Numeric variable 2", choices = num_cols, selected = default_num2),
    uiOutput("num_range2"),
    
    # Apply gate (prevents recomputation on every UI change)
    actionButton("apply_filters", "Apply Filters", class = "btn-primary")
  ),
  
  navs_tab(
    nav_panel("About",
              h3("About this App"),
              p("Purpose: Interactive exploration of the Kaggle Seoul Bike dataset with filtering, summaries, and plots."),
              p("Data: Kaggle “Seoul Bike Sharing Demand Prediction” (place ",
                code("SeoulBikeData.csv"), " in ", code("data/"), ")."),
              tags$ul(
                tags$li(strong("Sidebar:"), " choose filters then click ", strong("Apply"), " to update."),
                tags$li(strong("Data Download:"), " view and export the current subset as CSV."),
                tags$li(strong("Data Exploration:"), " build one-/two-way tables, numeric summaries, and ≥6 plots (≥4 multivariate, ≥1 faceted, ≥1 novel heatmap).")
              ),
              tags$img(src = "about.png",
                       style = "max-width:280px;border-radius:12px;box-shadow:0 4px 12px rgba(0,0,0,.15);")
    ),
    
    nav_panel("Data Download",
              h3("Filtered Data Table"),
              helpText("This reflects the subset created by the sidebar filters."),
              withSpinner(DTOutput("table")),
              br(),
              downloadButton("download_csv", "Download current subset")
    ),
    
    nav_panel("Data Exploration",
              h3("Summaries & Plots"),
              navset_card_pill(
                
                nav_panel("Categorical summaries",
                          fluidRow(
                            column(6,
                                   selectInput("one_way_var", "One-way table variable", choices = cat_cols, selected = default_cat1),
                                   withSpinner(tableOutput("one_way_table"))
                            ),
                            column(6,
                                   selectInput("two_way_row", "Two-way table rows", choices = cat_cols, selected = default_cat1),
                                   selectInput("two_way_col", "Two-way table cols", choices = cat_cols, selected = default_cat2),
                                   withSpinner(tableOutput("two_way_table"))
                            )
                          )
                ),
                
                nav_panel("Numeric summaries",
                          fluidRow(
                            column(7,
                                   selectInput("num_summary_var", "Numeric variable", choices = num_cols, selected = default_num1),
                                   selectInput("num_group_var", "Group by (categorical)", choices = cat_cols, selected = default_cat1),
                                   withSpinner(tableOutput("numeric_summary"))
                            ),
                            column(5, helpText("Shows n, mean, sd, median, min, max for the chosen numeric variable grouped by the selected categorical."))
                          )
                ),
                
                nav_panel("Plots",
                          fluidRow(
                            column(4,
                                   selectInput("plot_x", "X", choices = names(df_full),
                                               selected = if ("temperature_c" %in% names(df_full)) "temperature_c" else names(df_full)[1]),
                                   selectInput("plot_y", "Y", choices = names(df_full),
                                               selected = if ("rented_bike_count" %in% names(df_full)) "rented_bike_count" else default_num1),
                                   selectInput("plot_color", "Color (categorical)", choices = c("None", names(df_full)),
                                               selected = if ("seasons" %in% names(df_full)) "seasons" else "None"),
                                   selectInput("plot_facet", "Facet (categorical)", choices = c("None", cat_cols),
                                               selected = if ("functioning_day" %in% cat_cols) "functioning_day" else "None"),
                                   selectInput("plot_type", "Plot type",
                                               choices = c("Scatter" = "scatter", "Boxplot" = "box", "Bar (counts)" = "bar_count",
                                                           "Bar (mean Y by X)" = "bar_mean", "Histogram" = "hist", "Density" = "density",
                                                           "Heatmap (2D density)" = "heatmap2d"),
                                               selected = "scatter"),
                                   checkboxInput("plot_jitter", "Add jitter (scatter only)", value = TRUE),
                                   checkboxInput("plot_smooth", "Add smoother (scatter only)", value = FALSE)
                            ),
                            column(8, withSpinner(plotOutput("plot_main", height = "460px")))
                          ),
                          helpText("Target: ≥6 plots total; ≥4 multivariate (use Color/Facet); ≥1 faceted; ≥1 novel (2D density heatmap).")
                )
              )
    )
  )
)

# ==== Server ===================================================================
server <- function(input, output, session) {
  
  # Level selectors for categoricals
  output$cat_levels1 <- renderUI({
    req(input$cat_var1)
    lvls <- sort(unique(df_full[[input$cat_var1]]))
    selectizeInput("cat_vals1", "Levels to include (Cat 1)", choices = lvls, multiple = TRUE, selected = lvls)
  })
  output$cat_levels2 <- renderUI({
    req(input$cat_var2)
    lvls <- sort(unique(df_full[[input$cat_var2]]))
    selectizeInput("cat_vals2", "Levels to include (Cat 2)", choices = lvls, multiple = TRUE, selected = lvls)
  })
  
  # Dynamic numeric ranges
  output$num_range1 <- renderUI({
    req(input$num_var1)
    rng <- range(df_full[[input$num_var1]], na.rm = TRUE)
    sliderInput("num_rng1", paste0("Range for ", input$num_var1),
                min = rng[1], max = rng[2], value = rng, step = diff(rng)/100)
  })
  output$num_range2 <- renderUI({
    req(input$num_var2)
    rng <- range(df_full[[input$num_var2]], na.rm = TRUE)
    sliderInput("num_rng2", paste0("Range for ", input$num_var2),
                min = rng[1], max = rng[2], value = rng, step = diff(rng)/100)
  })
  
  # Reactive filtering (gated by Apply)
  filtered <- reactiveVal(df_full)
  observeEvent(input$apply_filters, {
    df <- df_full
    if (!is.null(input$cat_var1) && !is.null(input$cat_vals1)) {
      df <- df %>% filter(.data[[input$cat_var1]] %in% input$cat_vals1)
    }
    if (!is.null(input$cat_var2) && !is.null(input$cat_vals2)) {
      df <- df %>% filter(.data[[input$cat_var2]] %in% input$cat_vals2)
    }
    if (!is.null(input$num_var1) && !is.null(input$num_rng1)) {
      df <- df %>% filter(between(.data[[input$num_var1]], input$num_rng1[1], input$num_rng1[2]))
    }
    if (!is.null(input$num_var2) && !is.null(input$num_rng2)) {
      df <- df %>% filter(between(.data[[input$num_var2]], input$num_rng2[1], input$num_rng2[2]))
    }
    filtered(df)
  }, ignoreInit = TRUE)
  
  # Data table + download
  output$table <- renderDT({
    datatable(filtered(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$download_csv <- downloadHandler(
    filename = function() sprintf("subset_%s.csv", format(Sys.time(), "%%Y%%m%%d_%%H%%M%%S")),
    content = function(file) {
      write.csv(filtered(), file, row.names = FALSE)
    }
  )
  
  # Categorical summaries
  output$one_way_table <- renderTable({
    df <- filtered(); req(nrow(df) > 0, input$one_way_var)
    tbl <- df %>% count(.data[[input$one_way_var]]) %>% arrange(desc(n))
    janitor::adorn_totals(tbl, name = "Total")
  })
  output$two_way_table <- renderTable({
    df <- filtered(); req(nrow(df) > 0, input$two_way_row, input$two_way_col)
    df %>% count(.data[[input$two_way_row]], .data[[input$two_way_col]]) %>%
      tidyr::pivot_wider(names_from = .data[[input$two_way_col]], values_from = n, values_fill = 0) %>%
      janitor::adorn_totals(where = c("row", "col"))
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
  
  # Numeric summaries
  output$numeric_summary <- renderTable({
    df <- filtered(); req(nrow(df) > 0, input$num_summary_var, input$num_group_var)
    df %>%
      group_by(.data[[input$num_group_var]]) %>%
      summarise(
        n = n(),
        mean = mean(.data[[input$num_summary_var]], na.rm = TRUE),
        sd = sd(.data[[input$num_summary_var]], na.rm = TRUE),
        median = median(.data[[input$num_summary_var]], na.rm = TRUE),
        min = min(.data[[input$num_summary_var]], na.rm = TRUE),
        max = max(.data[[input$num_summary_var]], na.rm = TRUE),
        .groups = "drop"
      )
  })
  
  # Plots (≥6 total; ≥4 multivariate; ≥1 faceted; ≥1 novel)
  output$plot_main <- renderPlot({
    df <- filtered(); req(nrow(df) > 0, input$plot_type, input$plot_x)
    p <- ggplot(df, aes(x = .data[[input$plot_x]]))
    
    if (input$plot_type %in% c("scatter", "box", "bar_mean", "heatmap2d", "density")) {
      req(input$plot_y)
      p <- p + aes(y = .data[[input$plot_y]])
    }
    
    if (!is.null(input$plot_color) && input$plot_color != "None") {
      p <- p + aes(color = .data[[input$plot_color]], fill = .data[[input$plot_color]])
    }
    
    if (input$plot_type == "scatter") {
      p <- p + geom_point(alpha = if (input$plot_jitter) 0.7 else 1,
                          position = if (input$plot_jitter) position_jitter(width = 0.1, height = 0.1) else "identity")
    } else if (input$plot_type == "box") {
      p <- p + geom_boxplot(outlier.alpha = 0.5)
    } else if (input$plot_type == "bar_count") {
      p <- ggplot(df, aes(x = .data[[input$plot_x]])) + geom_bar()
    } else if (input$plot_type == "bar_mean") {
      p <- df %>% group_by(.data[[input$plot_x]]) %>% summarise(mean_y = mean(.data[[input$plot_y]], na.rm = TRUE)) %>%
        ggplot(aes(x = .data[[input$plot_x]], y = mean_y)) + geom_col()
    } else if (input$plot_type == "hist") {
      p <- ggplot(df, aes(x = .data[[input$plot_x]])) + geom_histogram(bins = 30)
    } else if (input$plot_type == "density") {
      p <- p + geom_density()
    } else if (input$plot_type == "heatmap2d") {
      p <- ggplot(df, aes(x = .data[[input$plot_x]], y = .data[[input$plot_y]])) + stat_bin2d(bins = 30)
    }
    
    if (input$plot_type == "scatter" && isTRUE(input$plot_smooth)) {
      p <- p + geom_smooth(se = FALSE, method = "loess")
    }
    
    if (!is.null(input$plot_facet) && input$plot_facet != "None") {
      p <- p + facet_wrap(vars(.data[[input$plot_facet]]))
    }
    
    p + labs(
      title = "Interactive Plot",
      x = input$plot_x,
      y = if (!is.null(input$plot_y)) input$plot_y else NULL
    ) + theme_minimal(base_size = 13)
  })
}

# ==== App entry ================================================================
shinyApp(ui, server)
# ------------------------------------------------------------------------------
