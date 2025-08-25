# https://rstudio.github.io/bslib/articles/bs5-variables/index.html

# "Hard-code" Bootstrap version before deployment
# https://rstudio.github.io/bslib/articles/dashboards/index.html
#bslib::version_default()

theme = 
  bslib::bs_theme(
    version = 5,
    preset = NULL,
    #...,
    bg = "#FFFFFF",
    fg = "#000000",
    primary = NULL,
    secondary = NULL,
    success = NULL,
    info = NULL,
    warning = NULL,
    danger = NULL,
    base_font = NULL,
    code_font = NULL,
    heading_font = NULL,
    font_scale = NULL,
    bootswatch = NULL,
    "bslib-spacer" = "1.0rem",
    "card-border-radius" = 0,
    "focus-ring-color" = rgb(red = 0/255, green = 0/255, blue = 0/255, 0.1),
    "focus-ring-width" = "0.1rem",
    "table-hover-bg" = rgb(red = 0/255, green = 0/255, blue = 0/255, 0.08),
    "table-striped-bg" = rgb(red = 0/255, green = 0/255, blue = 0/255, 0.04),
    "tooltip-bg" = rgb(red = 30/255, green = 82/255, blue = 136/255, alpha = 0.9)
  ) |>
  bslib::bs_add_rules("
    .nav-item {
      background-color: #FFFFFF !important;
    }
    
    .nav-link {
      background-color: rgba(226, 233, 235, 0.3) !important;
      border-color: #E2E9EB #E2E9EB #E2E9EB !important;
      border-width: 1px 1px 1px 1px !important;
      color: #8B0015 !important;
      text-decoration: underline !important;
    }
    
    .nav-link:hover {
      color: #3f0009 !important;
      text-decoration: underline;
    }
    
    .nav-link.active {
      background-color: #E2E9EB !important;
      border-color: #E2E9EB #E2E9EB #E2E9EB !important;
      border-width: 1px 1px 1px 1px !important;
      color: var(--bs-body-color) !important;
      text-decoration: underline;
    }
    
    .nav-link.active:hover {
      color: #3f0009 !important;
      text-decoration: underline;
    }
    
    .sorting {
      background-color: #FFFFFF !important;
      border-bottom-color: #dee2e6 !important;
      border-bottom-style: solid !important;
      border-bottom-width: 1px !important;
      color: #8B0015 !important;
      text-decoration: underline !important;
    }
    
    .sorting.active {
      background-color: #E2E9EB !important;
      border-color: #E2E9EB #E2E9EB #E2E9EB !important;
      border-width: 1px 1px 1px 1px !important;
      color: var(--bs-body-color) !important;
      text-decoration: underline;
    }
    
    .sorting.active:hover {
      color: #3f0009 !important;
      text-decoration: underline;
    }
    
    .sorting:hover {
      color: #3f0009 !important;
      text-decoration: underline;
    }
    
    .tooltip-inner {
      background-color: rgba(30, 82, 136, 0.9) !important;
    }
  ")
