# https://rstudio.github.io/bslib/articles/bs5-variables/index.html

# "Hard-code" Bootstrap version before deployment
# https://rstudio.github.io/bslib/articles/dashboards/index.html
#bslib::version_default()

theme = 
  bslib::bs_theme(
    version = 5,
    preset = NULL,
    #...,
    bg = NULL,
    fg = NULL,
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
    "card-border-radius" = 0
  ) |>
  bslib::bs_add_rules("
    .card-header {
      background-color: #FFFFFF !important;
      border-bottom-color: #E2E9EB !important;
      border-bottom-width: 6px !important;
    }
    
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
  ")
