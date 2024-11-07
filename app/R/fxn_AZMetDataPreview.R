#' `fxn_AZMetDataPreview.R` - Format downloaded AZMet hourly or daily data for HTML table preview
#' 
#' @param inData - Downloaded AZMet hourly or daily data from `fxn_AZMetDataELT.R`
#' @param timeStep - AZMet data time step
#' @return `dfAZMetDataPreview` - data table formatted for HTML table


fxn_AZMetDataPreview <- function(inData, timeStep) {
  
  # HOURLY "date_datetime", "wind_2min_timestamp"
  if (timeStep == "Hourly") {
    dfAZMetDataPreview <- inData %>%
      dplyr::mutate(dplyr::across(
        c("date_doy", "date_year", "meta_needs_review", "meta_version", "relative_humidity", "wind_2min_vector_dir", "wind_vector_dir", "wind_vector_dir_stand_dev"), 
        \(x) format(x, nsmall = 0)
      )) %>%
      dplyr::mutate(dplyr::across(
        c("dwpt", "dwptF", "eto_azmet", "heatstress_cottonC", "heatstress_cottonF", "precip_total", "temp_airC", "temp_airF", "temp_soil_10cmC", "temp_soil_10cmF", "temp_soil_50cmC", "temp_soil_50cmF", "wind_2min_spd_max_mph", "wind_2min_spd_max_mps", "wind_2min_spd_mean_mph", "wind_2min_spd_mean_mps", "wind_spd_max_mph", "wind_spd_max_mps", "wind_spd_mph", "wind_spd_mps", "wind_vector_magnitude", "wind_vector_magnitude_mph"), 
        \(x) format(x, nsmall = 1)
      )) %>%
      dplyr::mutate(dplyr::across(
        c("eto_azmet_in", "meta_bat_volt", "precip_total_in", "sol_rad_total", "sol_rad_total_ly", "vp_actual", "vp_deficit"), 
        \(x) format(x, nsmall = 2)
      )) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
      dplyr::mutate(dplyr::across(c("dwpt", "dwptF"), as.numeric))
  }
  
  # DAILY "datetime" "wind_2min_timestamp"
  if (timeStep == "Daily") {
    dfAZMetDataPreview <- inData %>%
      dplyr::mutate(dplyr::across(
        c("chill_hours_0C", "chill_hours_20C", "chill_hours_32F", "chill_hours_45F", "chill_hours_68F", "chill_hours_7C", "date_doy", "date_year", "meta_needs_review", "meta_version", "relative_humidity_max", "relative_humidity_mean", "relative_humidity_min", "wind_2min_vector_dir", "wind_vector_dir", "wind_vector_dir_stand_dev"), 
        \(x) format(x, nsmall = 0)
      )) %>%
      dplyr::mutate(dplyr::across(
        c("dwpt_mean", "dwpt_meanF", "eto_azmet", "eto_pen_mon", "heat_units_10C", "heat_units_13C", "heat_units_3413C", "heat_units_45F", "heat_units_50F", "heat_units_55F", "heat_units_7C", "heat_units_9455F", "heatstress_cotton_meanC", "heatstress_cotton_meanF", "precip_total_mm", "temp_air_maxC", "temp_air_maxF", "temp_air_meanC", "temp_air_meanF", "temp_air_minC", "temp_air_minF", "temp_soil_10cm_maxC", "temp_soil_10cm_maxF", "temp_soil_10cm_meanC",  "temp_soil_10cm_meanF", "temp_soil_10cm_minC", "temp_soil_10cm_minF", "temp_soil_50cm_maxC", "temp_soil_50cm_maxF", "temp_soil_50cm_meanC", "temp_soil_50cm_meanF", "temp_soil_50cm_minC", "temp_soil_50cm_minF", "wind_2min_spd_max_mph", "wind_2min_spd_max_mps", "wind_2min_spd_mean_mph", "wind_2min_spd_mean_mps", "wind_spd_max_mph", "wind_spd_max_mps", "wind_spd_mean_mph", "wind_spd_mean_mps", "wind_vector_magnitude", "wind_vector_magnitude_mph"), 
        \(x) format(x, nsmall = 1)
      )) %>%
      dplyr::mutate(dplyr::across(
        c("eto_azmet_in", "eto_pen_mon_in", "meta_bat_volt_max", "meta_bat_volt_mean", "meta_bat_volt_min", "precip_total_in", "sol_rad_total", "sol_rad_total_ly", "vp_actual_max", "vp_actual_mean", "vp_actual_min", "vp_deficit_mean"), 
        \(x) format(x, nsmall = 2)
      )) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  }
  
  
  
  # Format for `gt`
  #stubColumn <- function() {
  #  dplyr::contains("datetime")
  #}
  
  #dfAZMetDataPreview <- dfAZMetDataPreview |>
  #  gt::gt(
  #    id = "cardTable"#,
      #rowname_col = "date_datetime"#dplyr::select(stubColumn())
  #  ) |>
  #  gt::opt_row_striping(row_striping = TRUE) |>
  #  gt::tab_style(
  #    style = gt::cell_borders(
  #      color = "#dee2e6",
  #      sides = "all",
  #      style = "solid",
  #      weight = gt::px(1.0)
  #    ),
  #    locations = gt::cells_body()
  #  ) |>
  #  gt::opt_interactive(
  #    active = TRUE,
  #    height = 400,
  #    use_pagination = FALSE,
  #    use_sorting = FALSE
  #  ) |>
    #gt::opt_align_table_header()
    #gt::opt_css(
    #  css = "
    #  #cardTable {
    #    background-color: #001C48; 
    #    color: #fff;
    #  }
    #  "
    #)
    #gt::tab_header(
    #  title = gt::md("**Title**"),
    #  subtitle = gt::md("Sub*title*")
    #) |>
    #gt::tab_spanner(
    #  label = "Data Variables",
    #  columns = "meta_needs_review"
    #)
  
  #ncharToPixels <- function(input) {
  #  input * 12
  #}
  
  #dfAZMetDataPreview <- dfAZMetDataPreview |>
  #  reactable::reactable(
  #    bordered = TRUE,
  #    columns = list(
  #      meta_needs_review = reactable::colDef(width = ncharToPixels(nchar("meta_needs_review"))),
  #      meta_station_id = reactable::colDef(width = ncharToPixels(nchar("meta_station_id"))),
  #      meta_station_name = reactable::colDef(width = ncharToPixels(nchar("meta_station_name")))
  #    ),
  #    defaultColDef = reactable::colDef(
        #footer = function(values, name) {
        #  htmltools::div(name, style = list(fontWeight = 600))
        #},
    #    header = function(value) {
    #      sort_icon <- htmltools::span(class = "card-table-sort-icon", "aria-hidden" = TRUE)
    #      htmltools::tagList(value, sort_icon)
    #    },
  #      headerClass = "nav-link"#,
       #headerVAlign = "top",
  #      html = TRUE#,
        #minWidth = 150,
        #sortable = TRUE
        #width = 250
  #    ),
  #    height = 400,
  #    highlight = TRUE,
  #    pagination = FALSE,
      #resizable = TRUE,
    #  searchable = FALSE,
    #  showSortIcon = FALSE,
      #showSortable = TRUE,
      #sortable = TRUE,
  #    striped = TRUE#,
      #style = list(
      #  color = "red"
      #),
      #theme = reactableTheme(
      #  headerStyle = list(
      #    "&:hover[aria-sort]" = list(background = "hsl(0, 0%, 96%)"),
      #    "&[aria-sort='ascending'], &[aria-sort='descending']" = list(background = "hsl(0, 0%, 96%)")
      #  )
      #),
    #width = "auto"#,  
  #  wrap = FALSE
  #  )
  
  
  # https://stackoverflow.com/questions/69835894/workaround-for-issues-with-freezing-header-in-dtdatatable-in-r-shiny
  dfAZMetDataPreview <- dfAZMetDataPreview |>
   DT::datatable(
     #class = "compact",
     #extensions = "FixedColumns",
     #filter = "none",
     #height = 300,
     #caption = "This is the table caption.",
     options = list(
       cellBorder = TRUE,
       deferRender = TRUE,
       #fixedColumns = TRUE,
       #dom = "<lf<\"datatables-scroll\"t>ipr>",
  #    pageLength = -1,
       orderClasses = TRUE,
       ordering = TRUE,
       paging = FALSE,
       scrollCollapse = TRUE,
       scroller = TRUE,
       scrollX = TRUE,
       scrollY = "400px",
       searching = FALSE
     ),
     rownames = FALSE,
     selection = "none"
   ) |>
    DT::formatStyle(
      border = "1px solid #dee2e6",
      columns = colnames(dfAZMetDataPreview)
      #style = "overflow-x: auto"
    )
  
  
  
  return(dfAZMetDataPreview)
}
