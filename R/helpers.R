# Call this function with an input (such as `textInput("text", NULL, "Search")`) if you
# want to add an input to the navbar (from dean attali,
# https://github.com/daattali/advanced-shiny)
navbarPageWithInputs <- function(..., inputs) {
  navbar <- shiny::navbarPage(...)
  form <- shiny::tags$form(class = "navbar-form", inputs)

  # browser()

  navbar[[3]][[1]]$children[[1]]$children[[2]] <- htmltools::tagAppendChild(
    navbar[[3]][[1]]$children[[1]]$children[[2]], form
  )
  navbar
}

#' translate app function
#'
#' translate the app based on the lang selected
translate_app <- function(id, lang) {

  app_translations

  id %>%
    purrr::map_chr(
      ~ app_translations %>%
        dplyr::filter(text_id == .x) %>% {
          data_filtered <- .
          if (nrow(data_filtered) < 1) {
            .x
          } else {
            dplyr::pull(data_filtered, !! rlang::sym(glue::glue("translation_{lang}")))
          }
        }
    )
}


## Custom functions that help to build the shiny app as well as to prepare the data for
## the analysis

# Helper to extract variable values from a netCDF file based on coordinates
ncExtractVarValueByCoords <- function(nc_file, x_coord, y_coord, var_names) {

  # open connection to the netCDF file
  nc <- ncdf4::nc_open(nc_file)

  # extract X and Y values from netCDF file
  nc_x_coord_vals <- nc$dim$X$vals
  nc_y_coord_vals <- nc$dim$Y$vals

  # index for X
  x_dist <- abs(nc_x_coord_vals - x_coord)
  x_index <- which.min(x_dist)

  # index for Y
  y_dist <- abs(nc_y_coord_vals - y_coord)
  y_index <- which.min(y_dist)

  # build the index
  index <- c(x_index, y_index)

  # extract the variables values
  var_values <- lapply(var_names, function(x){
    ncdf4::ncvar_get(nc, x, start = index, count = c(1,1))
  })

  # name the vars
  names(var_values) <- var_names

  # return the values (unlist to generate a named vector)
  return(unlist(var_values))
}

# Helper function to convert lat/long coordinates from the leaflet map to UTM
# coordinates to help extract variables from the Topography file
convertTopographyCoords <- function(coord_df) {

  # make a coordinates object from the data frame provided
  sp::coordinates(coord_df) <- ~lng+lat

  # add the projection string attribute
  sp::proj4string(coord_df) <- sp::CRS("+proj=longlat +datum=WGS84")

  # transform the coordinates porjection to the Topography projection
  coord_utm <- sp::spTransform(
    coord_df,
    sp::CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +towgs84=0,0,0")
  )

  # return the result
  return(coord_utm)
}

# Function to build the SpatialPointsTopography object
getTopographyObject <- function(user_df) {
  # Convert latlong to utm
  user_coords_utm <- convertTopographyCoords(user_df)

  # get elevation, slope and aspect values
  n_coords <- length(user_coords_utm@coords[,1])

  vals <- vector('list', n_coords)

  for (i in 1:n_coords) {
    vals[[i]] <- ncExtractVarValueByCoords(
      nc_file = file.path(
        '/home', 'vgranda', 'LFC', '11_meteoland_data', 'MDT', 'Topology_grid.nc'
      ),
      x_coord = user_coords_utm@coords[i,1],
      y_coord = user_coords_utm@coords[i,2],
      var_names = c('Elevation', 'Slope', 'Aspect')
    )
  }

  # vals is a list of named vectors, this must be converted to a data frame
  # for easily add the variable values to the spatial topography object
  vals_df <- as.data.frame(matrix(unlist(vals), nrow = length(vals),
                                  byrow = TRUE))

  names(vals_df) <- names(vals[[1]])

  # build the topography object
  user_topo <- SpatialPointsTopography(
    points = user_coords_utm,
    elevation = vals_df$Elevation,
    slope = vals_df$Slope,
    aspect = vals_df$Aspect
  )

  return(user_topo)
}

# Interpolation process for "current mode"
current_points_mode_process <- function(user_df, user_dates,
                                        excludeRainFromStations = character(0),
                                        updateProgress = NULL) {

  # STEP 1 BUILD THE INTERPOLATOR OBJECT
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Building the interpolation object',
      value = 0.05
    )
  }


  # get the default parameters for the MetereologyInterpolationData object
  params <- meteoland::defaultInterpolationParams()

  # build the dates vector to read the metereology files
  user_dates <- as.Date(user_dates)
  datevec <- (user_dates[[1]] - max(params$St_Precipitation, params$St_TemperatureRange)):user_dates[[2]]
  datevec <- as.Date(datevec, format = '%j', origin = as.Date('1970-01-01'))
  ndays <- length(datevec)

  # load the metereological files
  day_data <- vector('list', ndays)
  for (i in seq_along(datevec)) {
    # files
    day_data[[i]] <- meteoland::readmeteorologypoint(
      file.path(
        '/home', 'vgranda', 'LFC', '11_meteoland_data', 'Climate',
        'Sources', 'AEMET', 'Download', 'DailyCAT',
        paste0(as.character(datevec[[i]]), '.txt')
      )
    )
    # codes
    codes <- row.names(day_data[[i]])
    # excluded codes
    excodes <- codes[codes %in% excludeRainFromStations]
    # NAs to excluded (apply quality check results)
    day_data[[i]][excodes, 'Precipitation'] <- NA
  }

  # get general info needed later
  stations_codes <- row.names(day_data[[1]])
  stations_elevation <- day_data[[1]]$elevation
  stations_slope <- rep(0, length(stations_elevation))
  stations_aspect <- rep(0, length(stations_elevation))
  stations_coords <- cbind(day_data[[1]]$coords.x1, day_data[[1]]$coords.x2)
  stations_coords_sp <- sp::SpatialPoints(
    stations_coords, sp::CRS("+proj=longlat +datum=WGS84")
  )
  station_coords_utm <- sp::spTransform(
    stations_coords_sp,
    sp::CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +towgs84=0,0,0")
  )

  stations_n <- length(stations_elevation)

  # reshape the data to build the MetereologyInterpolationData object
  MinTemperature <- matrix(
    NA, nrow = stations_n, ncol = ndays,
    dimnames = list(stations_codes, as.character(datevec))
  )
  MaxTemperature <- MinTemperature
  Precipitation <- MinTemperature
  RelativeHumidity <- MinTemperature
  Radiation <- MinTemperature
  WindSpeed <- MinTemperature
  WindDirection <- MinTemperature

  # fill the data
  for (i in seq_along(datevec)) {
    MinTemperature[,i] <- day_data[[i]][stations_codes, 'MinTemperature']
    MaxTemperature[,i] <- day_data[[i]][stations_codes, 'MaxTemperature']
    Precipitation[,i] <- day_data[[i]][stations_codes, 'Precipitation']
    RelativeHumidity[,i] <- day_data[[i]][stations_codes, 'MeanRelativeHumidity']
    Radiation[,i] <- day_data[[i]][stations_codes, 'Radiation']
    WindSpeed[,i] <- day_data[[i]][stations_codes, 'WindSpeed']
    WindDirection[,i] <- day_data[[i]][stations_codes, 'WindDirection']
  }

  # Finally, we build the interpolator object
  interpolator <- meteoland::MeteorologyInterpolationData(
    points = station_coords_utm,
    elevation = stations_elevation,
    slope = stations_slope,
    aspect = stations_aspect,
    MinTemperature = MinTemperature,
    MaxTemperature = MaxTemperature,
    Precipitation = Precipitation,
    RelativeHumidity = RelativeHumidity,
    Radiation = Radiation,
    WindSpeed = WindSpeed,
    WindDirection = WindDirection,
    params = params
  )

  # and set the parameters obtained in the calibration
  load('Data/calibrations.RData')
  interpolator@params$N_MinTemperature = tmin_cal$N
  interpolator@params$alpha_MinTemperature = tmin_cal$alpha
  interpolator@params$N_MaxTemperature = tmax_cal$N
  interpolator@params$alpha_MaxTemperature = tmax_cal$alpha
  interpolator@params$N_DewTemperature = tdew_cal$N
  interpolator@params$alpha_DewTemperature = tdew_cal$alpha
  interpolator@params$N_PrecipitationEvent = prec_cal$N
  interpolator@params$alpha_PrecipitationEvent = prec_cal$alpha
  interpolator@params$N_PrecipitationAmount = prec_cal$N
  interpolator@params$alpha_PrecipitationAmount = prec_cal$alpha
  rm(tmin_cal, tmax_cal, tdew_cal, prec_cal)

  # STEP 2 BUILD THE TOPOGRAPHY OBJECT
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Building the topography object',
      value = 0.34
    )
  }

  user_topo <- getTopographyObject(user_df)

  # STEP 3 MAKE THE INTERPOLATION
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Starting the interpolation process (this can take a while)',
      value = 0.55
    )
  }

  # we are gonna slice the user coordinates to be able to show the progress more
  # dinamically:
  # vector to store the interpolated data for each coordinate
  res_vec <- vector('list', length(user_topo@coords[,1]))

  # loop to iterate between coordinates and perform the interpolation
  for (i in 1:length(user_topo@coords[,1])) {
    # progress updates
    if (is.function(updateProgress)) {
      updateProgress(
        detail = paste0('Processing coordinates pair ', i, ' of ',
                        length(user_topo@coords[,1])),
        n_coords = length(user_topo@coords[,1])
      )
    }

    # interpolation, but storing only the data trimmed for the days selected by
    # the user
    res_vec[[i]] <- meteoland::interpolationpoints(
      object = interpolator,
      points = user_topo[i,],
      verbose = FALSE
    )@data[[1]][-c(1:15), ] ## OJO, 15 ESTA FIJADO
  }

  # now we build the spatialpointsmeteorology object
  res <- meteoland::SpatialPointsMeteorology2(
    points = user_topo,
    data = res_vec,
    dates = interpolator@dates[-c(1:15)] ## OJO, 15 ESTA FIJADO
  )

  return(res)
}

# Historical points mode logic
historical_points_mode_process <- function(user_df, user_dates,
                                           updateProgress = NULL) {

  # STEP 1 GET THE  INTERPOLATOR DATA
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Building the interpolation object',
      value = 0.05
    )
  }

  # load the interpolator mother data
  load(file.path(
    '/home', 'vgranda', 'LFC', '11_meteoland_data', 'Climate', 'Products',
    'MeteorologyInterpolationData', 'Interpolator_Mother.rda'
  ))

  # subset by the user dates
  datevec <- as.Date(user_dates)[[1]]:as.Date(user_dates)[[2]]
  datevec <- as.Date(datevec, format = '%j', origin = as.Date('1970-01-01'))

  ## workaround issue 3
  if (length(datevec) == 1) {
    datevec <- c(datevec[[1]] - 1, datevec, datevec[[1]] + 1)
  }

  interpolator <- meteoland::subsample(interpolator, dates = as.Date(datevec))

  # STEP 2 BUILD THE TOPOGRAPHY OBJECT
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Building the topography object',
      value = 0.34
    )
  }

  user_topo <- getTopographyObject(user_df)

  # STEP 3 PERFORMING THE INTERPOLATION
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Starting the interpolation process (this can take a while)',
      value = 0.45
    )
  }

  # we are gonna slice the user coordinates to be able to show the progress more
  # dinamically:
  # vector to store the interpolated data for each coordinate
  res_vec <- vector('list', length(user_topo@coords[,1]))

  # loop to iterate between coordinates and perform the interpolation
  for (i in 1:length(user_topo@coords[,1])) {
    # progress updates
    if (is.function(updateProgress)) {
      updateProgress(
        detail = paste0(
          'Processing coordinates pair ', i, ' of ',
          length(user_topo@coords[,1]), ' (This can take several minutes)'
        ),
        n_coords = length(user_topo@coords[,1])
      )
    }

    # interpolation, but storing only the data
    res_vec[[i]] <- meteoland::interpolationpoints(
      object = interpolator,
      points = user_topo[i,],
      verbose = FALSE
    )@data[[1]]
  }

  # now we build the spatialpointsmeteorology object
  res <- meteoland::SpatialPointsMeteorology(
    points = user_topo,
    data = res_vec,
    dates = interpolator@dates
  )

  return(res)

}

# Projection points mode
projection_points_mode_process <- function(user_df, rcm, rcp,
                                           updateProgress = NULL) {

  # STEP 1 BUILD THE INTERPOLATOR
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Building the interpolation object',
      value = 0.05
    )
  }

  # dates proj
  dates_interp <- c('1976-01-01', '2010-12-31')

  # lets build the interpolator, this can take a while
  # we need also a new progress object for the internal progress of
  # the interpolator
  # Create a Progress object
  progress_int <- shiny::Progress$new()
  progress_int$set(message = "Interpolating data", value = 0)
  # Close the progress when this reactive exits (even if there's an error)
  on.exit(progress_int$close())

  updateProgress_int <- function(value = NULL, detail = NULL, n_coords = NULL) {
    if (is.null(value)) {
      value <- progress_int$getValue()
      value <- value + ((progress_int$getMax() - value) / n_coords)
    }

    progress_int$set(value = value, detail = detail)
  }

  interpolator <- historical_points_mode_process(user_df, dates_interp,
                                                 updateProgress = updateProgress_int)

  # STEP 2 BUILD THE UNCORRECTED DATA
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Building the projection data',
      value = 0.75
    )
  }

  # folder & files paths
  dir_path <- file.path(
    '/home', 'vgranda', 'LFC', '11_meteoland_data', 'Climate', 'Sources', 'EUROCordex'
  )

  hist_pred_path <- file.path(dir_path, rcm, 'historical')
  future_pred_path <- file.path(dir_path, rcm, rcp)

  # get the metadata
  hist_pred <- read.table(file.path(hist_pred_path, 'MP_Cat_corr.txt'),
                          sep = '\t', header = TRUE)
  hist_pred$dir <- hist_pred_path

  future_pred <- read.table(file.path(future_pred_path, 'MP_Cat_corr.txt'),
                            sep = '\t', header = TRUE)
  future_pred$dir <- future_pred_path

  spatial_points_projs <- sp::SpatialPoints(
    hist_pred[,c('lon', 'lat')], sp::CRS('+proj=longlat +datum=WGS84')
  )

  # build the uncorrected data
  uncorrected <- meteoland::MeteorologyUncorrectedData(
    spatial_points_projs,
    hist_pred[, c('dir', 'filename')],
    future_pred[, c('dir', 'filename')],
    seq(as.Date('2006-01-01'), as.Date('2100-12-31'), by = 'day')
  )

  # get the topo
  topo <- getTopographyObject(user_df)@data

  # STEP 3 MAKE THE CORRECTION
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Performing the correction',
      value = 0.80
    )
  }
  res <- meteoland::correctionpoints(uncorrected, interpolator, topo, verbose = FALSE)


  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Finishing the process',
      value = 0.99
    )
  }
  return(res)

}

# Current grid mode
current_grid_mode_process <- function(user_coords, user_dates,
                                      excludeRainFromStations = character(0),
                                      updateProgress = NULL) {

  # STEP 1 SUBSETTING THE GRID TO GET THE TOPO
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Obtaining grid topography',
      value = 0.05
    )
  }

  # load the topography info for the grid (in this case we cheat a little, as
  # we are gonna treat the grid cells as points)
  load('Data/grid_as_points_topography.RData')

  # get the coords as an SpatialPoints object, we will need this to get the
  # intersection of cells in the polygon created by the user
  coords_topo <- sp::SpatialPoints(grid_as_points_topography@coords,
                               grid_as_points_topography@proj4string)

  # get and transform the user coords
  coords_to_polygon <- data.frame(
    x = c(user_coords$x[1], user_coords$x[2]),
    y = c(user_coords$y[1], user_coords$y[2])
  )

  coords_to_polygon_sp <- sp::SpatialPoints(
    coords_to_polygon,
    sp::CRS("+proj=longlat +datum=WGS84")
  )

  coords_to_polygon_sp <- sp::spTransform(
    coords_to_polygon_sp,
    sp::CRS('+init=epsg:3043 +proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
  )

  # create a SpatialPolygons object with the user provided coords for the grid
  user_transf_polygon_coords <- data.frame(
    x = c(coords_to_polygon_sp@coords[1,1], coords_to_polygon_sp@coords[1,1],
          coords_to_polygon_sp@coords[2,1], coords_to_polygon_sp@coords[2,1]),
    y = c(coords_to_polygon_sp@coords[1,2], coords_to_polygon_sp@coords[2,2],
          coords_to_polygon_sp@coords[2,2], coords_to_polygon_sp@coords[1,2])
  )

  user_polygon <- sp::SpatialPolygons(
    list(sp::Polygons(
      list(sp::Polygon(
        as.matrix(user_transf_polygon_coords)
      )),
      ID = 'user'
    )),
    proj4string = sp::CRS('+init=epsg:3043 +proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs')
  )

  # now we do the subset with the rgeos package
  user_topo <- grid_as_points_topography[which(rgeos::gIntersects(coords_topo, user_polygon, byid = TRUE)), ]
  user_topo_sp <- sp::SpatialPoints(user_topo@coords, proj4string = user_topo@proj4string)

  # but also we need the points outside the topo (i.e sea points, out of catalunya points...)
  spatial_grid_from_user_topo <- sp::SpatialGrid(sp::points2grid(user_topo), user_topo@proj4string)
  user_grid_points <- sp::SpatialPoints(sp::coordinates(spatial_grid_from_user_topo),
                                    proj4string = spatial_grid_from_user_topo@proj4string)

  out_of_bounds <- user_grid_points[which(
    !(
      paste(user_grid_points@coords[,1], user_grid_points@coords[,2], sep = '-') %in% paste(user_topo_sp@coords[,1], user_topo_sp@coords[,2], sep = '-')
    )
  ), ]

  # STEP 2 PREPARING THE INTERPOLATOR
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Building the interpolation object',
      value = 0.10
    )
  }

  # get the default parameters for the MetereologyInterpolationData object
  params <- meteoland::defaultInterpolationParams()

  # build the dates vector to read the metereology files
  user_dates <- as.Date(user_dates)
  datevec <- (head(user_dates, 1) - max(params$St_Precipitation, params$St_TemperatureRange)):tail(user_dates, 1)
  datevec <- as.Date(datevec, format = '%j', origin = as.Date('1970-01-01'))
  ndays <- length(datevec)

  # load the metereological files
  day_data <- vector('list', ndays)
  for (i in seq_along(datevec)) {
    # files
    day_data[[i]] <- meteoland::readmeteorologypoint(
      file.path(
        '/home', 'vgranda', 'LFC', '11_meteoland_data',
        'Climate', 'Sources', 'AEMET','Download', 'DailyCAT',
        paste0(as.character(datevec[[i]]), '.txt')
      )
    )
    # codes
    codes <- row.names(day_data[[i]])
    # excluded codes
    excodes <- codes[codes %in% excludeRainFromStations]
    # NAs to excluded (apply quality check results)
    day_data[[i]][excodes, 'Precipitation'] <- NA
  }

  # get general info needed later
  stations_codes <- row.names(day_data[[1]])
  stations_elevation <- day_data[[1]]$elevation
  stations_slope <- rep(0, length(stations_elevation))
  stations_aspect <- rep(0, length(stations_elevation))
  stations_coords <- cbind(day_data[[1]]$coords.x1, day_data[[1]]$coords.x2)
  stations_coords_sp <- sp::SpatialPoints(
    stations_coords, sp::CRS("+proj=longlat +datum=WGS84")
  )
  station_coords_utm <- sp::spTransform(
    stations_coords_sp,
    sp::CRS("+init=epsg:3043 +proj=utm +zone=31 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  )

  stations_n <- length(stations_elevation)

  # reshape the data to build the MetereologyInterpolationData object
  MinTemperature <- matrix(
    NA, nrow = stations_n, ncol = ndays,
    dimnames = list(stations_codes, as.character(datevec))
  )
  MaxTemperature <- MinTemperature
  Precipitation <- MinTemperature
  RelativeHumidity <- MinTemperature
  Radiation <- MinTemperature
  WindSpeed <- MinTemperature
  WindDirection <- MinTemperature

  # fill the data
  for (i in seq_along(datevec)) {
    MinTemperature[,i] <- day_data[[i]][stations_codes, 'MinTemperature']
    MaxTemperature[,i] <- day_data[[i]][stations_codes, 'MaxTemperature']
    Precipitation[,i] <- day_data[[i]][stations_codes, 'Precipitation']
    RelativeHumidity[,i] <- day_data[[i]][stations_codes, 'MeanRelativeHumidity']
    Radiation[,i] <- day_data[[i]][stations_codes, 'Radiation']
    WindSpeed[,i] <- day_data[[i]][stations_codes, 'WindSpeed']
    WindDirection[,i] <- day_data[[i]][stations_codes, 'WindDirection']
  }

  # Finally, we build the interpolator object
  interpolator <- meteoland::MeteorologyInterpolationData(
    points = station_coords_utm,
    elevation = stations_elevation,
    slope = stations_slope,
    aspect = stations_aspect,
    MinTemperature = MinTemperature,
    MaxTemperature = MaxTemperature,
    Precipitation = Precipitation,
    RelativeHumidity = RelativeHumidity,
    Radiation = Radiation,
    WindSpeed = WindSpeed,
    WindDirection = WindDirection,
    params = params
  )

  # and set the parameters obtained in the calibration
  load('Data/calibrations.RData')
  interpolator@params$N_MinTemperature = tmin_cal$N
  interpolator@params$alpha_MinTemperature = tmin_cal$alpha
  interpolator@params$N_MaxTemperature = tmax_cal$N
  interpolator@params$alpha_MaxTemperature = tmax_cal$alpha
  interpolator@params$N_DewTemperature = tdew_cal$N
  interpolator@params$alpha_DewTemperature = tdew_cal$alpha
  interpolator@params$N_PrecipitationEvent = prec_cal$N
  interpolator@params$alpha_PrecipitationEvent = prec_cal$alpha
  interpolator@params$N_PrecipitationAmount = prec_cal$N
  interpolator@params$alpha_PrecipitationAmount = prec_cal$alpha
  rm(tmin_cal, tmax_cal, tdew_cal, prec_cal)

  # STEP 3 PERFORM THE INTERPOLATION ON THE GRID

  # we are gonna slice the user coordinates to be able to show the progress more
  # dinamically:

  # vector to store the interpolated data for each coordinate
  res_vec <- vector('list', length(user_grid_points@coords[,1]))

  # out and user coords to check if we have data (as strings, this makes easy
  # to check if the point is in the stations grid)
  out_of_bounds_coords <- paste(out_of_bounds@coords[,1], out_of_bounds@coords[,2], sep = '-')
  user_topo_coords <- paste(user_topo@coords[,1], user_topo@coords[,2], sep = '-')

  # an empty data frame for those points out of bounds, with NAs
  dummy_days <- interpolator@dates[-c(1:15)] ## OJO, 15 ESTA FIJADO
  dummy_df <- data.frame(DOY = rep(NA, length(dummy_days)),
                         MeanTemperature = rep(NA, length(dummy_days)),
                         MinTemperature = rep(NA, length(dummy_days)),
                         MaxTemperature = rep(NA, length(dummy_days)),
                         Precipitation = rep(NA, length(dummy_days)),
                         MeanRelativeHumidity = rep(NA, length(dummy_days)),
                         MinRelativeHumidity = rep(NA, length(dummy_days)),
                         MaxRelativeHumidity = rep(NA, length(dummy_days)),
                         Radiation = rep(NA, length(dummy_days)),
                         WindSpeed = rep(NA, length(dummy_days)),
                         WindDirection = rep(NA, length(dummy_days)),
                         PET = rep(NA, length(dummy_days)))
  row.names(dummy_df) <- dummy_days

  # loop to iterate between coordinates and perform the interpolation
  for (i in 1:length(user_grid_points@coords[,1])) {

    if (is.function(updateProgress)) {
      updateProgress(
        detail = paste0('Processing grid cell ', i, ' of ',
                        length(user_grid_points@coords[,1])),
        n_coords = length(user_grid_points@coords[,1]),
        max_val = 0.80
      )
    }

    grid_coordinate <- paste(user_grid_points@coords[i,1],
                             user_grid_points@coords[i,2],
                             sep = '-')

    # check if point is out of bounds
    if (grid_coordinate %in% out_of_bounds_coords) {
      res_vec[[i]] <- dummy_df
    } else {
      # interpolation, but storing only the data trimmed for the days selected by
      # the user
      res_vec[[i]] <- meteoland::interpolationpoints(
        object = interpolator,
        points = user_topo[which(user_topo_coords == grid_coordinate), ],
        verbose = FALSE
      )@data[[1]][-c(1:15), ] ## OJO, 15 ESTA FIJADO
    }
  }

  # now we build the spatialpointsmeteorology object
  res <- meteoland::SpatialPointsMeteorology(
    points = user_grid_points,
    data = res_vec,
    dates = interpolator@dates[-c(1:15)] ## OJO, 15 ESTA FIJADO
  )

  # we need to transform the spatialpoints to spatialgrid
  # progress
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Building the interpolation object',
      value = 0.81
    )
  }

  # extract the dates data frames
  res_extracted <- meteoland::extractpointdates(res, res@dates)

  # in the case of only one date
  if (is(res_extracted, 'SpatialPointsDataFrame')) {
    res_data_list <- list(data = res_extracted@data)
  } else {
    # create a list with the dates dataframes
    res_data_list <- vector('list', length(res_extracted))

    for (i in 1:length(res_extracted)) {
      res_data_list[[i]] <- res_extracted[[i]]@data
    }
  }

  # we name the data list with the dates
  names(res_data_list) <- as.character(res@dates)

  # create the grid object
  res_grid <- meteoland::SpatialGridMeteorology(
    grid = sp::points2grid(res),
    proj4string = res@proj4string,
    data = res_data_list,
    dates = res@dates
  )

  # progress
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Building the interpolation object',
      value = 0.99
    )
  }

  return(res_grid)

}

################################################################################
# projection grid mode
projection_grid_mode_process <- function(user_coords, rcm, rcp,
                                         updateProgress = NULL) {

  # STEP 1 OPEN THE CONN TO THE netCDF FILE
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Accesing to the interpolated data',
      value = 0.05
    )
  }

  dir <- file.path('/home', 'miquel', 'Datasets', 'Climate', 'Products', 'Pixels1k',
                   'Projections', rcm, rcp)
  file <- paste0(rcm, '_', gsub('\\.', '', rcp), '.nc')

  nc <- nc_open(file.path(dir, file))

  # STEP 2 CONVERT TO UTM THE USER COORDS

  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Converting user coordinates',
      value = 0.1
    )
  }

  # convert to utm
  # make a coordinates object from the data frame provided
  coordinates(user_coords) <- ~x+y

  # add the projection string attribute
  proj4string(user_coords) <- CRS("+proj=longlat +datum=WGS84")

  # transform the coordinates porjection to the correct projection
  user_coords <- as.data.frame(spTransform(
    user_coords,
    CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +towgs84=0,0,0")
  ))

  # STEP 3 SUBSET THE netCDF DATA

  # extract X and Y values from netCDF file
  nc_x_coord_vals <- nc$dim$X$vals
  nc_y_coord_vals <- nc$dim$Y$vals

  # index for X
  x_dist_upper <- abs(nc_x_coord_vals - user_coords$x[1])
  x_index_upper <- which.min(x_dist_upper)

  x_dist_bottom <- abs(nc_x_coord_vals - user_coords$x[2])
  x_index_bottom <- which.min(x_dist_bottom)

  # index for Y
  y_dist_upper <- abs(nc_y_coord_vals - user_coords$y[1])
  y_index_upper <- which.min(y_dist_upper)

  y_dist_bottom <- abs(nc_y_coord_vals - user_coords$y[2])
  y_index_bottom <- which.min(y_dist_bottom)

  # number of x and y values to get. We add 1 to get the complete length
  x_index_length <- (x_index_bottom - x_index_upper) + 1
  y_index_length <- (y_index_upper - y_index_bottom) + 1

  # stop if the grid is too big
  if (x_index_length * y_index_length > 2500) {
    nc_close(nc)
    return('proj_grid_too_large')
  }

  # get the var names
  var_names <- names(nc$var)
  # empty list for arrays for each var
  res_list <- vector('list', length(var_names))

  # loop to retrieve each var data in an array
  for (i in 1:length(var_names)) {

    # progress
    if (is.function(updateProgress)) {
      updateProgress(
        detail = paste0('Processing variable ', i, ' of ',
                        length(var_names), ' (', var_names[i], ')'),
        n_coords = length(var_names)
      )
    }

    res_list[[i]] <- ncvar_get(nc, var_names[i],
                               start = c(x_index_upper, y_index_bottom, 1),
                               count = c(x_index_length, y_index_length, -1))
  }
  # name the list with the var names
  names(res_list) <- var_names

  # STEP 4 CREATE ALSO THE SPATIAL POINTS TO DRAW THE GRID IN THE VISUALIZATION

  # also, as here we have the indexes, we get the points to generate the grid
  x = ncvar_get(nc, 'X', x_index_upper, x_index_length)
  y = ncvar_get(nc, 'Y', y_index_bottom, y_index_length)

  points_sel <- SpatialPoints(
    expand.grid(list(x = x, y = y))
  )

  # create a list with the points and the data and return it
  res <- list(points_sel = points_sel,
              res_list = res_list,
              x_vals = x,
              y_vals = y)

  # close nc
  nc_close(nc)

  return(res)
}

################################################################################
# Historical Grid mode
historical_grid_mode_process <- function(user_coords, user_dates,
                                         updateProgress = NULL) {

  # STEP 1 CONVERT TO UTM THE USER COORDS

  # convert to utm
  if (is.function(updateProgress)) {
    updateProgress(
      detail = 'Converting user coordinates',
      value = 0.1
    )
  }

  # make a coordinates object from the data frame provided
  coordinates(user_coords) <- ~x+y

  # add the projection string attribute
  proj4string(user_coords) <- CRS("+proj=longlat +datum=WGS84")

  # transform the coordinates projection to the correct projection
  user_coords <- as.data.frame(spTransform(
    user_coords,
    CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +towgs84=0,0,0")
  ))

  # STEP 2 OPEN THE CONN TO THE netCDF FILE

  # for this we need first to extract the years to know which netCDF files we
  # have to open
  dates <- seq(as.Date(user_dates[1]), as.Date(user_dates[2]), by = 'day')
  years <- as.character(unique(lubridate::year(dates)))

  # stop if the timespan is too long
  if (length(dates) > 1828) {
    return('hist_time_span_too_large')
  }

  # res list for years
  years_list <- vector('list', length(years))
  names(years_list) <- years

  # loop for years
  for (year in years) {

    # progress
    if (is.function(updateProgress)) {
      updateProgress(
        detail = paste0('Processing dates corresponding to ', year),
        n_coords = length(years),
        max_val = 0.75
      )
    }

    # nc file name
    file_name <- file.path('/home', 'miquel', 'Datasets', 'Climate', 'Products',
                           'Pixels1k', 'Historical', 'netCDF',
                           paste0(year, '_historical_netCDF.nc'))
    nc <- nc_open(file_name)

    # STEP 3 SUBSET THE netCDF DATA

    # extract X and Y values from netCDF file
    nc_x_coord_vals <- nc$dim$X$vals
    nc_y_coord_vals <- nc$dim$Y$vals

    # time values
    historical_days <- seq(as.Date(paste0(year, '-01-01')),
                           as.Date(paste0(year, '-12-31')),
                           by = 'day')
    # which days provided by the user are in looped year
    user_days <- stringr::str_subset(dates, year)

    # index for X
    x_dist_upper <- abs(nc_x_coord_vals - user_coords$x[1])
    x_index_upper <- which.min(x_dist_upper)

    x_dist_bottom <- abs(nc_x_coord_vals - user_coords$x[2])
    x_index_bottom <- which.min(x_dist_bottom)

    # index for Y
    y_dist_upper <- abs(nc_y_coord_vals - user_coords$y[1])
    y_index_upper <- which.min(y_dist_upper)

    y_dist_bottom <- abs(nc_y_coord_vals - user_coords$y[2])
    y_index_bottom <- which.min(y_dist_bottom)

    # index for T
    t_index_upper <- which(as.character(historical_days) %in% tail(user_days, 1))
    t_index_bottom <- which(as.character(historical_days) %in% head(user_days, 1))

    # number of x, y and t values to get (we add 1 to get the full length)
    x_index_length <- (x_index_bottom - x_index_upper) + 1
    y_index_length <- (y_index_upper - y_index_bottom) + 1
    t_index_length <- (t_index_upper - t_index_bottom) + 1

    # stop if the grid is too big
    if (x_index_length * y_index_length > 2500) {
      nc_close(nc)
      return('hist_grid_too_large')
    }

    # get the var names
    var_names <- names(nc$var)
    # empty list for arrays for each var
    res_list <- vector('list', length(var_names))
    names(res_list) <- var_names

    # loop to retrieve each var data in an array
    for (i in 1:length(var_names)) {

      res_list[[i]] <- ncvar_get(nc, var_names[i],
                                 start = c(x_index_upper, y_index_bottom, t_index_bottom),
                                 count = c(x_index_length, y_index_length, t_index_length))
    }

    years_list[[year]] <- res_list

    # also, as here we have the indexes, we get the points to generate the grid
    # here this should be outside of the loop, but in  this way we are able to
    # close the nc file in order to avoid errors
    x = ncvar_get(nc, 'X', x_index_upper, x_index_length)
    y = ncvar_get(nc, 'Y', y_index_bottom, y_index_length)

    # close nc
    nc_close(nc)

  }

  # Now we need to create the final result by binding the arrays for each year
  # for each variable by means of the abind package
  result <- vector('list', length(years_list[[1]]))
  names(result) <- names(years_list[[1]])

  for (var in names(years_list[[1]])) {

    # progress
    if (is.function(updateProgress)) {
      updateProgress(
        detail = paste0('Retrieving ', var, ' data'),
        n_coords = length(names(years_list[[1]])),
        max_val = 1
      )
    }

    tmp_res <- array(dim = c(length(x), length(y), 1))

    for (year in names(years_list)) {
      tmp_res <- abind::abind(tmp_res, years_list[[year]][[var]], along = 3)
    }
    result[[var]] <- tmp_res[,,-1]
  }

  # grid to return in the res
  points_sel <- SpatialPoints(
    expand.grid(list(x = x, y = y))
  )

  # create a list with the points and the data and return it
  res <- list(points_sel = points_sel,
              res_list = result,
              x_vals = x,
              y_vals = y)

  return(res)

}

################################################################################
# Interpolated data wrapper function.
#
# Here is where the if's must reside, one function that only check one for the
# mode and return the interpolated data fot that mode. This makes the server.R
# file code more clean and it's better for maintenance.

one_ring_to_dominate_all <- function(input, user_coords,
                                     updateProgress = NULL,
                                     excludeRainFromStations = character(0)) {

  # In case of points mode
  if (input$point_grid_sel == 'Points') {

    # in case of current
    if (input$mode_sel == 'Current') {

      # check for requirements, no need to compute nothing if not all inputs are
      # present
      req(user_coords$df[1,1], user_coords$df[1,2],
          input$date_range_current[1], input$date_range_current[1])

      # interpolated data
      interpolated_data <- current_points_mode_process(
        user_df = user_coords$df,
        user_dates = input$date_range_current,
        updateProgress = updateProgress
      )

      # return
      return(interpolated_data)
    }

    # in case of historical
    if (input$mode_sel == 'Historical') {

      # check for requirements (inputs)
      req(user_coords$df[1,1], user_coords$df[1,2],
          input$date_range_historical[1], input$date_range_historical[2])

      # interpolated data
      interpolated_data <- historical_points_mode_process(
        user_df = user_coords$df,
        user_dates = input$date_range_historical,
        updateProgress = updateProgress
      )

      # return
      return(interpolated_data)
    }

    # in case of projection
    if (input$mode_sel == 'Projection') {

      # check for requirements (inputs)
      req(user_coords$df[1,1], user_coords$df[1,2],
          input$rcm, input$rcp)

      # interpolated data
      interpolated_data <- projection_points_mode_process(
        user_df = user_coords$df,
        rcm = input$rcm,
        rcp = input$rcp,
        updateProgress = updateProgress
      )

      # return
      return(interpolated_data)
    }
  }

  # in case of grid
  if (input$point_grid_sel == 'Grid') {

    # in case of current
    if (input$mode_sel == 'Current') {

      # check for requirements (inputs)
      req(input$longitude, input$longitude_bottom, input$latitude,
          input$latitude_bottom, input$date_range_current)

      # user_coords
      grid_coords <- data.frame(
        x = c(input$longitude, input$longitude_bottom),
        y = c(input$latitude, input$latitude_bottom)
      )

      # interpolated_data
      interpolated_data <- current_grid_mode_process(
        user_coords = grid_coords,
        user_dates = input$date_range_current,
        updateProgress = updateProgress
      )

      # return
      return(interpolated_data)
    }

    # in case of projection
    if (input$mode_sel == 'Projection') {

      # check for requirements
      req(input$longitude, input$longitude_bottom, input$latitude,
          input$latitude_bottom, input$rcm, input$rcp)

      # user_coords
      grid_coords <- data.frame(
        x = c(input$longitude, input$longitude_bottom),
        y = c(input$latitude, input$latitude_bottom)
      )

      # interpolated data
      interpolated_data <- projection_grid_mode_process(
        user_coords = grid_coords,
        rcm = input$rcm,
        rcp = input$rcp,
        updateProgress
      )

      # return
      return(interpolated_data)
    }

    # in case of historical
    if (input$mode_sel == 'Historical') {

      # check for requirements (inputs)
      req(input$longitude, input$longitude_bottom, input$latitude,
          input$latitude_bottom,
          input$date_range_historical[1], input$date_range_historical[2])

      # user_coords
      grid_coords <- data.frame(
        x = c(input$longitude, input$longitude_bottom),
        y = c(input$latitude, input$latitude_bottom)
      )

      # interpolated_data
      interpolated_data <- historical_grid_mode_process(
        user_coords = grid_coords,
        user_dates = input$date_range_historical,
        updateProgress
      )

      # return
      return(interpolated_data)
    }
  }

}

################################################################################
# Download button functions. This functions check for the mode selected by the
# user and generate the data file and filename accordingly.

filename_function <- function(input, data) {

  # points modes
  if (input$mode_sel %in% c('Current', 'Historical', 'Projection') & input$point_grid_sel == 'Points') {

    # check if there is one or more coordinates provided by the user:
    if (length(data@data) > 1) {
      # if more than one, file will be a zip
      return('meteoland_output.zip')
    } else {
      # if only one, file will be a txt
      return('meteoland_output.txt')
    }
  }

  # grid modes
  if (input$mode_sel %in% c('Current') & input$point_grid_sel == 'Grid') {

    # check if there is more than one date
    if (length(data@data) > 1) {
      return('meteoland_output.zip')
    } else {
      # if only one date
      return('meteoland_output.nc')
    }
  }

  if (input$mode_sel %in% c('Projection', 'Historical') & input$point_grid_sel == 'Grid') {
    return('meteoland_output.nc')
  }
}

content_function <- function(input, data, file) {

  # points mode
  if (input$mode_sel %in% c('Current', 'Historical', 'Projection') & input$point_grid_sel == 'Points') {

    # check if there is one or more coordinates provided by the user:
    if (length(data@data) > 1) {

      # if more than one, we must establish a temporal directory, create the
      # different txt files, compress them and return the file
      temporal_dir <- tempdir()
      setwd(tempdir())
      files_to_compress <- c()
      for (i in 1:length(data@data)) {
        tmp_file <- paste0('meteoland_output_', i, '.txt')
        files_to_compress <- c(files_to_compress, tmp_file)
        writemeteorologypoint(data@data[[i]], tmp_file)
      }

      # create the zip
      zip(file, files_to_compress)
    } else {
      # if only one, write it directly
      writemeteorologypoint(data@data[[1]], file)
    }
  }

  # grid modes
  if (input$mode_sel %in% c('Current') & input$point_grid_sel == 'Grid') {

    # check if more than one date
    if (length(data@dates) > 1) {

      # if more than one date, we create a zip with nc files for each day
      temporal_dir <- tempdir()
      setwd(tempdir())
      writemeteorologygridfiles(
        object = data,
        metadatafile = 'metadata_grid.txt'
      )

      # create the zip
      zip(file, dir(pattern = '*.nc$'))
    } else {
      writemeteorologygrid(data, data@dates[[1]], file)
    }

  }

  if (input$mode_sel %in% c('Projection') & input$point_grid_sel == 'Grid') {

    # create the nc file
    dimX <- ncdim_def('X', 'meters', data$x_vals)
    dimY <- ncdim_def('Y', 'meters', data$y_vals)
    dimT <- ncdim_def("Time", "months", 1:1140)

    Precipitation <- ncvar_def('Precipitation', 'mm',
                               list(dimX, dimY, dimT), NA)
    MeanTemperature <- ncvar_def('MeanTemperature', 'Celsius degrees',
                                 list(dimX, dimY, dimT), NA)
    MaxTemperature <- ncvar_def('MaxTemperature', 'Celsius degrees',
                                list(dimX, dimY, dimT), NA)
    MinTemperature <- ncvar_def('MinTemperature', 'Celsius degrees',
                                list(dimX, dimY, dimT), NA)
    MeanRelativeHumidity <- ncvar_def('MeanRelativeHumidity', 'Percentage',
                                      list(dimX, dimY, dimT), NA)
    PET <- ncvar_def('PET', 'mm', list(dimX, dimY, dimT), NA)

    nc <- nc_create(
      file,
      list(Precipitation, MeanTemperature,
           MaxTemperature, MinTemperature,
           MeanRelativeHumidity, PET)
    )

    for (var in names(data$res_list)) {
      ncvar_put(
        nc, var,
        data$res_list[[var]]
      )
    }

    nc_close(nc)

  }

  if (input$mode_sel %in% c('Historical') & input$point_grid_sel == 'Grid') {

    # create the nc file
    # dimension variables
    dimX <- ncdim_def('X', 'meters', data$x_vals)
    dimY <- ncdim_def('Y', 'meters', data$y_vals)
    dimT <- ncdim_def('Time', 'days',
                      1:length(seq(as.Date(input$date_range_historical[1]),
                                   as.Date(input$date_range_historical[2]),
                                   by = 'day')))

    # variables
    MeanTemperature <- ncvar_def('MeanTemperature', 'Celsius degrees',
                                 list(dimX, dimY, dimT), NA)
    MaxTemperature <- ncvar_def('MaxTemperature', 'Celsius degrees',
                                list(dimX, dimY, dimT), NA)
    MinTemperature <- ncvar_def('MinTemperature', 'Celsius degrees',
                                list(dimX, dimY, dimT), NA)
    Precipitation <- ncvar_def('Precipitation', 'mm',
                               list(dimX, dimY, dimT), NA)
    MeanRelativeHumidity <- ncvar_def('MeanRelativeHumidity', 'Percentage',
                                      list(dimX, dimY, dimT), NA)
    MaxRelativeHumidity <- ncvar_def('MaxRelativeHumidity', 'Percentage',
                                     list(dimX, dimY, dimT), NA)
    MinRelativeHumidity <- ncvar_def('MinRelativeHumidity', 'Percentage',
                                     list(dimX, dimY, dimT), NA)
    Radiation <- ncvar_def('Radiation', '',
                           list(dimX, dimY, dimT), NA)
    WindSpeed <- ncvar_def('WindSpeed', 'meters per second',
                           list(dimX, dimY, dimT), NA)
    WindDirection <- ncvar_def('WindDirection', '',
                               list(dimX, dimY, dimT), NA)
    PET <- ncvar_def('PET', 'mm',
                     list(dimX, dimY, dimT), NA)

    nc <- nc_create(
      file,
      list(MeanTemperature, MaxTemperature, MinTemperature,
           Precipitation, MeanRelativeHumidity, MaxRelativeHumidity,
           MinRelativeHumidity, Radiation, WindSpeed,
           WindDirection, PET)
    )

    # fill the nc file
    for (var in names(data$res_list)) {
      ncvar_put(
        nc, var,
        data$res_list[[var]]
      )
    }

    # close the nc file
    nc_close(nc)

  }
}

################################################################################
#### QA needed data ####
qa_years <- 1976:2016
qa_sum <- vector('list', length(qa_years))
qa_list <- vector('list', length(qa_years))

for (i in 1:length(qa_years)) {
  qa_list[[i]] <- readRDS(
    file.path('/home', 'miquel', 'Datasets', 'Climate', 'Products',
              'MeteorologyInterpolationData', 'CrossValidations',
              paste0('CV_', qa_years[[i]], '.rds'))
    # paste0('/run/user/1000/gvfs/smb-share:server=serverprocess,share=miquel/Datasets/Climate/Products/MeteorologyInterpolationData/CrossValidations/',
    #        paste0('CV_', qa_years[[i]], '.rds'))
  )

  qa_sum[[i]] <- summary(qa_list[[i]])
}

qa_vars <- row.names(qa_sum[[1]])
qa_statistics <- names(qa_sum[[1]])
