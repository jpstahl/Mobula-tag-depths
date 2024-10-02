
# if you want to set ylims, use c(depthmax, 0), where depthmax is the deepest 
# depth you want displayed.

#Boxplot function for summarizing tag depth data by hour over multiple days.

brett_box_plot <- function(depths_df, Popoff_datetime, 
                           chart_title = "234820 Box",
                           ylims = NULL,
                           x_label = "Hours",
                           lat_to_use = NULL,
                           lon_to_use = NULL){
  
  
  # Let's get the sun times of day. 
  
  # Reformat into R dates, get rid of NA/duplicates
  dates <- as.Date(format(depths_df$Popoff_datetime, "%Y-%m-%d"))
  dates <- na.omit(dates)
  dates <- dates[!duplicated(dates)]
  
  median_date <- as.Date(format(median(depths_df$Popoff_datetime),  "%Y-%m-%d"))
  
  # get the hours of the times for the tag
  hours <- format(depths_df$Popoff_datetime, "%H")
  
  # make sure we have no NA
  if(any(is.na(hours)))
    stop("There are NAs in the Popoff_datetime field.")
  
  # add it to the DF
  depths_df$hours <- hours
  
  # if lat/lon not specified, get them from depths_df
  if(is.null(lat_to_use))
    lat_to_use <- unique(depths_df$Popoff_lat)
  if(is.null(lon_to_use))
    lon_to_use <- unique(depths_df$Popoff_long)
  
  # if still null or NA, then stop
  if(!is.numeric(lon_to_use) & !is.numeric(lat_to_use))
    stop("Please provide a lat/lon for sunlight times.")
  
  # Get the sun times of day
  sun <- getSunlightTimes(date  = median_date,
                          lat = lat_to_use,
                          lon = lon_to_use,
                          tz = Popoff_datetime,
                          keep = c("nightEnd","sunrise", "sunset", "night"))
  
  # only keep the times
  sun <- sun[, c("nightEnd","sunrise", "sunset", "night")]
  
  # convert to times
  sun_times <- apply(sun, 1, function(x){return(as.numeric(strftime(x, format = '%H', tz = 'UTC')) * 1  +
                                                  as.numeric(strftime(x, format = '%M', tz = 'UTC'))/60 + 
                                                  as.numeric(strftime(x, format = '%H', tz = 'UTC'))/3600)})
  
  # give it names
  sun_times = t(tibble(sun_times))
  colnames(sun_times) <- c("nightEnd","sunrise", "sunset", "night")
  
  # Y limits for shadow boxes of sun timing
  max_depth <- max(depths_df$Depth)
  ymin = -Inf
  ymax = Inf
  
  nighttimes <- rbind(tibble(x1 = 0, x2 = sun_times[1],  ymin, ymax),
                      tibble(x1 = sun_times[4], x2 = Inf, ymin, ymax))
  
  # Make our tibbles of times covering night, dusk, and dawn.
  # nighttimes <- tibble(x1 = sun$night[1:(nrow(sun) - 1)],  
  #                      x2 = sun$nightEnd[2:(nrow(sun))],  ymin, ymax)
  # 
  dawn_times <- tibble(x1 = sun_times[1],  
                       x2 = sun_times[2],  ymin, ymax)
  
  dusk_times <- tibble(x1 = sun_times[3],  
                       x2 = sun_times[4],  ymin, ymax)
  
  
  # this is if we want to make the background rectangles more see through
  night_opacity = 1 #I added this from the line plot code.
  
  depth_box_pl <- ggplot() + 
    geom_rect(data = nighttimes, aes(xmin = x1, 
                                     xmax = x2, 
                                     ymin = ymin, 
                                     ymax = ymax,
                                     fill = "Night"), alpha = night_opacity) +
    geom_rect(data = dawn_times, aes(xmin = x1, 
                                     xmax = x2, 
                                     ymin = ymin, 
                                     ymax = ymax,
                                     fill = "Dawn"), alpha = night_opacity) +
    geom_rect(data = dusk_times, aes(xmin = x1, 
                                     xmax = x2, 
                                     ymin = ymin, 
                                     ymax = ymax,
                                     fill = "Dusk"), alpha = night_opacity) +
    scale_fill_manual(' ',
                      values = c('grey90', 'grey90', 'grey'),  
                      guide = guide_legend(override.aes = list(alpha = night_opacity))) + 
    geom_boxplot(data = depths_df, aes(x=hours, y=Depth)) + 
    scale_y_reverse(limits = ylims) + 
    xlab(paste(x_label)) + 
    labs(y = "Depth (m)") +
    ggtitle(paste(chart_title, "-", range(dates)[1],
                  ":", range(dates)[2])) +
    theme(
      panel.background = element_rect(fill='#FAFAFA'),
      plot.background = element_rect(fill='#FAFAFA', color=NA),
      panel.grid.minor = element_line(),
      panel.border = element_rect(color = "black", fill = NA),
      axis.title.y = element_text(margin = margin(r = 5))
    )
  
  return(depth_box_pl)
}

