
#brett_depth_plot_good_label(Depths_234811, "Etc/GMT+10", "Tag ID 234811",x_label = "Date/time UTC+10")#example of function.

brett_depth_plot_good_label_yaxis <- function(depth_df, Popoff_datetime, 
                                              Ptt = "Moby the mobula",
                                              ylims = NULL,
                                              x_label = "Time"){
  
  # Let's get the sun times of day. 
  
  # Reformat into R dates, get rid of NA/duplicates
  dates <- as.Date(format(depth_df$Popoff_datetime, "%Y-%m-%d"))
  dates <- na.omit(dates)
  dates <- dates[!duplicated(dates)]
  
  #number of days
  ndays <- length(dates)
  
  # Get the sun times of day
  sun <- getSunlightTimes(date  = dates,
                          lat = unique(depth_df$Popoff_lat),
                          lon = unique(depth_df$Popoff_long),
                          tz = Popoff_datetime,
                          keep = c("nightEnd","sunrise", "sunset", "night"))
  
  # Y limits
  max_depth <- max(depth_df$Depth)
  ymin = -Inf
  ymax = Inf
  
  # Make our tibbles of times covering night, dusk, and dawn.
  nighttimes <- tibble(x1 = sun$night[1:(nrow(sun) - 1)],  
                       x2 = sun$nightEnd[2:(nrow(sun))],  ymin, ymax)
  
  dawn_times <- tibble(x1 = sun$nightEnd,  
                       x2 = sun$sunrise,  ymin, ymax)
  
  dusk_times <- tibble(x1 = sun$sunset,  
                       x2 = sun$night,  ymin, ymax)
  
  # adjust the tibbles if we need/don't need certain time blocks
  # if the first datapoint is before nightEnd, then we should add a row
  # to the nighttimes
  if(min(depth_df$Popoff_datetime, na.rm = T) < sun$nightEnd[1]){
    row_to_add <- data.frame(x1 = min(depth_df$Popoff_datetime, na.rm = T), 
                             x2 = sun$nightEnd[1],
                             ymin = ymin, ymax = ymax)
    
    # bind together
    nighttimes <- rbind(row_to_add, nighttimes)
  }
  
  # if the first datapoint is after sunrise, then we should remove a row
  # dawntimes
  if(min(depth_df$Popoff_datetime, na.rm = T) > sun$sunrise[1]){
    dawn_times <- dawn_times[-1, ]
  }
  
  # if the last datapoint is before sunset, then we should remove a row
  # dusktimes
  if(max(depth_df$Popoff_datetime, na.rm = T) < sun$sunset[nrow(sun)]){
    dusk_times <- dusk_times[-nrow(dusk_times), ]
  }
  
  # if the last datapoint is after night starts, we should add a night row
  if(max(depth_df$Popoff_datetime, na.rm = T) > sun$sunset[nrow(sun)]){
    row_to_add <- data.frame(x1 = sun$night[nrow(sun)], 
                             x2 = max(depth_df$Popoff_datetime, na.rm = T),
                             ymin = ymin, ymax = ymax)
    
    nighttimes <- rbind(nighttimes, row_to_add)
  }
  
  # this is if we want to make the background rectangles more see through
  night_opacity = 1
  
  # get midday times for the dates
  midday_time <- lubridate::force_tz(dates, Popoff_datetime) + 60*60*12
  
  # run the actual plot
  ggplot() +
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
    geom_line(data = depth_df, aes(x = Popoff_datetime, y = Depth), size=1, color = "blue") +
    scale_y_reverse(limits = ylims) + 
    scale_x_datetime(date_breaks = "6 hours", date_labels = "%H") +
    theme_bw() +
    xlab("Date (Tag Popoff TZ)") + 
    annotate(geom = "text", x = midday_time, y = ylims[1], vjust = 5.5, label = dates, size = 3) +
    #annotate(geom = "text", x = mean(depth_df$Popoff_datetime, na.rm = T), y = max_depth * 1.66, label = x_label, size = 4.5) +
    #annotate(geom = "text", x = midday_time, y = 0, vjust = 30, label = dates, size = 3) + #need to figure out how to get this to show up below chart
    annotate(geom = "text", x = mean(depth_df$Popoff_datetime, na.rm = T), y = ylims[1], vjust = 5, label = x_label, size = 4.5) +
    ylab("Depth (m)") +
    ggtitle(paste0("", Ptt)) +
    #coord_cartesian(ylim = c(max_depth*1.05, -max_depth*.05), expand = FALSE, clip = "off") + #if remove this line then can use function to set max depth.
    #coord_cartesian(ylim = c(400*1.05, -400*0.05),expand = FALSE, clip = "off") +
    coord_cartesian(ylim = c(ylims[1]*1.05, -ylims[1]*.05), expand = FALSE, clip = "off") + 
    #coord_cartesian(expand = FALSE, clip = "off") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          plot.margin = unit(c(2, 1, b = 4, 1), "lines"),
          axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
}
#axis.title.x = element_text(vjust = -5),
