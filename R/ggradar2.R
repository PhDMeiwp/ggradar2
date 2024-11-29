#' ggradar2
#' 
#' This is ggradar2.r function from https://github.com/pedrosdr/ggradar/tree/ggradar2, with some minor adjustments made.
#'
#' @param plot.data dataframe comprising one row per group
#' @param base.size text size
#' @param font.radar text font family
#' @param values.radar values to print at 6 intervals for gridlines
#' @param label.gridlines.show whether labels of grids should be shown
#' @param axis.labels  names of axis labels if other than column names supplied via plot.data
#' @param centre.y value of y at centre of plot
#' @param plot.extent.x.sf controls relative size of plot horizontally
#' @param plot.extent.y.sf controls relative size of plot vertically
#' @param x.centre.range controls axis label alignment
#' @param label.centre.y whether value of y at centre of plot should be labelled, default is FALSE
#' @param grid.line.width width of gridline
#' @param gridline.min.linetype line type of minimum gridline, default is "longdash"
#' @param gridline.n2.linetype line type of No.2 gridline, default is "longdash"
#' @param gridline.n3.linetype line type of No.3 gridline, default is "longdash"
#' @param gridline.n4.linetype line type of No.4 gridline, default is "longdash"
#' @param gridline.n5.linetype line type of No.5 gridline , default is "longdash"
#' @param gridline.max.linetype line type of maximum gridline, default is "longdash"
#' @param gridlines.show whether gridlines should be shown
#' @param gridline.min.colour colour of minimum gridline, default is "grey"
#' @param gridline.n2.colour colour of No.2 gridline, default is "grey"
#' @param gridline.n3.colour colour of No.3 gridline, default is "grey"
#' @param gridline.n4.colour colour of No.4 gridline, default is "grey"
#' @param gridline.n5.colour colour of No.5 gridline, default is "grey" 
#' @param gridline.max.colour colour of maximum gridline, default is "grey"
#' @param grid.label.size text size of gridline label, default is 4.5
#' @param grid.label.color text colour of gridline label, default is "grey30"
#' @param gridline.label.offset.x displacement to left/right of central vertical axis
#' @param gridline.label.offset.y displacement to top/bottom of central vertical axis
#' @param axis.label.offset vertical displacement of axis labels from maximum grid line, measured relative to circle diameter, default is 1.15
#' @param axis.label.size text size of axis label, default is 5
#' @param axis.line.colour colour of axis line, default is "grey"
#' @param group.line.width line width of group, default is 1
#' @param group.point.size point size of group, default is 2
#' @param group.colours colour of group, default is NULL
#' @param background.circle.colour colour of background circle/radar, default is "grey90"
#' @param background.circle.transparency transparency of background circle/radar, default is 0.2
#' @param plot.legend whether to include a plot legend
#' @param legend.title title of legend
#' @param plot.title title of radar plot
#' @param legend.text.size text size in legend, default is 14
#' @param legend.position position of legend, valid values are "top", "right", "bottom", "left"
#' @param fill whether to fill polygons, TRUE or FALSE? Default is FALSE
#' @param fill.alpha if filling polygons, transparency values, default is 0.3
#'
#' @import ggplot2
#' @return a ggplot object
#'
#' @name ggradar2
#'
#' @export
#'
#' @examples
#' 
#' library(ggradar2)
#'
#' # Example 1 (one group):
#' df <- data.frame(
#'       NAME = 'G1',
#'       A = 0.4,
#'       B = 0.8,
#'       C = 0.5,
#'       D = 0.8,
#'       E = 0.5
#'       )
#' ggradar2(df)
#'
#' # Example 2 (grid line setting):
#' df <- data.frame(
#'         NAME = 'G1',
#'         A = 123,
#'         B = 65,
#'         C = 157,
#'         D = 181,
#'         E = 99
#'         )
#' ggradar2(df,
#'         # radar values
#'         values.radar = c("0", "50", "100", "150", "", "200"),
#'         # grid lines
#'         gridline.n2.colour = "blue",
#'         gridline.n4.colour = "blue",
#'         # grid labels
#'         gridline.label.offset.y = 0,
#'         # axis labels
#'         axis.label.offset = 1.2
#'         )
#'
#' # Example 3 (multiple groups):
#' df <- data.frame(
#'         NAME = c('G1', 'G2', 'G3'),
#'         A = c(0.2, 0.4, 0.5),
#'         B = c(0.3, 0.6, 0.3),
#'         C = c(0.7, 0.3, 0.5),
#'         D = c(0.4, 0.8, 0.2),
#'         E = c(0.5, 0.6, 0.3)
#'         )
#' ggradar2(df)
#'
#' # Example 4 (fill setting):
#' df <- data.frame(
#'         NAME = c('G1', 'G2', 'G3'),
#'         A = c(2, 4, 5),
#'         B = c(3, 6, 3),
#'         C = c(7, 3, 5),
#'         D = c(4, 8, 2),
#'         E = c(5, 6, 3)
#'         )
#' ggradar2(df,
#'         values.radar = c("0", "2", "4", "6", "8", "10"),
#'         grid.line.width = 0.3,
#'         # group parameters
#'         group.line.width = 0.5,
#'         group.point.size = 2,
#'         group.colours = c("red","orange","green"),
#'         fill = TRUE,
#'         fill.alpha = 0.2,
#'         axis.label.offset = 1.15,
#'         axis.label.size = 4,
#'         background.circle.colour = "white"
#'         )
#'
#' # Example 5 (default ggradar plot): 
#' df <- data.frame(
#'         NAME = 'G1',
#'         A = 0.4,
#'         B = 0.8,
#'         C = 0.5,
#'         D = 0.8,
#'         E = 0.5
#'        )
#' ggradar2(df,
#'         # radar values
#'         values.radar = c("0%", "50%", "", "", "", "100%"),
#'         # grid labels
#'         grid.label.color = "#000000",
#'         grid.label.size = 6,
#'         gridline.label.offset.x = -0.1,
#'         gridline.n2.colour = "#007A87",
#'         # group
#'         group.point.size = 6
#'         )
#'
#' #End Examples


ggradar2 <- function(plot.data,
                    base.size = 15,
                    font.radar = "sans",
                    values.radar = c("0%", "20%", "40%", "60%", "80%", "100%"),
                    label.gridlines.show = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),
                    axis.labels = colnames(plot.data)[-1],
                    centre.y = grid.min - ((1 / 9) * (grid.max - grid.min)),
                    plot.extent.x.sf = 1,
                    plot.extent.y.sf = 1.2,
                    x.centre.range = 0.02 * (grid.max - centre.y),
                    label.centre.y = FALSE,
                    grid.line.width = 0.5,
                    gridline.min.linetype = "longdash",
                    gridline.n2.linetype = "longdash",
                    gridline.n3.linetype = "longdash",
                    gridline.n4.linetype = "longdash",
                    gridline.n5.linetype = "longdash",
                    gridline.max.linetype = "longdash",
                    gridlines.show = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE),                    
                    gridline.min.colour = "grey",
                    gridline.n2.colour = "grey",
                    gridline.n3.colour = "grey",
                    gridline.n4.colour = "grey",
                    gridline.n5.colour = "grey",
                    gridline.max.colour = "grey",
                    grid.label.size = 4.5,
                    grid.label.color = "#555555",
                    gridline.label.offset.x = -0.05 * (grid.max - centre.y),
                    gridline.label.offset.y = -0.5,
                    axis.label.offset = 1.15,
                    axis.label.size = 5,
                    axis.line.colour = "grey",
                    group.line.width = 1.5,
                    group.point.size = 3,
                    group.colours = NULL,
                    background.circle.colour = "#D7D6D1",
                    background.circle.transparency = 0.2,
                    plot.legend = if (nrow(plot.data) > 1) TRUE else FALSE,
                    legend.title = "",
                    plot.title = "",
                    legend.text.size = 14,
                    legend.position = "bottom",
                    fill = FALSE,
                    fill.alpha = 0.3) {
  
  # Functions
  #
  convert_to_numeric <- function(value) {
    if (is.character(value)) {
      if (grepl("%", value)) {
        return(as.numeric(gsub("%", "", value)) / 100)
      } else {
        if (value == "") {
          return(0)
        } else {
          return(as.numeric(value))
        }
      }
    } else {
      if (is.na(value)) {
        return(0)
      } else {
        return(as.numeric(value))
      }
    }
  }
  grid.min = convert_to_numeric(values.radar[1]) # 
  grid.n2 = convert_to_numeric(values.radar[2]) # 
  grid.n3 = convert_to_numeric(values.radar[3]) # 
  grid.n4 = convert_to_numeric(values.radar[4]) # 
  grid.n5 = convert_to_numeric(values.radar[5]) # 
  grid.max = convert_to_numeric(values.radar[6]) # 
  
  #
  CalculateAxisPath <- function(var.names, min, max) {
    # var.names <- c("v1","v2","v3","v4","v5")
    n.vars <- length(var.names) # number of vars (axes) required
    # Cacluate required number of angles (in radians)
    angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
    # calculate vectors of min and max x+y coords
    min.x <- min * sin(angles)
    min.y <- min * cos(angles)
    max.x <- max * sin(angles)
    max.y <- max * cos(angles)
    # Combine into a set of uniquely numbered paths (one per variable)
    axisData <- NULL
    for (i in 1:n.vars) {
      a <- c(i, min.x[i], min.y[i])
      b <- c(i, max.x[i], max.y[i])
      axisData <- rbind(axisData, a, b)
    }
    # Add column names + set row names = row no. to allow conversion into a data frame
    colnames(axisData) <- c("axis.no", "x", "y")
    rownames(axisData) <- seq(1:nrow(axisData))
    # Return calculated axis paths
    as.data.frame(axisData)
  }
  
  CalculateGroupPath <- function(df) {
    # Drop dead levels. This might happen if the data is filtered on the way
    # into ggradar.
    path <- forcats::fct_drop(df[, 1])
    # set the name of the variable that will be used for grouping
    theGroupName <- colnames(df)[1]
    
    ## find increment
    nPathPoints <- ncol(df) - 1
    angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / nPathPoints)
    ## create graph data frame
    nDataPoints <- ncol(df) * length(levels(path))
    graphData <- data.frame(
      seg = rep("",nDataPoints),
      x = rep(0, nDataPoints),
      y = rep(0, nDataPoints))
    colnames(graphData)[1] <- theGroupName
    
    rowNum <- 1
    for (i in 1:length(levels(path))) {
      pathData <- subset(df, df[, 1] == levels(path)[i])
      for (j in c(2:ncol(df))) {
        graphData[rowNum,theGroupName] <- levels(path)[i]
        graphData$x[rowNum] <- pathData[, j] * sin(angles[j - 1])
        graphData$y[rowNum] <- pathData[, j] * cos(angles[j - 1])
        rowNum <- rowNum + 1
      }
      ## complete the path by repeating first pair of coords in the path
      graphData[rowNum,theGroupName] <- levels(path)[i]
      graphData$x[rowNum] <- pathData[, 2] * sin(angles[1])
      graphData$y[rowNum] <- pathData[, 2] * cos(angles[1])
      rowNum <- rowNum + 1
    }
    # Make sure that name of first column matches that of input data (in case !="group")
    graphData[,1] <- factor(graphData[,1], levels=levels(path) ) # keep group order
    graphData # data frame returned by function
  }
  
  funcCircleCoords <- function(center = c(0, 0), r = 1, npoints = 100) {
    tt <- seq(0, 2 * pi, length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  #
  #
  # End Functions
  
  plot.data <- as.data.frame(plot.data)

  if(!is.factor(plot.data[, 1])) {
    plot.data[, 1] <- as.factor(as.character(plot.data[, 1]))
  }

  var.names <- colnames(plot.data)[-1] # Short version of variable names
  # axis.labels [if supplied] is designed to hold 'long version' of variable names
  # with line-breaks indicated using \n

  # calculate total plot extent as radius of outer circle x a user-specifiable scaling factor
  plot.extent.x <- (grid.max + abs(centre.y)) * plot.extent.x.sf
  plot.extent.y <- (grid.max + abs(centre.y)) * plot.extent.y.sf

  # Check supplied data makes sense
  if (length(axis.labels) != ncol(plot.data) - 1) {
    stop("'axis.labels' contains the wrong number of axis labels", call. = FALSE)
  }
  if (min(plot.data[, -1]) < centre.y) {
    stop("plot.data' contains value(s) < centre.y", call. = FALSE)
  }
  if (max(plot.data[, -1]) > grid.max) {
    stop("'plot.data' contains value(s) > grid.max", call. = FALSE)
  }

  ### Convert supplied data into plottable format
  # (a) add abs(centre.y) to supplied plot data
  # [creates plot centroid of 0,0 for internal use, regardless of min. value of y
  # in user-supplied data]
  plot.data.offset <- plot.data
  plot.data.offset[, 2:ncol(plot.data)] <- plot.data[, 2:ncol(plot.data)] + abs(centre.y)
  # print(plot.data.offset)
  # (b) convert into radial coords
  group <- NULL
  group$path <- CalculateGroupPath(plot.data.offset)

  # print(group$path)
  # (c) Calculate coordinates required to plot radial variable axes
  axis <- NULL
  axis$path <- CalculateAxisPath(var.names, grid.min + abs(centre.y), grid.max + abs(centre.y))
  # print(axis$path)
  # (d) Create file containing axis labels + associated plotting coordinates
  # Labels
  axis$label <- data.frame(
    text = axis.labels,
    x = NA,
    y = NA
  )
  # print(axis$label)
  # axis label coordinates
  n.vars <- length(var.names)
  angles <- seq(from = 0, to = 2 * pi, by = (2 * pi) / n.vars)
  axis$label$x <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * sin(angles[i])
  })
  axis$label$y <- sapply(1:n.vars, function(i, x) {
    ((grid.max + abs(centre.y)) * axis.label.offset) * cos(angles[i])
  })
  # print(axis$label)
  # (e) Create Circular grid-lines + labels
  # caclulate the cooridinates required to plot circular grid-lines for three user-specified
  # y-axis values: min, n2, n3, n4, n5 and max [grid.min; grid.n2; grid.n3; grid.n4; grid.n5; grid.max]
  gridline <- NULL
  gridline$min$path <- funcCircleCoords(c(0, 0), grid.min + abs(centre.y), npoints = 360)
  gridline$n2$path <- funcCircleCoords(c(0, 0), grid.n2 + abs(centre.y), npoints = 360)
  gridline$n3$path <- funcCircleCoords(c(0, 0), grid.n3 + abs(centre.y), npoints = 360)
  gridline$n4$path <- funcCircleCoords(c(0, 0), grid.n4 + abs(centre.y), npoints = 360)
  gridline$n5$path <- funcCircleCoords(c(0, 0), grid.n5 + abs(centre.y), npoints = 360)
  gridline$max$path <- funcCircleCoords(c(0, 0), grid.max + abs(centre.y), npoints = 360)
  # print(head(gridline$max$path))
  # gridline labels
  gridline$min$label <- data.frame(
    x = gridline.label.offset.x, y = grid.min + abs(centre.y),
    text = as.character(grid.min)
  )
  gridline$max$label <- data.frame(
    x = gridline.label.offset.x, y = grid.max + abs(centre.y),
    text = as.character(grid.max)
  )
  gridline$n2$label <- data.frame(
    x = gridline.label.offset.x, y = grid.n2 + abs(centre.y),
    text = as.character(grid.n2)
  )
  gridline$n3$label <- data.frame(
    x = gridline.label.offset.x, y = grid.n3 + abs(centre.y),
    text = as.character(grid.n3)
  )
  gridline$n4$label <- data.frame(
    x = gridline.label.offset.x, y = grid.n4 + abs(centre.y),
    text = as.character(grid.n4)
  )
  gridline$n5$label <- data.frame(
    x = gridline.label.offset.x, y = grid.n5 + abs(centre.y),
    text = as.character(grid.n5)
  )
  # print(gridline$min$label)
  # print(gridline$max$label)
  # print(gridline$n2$label)
  # print(gridline$n3$label)
  # print(gridline$n4$label)
  # print(gridline$n5$label)
  ### Start building up the radar plot

  # Declare 'theme_clear', with or without a plot legend as required by user
  # [default = no legend if only 1 group [path] being plotted]
  theme_clear <- theme_bw(base_size = base.size) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      legend.key = element_rect(linetype = "blank")
    )

  if (plot.legend == FALSE) legend.position = "none"

  # Base-layer = axis labels + plot extent
  # [need to declare plot extent as well, since the axis labels don't always
  # fit within the plot area automatically calculated by ggplot, even if all
  # included in first plot; and in any case the strategy followed here is to first
  # plot right-justified labels for axis labels to left of Y axis for x< (-x.centre.range)],
  # then centred labels for axis labels almost immediately above/below x= 0
  # [abs(x) < x.centre.range]; then left-justified axis labels to right of Y axis [x>0].
  # This building up the plot in layers doesn't allow ggplot to correctly
  # identify plot extent when plotting first (base) layer]

  # base layer = axis labels for axes to left of central y-axis [x< -(x.centre.range)]
  base <- ggplot(axis$label) + xlab(NULL) + ylab(NULL) + coord_equal() +
    geom_text(
      data = subset(axis$label, axis$label$x < (-x.centre.range)),
      aes(x = x, y = y, label = text), size = axis.label.size, hjust = 1, family = font.radar
    ) +
    scale_x_continuous(limits = c(-1.5 * plot.extent.x, 1.5 * plot.extent.x)) +
    scale_y_continuous(limits = c(-plot.extent.y, plot.extent.y))

  # ... + circular grid-lines at 'min', 'n2', 'n3', 'n4', 'n5' and 'max' y-axis values
  if(gridlines.show[1]) {
    base <- base + geom_path(
      data = gridline$min$path, aes(x = x, y = y),
      lty = gridline.min.linetype, colour = gridline.min.colour, size = grid.line.width
    )
  }
  if(gridlines.show[2]) {
    base <- base + geom_path(
      data = gridline$n2$path, aes(x = x, y = y),
      lty = gridline.n2.linetype, colour = gridline.n2.colour, size = grid.line.width
    )
  }
  if(gridlines.show[3]) {
    base <- base + geom_path(
      data = gridline$n3$path, aes(x = x, y = y),
      lty = gridline.n3.linetype, colour = gridline.n3.colour, size = grid.line.width
    )
  }
  if(gridlines.show[4]) {
    base <- base + geom_path(
      data = gridline$n4$path, aes(x = x, y = y),
      lty = gridline.n4.linetype, colour = gridline.n4.colour, size = grid.line.width
    )
  }
  if(gridlines.show[5]) {
    base <- base + geom_path(
      data = gridline$n5$path, aes(x = x, y = y),
      lty = gridline.n5.linetype, colour = gridline.n5.colour, size = grid.line.width
    )
  }
  if(gridlines.show[6]) {
    base <- base + geom_path(
      data = gridline$max$path, aes(x = x, y = y),
      lty = gridline.max.linetype, colour = gridline.max.colour, size = grid.line.width
    )
  }

  # + axis labels for any vertical axes [abs(x)<=x.centre.range]
  base <- base + geom_text(
    data = subset(axis$label, abs(axis$label$x) <= x.centre.range),
    aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0.5, family = font.radar
  )
  # + axis labels for any vertical axes [x>x.centre.range]
  base <- base + geom_text(
    data = subset(axis$label, axis$label$x > x.centre.range),
    aes(x = x, y = y, label = text), size = axis.label.size, hjust = 0, family = font.radar
  )
  # + theme_clear [to remove grey plot background, grid lines, axis tick marks and axis text]
  base <- base + theme_clear
  #  + background circle against which to plot radar data
  base <- base + geom_polygon(
    data = gridline$max$path, aes(x, y),
    fill = background.circle.colour,
    alpha = background.circle.transparency
  )

  # + radial axes
  base <- base + geom_path(
    data = axis$path, aes(x = x, y = y, group = axis.no),
    colour = axis.line.colour
  )

  theGroupName <- names(group$path[1])

  # ... + group (cluster) 'paths'
  base <- base + geom_path(
    data = group$path, aes_string(x = "x", y = "y", group = theGroupName, colour = theGroupName),
    size = group.line.width
  )

  # ... + group points (cluster data)
  base <- base + geom_point(data = group$path, aes_string(x = "x", y = "y", group = theGroupName, colour = theGroupName), size = group.point.size)

  # ... + group (cluster) fills
  if(fill == TRUE) {
    base <- base + geom_polygon(data = group$path, aes_string(x = "x", y = "y", group = theGroupName, fill = theGroupName), alpha = fill.alpha)
  }


  # ... + amend Legend title
  if (plot.legend == TRUE) base <- base + labs(colour = legend.title, size = legend.text.size)

  # ... + grid-line labels (min; n2; n3; n4; n5; max)
  if (label.gridlines.show[1]) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[1]), color = grid.label.color, data = gridline$min$label, size = grid.label.size * 0.8, hjust = 1, vjust = -gridline.label.offset.y, family = font.radar)
  }
  if (label.gridlines.show[2]) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[2]), color = grid.label.color, data = gridline$n2$label, size = grid.label.size * 0.8, hjust = 1, vjust = -gridline.label.offset.y, family = font.radar)
  }
  if (label.gridlines.show[3]) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[3]), color = grid.label.color, data = gridline$n3$label, size = grid.label.size * 0.8, hjust = 1, vjust = -gridline.label.offset.y, family = font.radar)
  }
  if (label.gridlines.show[4]) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[4]), color = grid.label.color, data = gridline$n4$label, size = grid.label.size * 0.8, hjust = 1, vjust = -gridline.label.offset.y, family = font.radar)
  }
  if (label.gridlines.show[5]) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[5]), color = grid.label.color, data = gridline$n5$label, size = grid.label.size * 0.8, hjust = 1, vjust = -gridline.label.offset.y, family = font.radar)
  }
  if (label.gridlines.show[6]) {
    base <- base + geom_text(aes(x = x, y = y, label = values.radar[6]), color = grid.label.color, data = gridline$max$label, size = grid.label.size * 0.8, hjust = 1, vjust = -gridline.label.offset.y, family = font.radar)
  }
  # ... + centre.y label if required [i.e. value of y at centre of plot circle]
  if (label.centre.y == TRUE) {
    centre.y.label <- data.frame(x = 0, y = 0, text = as.character(centre.y))
    base <- base + geom_text(aes(x = x, y = y, label = text), data = centre.y.label, size = grid.label.size, hjust = 0.5, family = font.radar)
  }

  if (!is.null(group.colours)) {
    colour_values <- rep(group.colours, 100)
  } else {
    colour_values <- rep(c(
      "#FF5A5F", "#FFB400", "#007A87", "#8CE071", "#7B0051",
      "#00D1C1", "#FFAA91", "#B4A76C", "#9CA299", "#565A5C", "#00A04B", "#E54C20"
    ), 100)
  }

  base <- base + theme(legend.key.width = unit(3, "line")) + theme(text = element_text(
    size = 20,
    family = font.radar
  )) +
    theme(legend.text = element_text(size = legend.text.size), legend.position = legend.position) +
    theme(legend.key.height = unit(2, "line")) +
    scale_colour_manual(values = colour_values) +
    theme(text = element_text(family = font.radar)) +
    theme(legend.title = element_blank())


  if(isTRUE(fill)) {
    base <- base +
      scale_fill_manual(values = colour_values, guide = "none")
  }

  if(legend.title != "") {
    base <- base + theme(legend.title = element_text())
  }

  if (plot.title != "") {
    base <- base + ggtitle(plot.title)
  }

  return(base)
}
