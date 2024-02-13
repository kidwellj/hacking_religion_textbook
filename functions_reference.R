```{r}

# Load some new libraries used by functions below

library(RColorBrewer)
library(hrbrthemes) # Used for ipsum theme etc.
library(ggeasy) # used for easy_center_title() which is not strictly necessary, but tidier than theme(plot.title = element_text(hjust = 0.5))

# Define colour palettes
# TODO: confirm final colour scheme for charts and normalise across usage of different themes
coul3 <- brewer.pal(3, "RdYlBu") # Using RdYlBu range to generate 3 colour palette: https://colorbrewer2.org/#type=diverging&scheme=RdYlBu&n=5
coul4 <- brewer.pal(4, "RdYlBu")
coul5 <- brewer.pal(5, "RdYlBu")
coul6 <- brewer.pal(6, "RdYlBu")
coul7 <- brewer.pal(7, "RdYlBu")
coul4_reversed <- c("#2C7BB6", "#ABD9E9", "#FDAE61", "#D7191C")
coul6_reversed <- c("#4575B4", "#91BFDB" , "#E0F3F8" , "#FEE090", "#FC8D59", "#D73027")
white <- "#ffffff"
purple <- "#590048"
ochre <- "#B18839"
ochre_12 <- wheel(ochre, num = 12)
purple_12 <- wheel(purple, num = 12)

# Reusable Functions ------------------------------------------------------

# Importing code for colortools() now deprecated and removed from CRAN here. Some minor modifications to update code, but generally all credit here goes to Gaston Sanchez

setColors <- function(color, num) {
  # convert to RGB
  rgb_col = col2rgb(color)
  # convert to HSV
  hsv_col = rgb2hsv(rgb_col)[,1]
  # get degree
  hue = hsv_col[1]
  sat = hsv_col[2]
  val = hsv_col[3]
  cols = seq(hue, hue + 1, by=1/num)
  cols = cols[1:num]
  cols[cols > 1] <- cols[cols > 1] - 1
  # get colors with hsv
  colors = hsv(cols, sat, val)
  # transparency
  if (substr(color, 1, 1) == "#" && nchar(color) == 9)
    ({
      alpha = substr(color, 8, 9)
      colors = paste(colors, alpha, sep="")
    })
  colors
}

complementary <- function(color, plot=TRUE, bg="white", labcol=NULL, cex=0.8, title=TRUE) {	
  tmp_cols = setColors(color, 12)
  comp_colors <- tmp_cols[c(1, 7)]
  
  # plot
  if (plot)
    ({
      # labels color
      if (is.null(labcol)) 
        ({
          lab_col = rep("", 12)
          if (mean(col2rgb(bg)) > 127)
            ({
              lab_col[c(1, 7)] <- "black"
              lab_col[c(2:6,8:12)] <- col2HSV(bg)
            }) else ({
              lab_col[c(1, 7)] <- "white"
              lab_col[c(2:6,8:12)] <- col2HSV(bg)
            })
        }) else ({
          lab_col = rep(labcol, 12)
          if (mean(col2rgb(bg)) > 127)
            ({
              lab_col[c(1, 7)] <- labcol
              lab_col[c(2:6,8:12)] <- col2HSV(bg)
            }) else ({
              lab_col[c(1, 7)] <- labcol
              lab_col[c(2:6,8:12)] <- col2HSV(bg)
            })
        })	
      # hide non-adjacent colors
      tmp_cols[c(2:6,8:12)] <- paste(substr(tmp_cols[c(2:6,8:12)],1,7), "0D", sep="")
      pizza(tmp_cols, labcol=lab_col, bg=bg, cex=cex)
      # title
      if (title)
        title(paste("Complementary (opposite) color of: ", tmp_cols[1]), 
              col.main=lab_col[1], cex.main=0.8)
    })
  # result
  comp_colors
}

sequential <- function(color, percentage=5, what="saturation", s=NULL, v=NULL, alpha=NULL, fun="linear", plot=TRUE, verbose=TRUE)  {
  # convert to HSV
  col_hsv = rgb2hsv(col2rgb(color))[,1]
  # transparency
  if (is.null(alpha))
    alpha = 1
  if (substr(color, 1, 1) == "#" && nchar(color) == 9)
    alpha = substr(color, 8, 9)
  # get hue, saturation, and value
  hue = col_hsv[1]
  if (is.null(s)) s = col_hsv[2]
  if (is.null(v)) v = col_hsv[3]
  # sequence function
  getseq = switch(fun, 
                  linear = seq(0, 1, by=percentage/100),
                  sqrt = sqrt(seq(0, 1, by=percentage/100)),
                  log = log1p(seq(0, 1, by=percentage/100)),
                  log10 = log10(seq(0, 1, by=percentage/100))
  )
  # what type of sequence?
  if (what == "saturation") ({
    sat = getseq
    fixed = paste("v=", round(v,2), " and alpha=", alpha, sep="")
    if (is.numeric(alpha))
      seq_col = hsv(hue, s=sat, v=v, alpha=alpha)
    if (is.character(alpha)) ({
      seq_col = hsv(hue, s=sat, v=v)
      seq_col = paste(seq_col, alpha, sep="")
    })
  })
  if (what == "value") ({
    val = getseq
    fixed = paste("s=", round(s,2), " and alpha=", alpha, sep="")
    if (is.numeric(alpha))
      seq_col = hsv(hue, s=s, v=val, alpha=alpha)
    if (is.character(alpha)) ({
      seq_col = hsv(hue, s=s, v=val)
      seq_col = paste(seq_col, alpha, sep="")
    })
  })
  if (what == "alpha") ({
    alpha = getseq
    fixed = paste("s=", round(s,2), " and v=", round(v,2), sep="")
    seq_col = hsv(hue, s=s, v=v, alpha=alpha)
  })
  # if plot TRUE
  if (plot)
    ({
      n = length(seq(0, 1, by=percentage/100))
      fx = unlist(fixed)
      #dev.new()
      plot(0, 0, type="n", xlim=c(0,1), ylim=c(0,1), axes=FALSE, xlab="", ylab="")
      rect(0:(n-1)/n, 0, 1:n/n, 1, col=seq_col, border="lightgray")
      mtext(seq_col, side=1, at=0.5:(n)/n, cex=0.8, las=2)
      title(paste("Sequential colors based on ", what, "\n with fixed ", fx, sep=""),
            cex.main=0.9)
    })
  # result
  if (verbose)
    seq_col
}

wheel <- function(color, num=12, bg="gray95", border=NULL, init.angle=105, cex=1, lty=NULL, main=NULL, verbose=TRUE, ...) {
  if (!is.numeric(num) || any(is.na(num) | num < 0)) 
    stop("\n'num' must be positive")
  x <- rep(1, num)
  x <- c(0, cumsum(x)/sum(x))
  dx <- diff(x)
  nx <- length(dx)
  # set colors
  col = setColors(color, num)
  labels = col
  # labels color
  labcol = ifelse( mean(col2rgb(bg)) > 127, "black", "white")
  # prepare plot window
  par(bg = bg)
  plot.new()
  pin <- par("pin")
  xlim <- ylim <- c(-1, 1)
  if (pin[1L] > pin[2L]) 
    xlim <- (pin[1L]/pin[2L]) * xlim
  else ylim <- (pin[2L]/pin[1L]) * ylim
  dev.hold()
  on.exit(dev.flush())
  plot.window(xlim, ylim, "", asp = 1)
  # get ready to plot
  if (is.null(border[1])) ({
    border <- rep(bg, length.out = nx)    
  }) else ({
    border <- rep(border, length.out = nx)    
  })
  if (!is.null(lty))
    lty <- rep(NULL, length.out = nx)
  angle <- rep(45, length.out = nx)
  radius = seq(1, 0, by=-1/num)[1:num]
  twopi <- -2 * pi
  t2xy <- function(t, rad) ({
    t2p <- twopi * t + init.angle * pi/180
    list(x = rad * cos(t2p), y = rad * sin(t2p))
  })
  # plot colored segments
  for (i in 1L:nx)
    ({
      n <- max(2, floor(200 * dx[i]))
      P <- t2xy(seq.int(x[i], x[i + 1], length.out = n), rad=radius[1])
      polygon(c(P$x, 0), c(P$y, 0), angle = angle[i], 
              border = border[i], col = col[i], lty = lty[i])
      P <- t2xy(mean(x[i + 0:1]), rad=radius[1])
      lab <- labels[i]
      if (!is.na(lab) && nzchar(lab)) ({
        adjs = 0.5
        if (P$x > 1e-08) adjs <- 0
        if (P$x < -1e-08) adjs <- 1
        lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
        text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, 
             adj = adjs, cex=cex, col=labcol, ...)
      })
    })
  # add title
  title(main = main, ...)
  # return color names
  if (verbose)
    col
}

# function to produce horizontal bar chart, colours drawn from "ochre" colour wheel defined above to match report
plot_horizontal_bar <- function(x) {
  ## code if a specific palette is needed for matching
  fill = wheel(ochre, num = as.integer(count(x[1])))
  #fill = scale_fill_brewer()
  # make plot
  ggplot(x, aes(x = n, y = response, fill = fill)) +
    geom_col(colour = "white") +
    ## add percentage labels
    geom_text(aes(label = perc),
              ## make labels left-aligned and white
              hjust = 1, nudge_x = -.5, colour = "black", size=3) +
    ## reduce spacing between labels and bars
    scale_fill_identity(guide = "none") +
    ## get rid of all elements except y axis labels + adjust plot margin
    theme_ipsum_rc() +
    theme(plot.margin = margin(rep(15, 4))) +
    easy_center_title()
}

qualtrics_process_single_multiple_choice <- function(x) {
  # create separate data frame
  df <- as.data.frame(x)
  # make column names coherent and simplified
  names(df) <- c("response")
  # filter out NA values
  df <- filter(df, !is.na(response))
  # generate new dataframe with sums per category and sort in descending order
  sums <- df %>%
    dplyr::count(response, sort = TRUE) %>% 
    dplyr::mutate(
      response = forcats::fct_rev(forcats::fct_inorder(response))
    )    
  # add new column with percentages for each sum
  sums <- sums %>% 
    dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = 1, trim = FALSE))
}

qualtrics_process_single_multiple_choice_unsorted_streamlined <- function(x) {
  # create separate data frame
  df <- as.data.frame(as_factor(x))
  # make column names coherent and simplified
  names(df) <- c("response")
  # filter out NA values
  df <- filter(df, !is.na(response))
  # generate new dataframe with sums per category and sort in descending order
  sums <- df %>%
    dplyr::count(response, sort = FALSE) 
  # add new column with percentages for each sum
  sums <- sums %>% 
    dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = 1, trim = FALSE))
}

qualtrics_process_single_multiple_choice_basic <- function(x) {
  # create separate data frame
  df <- as_factor(x)
  # make column names coherent and simplified
  names(df) <- c("response")
  # filter out NA values
  df <- filter(df, !is.na(response))
  # generate new dataframe with sums per category and sort in descending order
  sums <- df %>%
    dplyr::count(response, sort = FALSE) 
  # add new column with percentages for each sum
  sums <- sums %>% 
    dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = 1, trim = FALSE))
}

qualtrics_process_single_multiple_choice_unsorted <- function(x) {
  # create separate data frame
  df <- as.data.frame(x)
  # make column names coherent and simplified
  names(df) <- c("response")
  # filter out NA values
  df <- filter(df, !is.na(response))
  # generate new dataframe with sums per category and sort in descending order
  sums <- df %>%
    dplyr::count(response, sort = FALSE) %>% 
    dplyr::mutate(
      response = forcats::fct_rev(forcats::fct_inorder(response))
    )    
  # add new column with percentages for each sum
  sums <- sums %>% 
    dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = 1, trim = FALSE))
}

# function to produce a summary table of results for a single column using flextable

chart_single_result_flextable <- function(.data, var) {
  table <- table(.data)
  # add calculations and convert to a flextable object
  table %>%
    prop.table %>% # turn this into a table of proportions
    # flextable requires a dataframe
    as.data.frame() %>%
    set_names(c("Variable", "Count")) %>%
    # arrange in descending order
    arrange({{ var }}) %>%
    # convert table object to a flextable()
    flextable(defaults = TRUE) %>%
    # adjust column widths automatically to fit widest values
    style(part = 'body', pr_t=fp_text(font.family='Roboto')) %>%
    style(part = 'header', pr_t=fp_text(font.family='Roboto')) %>%
    # note, likert also uses set_caption() so need to specify flextable:: here
    flextable::set_caption(caption, style = "Table Caption", autonum = run_autonum(seq_id = "tab", bkm = "figures", bkm_all = TRUE)) %>%
    autofit() %>%
    theme_vanilla() %>%
    # format numbers in count column as rounded percentages
    set_formatter( table, Count = function(x) sprintf( "%.1f%%", x*100 ))
}

chart_single_result_flextable_unsorted <- function(.data, var) {
  table <- table(.data)
  # add calculations and convert to a flextable object
  table %>%
    prop.table %>% # turn this into a table of proportions
    # flextable requires a dataframe
    as.data.frame() %>%
    set_names(c("Variable", "Count")) %>%
    # convert table object to a flextable()
    flextable(defaults = TRUE) %>%
    # adjust column widths automatically to fit widest values
    style(part = 'body', pr_t=fp_text(font.family='Roboto')) %>%
    style(part = 'header', pr_t=fp_text(font.family='Roboto')) %>%
    # note, likert also uses set_caption() so need to specify flextable:: here
    flextable::set_caption(caption, style = "Table Caption", autonum = run_autonum(seq_id = "tab", bkm = "figures", bkm_all = TRUE)) %>%
    autofit() %>%
    theme_vanilla() %>%
    # format numbers in count column as rounded percentages
    set_formatter( table, Count = function(x) sprintf( "%.1f%%", x*100 ))
}
```