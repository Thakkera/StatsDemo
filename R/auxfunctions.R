## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="yellow")
}

## next function can be used in setting xlim and ylim to make sure the
## origin is always visible...
unsigned.range <- function(x)
{
  c(-abs(min(x, na.rm = TRUE)), abs(max(x, na.rm = TRUE)))
}
