#' Confusion table to a mosaic plot in ggplot2
#' makeData_mosaic function generate data for a mosaic plot in ggplot2
#'
#' @param data a data.frame
#' @param x the name of the first variable (a quality factor)
#' @param y the name of the second variable (a quality factor)
#'
#' @return a list with informations for ggplot2


makeData_mosaic <- function(data, x, y, ...){
  require(ggplot2)
  xvar <- deparse(substitute(x))
  yvar <- deparse(substitute(y))
  mydata <- data[c(xvar, yvar)];
  mytable <- table(mydata);
  nclass <- dim(mytable)[1]
  widths <- c(0, cumsum(apply(mytable, 1, sum)));
  widths.text <- as.numeric(widths[1:nclass])+((widths[1+(1:nclass)]-widths[1:nclass])/2)
  heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))});

  alldata <- data.frame();
  allnames <- data.frame();
  for(i in 1:nrow(mytable)){
    for(j in 1:ncol(mytable)){
      alldata <- rbind(alldata, c(widths[i], widths[i+1], heights[j, i], heights[j+1, i]));
    }
  }
  colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")

  alldata[[xvar]] <- rep(dimnames(mytable)[[1]],rep(ncol(mytable), nrow(mytable)));
  alldata[[yvar]] <- rep(dimnames(mytable)[[2]],nrow(mytable));

  return(list(alldata=alldata, annot.text=widths.text))
  
 }
