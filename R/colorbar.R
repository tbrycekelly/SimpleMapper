#' @title All Colorbar
#' @author Thomas Bryce Kelly
#' @keywords Gridding
#' @export
colorbar = function(pal, zlim, num_labels = NULL, x = NULL, y = NULL, width = 1, height = 1) {
  
  usr = par()$usr
  
  width = diff(usr[1:2]) * 0.05 * 0.5 * width
  height = diff(usr[3:4]) * 0.5 * height
  
  if (is.null (x)) {
    x = usr[2] + width
  }
  if (is.null(y)) {
    y = (usr[3] + usr[4])/2
  }
  
  if (is.null(num_labels)) {
    if (length(pal) > 10) {
      div = primeFactors(length(pal))[-1]
      div = div[length(div)] ^ sum(div == div[length(div)])
      num_labels =  (length(pal)) / div + 1
    } else {
      num_labels = length(pal) + 1
    }
  }
  zlab = seq(zlim[1], zlim[2], length.out = num_labels)
  
  
  dx = width * 0.8
  dy = 2 * height / length(pal)
  dy_label = (2 * height) / (num_labels-1)
  
  par(xpd = T)
  for (i in 1:num_labels) {
    text.default(x + dx, y - height + dy_label * (i-1), labels = zlab[i], pos = 4)
    lines(x = c(-dx, 1.5*dx) + x, y = rep(y - height + dy_label * (i-1), 2))
  }
  lines(x = c(x-dx, x+dx, x+dx, x-dx, x-dx),
        y = c(y-height, y-height, y+height,y+height,y-height))
  
  for (i in 1:length(pal)) {
    rect(x - dx, y - height + dy * (i-1), x + dx, y - height + dy * (i), col = pal[i], border = pal[i])
  }
  
  
  text.default(x + dx, y + height, labels = zlab[i+1], pos = 4)
  par(xpd = F)
}
