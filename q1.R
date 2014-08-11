q1_1 <- function() {
  zalpha <- qnorm(.05)
  mu = 12
  sd = 4
  n = 100
  # P( x <= C | mu = 12) is equivalent to
  # P[ z <= cost - 12 / (sigma / sqrt(n)] = 0.05
  # value of z is exactly zalpha hence
  
  # zalpha <= (cost - mu) * sqrt(n) / sd 
  # so c > climit
  
  climit <- zalpha * sd / sqrt(n) + mu
  
  xval <- seq(-3.2, 3.2, length = 1000)
  yval<- dnorm(xval)
  plot(xval, yval, type = "l", axes = TRUE, frame = FALSE, lwd = 3, xlab = "", ylab = "")
  x <- seq(-3.2, qnorm(.05), length = 100)
  polygon(c(x, rev(x)),c(dnorm(x), rep(0, length(x))), col = "salmon")
  text(qnorm(.05), .01, round(qnorm(.05), digits=2), cex = 2)
  text(0, dnorm(0) / 5, "95%", cex = 2)
}

q1_2 <- function() {
  #paired t test vs unpaired t test (two sided 5%)
  g1 <- c(140, 138, 150,148,135)
  g2 <- c(138,136,148,146,133)
  difference = g1 - g2
  mu <- mean(difference)
  s <- sd(difference)
  n <- 5
  #condifence interval
  mu + c(-1,1) * s * qt(.95, n-1) / sqrt(n)
  # equivalent to 
  t.test(difference)$conf.int
  # but s = 0 so 
  
  #unpaired t test
  t.test(g1, g2, paired=FALSE)
  
}

q1_4 <- function(){
  g1 <- c(2.233, -2.513,	1.204,	1.938,	2.533)
  g2 <- c(0.929,  -1.745,	1.677,	0.701,	0.128)
  difference = g2 - g1
  t.test(difference, alternative="less")
}