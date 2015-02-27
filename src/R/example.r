input <- matrix(c(rnorm(3000,10, 10), rnorm(3000,20, 10)),10, 600)
BootstrapValues(input[ ,1:200], family.pa = 'Gaussian', 
                H = c(5, 10, 15), M = 500, alpha = .03)

InitMultiscaleCP(data = input[ , (1 : (2 * max(H) + 1) )], H == c(5, 10, 15))

for (i in (2*max(H) + 1) : ncol(input)){
  MultiScaleCP(input[,i], x = i, H = c(5, 10, 15), family.pa = 'Gaussian')
}
