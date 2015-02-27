require(Matrix)

Likelihood <- function(data, family = c('Poisson', 'Gaussian', 'Bernoulli', 'Volatility')){
  stat <- rep(0, ncol(data))
  theta <- rowMeans(data)
  theta<- t(t(theta))

  if (family == "Volatility"){
     v = 0.0

        for (j in 1:ncol(data)){
           v = v + sum((data[,j] - theta)^2)
        }

        v = v / (ncol(data) - 1)

        if (v < 0.01) v = 0.01

        for (j in 1:ncol(data)){
              stat[j] <- -.5 * sum((data[,j] - theta)^2)/ v
        }
  }


  if (family == 'Poisson'){
    ind <-which(theta == 0)
    theta[ind] <- 0.001
    for(i in 1:ncol(data)){
     stat[i] <- sum(data[, i] * log(theta) - theta)
    }
  }

  if (family == 'Gaussian'){
    #E <- Diagonal(p)
    for (i in 1:ncol(data)){
      stat[i] <- -.5 * sum((data[, i] - theta) * (data[i] - theta))
    }
  }

  if (family == 'Bernoulli'){
    ind <- which(theta == 0)
    theta[ind] <- 0.001
    ind <- which(theta == 1)
    theta[ind] <- 0.999

    for(i in ncol(data)){
      stat[i] <- sum(data[,i] * (log(theta) - log(1 - theta)) + log(1 - theta) )
    }
  }
  out <- sum(stat)
  return(out)
}

BootLikelihood <- function(data, coeff,family = c('Poisson', 'Gaussian', 'Bernoulli', 'Volatility')){
  if (length(ncol(data)) == 0) {
    data <- t(data)
  }

  stat <- rep(0, ncol(data))

  if (sum(coeff) > 0){
    theta <- data %*% coeff / sum(coeff)
  }else{
    theta <- (data %*% coeff) / 0.0001
  }

  if (family == "Volatility"){

    v <- 1.0

    for (j in 1:ncol(data)){
       v <- v + sum((data[,j] - theta)^2)* coeff[j]
    }

    if (sum(coeff) > 0.1) {
       v <- v / (sum(coeff))
    } else {
        v <- v / (sum(coeff) + 0.1)
    }


    for (j in 1:ncol(data)){
          stat[j] <- -.5 * log(6.28 * v) * coeff[j] -.5 * sum((data[,j] - theta)^2)* coeff[j] / v
    }
  }

  if (family == 'Poisson'){
    ind <- which(theta == 0)
    theta[ind] <- 0.001
    for(i in 1:ncol(data)){
      stat[i] <- sum(data[,i] * log(theta) - theta) *coeff[i]
    }
  }

  if (family == 'Gaussian'){
    for (j in 1:ncol(data)){
      stat[j] <- -.5 * sum((data[,j] - theta)^2)* coeff[j]
    }
  }

  if (family == 'Bernoulli'){
    ind <- which(theta == 0)
    theta[ind] <- 0.001
    ind <- which(theta == 1)
    theta[ind] <- 0.999

    for(i in ncol(data)){
      stat[i] <- sum(data[,i] * (log(theta) - log(1 - theta)) + log(1 - theta) ) * coeff[i]
    }
  }
  out <- sum(stat)
  return(out)
}


BootLRT <- function(data.left, data.right, coeff.left, coeff.right, family){
  LL.left  <- BootLikelihood(data.left, coeff.left, family)
  LL.right <- BootLikelihood(data.right, coeff.right, family)
  if (length(ncol(data.left)) == 0){
    data.concat <- c(data.left, data.right)
  }else{
    data.concat <- cbind(data.left, data.right)
  }
  LL.lr    <- BootLikelihood(data.concat, c(coeff.left, coeff.right), family)

  return(LL.left + LL.right - LL.lr)
}


BootstrapValues <- function(data, family.pa, H, M, alpha){
  if (length(ncol(data)) == 0 ){
    data <- t(data)
  }

  stat <- matrix(NA, length(H), M * ncol(data))
  stat.triang <- matrix(NA, length(H), M * ncol(data))
  out <- rep(0, length(H))

  for (m in 1:M){
    coeff <- rpois(ncol(data), 1)

    for (h in seq_along(H)){
      #triangle <- c((1: H[h]), (H[h] : 1)) / H[h]
      triangle <- (1: H[h]) / H[h]
      for (i in (2 * H[h] + 1) : ncol(data)){
        clmn <- (m-1)*ncol(data) + i

        stat[h, clmn] <- sqrt(2 * abs(BootLRT(data.left = data[, (i - 2 * H[h] + 1):(i - H[h])],
                                              data.right = data[, (i - H[h] + 1 ):i],
                                              coeff.left = coeff[(i - 2 * H[h] + 1):(i - H[h])],
                                              coeff.right = coeff[(i - H[h] + 1 ):i],
                                              family = family.pa)))

        if (i >= 3 * H[h] + 1){
          stat.triang[h, (m-1)*ncol(data) + i] <- sum(stat[h,(i- length(triangle) + 1):i] * triangle)
        }
      }
    }
  }


  alphas <- seq(alpha,0 ,-0.005)
  thlds <- matrix(0,length(H), length(alphas))

  for (h in seq_along(H)){
    thlds[h, ] <- quantile(stat.triang[h, !is.na(stat.triang[h,])], probs = 1-alphas)
  }


  stat <- stat[ ,colSums(is.na(stat)) == 0]

  for (i in seq_along(alphas)){
    stat.normalized <- stat - thlds[, i]
    stat.normalized <- apply(stat.normalized, 2, function(x) max(x))
    Fn <- ecdf(stat.normalized)
    if (1 - Fn(0) <= alpha){
      BootstrapLine <<- thlds[, i]
      break
    }
  }


  #BootstrapLine <<- out
}

InitMultiscaleCP <- function(data, H){

  #data - matrix n x 2h, n - dim. of each data point, h - half-size of running window,
  #2h - number of observations
  #H - set of scales, each scale equal to a half of win size

  data.set <<- list()
  for (h in seq_along(H)) {
    if (length(ncol(data)) == 0 ) {
      data.input <- t(data[(length(data) - 2 * H[h]) : length(data)])
    }else{
      data.input <- data[, (ncol(data) - 2 * H[h]) : ncol(data)]
    }
    data.set[[h]] <<- as.data.frame(data.input)
  }

  names(data.set) <<- as.character(H)
  cp.ind <<- matrix(0, length(H), 1)
  flag <<- 0
  LRT <<- matrix(NA, length(H), 10000)
  LRT.Triangle <<- matrix(NA, length(H), 10000)
  LRT.len <<- matrix(0, length(H), 1)
}

MultipleCPCheck <- function(data, x, H, family.pa, upperBounds){

  out <- rep(0, length(H))

  for (h in seq_along(H)){
      data.set[[h]][1:(length(data.set[[h]]) - 1)] <<- data.set[[h]][2:length(data.set[[h]])]
      data.set[[h]][length(data.set[[h]])] <<- as.data.frame(data)
      LL.left  <- Likelihood(data.set[[h]][(length(data.set[[h]]) - 2 * H[h]+1) : (length(data.set[[h]]) - H[h])], family = family.pa)
      LL.right <- Likelihood(data.set[[h]][(length(data.set[[h]]) - H[h] + 1) : length(data.set[[h]])], family = family.pa)
      LL.lr    <- Likelihood(data.set[[h]][2 : length(data.set[[h]])], family = family.pa)

      if(LL.left + LL.right - LL.lr< 0){
        show(paste('i = ', i, sep = ''))
        show(paste('h = ', h, sep =''))
        data.set.oops <<-data.set
        show('------')
      }

      LRT[h, x] <<- sqrt(2 * (abs(LL.left + LL.right - LL.lr)))
      LRT.len[h] <<- LRT.len[h] + 1

      if (LRT.len[h] >=  H[h] + 1){
        triangle <- (1: H[h]) / H[h]
        LRT.Triangle[h, x] <<- sum(triangle * LRT[h, (x -length(triangle) + 1) : x])

        if(LRT.Triangle[h,x] > upperBounds[h]){
            out[h] <- 1
        }

      }
  }

  return(out)
}


