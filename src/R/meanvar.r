require(changepoint)

cpLocations <- function(data, method, stat) {

  res = cpt.meanvar(data,
                    penalty="Asymptotic",
                    pen.value=0.01,
                    method=method,
                    Q=50,
                    test.stat=stat,
                    class=TRUE,
                    param.estimates=FALSE)

  return (cpts(res))

}