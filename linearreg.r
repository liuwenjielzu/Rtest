# input data
X = matrix(rnorm(20000), ncol = 10)
X.index = to.dfs(cbind(1:nrow(X), X))
y = as.matrix(rnorm(2000))

Sum = function(., YY) keyval(1, list(Reduce('+', YY)))

XtX=
  values(
    from.dfs(
      mapreduce(
        input = X.index,
        map =
          function(., Xi) {
            yi = y[Xi[,1],]
            Xi = Xi[,-1]
            keyval(1, list(t(Xi) %*% Xi))},
        reduce = Sum,
        combine = TRUE)))[[1]]


XtY=
  values(
    from.dfs(
      mapreduce(
        input = X.index,
        map =
          function(., Xi) {
            yi = y[Xi[,1],]
            Xi = Xi[,-1]
            keyval(1, list(t(Xi) %*% yi))},
        reduce = Sum,
        combine = TRUE)))[[1]]

coefficient = solve(XtX, XtY)
