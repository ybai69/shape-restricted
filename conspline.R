function (y, x, type, zmat = 0, wt = 0, knots = 0, test = FALSE, 
          c = 1.2, nsim = 10000) 
{
  n = length(y)
  if (c > 2) {
    c = 2
  }
  if (c < 1) {
    c = 1
  }
  if (n < 10) {
    print("ERROR: must have at least 10 observations")
  }
  if (length(wt) > 1 & min(wt) <= 0) {
    print("ERROR: must have positive weights")
  }
  one = 1:n * 0 + 1
  if (length(x) != length(y)) {
    print("ERROR: length of x must be length of y")
  }
  if (length(zmat) > 1) {
    if (length(zmat) == n) {
      zmat = matrix(zmat, ncol = 1)
    }
    if (dim(zmat)[1] != n) {
      print("ERROR: number of rows of zmat must be length of y")
    }
    k = dim(zmat)[2]
    rone = one - zmat %*% solve(t(zmat) %*% zmat) %*% t(zmat) %*% 
      one
    if (sum(rone^2) > 1e-08) {
      zmat = cbind(one, zmat)
      k = k + 1
    }
  }else {
    zmat = matrix(one, ncol = 1)
    k = 1
  }
  add = 3
  if (type > 2) {
    add = 4
  }
  if (length(knots) > 1) {
    if (min(knots) <= min(x) & max(knots) >= max(x)) {
      t = knots
    }else {
      br = c(10, 25, 100, 200, 400, 1000, 1e+10)
      obs = 1:7
      nk = min(obs[n <= br]) + add
      t = 0:(nk - 1)/(nk - 1) * (max(x) - min(x)) + min(x)
    }
  }else {
    br = c(10, 25, 100, 200, 400, 1000, 1e+10)
    obs = 1:7
    nk = min(obs[n <= br]) + add
    t = 0:(nk - 1)/(nk - 1) * (max(x) - min(x)) + min(x)#seq(min(x),max(x),length.out = nk)
  }
  if (type == 1) {
    bas = monincr(x, t)
    delta = bas$sigma
    slopes = bas$dsigma
  }
  if (type == 2) {
    bas = monincr(x, t)
    delta = 1 - bas$sigma
    slopes = -bas$dsigma
  }
  if (type == 3) {
    bas = convex(x, t)
    delta = bas$sigma
    slopes = bas$dsigma
  }
  if (type == 4) {
    bas = concave(x, t)
    delta = bas$sigma
    slopes = bas$dsigma
  }
  if (type == 5) {
    bas = convex(x, t)
    delta = bas$sigma
    slopes = bas$dsigma
  }
  if (type == 6) {
    bas = concave(x, t)
    delta = 1 - bas$sigma
    slopes = -bas$dsigma
  }
  if (type == 7) {
    bas = concave(x, t)
    delta = bas$sigma
    slopes = bas$dsigma
  }
  if (type == 8) {
    bas = convex(x, t)
    delta = 1 - bas$sigma
    slopes = -bas$dsigma
  }
  incr = 0
  decr = 0
  if (type == 1 | type == 5 | type == 7) {
    incr = 1
  }
  if (type == 2 | type == 6 | type == 8) {
    decr = 1
  }
  m = length(delta)/n
  if (incr == 0 & decr == 0) {
    zmat = cbind(zmat, x)
  }
  if (length(wt) > 1) {
    ytr = y * sqrt(wt)
    dtr = delta
    ztr = zmat
    for (i in 1:n) {
      dtr[, i] = delta[, i] * sqrt(wt[i])
      ztr[i, ] = zmat[i, ] * sqrt(wt[i])
    }
  }
  else {
    ztr = zmat
    dtr = delta
    ytr = y
  }
  ans = coneB(ytr, dtr, ztr)
  dfuse = min(c * ans$df, m + k)
  sighat = sum((ytr - ans$yhat)^2)/(n - dfuse)
  if (k > 1) {
    use = abs(ans$coef) > 1e-08
    if (k > 1) {
      use[2:k] = FALSE
    }
    xj = cbind(ztr, t(dtr))
    xj = xj[, use]
    pj = xj %*% solve(t(xj) %*% xj) %*% t(xj)
    zm = ztr[, 2:k]
    ppinv = solve(t(zm) %*% (diag(one) - pj) %*% zm)
    zcoef = ans$coef[2:k]
    sez = sqrt(diag(ppinv) * sighat)
    tz = zcoef/sez
    pz = 2 * (1 - pt(abs(tz), n - dfuse))
  }
  if (length(wt) > 1) {
    muhat = ans$yhat/sqrt(wt)
  }
  else {
    muhat = ans$yhat
  }
  if (test) {
    pmat = ztr %*% solve(t(ztr) %*% ztr) %*% t(ztr)
    th0 = pmat %*% ytr
    sse0 = sum((ytr - th0)^2)
    sse1 = sum((ytr - ans$yhat)^2)
    bstat = (sse0 - sse1)/sse0
    mdist = 1:(m + 1) * 0
    k0 = dim(zmat)[2]
    for (isim in 1:nsim) {
      ysim = rnorm(n)
      asim = coneB(ysim, dtr, ztr)
      df0 = asim$df - k0
      mdist[df0 + 1] = mdist[df0 + 1] + 1
    }
    mdist = mdist/nsim
    ps = mdist[1]
    for (d in 1:m) {
      ps = ps + pbeta(bstat, d/2, (n - d - k0)/2) * mdist[d + 
                                                            1]
    }
    pval = 1 - ps
  }
  if (incr == 0 & decr == 0) {
    fhat = t(delta) %*% ans$coef[(k + 2):(k + 1 + m)]
    fhat = fhat + x * ans$coef[k + 1]
    fhat = fhat + ans$coef[1]
    fslope = t(slopes) %*% ans$coef[(k + 2):(k + 1 + m)]
    fslope = fslope + ans$coef[k + 1]
  }
  else {
    fhat = t(delta) %*% ans$coef[(k + 1):(k + m)]
    fhat = fhat + ans$coef[1]
    fslope = t(slopes) %*% ans$coef[(k + 1):(k + m)]
  }
  cans = new.env()
  cans$sighat = sighat
  if (k > 1) {
    cans$zhmat = ppinv
    cans$zcoef = zcoef
    cans$sez = sqrt(sez)
    cans$pvalz = pz
  }
  if (test) {
    cans$pvalx = pval
  }
  cans$muhat = muhat
  cans$fhat = fhat
  cans$fslope = fslope
  cans$knots = t
  cans$df = dfuse
  wp1 = sum(ytr * ztr[, 1])/sum(ztr[, 1]^2) * ztr[, 1]
  cans$rsq = 1 - sum((ytr - ans$yhat)^2)/sum((ytr - wp1)^2)
  cans
}