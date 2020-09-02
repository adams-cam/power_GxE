################################################################################
#
# helper functions for Cam Adams GxE power shiny app
#
################################################################################

########
# case-control functions
########

# get b3 variance
get_b3_var_cc <- function(k = NULL, p_e = NULL, p_g = NULL, ORg, ORe, ORgxe,
                          pi = NULL, delta = 1) {
    
    # set k, if not specified
    if (is.null(k)) {k <- 0.5}
    
    # make coef on log scale
    b1 <- log(ORg)
    b2 <- log(ORe)
    b3 <- log(ORgxe)
    
    # joint GxE probs
    
    # get joint probs from marginal probs
    q <- p_g * (1 + delta) + p_e*(1 - delta) - 1
    C <- (q + sqrt(q^2 + 4*p_g*(1-p_g)*delta)) / (2*(1-p_g)*delta)
    pi00 <- (1 - p_e) / (1 + C)
    pi10 <- ((1 - p_e)*C) / (1 + C)
    pi01 <- (p_e) / (1 + C * delta)
    pi11 <- (C * delta * p_e) / (1 + C*delta)

    
    # baseline probability approx.
    b0 <- (1 / (1 + (pi00 + pi10*exp(b1) + pi01*exp(b2) + 
                         pi11*exp(b1 + b2 + b3)) * (1-k)/k))
    
    # update joint probabilities
    denom <- (pi00 + pi10*exp(b1) + pi01*exp(b2) + pi11*(exp(b1 + b2 + b3)))
    pi00_star <- pi00*(1-k) + pi00 / denom * k
    pi10_star <- pi10*(1-k) + (pi10*k) / denom
    pi01_star <- pi01*(1-k) + (pi10*k) / denom
    pi11_star <- pi11*(1-k) + (pi11*k) / denom 
    pi00 <- pi00_star; pi10 <- pi10_star; pi01 <- pi01_star; pi11 <- pi11_star
    
    # var(b3)
    g0 <- qlogis(b0)
    l <- (exp(g0) / (1 + exp(g0))^2) * pi00
    f <- (exp(g0 + b1) / (1 + exp(g0 + b1))^2) * pi10
    j <- (exp(g0 + b2) / (1 + exp(g0 + b2))^2) * pi01
    r <- (exp(g0 + b1 + b2 + b3) / (1 + exp(g0 + b1 + b2 + b3))^2) * pi11
    var_b3 <- 1/l + 1/f + 1/j + 1/r
    
    # return value
    return(var_b3)
}

get_b3_var_cc2 <- function(k = NULL, p_e = NULL, p_g = NULL, coef,
                       pi = NULL, delta = 1) {
    
    # set k, if not specified
    if (is.null(k)) {k <- 0.5}
    
    # make coef on log scale
    b1 <- log(coef[1])
    b2 <- log(coef[2])
    b3 <- log(coef[3])
    
    # joint GxE probs
    
    # get joint probs from marginal probs
    if (is.null(pi)) {
        q <- p_g * (1 + delta) + p_e*(1 - delta) - 1
        C <- (q + sqrt(q^2 + 4*p_g*(1-p_g)*delta)) / (2*(1-p_g)*delta)
        pi00 <- (1 - p_e) / (1 + C)
        pi10 <- ((1 - p_e)*C) / (1 + C)
        pi01 <- (p_e) / (1 + C * delta)
        pi11 <- (C * delta * p_e) / (1 + C*delta)
    } else {
        # if specifying joint probs directly
        pi00 <- pi[1]
        pi10 <- pi[2]
        pi01 <- pi[3]
        pi11 <- pi[4]
        
    }
    
    # baseline probability approx.
    b0 <- (1 / (1 + pi00 + pi10*exp(b1) + pi01*exp(b2) + 
                    pi11*exp(b1 + b2 + b3)* (k/(1-k))))
    b0 <- log(b0 / (1-b0))
    
    # update joint probabilities
    denom <- (pi00 + pi10*exp(b1) + pi01*exp(b2) + pi11*(exp(b1 + b2 + b3)))
    pi00_star <- pi00*(1-k) + pi00 / denom #* k
    pi10_star <- pi10*(1-k) + (exp(b1)*pi10) / denom #* k
    pi01_star <- pi01*(1-k) + (exp(b2)*pi01) / denom * k
    pi11_star <- pi11*(1-k) + (exp(b1 + b2 + b3)*pi11) / denom# * k
    pi00 <- pi00_star; pi10 <- pi10_star; pi01 <- pi01_star; pi11 <- pi11_star
    
    # var(b3)
    l <- (exp(b0) / (1 + exp(b0))^2) * pi00
    f <- (exp(b0 + b1) / (1 + exp(b0 + b1))^2) * pi10
    j <- (exp(b0 + b2) / (1 + exp(b0 + b2))^2) * pi01
    r <- (exp(b0 + b1 + b2 + b3) / (1 + exp(b0 + b1 + b2 + b3))^2) * pi11
    var_b3 <- 1/l + 1/f + 1/j + 1/r
    
    # return value
    return(var_b3)
}

get_b3_var_co <- function(Pg, Pe, coef) {
    
    ORg <- coef[1]
    ORe <- coef[2]
    ORge <- coef[3]
    
    m1 <- (1-Pg)*(1-Pe)
    m2 <- Pg*(1-Pe)*ORg
    m3 <- (1-Pg)*Pe*ORe
    m4 <- (Pg*Pe*ORg*ORe*ORge)
    v <- (m1+m2+m3+m4) * (1/m1+1/m2+1/m3+1/m4) 
    return(v) # return Var(OR_GE)
}

# get b1 variance
get_b1_var_cc <- function(p_x, A, B, k) {
    
    # method 1
    #V <- (p_x*(1 + A)^2*B + (1 - p_x)*(1 + A*B)^2) / 
    #    (p_x*(1 - p_x)*A*B)
    
    # method 2
    #I <- (A*B*p_x) / (1 + A*B)^2
    #V <- 1 / I
    #V <- 1 / I #+ 1 / (1-I) + 1 / p_x + 1 / (1 - p_x)
    
    # method 3
    #p_case <- ( p_x * A*B) / (p_x * (B - 1) + 1)
    #p_case <- ( p_x * A*B) / (p_x * (A*B - 1) + 1)
    
    #p_all <- (p_case + p_x*k) / (1 + k)
    #V <- 1 / (p_all * (1 - p_all))
    #V <- 1 / p_case + 1 / (1 - p_case) + (1 / p_x) + (1 / (1 - p_x))
    
    
    # method 4
    p_y <- 1 / (k + 1)
    A <- p_x / (1-p_x)
    V <- ((1 + A)^2 / (A*(1-p_y)) + (1 + A*B)^2/(A*B*p_y))
    
    
    return(V)
}


# get sample size
get_sample_size_cc <- function(alpha, beta, var, b) {
    Zalpha <- qnorm(1-alpha/2)
    Zbeta <- qnorm(beta)
    ceiling(((Zalpha + Zbeta)^2 * var) / b^2)
    #ceiling(((Zalpha + Zbeta)^2) / (var * b^2))
    
}


# get b3 sample size
get_b3_sample_size_co <- function(alpha, beta, var, b3) {
    Zalpha <- qnorm(1-alpha/2)
    Zbeta <- qnorm(beta)
    ceiling((Zalpha + Zbeta)^2 * var / b3^2)
}

get_b3_sample_size_cc <- function(alpha, beta, var, b3) {
    Zalpha <- qnorm(1-alpha/2)
    Zbeta <- qnorm(beta)
    ceiling(((Zalpha + Zbeta)^2 * var) / b3^2)
}


# get power
get_power <- function(alpha, var, b, n) {
    Zalpha <- qnorm(1-alpha/2)
    pnorm(-Zalpha + b * sqrt(n/var)) # power
}

get_b3_power_cc <- function(alpha, var, b3, n) {
    Zalpha <- qnorm(1-alpha/2)
    pnorm(-Zalpha + b3 * sqrt(n/var)) # power
}

get_b3_power <- function(alpha, var, b3, n) {
    Zalpha <- qnorm(1-alpha/2)
    pnorm(-Zalpha + b3 * sqrt(n/var)) # power
}


# get mde
get_b3_mde_co <- function(alpha, beta, var, n) {
    Zalpha <- qnorm(1-alpha/2)
    Zbeta <- qnorm(beta)
    sqrt(((Zalpha + Zbeta)^2 * var) / n) # b3=log(OR)
}

get_b3_mde_cc <- function(alpha, beta, var, n) {
    Zalpha <- qnorm(1-alpha/2)
    Zbeta <- qnorm(beta)
    sqrt(((Zalpha + Zbeta)^2 * var) / n) # b3=log(OR)
}


