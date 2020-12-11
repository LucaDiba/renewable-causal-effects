#! /usr/bin/env Rscript
library(VLTimeCausality)
library(lmtest)

#build my experiment
seedVal <- -1
time_series_length <- 300

#custom eval function because the original doesn't consider dim
check_simulation <- function(true, pred) {
    TP <- 0
    FP <- 0
    FN <- 0
    for (i in seq(dim(true)[1])) {
        for (j in seq(dim(true)[2])) {
	    if(true[i,j] && pred[i,j])
		TP <- TP+1
	    else if( (!true[i,j]) && pred[i,j])
		FP <- FP+1
	    else if(true[i,j] && (!pred[i,j]) )
		FN <- FN+1
	}
    }
    prec <- TP/(TP+FP)
    rec <- TP/(TP+FN)
    F1 <- 2*prec*rec/(prec+rec)
    return(list(prec=prec,rec=rec,F1=F1))
}

make_simple_sim <- function(i, n=100, lag=1, yf=110, ye=170) {

    A <- VLTimeCausality::SimpleSimulationVLtimeseries(
        n=n,
        arimaFlag = TRUE,
        causalFlag = TRUE,
        lag=lag,
        YstFixInx=yf,
        YfnFixInx=ye,
        seedVal = i
    )
    return(A)
}

#helper function for computing plain granger
granger_plain <- function(TS, family = gaussian, alpha=0.01, gamma=0.5) {
    max_dim <-dim(TS)[2]
    adjMat <- matrix(FALSE, max_dim, max_dim) #row causes column

    for (first_i in 1:max_dim) {
        for(first_j in 1:max_dim) {
            if (first_i != first_j) {
                Y <- TS[, first_j]
                maxLag <- 0.2*length(Y)
                X <- TS[, first_i]
                YX<-cbind(ts(Y),ts(X))
                D <- YX

                # Create time-shift vesions of y and x (y(t),x(t),y(t-1),x(t-1),...)
                for(q in 1:maxLag)
                    D <-ts.intersect(D, lag(YX,  - q))

                y  <- D[, 1]
                n  <- length(Y)
                xyPast <- D[,  - (1:2)] # delete two targted columns (leave only y past and x past)
                yPast <- xyPast[, ((1:maxLag) * 2) - 1] # delete all x columns (leave only y past)
                H1 <- glm(y ~ xyPast,family=family)
                H0 <- glm(y ~ yPast,family=family)
                S1 <- sum(H1$resid^2)
                S0 <- sum(H0$resid^2)

                ftest <- ((S0 - S1)/maxLag)/(S1/(n - 2 * maxLag - 1))
                pval <- 1 - pf(ftest, maxLag, n - 2 * maxLag - 1)
                BIC_H0<-(S0/n)*n^( (maxLag+1)/n ) # less value is better
                BIC_H1<-(S1/n)*n^( (2*maxLag+1)/n ) # less value is better
                if( (pval<=alpha) )
                    XgCsY_ftest=TRUE
                XgCsY_BIC<- ( (BIC_H1<BIC_H0) )

                BICDiffRatio<-(BIC_H0-BIC_H1)/BIC_H0
                #adjMat[first_i, first_j]<- (BICDiffRatio>=gamma)
                adjMat[first_i, first_j]<- pval <= alpha
            }
        }
    }
    res <- list(adjMat=adjMat)
    return (res)
}

make_exp_lag <- function(time_series_length=200, lag=5) {
    #set some seed stuff
    if(seedVal ==-1) {
        seeds<-numeric(4)-1
    }
    else {
        set.seed(seedVal)
        seeds<-runif(4,1000,250000)
    }

    TS <- matrix(0, time_series_length, 9) #12 nodes

    A = make_simple_sim(seeds[1], n=time_series_length, lag=lag)
    B = make_simple_sim(seeds[2], n=time_series_length, lag=lag+4)
    C = make_simple_sim(seeds[3], n=time_series_length, lag=lag+6)
    D = make_simple_sim(seeds[4], n=time_series_length, lag=lag+8)

    TS[,1] <- A$X
    TS[,2] <- B$X
    TS[,3] <- C$X
    TS[,4] <- D$X
    TS[,5] <- A$Y + B$Y
    TS[,6] <- A$Y
    TS[,7] <- A$Y + B$Y + C$Y
    TS[,8] <- C$Y
    TS[,9] <- TS[,5] + C$Y + D$Y

    #now GT
    G <- matrix(FALSE, 9, 9)
    G[1, c(5, 6, 7)] <- TRUE
    G[2, c(5, 7)] <- TRUE
    G[3, c(7, 8, 9)] <- TRUE
    G[4, c(9)] <- TRUE
    G[5, c(9)] <- TRUE

    #finally get metrics
    out <- VLTimeCausality::multipleVLTransferEntropy(TS)
    #out <- granger_plain(TS)
    final <- check_simulation(G, out$adjMat)
    return (final)
}

make_exp_nolag <- function(time_series_length=200, lag=5) {
    #set some seed stuff
    if(seedVal ==-1) {
        seeds<-numeric(4)-1
    }
    else {
        set.seed(seedVal)
        seeds<-runif(4,1000,250000)
    }

    TS <- matrix(0, time_series_length, 9) #12 nodes

    A = make_simple_sim(seeds[1], n=time_series_length, lag=lag+4, yf=200, ye=200)
    B = make_simple_sim(seeds[1], n=time_series_length, lag=lag+6, yf=200, ye=200)
    C = make_simple_sim(seeds[1], n=time_series_length, lag=lag+8, yf=200, ye=200)
    D = make_simple_sim(seeds[1], n=time_series_length, lag=lag+9, yf=200, ye=200)

    TS[,1] <- A$X
    TS[,2] <- B$X
    TS[,3] <- C$X
    TS[,4] <- D$X
    TS[,5] <- A$Y + B$Y
    TS[,6] <- A$Y
    TS[,7] <- A$Y + B$Y + C$Y
    TS[,8] <- C$Y
    TS[,9] <- TS[,5] + C$Y + D$Y

    #now GT
    G <- matrix(FALSE, 9, 9)
    G[1, c(5, 6, 7)] <- TRUE
    G[2, c(5, 7)] <- TRUE
    G[3, c(7, 8, 9)] <- TRUE
    G[4, c(9)] <- TRUE
    G[5, c(9)] <- TRUE

    #finally get metrics
    out <- VLTimeCausality::multipleVLTransferEntropy(TS)
    #out <- granger_plain(TS)
    final <- check_simulation(G, out$adjMat)
    return (final)
}

num_experiments = 30
base = matrix(0, 3, num_experiments)
new = matrix(0, 3, num_experiments)

print('Running Experiments')
for (i in seq(num_experiments)) {
    print(i)
    base_result = make_exp_nolag()
    base[1, i] = base_result$prec
    base[2, i] = base_result$rec
    base[3, i] = base_result$F1

    new_result = make_exp_lag()
    new[1, i] = new_result$prec
    new[2, i] = new_result$rec
    new[3, i] = new_result$F1
}

library(dplyr)
library(matrixStats)

print("Indiviual Stats")
print(rowMeans(base))
print(rowSds(base))
print(rowMeans(new))
print(rowSds(new))

diff_f1 = base[,3] - new[,3]
print(mean(diff_f1))
print(sd(diff_f1))

#how to run the base experiment
#G <- matrix(FALSE, 10, 10)
#G[1, c(4, 7, 8, 10)] <- TRUE
#G[2, c(5, 7, 9, 10)] <- TRUE
#G[3, c(6, 8, 9, 10)] <- TRUE
#
#TS <- VLTimeCausality::MultipleSimulationVLtimeseries()
#out <- VLTimeCausality::multipleVLGrangerFunc(TS)
