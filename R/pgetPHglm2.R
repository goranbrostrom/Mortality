pgetPHglm2 <- function(dat, nper, labb, from = 40, median = TRUE){
    ## For calculating remaining life:
    ## dat$age must be a factor ...
    ## Note: return value is Cumulative hazards at given age points (straight line between).
    ##
    ## 'dat$age' is assumed to be grouped in equidistant intervals!
    source("R/mpch.R")
    res <- vector(nper, mode = "list")
    hc <- levels(dat$hisclass)
    m <- length(hc)
    k <- length(unique(dat$age))
    if (k <= 1) error("No. of age intervals must be at least 2!")
    ageName <- as.numeric(levels(dat$age))
    ageInt <- ageName[2] - ageName[1]
    levs <- matrix(0, nrow = m, ncol = k + 1)
   
    for (i in 1:nper){
        fit <- glm(event ~ offset(log(exposure)) + urban + 
                       civst +
                       age * hisclass, 
                   data = dat[dat$period == labb[i], ], family = poisson)
        
        co <- fit$coefficients
        ##if(any(is.na(co))) cat("NA in ", i, " period\n")
        ages <- grep("age", names(co))
        hisces <- grep("hisclass", names(co))
        interact <- ages[ages %in% hisces]
        amain <- co[ages[!(ages %in% interact)]]
        hmain <- co[hisces[!(hisces %in% interact)]]
        inter <- matrix(co[interact], ncol = k - 1, byrow = TRUE)
        tmp <- matrix(0, nrow = m, ncol = k)
        for (j in 1:m){
            ##cat("j = ", j, "\n")
            tmp[j, ] <- c(co[1], co[1] + amain)
        }
        for (j in 2:m){
            tmp[j, ] <- tmp[j, ] + hmain[j - 1]
        }
        tmp[-1, -1] <- tmp[-1, -1] + inter
        tmp <- exp(tmp)
        levs <- cbind(rep(0, m), tmp)
        levs[is.na(levs)] <- 0 ## HOPPSAN!!!
        for (j in 1:m){
            levs[j, ] <- cumsum(levs[j, ]) * ageInt
        }
        

        colnames(levs) <- c(ageName, ageName[k] + ageInt)
        rownames(levs) <- hc
        res[[i]] <- levs
    }
    names(res) <- labb
    ## New calc starts here:
    cuts <- as.numeric(colnames(res[[1]]))
    cuts <- cuts[-c(1, length(cuts))]
    medi <- matrix(0, ncol = nper, nrow = 4)
    cat("cuts = ", cuts, "\n")
    ##cat("levs = ", levs, "\n")
    for (i in 1:nper){
      mat <- res[[i]]
      mat <- mat[, -1]
      for (j in 1:4){
        if (median){
          medi[j, i] <- qpch(0.5, cuts = cuts - from, levels = mat[j, ])
        }else{
          medi[j, i] <- mpch(cuts = cuts - from, levels = mat[j, ])
        }
      }
    }
    ##res
    colnames(medi) <- names(res)
    rownames(medi) <- c("elite", "middle", "worker", "NA")
    medi + 40
}
