#' Compute basic values indices as measured in European Social Survey
#'
#' Computed in line with Shalom Schwartz's instructions on the ESS EduNet website.
#' @param data ESS dataset of any other dataset with the same 21 value indicator variable names
#' @param v2 Two higher value dimensions
#' @param v4 Four higher order values
#' @param v10 Compute 10 value indices
#' @param v21 Add 21 raw items
#' @param center Apply within-individual centering?
#' @param abbr Logical. Use abbreviated names, e.g. SE, ST, CO..
#' @param suffix NULL of character. What is added to the names of centered indices.
#' @return  Returns a data with appended variables.
#'
#' @export

<<<<<<< HEAD
ess_values <- function(data, v2=TRUE, v4=TRUE, v10 = TRUE, v21=FALSE, center=TRUE, abbr=FALSE, suffix="") {


  # Reverse items
  d <- suppressMessages(drop_labs(untibble(data[,values$items])))
  d <- sapply(names(d), function(x) car::Recode(d[,x], "1=6; 2=5; 3=4; 4=3; 5=3; 6=1; else=NA"))

=======
ess_values <- function(data, v2=TRUE, v4=TRUE, v10 = TRUE, v21=FALSE, center=TRUE, abbr=FALSE, suffix=NULL) {

  value.items<-c("ipcrtiv",  "imprich",  "ipeqopt",  "ipshabt",  "impsafe",  "impdiff",  "ipfrule",  "ipudrst",  "ipmodst",  "ipgdtim",  "impfree","iphlppl",  "ipsuces",  "ipstrgv",  "ipadvnt",  "ipbhprp",  "iprspot",  "iplylfr",  "impenv",   "imptrad",  "impfun")

  stored.nonreversed <- data[, value.items]

  data[,value.items] <- apply(data[, value.items],  2,
                              function(var) {
                                  straight<-sort(unique(var), F)
                                  reversed<-sort(unique(var), T)
                                  sapply(var, function(x) reversed[straight==x][1] )
                              }
                      )
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5

  #Compute mean rating for centering value indices

  #Center? Compute mrat
  if(center) {
<<<<<<< HEAD
    mrat <- rowMeans(d, na.rm = TRUE)

  } else {
    mrat <- rep(0, nrow(d))
=======
    mrat <- rowMeans(data[,value.items], na.rm = TRUE)
  } else {
    mrat <- rep(0, nrow(data))
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5
  }

  #Compute 10 basic values
  if(v10) {
<<<<<<< HEAD
    # d$Security        <- with(d, rowMeans(cbind(impsafe, ipstrgv), na.rm=T)-mrat )
    # d$Conformity      <- with(d, rowMeans(cbind(ipfrule, ipbhprp), na.rm=T)-mrat )
    # d$Tradition       <- with(d, rowMeans(cbind(ipmodst, imptrad), na.rm=T)-mrat)
    # d$Benevolence     <- with(d, rowMeans(cbind(iphlppl, iplylfr), na.rm=T)-mrat)
    # d$Universalism    <- with(d, rowMeans(cbind(ipeqopt, ipudrst, impenv), na.rm=T)-mrat)
    # d$Self.Direction  <- with(d, rowMeans(cbind(ipcrtiv, impfree), na.rm=T)-mrat)
    # d$Stimulation     <- with(d, rowMeans(cbind(impdiff, ipadvnt), na.rm=T)-mrat)
    # d$Hedonism        <- with(d, rowMeans(cbind(ipgdtim, impfun), na.rm=T)-mrat)
    # d$Achievement     <- with(d, rowMeans(cbind(ipshabt, ipsuces), na.rm=T)-mrat)
    # d$Power           <- with(d, rowMeans(cbind(imprich, iprspot), na.rm=T)-mrat)

  v10d <- sapply(values$codes.for.ten, function(x) rowMeans(d[,x], na.rm=T) - mrat )


  # assign names
  if(abbr) dimnames(v10d)[[2]] <- values$ten.abbr

  #add suffix
  if(suffix!="") dimnames(v10d)[[2]] <- paste(dimnames(v10d)[[2]], suffix, sep="")

=======
    data$Security        <- with(data, rowMeans(cbind(impsafe, ipstrgv), na.rm=T)-mrat )
    data$Conformity      <- with(data, rowMeans(cbind(ipfrule, ipbhprp), na.rm=T)-mrat )
    data$Tradition       <- with(data, rowMeans(cbind(ipmodst, imptrad), na.rm=T)-mrat)
    data$Benevolence     <- with(data, rowMeans(cbind(iphlppl, iplylfr), na.rm=T)-mrat)
    data$Universalism    <- with(data, rowMeans(cbind(ipeqopt, ipudrst, impenv), na.rm=T)-mrat)
    data$Self.Direction  <- with(data, rowMeans(cbind(ipcrtiv, impfree), na.rm=T)-mrat)
    data$Stimulation     <- with(data, rowMeans(cbind(impdiff, ipadvnt), na.rm=T)-mrat)
    data$Hedonism        <- with(data, rowMeans(cbind(ipgdtim, impfun), na.rm=T)-mrat)
    data$Achievement     <- with(data, rowMeans(cbind(ipshabt, ipsuces), na.rm=T)-mrat)
    data$Power           <- with(data, rowMeans(cbind(imprich, iprspot), na.rm=T)-mrat)

  if(abbr) names(data[,values$ten]) <- values$ten.abbr
  if(!is.null(suffix) & center) names(data)[(ncol(data)-9):ncol(data)] <- paste(names(data)[(ncol(data)-9):ncol(data)], suffix, sep=".")
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5

  }



  #Compute 4 higher-order values
<<<<<<< HEAD
  if(v4|v2) {
    # d$Openness.to.Change <- with(d, rowMeans(cbind(ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun), na.rm=T) - mrat)
    # d$Conservation       <- with(d, rowMeans(cbind(impsafe, ipstrgv, ipfrule, ipbhprp, ipmodst, imptrad), na.rm=T) - mrat)
    # d$Self.Transcendence <- with(d, rowMeans(cbind(ipeqopt, ipudrst, impenv, iphlppl, iplylfr), na.rm=T) - mrat)
    # d$Self.Enhancement   <- with(d, rowMeans(cbind(imprich, iprspot, ipshabt, ipsuces), na.rm=T) - mrat)

    v4d <- sapply(values$codes.for.four, function(x) rowMeans(d[,x], na.rm=T))

    # # assign names
    # if(abbr) names(d[,values$four]) <- values$four.abbr
    # #add suffix if it is centered
    # if(!is.null(suffix) & center & !abbr) names(d)[names(d) %in% values$four] <- paste(names(d)[names(d) %in% values$four], suffix, sep=".")
    # if(!is.null(suffix) & center & abbr) names(d)[names(d) %in% values$four.abbr] <- paste(names(d)[names(d) %in% values$four.abbr], suffix, sep=".")

    #Compute 2 higher-order value dimensions
    if(v2) {
      v2d <- data.frame(Conservation_Openness = v4d[,"Openness.to.Change"]-v4d[,"Conservation"],
                        Self.Enhancement_Self.Transcendence = v4d[,"Self.Transcendence"]-v4d[,"Self.Enhancement"])

      # assign names
      if(abbr) names(v2d) <- values$two.abbr

    }

    #Finish with four higher order values
    if(v4) {

    # Centering
    v4d <- v4d  - mrat

    # assign abbreviated names, if requested
    if(abbr) dimnames(v4d)[[2]] <- values$four.abbr

    # add suffix, if requested
    if(suffix!="") dimnames(v4d)[[2]] <- paste(dimnames(v4d)[[2]], suffix, sep="")
    }
  }

  # #Compute 2 higher-order value dimensions
  # if(v2) {
  #   d$Conservation_Openness <- with(d, rowMeans(cbind(ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun), na.rm=T)
  #                                        - rowMeans(cbind(impsafe, ipstrgv, ipfrule, ipbhprp, ipmodst, imptrad), na.rm=T))
  #   d$Self.Enhancement_Self.Transcendence <- with(d, rowMeans(cbind(ipeqopt, ipudrst, impenv, iphlppl, iplylfr), na.rm=T)
  #                                                      - rowMeans(cbind(imprich, iprspot, ipshabt, ipsuces), na.rm=T))
  #
  #   if(abbr) names(d)[(ncol(d)-1):ncol(d)] <- c("Conservation_Openness", "Self_Enhancement_Self_Transcendence")
  #   }

  #Compute 21 reversed and/or centered items
  if(v21) {
    v21d <- sapply(values$items, function(item) d[,item]-mrat)
    if(suffix=="") {
     names(v21d) <-paste(values$items, ".rev", sep="")
    } else {
     colnames(v21d) <-paste(values$items, suffix, sep="")}
  }




# Outputting

  output.vars<-c("v2d" , "v4d", "v10d", "v21d", "mrat")[c(v2, v4, v10, v21, center)]
  if(center) mrat<-matrix(mrat, ncol=1, dimnames=list(NULL, "mrat"))

  d1<-lapply(output.vars, function(x) get(x) )

  d1<-Reduce("cbind",  d1)

  # Create index of missings < 17 items and delete cases
  n.new.columns<-v2*2+ v4*4 + v10*10 + v21*21
  Nmissed_index <- rowSums(is.na(d[,values$items]))
  #print(paste(sum(d$Nmissed_index>6), "scores of value indices were deleted"))
  #verb("aLL", (ncol(d)-n.new.columns):(ncol(d)), "are set missing")
  d1[Nmissed_index>6, 1:ncol(d1)]  <- NA
  #d$Nmissed_index <-NULL
  #d$mrat <-NULL
  rm(d)

  return(cbind(data, d1))
=======
  if(v4) {
    data$Openness.to.Change <- with(data, rowMeans(cbind(ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun), na.rm=T) - mrat)
    data$Conservation       <- with(data, rowMeans(cbind(impsafe, ipstrgv, ipfrule, ipbhprp, ipmodst, imptrad), na.rm=T) - mrat)
    data$Self.Transcendence <- with(data, rowMeans(cbind(ipeqopt, ipudrst, impenv, iphlppl, iplylfr), na.rm=T) - mrat)
    data$Self.Enhancement   <- with(data, rowMeans(cbind(imprich, iprspot, ipshabt, ipsuces), na.rm=T) - mrat)

  if(abbr) names(data[,values$four]) <- values$four.abbr
  if(!is.null(suffix) & center) names(data)[(ncol(data)-3):ncol(data)] <- paste( names(data)[(ncol(data)-3):ncol(data)], suffix, sep=".")
    }

  #Compute 2 higher-order value dimensions
  if(v2) {
    data$Conservation_Openness <- with(data, rowMeans(cbind(ipcrtiv, impfree, impdiff, ipadvnt, ipgdtim, impfun), na.rm=T)
                                         - rowMeans(cbind(impsafe, ipstrgv, ipfrule, ipbhprp, ipmodst, imptrad), na.rm=T))
    data$Self.Enhancement_Self.Transcendence <- with(data, rowMeans(cbind(ipeqopt, ipudrst, impenv, iphlppl, iplylfr), na.rm=T)
                                                       - rowMeans(cbind(imprich, iprspot, ipshabt, ipsuces), na.rm=T))

    if(abbr) names(data)[(ncol(data)-1):ncol(data)] <- c("Conservation_Openness", "Self_Enhancement_Self_Transcendence")
    }

  #Compute 21 centered items
  if(v21) {
    v21centered <- sapply(value.items, function(item) data[,item]-mrat)
    if(!is.null(suffix) & center) {
      names(v21centered) <-paste(value.items, suffix, sep=".")
    } else {
    names(v21centered) <-paste(value.items, "centered", sep=".")
    }

    data<- cbind(data, v21centered)

  }

  # Create index of missings < 17 items and delete cases
  n.new.columns<-v2*2+ v4*4 + v10*10 + v21*21
  Nmissed_index <- rowSums(is.na(data[,value.items]))
  #print(paste(sum(data$Nmissed_index>6), "scores of value indices were deleted"))
  #verb("aLL", (ncol(data)-n.new.columns):(ncol(data)), "are set missing")
  data[Nmissed_index>6, (ncol(data)-n.new.columns+1):(ncol(data))] <- NA
  #data$Nmissed_index <-NULL
  #data$mrat <-NULL

  data[,value.items] <- stored.nonreversed

  data
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5

}



#' List of Schwartz values
#'
#' @author Collected using multiple publications by S.H.Schwartz and coauthors.
"values"

values <- list(
  ten= c("Security", "Conformity", "Tradition",
            "Benevolence", "Universalism",
            "Self.Direction", "Stimulation",
            "Hedonism", "Achievement", "Power"),
  ten.abbr =  c("SE", "CO", "TR", "BE", "UN", "SD", "ST", "HE", "AC", "PO"),
<<<<<<< HEAD

  four =      c("Openness.to.Change", "Conservation", "Self.Transcendence", "Self.Enhancement"),
  four.abbr = c("Openness", "Conserv", "Self_Trans", "Self_Enhance"),

  two = c("Conservation_Openness", "Self.Enhancement_Self.Transcendence"),
  two.abbr = c("Conservation_Openness", "Self_Enhancement_Self_Transcendence"),

  items=c("ipcrtiv",  "imprich",  "ipeqopt",  "ipshabt",  "impsafe",  "impdiff",  "ipfrule",  "ipudrst",  "ipmodst",  "ipgdtim",  "impfree","iphlppl",  "ipsuces",  "ipstrgv",  "ipadvnt",  "ipbhprp",  "iprspot",  "iplylfr",  "impenv",   "imptrad",  "impfun"),

  codes.for.ten = list(Security=c("impsafe", "ipstrgv"),
                       Conformity=c("ipfrule", "ipbhprp"),
                       Tradition=c("ipmodst", "imptrad"),
                       Benevolence=c("iphlppl", "iplylfr"),
                       Universalism=c("ipeqopt", "ipudrst", "impenv"),
                       Self.Direction=c("ipcrtiv", "impfree"),
                       Stimulation=c("impdiff", "ipadvnt"),
                       Hedonism=c("ipgdtim", "impfun"),
                       Achievement=c("ipshabt", "ipsuces"),
                       Power=c("imprich", "iprspot")
                       ),
  codes.for.four = list(Openness.to.Change=c("ipcrtiv", "impfree","impdiff", "ipadvnt", "ipgdtim", "impfun"),
                        Conservation = c("impsafe", "ipstrgv", "ipfrule", "ipbhprp", "ipmodst", "imptrad"),
                        Self.Transcendence=c("iphlppl", "iplylfr", "ipeqopt", "ipudrst", "impenv"),
                        Self.Enhancement = c("ipshabt", "ipsuces", "imprich", "iprspot")
                        )
   )
=======
  four =      c("Openness.to.Change", "Conservation", "Self.Transcendence", "Self.Enhancement"),
  four.abbr = c("Openness", "Conserv", "Self_Trans", "Self_Enhance"),
  two = c("Conservation_Openness", "Self.Enhancement_Self.Transcendence"),
  two.abbr = c("Conservation_Openness", "Self_Enhancement_Self_Transcendence"),
  items=c("ipcrtiv",  "imprich",  "ipeqopt",  "ipshabt",  "impsafe",  "impdiff",  "ipfrule",  "ipudrst",  "ipmodst",  "ipgdtim",  "impfree","iphlppl",  "ipsuces",  "ipstrgv",  "ipadvnt",  "ipbhprp",  "iprspot",  "iplylfr",  "impenv",   "imptrad",  "impfun"))
>>>>>>> 3ba7ee8f1e8c681e900d55fc66ae9042bcc8c4f5
save(values, file="data/values.RData")

