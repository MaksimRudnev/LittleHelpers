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

  #Compute mean rating for centering value indices

  #Center? Compute mrat
  if(center) {
    mrat <- rowMeans(data[,value.items], na.rm = TRUE)
  } else {
    mrat <- rep(0, nrow(data))
  }

  #Compute 10 basic values
  if(v10) {
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

  }



  #Compute 4 higher-order values
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
  four =      c("Openness.to.Change", "Conservation", "Self.Transcendence", "Self.Enhancement"),
  four.abbr = c("Openness", "Conserv", "Self_Trans", "Self_Enhance"),
  two = c("Conservation_Openness", "Self.Enhancement_Self.Transcendence"),
  two.abbr = c("Conservation_Openness", "Self_Enhancement_Self_Transcendence"),
  items=c("ipcrtiv",  "imprich",  "ipeqopt",  "ipshabt",  "impsafe",  "impdiff",  "ipfrule",  "ipudrst",  "ipmodst",  "ipgdtim",  "impfree","iphlppl",  "ipsuces",  "ipstrgv",  "ipadvnt",  "ipbhprp",  "iprspot",  "iplylfr",  "impenv",   "imptrad",  "impfun"))
save(values, file="data/values.RData")

