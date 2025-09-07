#' Compute basic values indices as measured in European Social Survey
#'
#' Computes in line with Shalom Schwartz's instructions on the ESS EduNet website.
#' @param data ESS dataset, or any other dataset with the same 21 value indicator variable names
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


ess_values <- function(data, v2=TRUE, v4=TRUE, v10 = TRUE, v21=FALSE, center=TRUE, abbr=FALSE, suffix="") {


  # Reverse items
  d <- suppressMessages(drop_labs(untibble(data[,values$items])))
  d <- sapply(names(d), function(x) car::Recode(d[,x], "1=6; 2=5; 3=4; 4=3; 5=3; 6=1; else=NA"))



  #Compute mean rating for centering value indices

  #Center? Compute mrat
  if(center) {

    mrat <- rowMeans(d, na.rm = TRUE)

  } else {
    mrat <- rep(0, nrow(d))

  }

  #Compute 10 basic values
  if (v10) {
    v10d <- sapply(values$codes.for.ten, function(x)
      rowMeans(d[, x], na.rm = T) - mrat)

    # assign names
    if (abbr)
      dimnames(v10d)[[2]] <- values$ten.abbr

    #add suffix
    if (suffix != "")
      dimnames(v10d)[[2]] <- paste(dimnames(v10d)[[2]], suffix, sep = "")
  }



  # Compute 4 higher-order values
  if (v4 | v2) {
    v4d <- sapply(values$codes.for.four, function(x)
      rowMeans(d[, x], na.rm = T))

    # Compute 2 higher-order value dimensions
    if (v2) {
      v2d <- data.frame(
        Conservation_Openness = v4d[, "Openness.to.Change"] - v4d[, "Conservation"],
        Self.Enhancement_Self.Transcendence = v4d[, "Self.Transcendence"] -
          v4d[, "Self.Enhancement"]
      )

      # Assign names
      if (abbr)
        names(v2d) <- values$two.abbr

    }

    # Finish with four higher order values
    if(v4) {

    # Centering
    v4d <- v4d  - mrat

    # assign abbreviated names, if requested
    if(abbr) dimnames(v4d)[[2]] <- values$four.abbr

    # add suffix, if requested
    if(suffix!="") dimnames(v4d)[[2]] <- paste(dimnames(v4d)[[2]], suffix, sep="")
    }
  }


  #Compute 21 reversed and/or centered items
  if(v21) {
    v21d <- sapply(values$items, function(item) d[,item] - mrat )

    warning(paste(values$items, collapse=", "), "were updated (centered)!")

    if(suffix=="") {
     names(v21d) <-paste(values$items, ".rev", sep="")
    } else {
     colnames(v21d) <-paste(values$items, suffix, sep="")}
  }




# Outputting

  output.vars <- c("v2d" , "v4d", "v10d", "v21d", "mrat")[c(v2, v4, v10, v21, center)]

  if(center)
    mrat<-matrix(mrat, ncol = 1, dimnames = list(NULL, "mrat"))

  d1 <- lapply(output.vars, function(x) get(x))
  d1 <- Reduce("cbind", d1)

  # Create index of missings < 17 items and delete cases
  n.new.columns <- v2 * 2 + v4 * 4 + v10 * 10 + v21 * 21
  Nmissed_index <- rowSums(is.na(d[, values$items]))
  d1[Nmissed_index > 6, 1:ncol(d1)]  <- NA
  rm(d)

  return(cbind(data, d1))

}


#' List of Schwartz values
#'
#' @author Collected from multiple publications by S.H.Schwartz and coauthors.
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
save(values, file="data/values.RData")

