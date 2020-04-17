#' Create a path diagram using lavaan syntax
#'
#' @description Converts lavaan syntax or object to graphviz code. Requires 'DiagrammeR' package.
#' @param m character, following lavaan syntax model conventions (see examples), or fitted lavaan object.
#' @param file character, file name to save svg code, usually with 'svg' extension.
#' @param rmarkdown Logical. If the function used in the context of Rmarkdown.
#' @param ... arguments passed to DiagrammeR::grViz function.
#'
#' @examples lav_to_graph("F =~ a1 + a2 + a3 + a4")
#' lav_to_graph("F =~ a1 + a2 + a3 + a4; a1 ~~ a2; d ~ F ", engine = "neato")
#' @return The function invisibly returns the dot code which can be edited to make more customized diagrams. You can use package \pkg{DiagrammeR} or any other graphviz engine, e.g. \url{http://graphviz.it}. It will most likely to be useful with large and complex models.
#'
#' @export
lav_to_graph <- function(m, layout = "dot", adds=NULL, file=NA, rmarkdown=FALSE, ...) {

  if(class(m)=="lavaan") {
    pt <- lavaan::parameterTable(m)
    pt <- pt[pt$rhs!="",]
    pt <- pt[pt$op!=":=", ]
    message("Currently, intercepts are not supported.")
    st<-abs(pt$est/pt$se)>1.96 & pt$free != 0
    st[st==T]<-"^^"
    st[st=="FALSE"]<-""
    m <- paste0(pt$lhs, " ", pt$op, " ", sprintf("%1.2f",pt$est), st, "*", pt$rhs,
                collapse=";\n")
  }

    # m <- "F =~ a1 + a2 + a3 ;
    #                      F2 =~ b1 + b3 + NA*hh;
    #       b1 + b3 ~~ 0*a3 + NA*a2 + G;"
  m <- gsub(" +", " ", m)
  m <- gsub("\\+ *\n", " + ", m)
  m <- gsub(";", "\n", m)
  m <- gsub("\n\n", "\n", m)
  m <- gsub("\n ", "\n", m)
  m <- gsub(" \n ", "\n", m)
  m <- gsub("  ", " ", m)

  m<-strsplit(m, "\n")[[1]]
  m<- gsub("#.*", "", m)
  m <- m[!m==""]

  all.vars <- unname(unlist(sapply(m, strsplit, "=~|~~|~|\\+")))
  all.vars <- gsub(" ", "", all.vars)
  all.vars <- gsub("^.*\\*", "", all.vars)
  all.vars <- all.vars[!duplicated(all.vars)]
  all.vars <- all.vars[all.vars!=1]

  all.factors <- sapply(m[grepl("^.*=~", m)], function(x) strsplit(x, "=~")[[1]][1] )
  all.factors <- unname(gsub(" ", "", all.factors))


  all.indicators <- sapply(m[grepl("^.*=~", m)], function(x) strsplit(x, "=~")[[1]][2] )
  m<-m[!grepl("^.*=~", m)]
  m <- append(unlist(lapply(unique(all.factors), function(x) paste(x, "=~", paste(all.indicators[all.factors==x], collapse=" + ")    ))), m)

  if(length(all.factors)!=0) {
   all.indicators <- unlist(strsplit(all.indicators, "\\+"))
   all.indicators<- gsub(" ", "", all.indicators)
   all.indicators <- unname(gsub("^.*\\*", "", all.indicators))
  }
  observed.vars <- all.vars[!all.vars %in% all.factors]

  regr <- m[grepl("~", m) & !grepl("~~|=~", m)]
  dep.regr.vars <- gsub("~.*$", "", regr)
  dep.regr.vars <- gsub(" ", "", dep.regr.vars)

  endogenous.vars <- c(all.indicators, dep.regr.vars)
  exogenous.vars <- all.vars[!all.vars %in% endogenous.vars]





  all.correlated <- sapply(m[grepl("^.*~~", m)], function(x) strsplit(x, "~~")[[1]] )
  if(length(all.correlated)>0) {
  all.correlated <- apply(all.correlated, 2, function(x) {
    a = strsplit(x, "\\+")
    a = lapply(a, function(u) gsub(" ", "",u))
    a = expand.grid(a,  stringsAsFactors = F)
    a$constraint<- sapply(a[,2], function(u) ifelse(grepl("\\*", u), sub("\\*.*", "", u), NA))
    a[,2]<-gsub("^.*\\*", "",a[,2])
    a[,3][a[,3]=="NA"]<-NA
    a
    })

   all.correlated <- Reduce("rbind", all.correlated)
   all.exog.cors <- expand.grid(exogenous.vars, exogenous.vars,  stringsAsFactors = F)


   # from all.exog.cors  exclude  all.correlated

   excl.corr <- apply(all.exog.cors, 1, function(x) {

     (x[1] %in% all.correlated$Var1) & (x[2] %in% all.correlated$Var2) |
     (x[2] %in% all.correlated$Var1) & (x[1] %in% all.correlated$Var2)
   })

   all.exog.cors <- all.exog.cors[!excl.corr,]

  } else {
    all.exog.cors <- expand.grid(exogenous.vars, exogenous.vars,  stringsAsFactors = F)
  }

  all.exog.cors <- all.exog.cors[!all.exog.cors[,1]==all.exog.cors[,2],]

  if(length(unlist(all.exog.cors))>0) {

  excl.dupl.corr <- sapply( 1:nrow(all.exog.cors), function(x) which(
    (all.exog.cors[x,2] == all.exog.cors[x:nrow(all.exog.cors),1]) &
      (all.exog.cors[x,1] == all.exog.cors[x:nrow(all.exog.cors),2]))+x-1)
  all.exog.cors <- all.exog.cors[ -unlist(excl.dupl.corr),]
  }

  # all.factors.cors.excl <- apply(all.factors.cors,1, function(x) any(apply(all.correlated, 1, function(u) {
  #   sum(u[1:2] == x)==2 & u[3]==0 | sum(u[2:1] == x)==2 & u[3]==0
  # })))
  #
  # all.factors.cors <-all.factors.cors[(!all.factors.cors.excl) &
  #                                       all.factors.cors[,1]!=all.factors.cors[,2],]
  #
  #


  lines <-
    sapply(m, function(i) {

      if(grepl("=~", i)) {
        m0 <- strsplit(i, "=~")[[1]]
        factr <- gsub(" ", "", m0[1])
        factr  <- `names<-`(gsub("\\.|-", "_", factr), factr) #values are dotless, names are original varnames

        indicators <- strsplit(m0[2], "\\+")[[1]]
        indicators <- gsub(" ", "", indicators)

        #params <- rep("", length(indicators))

        params = gsub("\\*.*$", "", indicators[grep("\\*", indicators)])
        params = gsub("NA", "", params)
        params[params!=""] <- paste0('[label="',params,'"]')[params!=""]

        indicators <- gsub("^.*\\*", "", indicators)
        indicators <- `names<-`(gsub("\\.|-", "_", indicators), indicators) #values are dotless, names are original varnames

        c(paste0("\n\nsubgraph cluster_", factr, '  {\n  color = white;\n'),
          paste0("\t", factr, ' [shape = ellipse label = ', paste0('"', names(factr), '"'), '];\n'),
          if(any(!names(indicators) %in% all.factors)) paste0("\t",
                                                      indicators[!names(indicators) %in% all.factors],
                                                      '[shape = rect label = ', paste0('"', names(indicators), '"'), '];\n'),

          paste(paste0("\tResid_", indicators),
                '[shape = circle style = filled color=lightgrey',
                'fontsize = 10 width= 0.2 label = "&epsilon;"];\n',
                collapse = ""),
          paste("\t{rank = ", ifelse(names(factr) %in% exogenous.vars, "min", "max"),
                paste(paste0("Resid_",indicators), collapse = " "), "};\n"),
          paste("\t{rank = ", ifelse(names(factr) %in% exogenous.vars, "max", "min"),
                factr, "};\n"),

          paste0("\t",  factr, " -> ",  indicators, params, ";\n"),

          paste0("\t", paste0("Resid_",indicators), " -> ", indicators, "[color=grey];\n"),
          paste("}\n"),
          paste(" // end of ", names(factr), "measurement model.",
                names(factr), " is ", ifelse(names(factr) %in% exogenous.vars, "exogenous.\n\n", "endogenous.\n\n"))
        )

      } else if(grepl("(~~)", i)) {

        m0  <- strsplit(i, "~~")[[1]]
        lhs <- strsplit(m0[1], "\\+")[[1]]
        lhs <- gsub(" ", "", lhs)
        lhs <- gsub("^.*\\*", "", lhs)
        lhs <- `names<-`(gsub("\\.|-", "_", lhs), lhs) #values are dotless, names are original varnames


        rhs <- strsplit(m0[2], "\\+")[[1]]
        rhs <- gsub(" ", "", rhs)
        pars <- rep(NA, length(rhs))
        pars[grep("\\*", rhs)] <- gsub("\\*.*$", "", rhs[grep("\\*", rhs)])

        # paste0("[label=",
        #        gsub("\\*.*$", "", rhs[grep("\\*", rhs)]),
        #        "]")

        rhs <-gsub("^.*\\*", "", rhs)
        rhs <- `names<-`(gsub("\\.|-", "_", rhs), rhs) #values are dotless, names are original varnames

        # turn to residuals if variables are indicators
        lhs[names(lhs) %in% all.indicators] <- paste0("Resid_", lhs[names(lhs) %in% all.indicators])
        rhs[names(rhs) %in% all.indicators] <- paste0("Resid_", rhs[names(rhs) %in% all.indicators])


       covs <- data.frame(lhs=unname(lhs), rhs=unname(rhs), pars, stringsAsFactors = F)

       sapply(1:nrow(covs), function(co) {
         co <- covs[co,]
         if(co$lhs == co$rhs) {
           paste(
             paste0(co$rhs, '[xlabel="&sigma;&sup2;&#61;', as.character(co$pars), '"];\n'),
             ifelse(co$pars==0, paste0(co$rhs, '[style=dashed];\n'),"")
           )

         } else  {


             if(!is.na(co$pars)) {
               if(co$pars!=0)
               paste(co$lhs, "->", co$rhs,
                     '[ dir = "both" splines=curved constraint=false label="', co$pars, '" fontsize = 10 ];\n')
             } else {
               paste(co$lhs, "->", co$rhs,
                     '[ dir = "both" splines=curved constraint=false];\n')

               }
         }
       })

      } else if(grepl("~", i) & !grepl("~~|=~", i)) {




        m0 <- strsplit(i, "~")[[1]]
        dep <- strsplit(m0[1], "\\+")[[1]]
        dep <- gsub(" ", "", dep)
        dep<-dep[!dep==""]
        dep  <- `names<-`(gsub("\\.|-", "_", dep), dep) #values are dotless, names are original varnames

        indep <- strsplit(m0[2], "\\+")[[1]]
        indep <- gsub(" ", "", indep)
        indep<-indep[!indep==""]
        cof = rep("", length(indep))
        cof[grep("\\*", indep)] <- gsub("\\*.*$", "", indep[grep("\\*", indep)])
        indep <- gsub("^.*\\*", "", indep)
        indep  <- `names<-`(gsub("\\.|-", "_", indep), indep) #values are dotless, names are original varnames

        intercepts <- indep[indep == 1]
        indep      <- indep[indep != 1]
        reg.otp<-"// Regressions and intercepts \n"
        if(length(indep)>0) {
         reg.otp <- append(reg.otp, c(
              paste(dep, "[label=", paste0('"', names(dep), '"'), "];\n"),
              paste(indep, "[label=", paste0('"', names(indep), '"'), "];\n"),
              paste0(indep, " -> ",  dep, ' [style=bold ', paste0('label="',cof)[cof!=""],'"];\n'),
              paste(paste0("Resid_",dep), ' [shape = circle style = filled color=lightgrey',
                    'fontsize=10 width=0.2, label = "&epsilon;"];\n'),
              paste0(paste0("Resid_",dep), " -> ", dep, " [color=grey];\n")
            ))
        }

        if(length(intercepts)>0) {
          reg.otp <- append(reg.otp,
           c(paste(dep, "[label=", paste0('"', names(dep), '"'), "];\n"),

            paste0(paste0('intercept_', dep),
                  ' [label="&tau;', ifelse(length(cof)!=0, paste0('&#61;', cof), ''),
                  '" shape=triangle', ifelse(cof==0, ' style=dashed', ' style=filled'),
                  '  color=lightgrey];\n'),
            paste0(paste0('intercept_', dep), ' -> ',  dep, ' [color=lightgrey];\n')
          ))
        }

        reg.otp


      } else  { # if(i!="")
        warning("Unknown operator found in line: ", i)
      }
    })

  # Add covariances between exogenous vars
  lines <- append(lines, c("\n\n// Add covariances of all exog variables\n",
                  if(nrow(all.exog.cors)>1) {
                    all.exog.cors  <- sapply(all.exog.cors, function(u) gsub("\\.|-", "_", u))
                  apply(all.exog.cors,
                        1,
                        function(x) paste(x[1], "->", x[2],
                                          '[dir = "both" splines=curved constraint=false color=grey];\n')
                        )
                  } else if(length(unlist(all.exog.cors))>0) {
                    paste(all.exog.cors[1], "->", all.exog.cors[2], '[dir = "both" splines=curved constraint=false color=grey];\n')


                  } else {

                    paste("")
                  }
                  ))

  lines <- unlist(lines)
  excl.lines <- duplicated(lines)
  excl.lines [ lines == "}\n"] <- FALSE
  lines <- lines[!excl.lines]

  lines <- paste("digraph lav_to_graph {",
                  " node [shape=rect];",
                 " rankdir=LR;",
                 paste0(" layout=", layout, ";"),
                 " forcelabels=true;",
                 paste0(lines, collapse = ""),
                 ifelse(is.null(adds), "", paste("// code added manually \n", paste(adds, collapse="\n" ))),
                 "}",
                 sep="\n")

  lines <- gsub("^^", "*", lines, fixed = T)

if(!rmarkdown) {
  trash<- capture.output(DiagrammeR::grViz(lines, ...))
  #cat(lines)
  if(!is.na(file)) {
    require("DiagrammeRsvg")
    cat(DiagrammeRsvg::export_svg(DiagrammeR::grViz(lines, ...)), file=file)
  }
  invisible(lines)
} else {
  DiagrammeR::grViz(lines, ...)
  }

}
# a=Sys.time()
# lav_to_graph(mdl, adds = "rankdir='LR'")
# Sys.time()-a
# # #
#
# lav_to_graph("a ~ b; b ~ c; c ~ d; a ~ d0 ", adds = "rankdir='BT'")
#
#
# ess7 <- haven::read_sav("~/Dropbox/STAT/European Social Survey/data/ESS7/ESS7e02_1.sav")
# Austria <- ess7[ess7$cntry == "AT",]
#
# library(lavaan)
# cfa1 <- cfa( 'F1 =~ ipadvnt + impfun + impdiff + ipgdtim;
#              F2 =~  ipcrtiv+ impfree;
#              impfun ~~ ipgdtim;
#              ', data=Austria)
#
# lav_to_graph(cfa1)
#
# mdl <- "institutionalized =~ NA*contact+ NA*workorg;
# non.institutionalized =~ NA*petition + NA*demonstr + NA*boycott;
# institutionalized ~~ 1*institutionalized;
# non.institutionalized  ~~ 1*non.institutionalized;
# associationalism =~ NA*voluntary + NA*participate; associationalism~~1*associationalism;
# closeness =~ 1*close.party; close.party ~~ 0*close.party;
# social.trust =~ NA*trust + NA*fairness; social.trust ~~ 1*social.trust;
# information =~ NA*tv + NA*radio; information ~~ 1*information;
# pol.interest =~ 1*interest; interest ~~ 0*interest;
# sociability =~ 1*soc.activity; soc.activity ~~ 0*soc.activity;
# institutionalized ~ associationalism + closeness + social.trust +
#     information + pol.interest + sociability + non.institutionalized;
#  non.institutionalized ~ associationalism + closeness + social.trust +
#     information + pol.interest + sociability + institutionalized
#     institutionalized ~~ 0*institutionalized
#     institutionalized ~ 0*1
# "
# lav_to_graph(mdl, file = "~/Downloads/hopahopa.svg")
#
# lav_to_graph('F =~ 1*a1 + 2*a2 + NA*a3
#     F2 =~ a4 + a5 + a6
#     G =~ NA*a1 + a2 + a3 + a4 + a5 + a6;
#     G ~~ 0*G;
#     a1~~ 0.5*a2;
#     a1 ~~ 5*a1;
#     G ~ 1*F2;', adds="splines=false;")


#' Combine fit measures form several models and compare
#' @details  Extracts select fit measures from lavaan objects and lists it in the tables.
#' @param ... lavaan fitted objects
#'
#' @examples lav_compare(fit1, fit2, fit3)
#'
#'
#' @export
lav_compare <- function(...) {


  out <- lavaan::lavTestLRT(..., model.names = as.character(substitute(...()) ) )

  out2<-t(sapply(list(...),   fitmeasures)[c("cfi", "tli", "rmsea", "srmr", "bic"),])
  rownames(out2)<-as.character(substitute(...()) )


  print(out)
  cat("\n")
  print(round(out2, 3), digits=3, row.names = TRUE, na.print = "" )

  invisible(list(LRT=out, Other.fit=out2))
}


