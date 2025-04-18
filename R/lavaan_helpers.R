#' Create a Graphviz path diagram using lavaan syntax
#'
#' @description Converts lavaan syntax or object to graphviz code. Requires 'DiagrammeR' package.
#' @param m character, following lavaan syntax model conventions (see examples), or fitted lavaan object.
#' @param layout Can be 'dot', 'neato', 'twopi', 'circo', or 'fdp'
#' @param file character, file name to save svg code, usually with 'svg' extension.
#' @param rmarkdown Logical. If the function used in the context of Rmarkdown.
#' @param adds Any graphviz code to be added to the graph.
#' @param try.labels Try extracting labels from the data.
#' @param label.wrap Number of character to wrap a label
#' @param std Whether include standardized or unstandardized estimates.
#' @param elements Character vector. What to include in the diagram, possible values "obs", "lat.covs", "resid.covs", "resid", "slopes", and "intercepts/thresholds" (currently not supported).
#' @param ... arguments passed to DiagrammeR::grViz function.
#'
#' @examples lav_to_graph("F =~ a1 + a2 + a3 + a4")
#' lav_to_graph("F =~ a1 + a2 + a3 + a4; a1 ~~ a2; d ~ F ", engine = "neato")
#' @return The function invisibly returns the dot code which can be edited to make more customized diagrams. You can use package \pkg{DiagrammeR} or any other graphviz engine, e.g. \url{http://graphviz.it}. It will most likely to be useful with large and complex models.
#'
#' @export
lav_to_graph <- function(m, layout = "dot",
                         adds=NULL,
                         file=NA,
                         rmarkdown=FALSE,
                         code.only = F,
                         try.labels = T,
                         label.wrap = 15,
                         std=F,
                         elements = c("obs", "lat.covs", "resid.covs", "resid", "slopes"),
                         ...) {
 # elements = c("slopes")
  resid.prefix <- ifelse("resid" %in% elements, "Resid_", "")
  m.original <- m

  if(class(m)=="lavaan") {
   # pt <- lavaan::parameterTable(m)

    if(std) {
      pt <- lavaan::standardizedSolution(m)
      pt$est= pt$est.std

    } else {
      pt <- lavaan::parameterEstimates(m)
    }

    # remove thresholds
    pt <- pt[pt$op!="|",]
    # remove scaling factors
    pt <- pt[pt$op!="~*~",]

    pt <- pt[pt$rhs!="",]
    pt <- pt[pt$op!=":=", ]
    if(any(elements %in% "intercepts/thresholds" )) message("Currently, intercepts/thresholds are not supported.")
    st<-abs(pt$est/pt$se)>1.96 & pt$se != 0
    st[st==T]<-"^^"
    st[st=="FALSE"]<-""
    m <- paste0(pt$lhs, " ", pt$op, " ", sprintf("%1.2f",pt$est), st, "*", pt$rhs,
                collapse=";\n")
} else {

  pt <- lavaanify(m)
  pt$est= pt$label
  # remove thresholds
  pt <- pt[pt$op!="|",]
  # remove scaling factors
  pt <- pt[pt$op!="~*~",]

  pt <- pt[pt$rhs!="",]
  pt <- pt[pt$op!=":=", ]
  if(any(elements %in% "intercepts/thresholds" )) message("Currently, intercepts/thresholds are not supported.")
  m <- paste0(pt$lhs, " ", pt$op, " ", ifelse(pt$free==0, paste0(pt$ustart, "*"), ""), pt$rhs,
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
  m<- m[!grepl(":=",  m)] # remove constructed
  m <- m[!m==""]


  all.vars <- unname(unlist(sapply(m, strsplit, "=~|~~|~|\\+")))
  all.vars <- gsub(" ", "", all.vars)
  all.vars <- gsub("^.*\\*", "", all.vars)
  all.vars <- all.vars[!duplicated(all.vars)]
 # all.vars <- all.vars[all.vars!=1]
  all.vars <- all.vars[all.vars!=""]

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

        # assign labels to OVs
        if (class(m.original) == "lavaan" & try.labels) {
          message("Trying variable labels")
          d <- eval(lavInspect(m.original, "call")$data, .GlobalEnv)
          ind.labels <- sapply(indicators,
                               function(v)
                                 ifelse(is.null(attr(d[, v], "label", exact = T)),
                                        v,
                                        attr(d[, v], "label", exact = T)
                                        )
                               )
          ind.labels <- gsub("'", "&#39;", ind.labels) #000A new line &#94;
          ind.labels <- stringr::str_wrap(ind.labels, label.wrap)
          rm(d)


        } else {
          #message("Not even trying")
          ind.labels <- indicators

        }

        #values are dotless
        indicators <- gsub("\\.|-", "_", indicators)

        #names are original varnames/original labels
        names(indicators) <- ind.labels



if( ("obs" %in% elements)| any(names(indicators) %in% all.factors)) {
        c(paste0("\n\nsubgraph cluster_", factr, '  {\n  color = white;\n'),
          paste0("\t", factr, ' [shape = ellipse label = ', paste0('"', names(factr), '"'), '];\n'),
          if(any(!names(indicators) %in% all.factors)) paste0("\t",
                                                      indicators[!names(indicators) %in% all.factors],
                                                      '[shape = rect label = ', paste0('"', names(indicators), '"'), '];\n'),
          ifelse("resid" %in% elements,
            paste(paste0("\tResid_", indicators),
                  '[shape = circle style = filled color=lightgrey',
                  'fontsize = 10 width= 0.2 label = "&epsilon;"];\n',
                  collapse = ""),
             ""
          ),


             paste(#paste0("\t", resid.prefix, indicators),
                   paste("\t{rank = ", ifelse(names(factr) %in% exogenous.vars, "min", "max"),
                    paste(paste0(resid.prefix,indicators), collapse = " "), "};\n"),
                   paste("\t{rank = ", ifelse(names(factr) %in% exogenous.vars, "max", "min"),
                    factr, "};\n"),

                   paste0("\t",  factr, " -> ",  indicators, params, collapse=";\n"),
                   ";\n",
                   if("resid" %in% elements) {
                          paste0("\t", paste0(resid.prefix,indicators), " -> ", indicators, "[color=grey];", collapse="\n")
                   } else {
                          ""
                     },
                   paste("\n}\n"),
                   paste(" // end of ", names(factr), "measurement model.",
                    names(factr), " is ", ifelse(names(factr) %in% exogenous.vars,
                                                 "exogenous.\n\n", "endogenous.\n\n"))
              )
        )
} else {

  paste0("\t", factr, ' [shape = ellipse label = ', paste0('"', names(factr), '"'), '];\n')
}

      } else if(grepl("(~~)", i) & any(c("lat.covs","resid.covs") %in% elements )) {

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
     # if("resid" %in% elements) {
        lhs[names(lhs) %in% all.indicators] <- paste0(resid.prefix, lhs[names(lhs) %in% all.indicators])
        rhs[names(rhs) %in% all.indicators] <- paste0(resid.prefix, rhs[names(rhs) %in% all.indicators])
     # }

       covs <- data.frame(lhs=unname(lhs), rhs=unname(rhs), pars, stringsAsFactors = F)

       sapply(1:nrow(covs), function(co) {
         co <- covs[co,]
         #  if it's variance
         if(co$lhs == co$rhs) {
           if(names(lhs) %in% all.indicators & "obs" %in%  elements) {
           paste(
             paste0(co$rhs, '[xlabel="&sigma;&sup2;&#61;', as.character(co$pars), '"];\n'),
             ifelse(co$pars==0, paste0(co$rhs, '[style=dashed];\n'),"")
           )}
         # if it's covariance
         } else  {
if(  (names(lhs) %in% all.indicators & "resid.covs" %in%  elements) |
     (!(names(lhs) %in% all.indicators) & "lat.covs" %in%  elements) ) {

             if(!is.na(co$pars)) {
               if(co$pars!=0)
               paste(co$lhs, "->", co$rhs,
                     '[ dir = "both" splines=curved constraint=false label="', co$pars, '" fontsize = 10 color=grey ];\n')
             } else {
               paste(co$lhs, "->", co$rhs,
                     '[ dir = "both" splines=curved constraint=false];\n')

             }
}
         }

       })

      } else if(grepl("~", i) & !grepl("~~|=~", i) & "slopes" %in% elements) {




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
              paste0(indep, " -> ",  dep, ' [style=bold ', paste0('label="',cof, '"')[cof!=""],
                     ' color=',
                         ifelse(grepl("\\^\\^", cof),
                                ifelse(sub("\\^\\^", "", cof)<0, "red", "black"),
                                "grey70"),
                     ' fontcolor=',
                         ifelse(grepl("\\^\\^", cof),
                                ifelse(gsub("\\^\\^", "", cof)<0, "red", "black"),
                                "grey70"),
                     '];\n'),
              ifelse("resid" %in% elements,
                 paste(
                   paste0("Resid_",dep), ' [shape = circle style = filled color=lightgrey',
                    'fontsize=10 width=0.2, label = "&epsilon;"];\n',
                   paste0("Resid_",dep), "->", dep, " [color=grey];\n"
                   ),
                 "")
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


      } #else  { # if(i!="")
        #warning("Unknown operator found in line: ", i)
      #}
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

if(!code.only) {
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
} else {
  return(lines)
}

}


#' Convert lavaan to draw.io diagram
#'
#' Experimental. Produces  SEM diagrams that can be manually edited in draw.io environment. It's such a pain to work with "static" images from semPlot and similar packages. You would often give up and make your own using MS POwerpoint or similar software... Now you can just run this function using the fitted lavaan model (or simply the syntax for the model - useful for representing theoretical models) and paste the code into draw.io. The result of the automatic layour is likely still  messy, but at least now you can arrange the variables and arrows manually with all the power of draw.io -- and achieve the desired result in a few seconds. See details for specific instructions.
#'
#' @param m character, following lavaan syntax model conventions (see examples), or fitted lavaan object.
#' @param layout Format of the output.  Possible values are  'verticaltree', 'horizontaltree', 'verticalflow', 'horizontalflow', 'organic', 'circle', 'orgchart', 'auto', 'none', or a JSON string as used in draw.io -> Layout -> Apply. Default is 'horizontaltree'
#' @param item.labels Named vector where names are item/variable names and values are labels. Adds custom labels.
#' @param std Whether include standardized or unstandardized estimates.
#' @param elements Feature in development. Character vector. What to include in the diagram, possible values "obs", "lat.covs", "resid.covs", "resid", "slopes", and "intercepts/thresholds" (currently not supported).
#' @param thickness The line width multiplier: the paths are made proportional to the coefficients in the model, but sometimes parameters are too small for a line (.e.g, .5 would mean half a pixel). Use higher numbers to make the paths more visible.
#' @details Produces csv-like code that should be pasted to draw.io
#' The cool thing about it is that you can manually adjust the result --  a feature unavailable in any known diagram producers for lavaan so far.
#' Step 1.  Run the function using either fitted lavaan model or a syntax.
#' Step 2. The function will produce the code (which is also copied to the clipboard automatically).
#' Step 3. Go to draw.io (either website or a desktop version), Arrange -> Insert - Advanced - CSV... and insert the code into the window.
#' Step 4. The diagram is already there and you can start editing it in line with your goals. Probably the first thing to try is different layouts - which you can apply at Arrange -> Layout -> choose one of them.
#'
#' @export
lav_to_draw = function(m, layout="horizontaltree", item.labels = NULL, std = T, elements = NULL, thickness = 1, clip = T) {

    if(is(m, "lavaan")) {
      if(std) {
        pt <- lavaan::standardizedSolution(m)
        pt$est = pt$est.std


      } else {
        pt <- lavaan::parameterEstimates(m)
      }

      pt = merge(lavaan::parameterTable(m)[-(14:15)], pt, all.y=T)
      pt$est.char = eststar(pt[,"est"], pt[,"pvalue"])
      pt$est.numeric = as.numeric(pt[,"est"])

    } else {

      pt <- lavaanify(m)
      pt$est= pt$label
      if(all(pt$label=="")) pt$est= pt$ustart

      pt$est.char = pt[,"est"]
      pt$est.numeric = as.numeric(pt[,"est"])
      if(length(pt$est.numeric)==0 | all(is.na(pt$est.numeric)))
           pt$est.numeric <- 1

    }

    # remove thresholds
    pt <- pt[pt$op!="|",]
    # remove scaling factors
    pt <- pt[pt$op!="~*~",]

    pt <- pt[pt$rhs!="",]
    pt <- pt[pt$op!=":=", ]



    all.vars = unique(c(pt$lhs, pt$rhs))
    all.lvs = all.vars[all.vars %in% pt[pt$op=="=~","lhs"]]
    all.ovs = all.vars[!all.vars %in% pt[pt$op=="=~","lhs"]]

    csv = data.frame(id1 = all.vars,
                     fill = "#cccccc",
                     shape = ifelse(all.vars %in% all.lvs, "ellipse", "square")
    )

    csv$labels  = sapply(csv$id1, function(x)
      ifelse(x %in% names(item.labels), item.labels[[x]], x))
    #print(pt)
    # adding paths
    paths.pt = pt %>%
      dplyr::filter(op %in% c("=~", "~") | (op == "~~" & lhs != rhs))

    paths.l = lapply(setNames(nm=unique(paths.pt$lhs)), function(lhs)
      t(paths.pt[paths.pt$lhs==lhs,]) %>% `[`("rhs",)
    )

    library(reshape2)
    paths.df = melt(paths.l) %>%
      dplyr::mutate(varn = rownames(.)) %>%
      dcast(L1 ~ paste0("path", varn), value.var = "value")

    csv = merge(csv, paths.df, by.x = "id1", by.y = "L1", all.x = T)
    csv$perimeter = ifelse(csv$shape=="ellipse", "ellipsePerimeter;", "rectanglePerimeter")

    # preamble

    lbs = ifelse(is.null(item.labels), "# label: %id1%", "# label: %labels%")
preamble.start = paste(lbs,
    "
# style: shape=%shape%;fillColor=%fill%;strokeColor=black;perimeter=%perimeter%
# namespace: csvimport-")

    ## add paths
    preamble.loadings =

      apply(paths.pt[paths.pt$op %in% c("=~", "~~", "~"),], 1, function(x) {
        is.loading = x["op"]=="=~"
        is.regresson = x["op"]=="~"
        is.cov = x["op"]=="~~"
        location = names(csv)[which(csv[csv$id1==x["lhs"],]==x["rhs"])]

        path.width = ifelse(is(m, "lavaan"),
                            abs(as.numeric(x[["est.numeric"]])),
                            ifelse(is.na(x[["est.numeric"]]), 1,
                                   as.numeric(x[["est.numeric"]])))

        path.width = path.width*thickness

        path.color = ifelse(is(m, "lavaan"),
                            ifelse(as.numeric(x[["est.numeric"]])<0,"red","black"),
                            ifelse(!is.na(x[["ustart"]]),
                                   ifelse(as.numeric(x[["ustart"]])<0,"red","black"),"black"))

        paste0(
             '# connect: {',
               '"from":"', location, '","to":"id1",',
               '"label":"', ifelse(is.na(x[["est.char"]]), "", x[["est.char"]]),
               '","invert":', ifelse(is.loading, "false", "true"),',',

               '"style":"endArrow=blockThin;endFill=1',
                       ';dashed=', ifelse(is.loading & x[["free"]]!=0 & is(m, "lavaan") == 0, 1, 0),
                       ';strokeColor=', path.color,
                       ';strokeWidth=', path.width, ';',
               ifelse(is.cov,
                      "startArrow=blockThin;startFill=1;curved=1;rounded=0;",
                      "curved=0;"),
               '"}')

      })

    preamble.closing =  paste0(
"# width: 130
# height: 40
# nodespacing: 40
# levelspacing: 40
# edgespacing: 40
# layout: ", layout)

    if(is.null(item.labels))
       skip.in.ignore.line =  which(names(csv)=="id1")
    else
       skip.in.ignore.line = which(names(csv) %in% c("id1", "labels"))

    preamble = paste(preamble.start,
                     "## Factor loadings",
                     paste(preamble.loadings, collapse = "\n"),

                     paste("# ignore:",
                           paste(names(csv)[-skip.in.ignore.line],
                                 collapse = ",")),
                     preamble.closing,
                     sep = "\n")



    csv.out = capture.output(write.csv(csv, quote = F, row.names = F,na = ""))

    output = paste0(preamble, "\n## CSV data starts below this line\n",
                    paste( csv.out, collapse = "\n"), sep = "")

    cat(output)

if(clip) clipr::write_clip(output, "character", allow_non_interactive = TRUE)

    }



#' Combine fit measures form several models and compare
#' @details  Extracts select fit measures from lavaan objects and lists it in the tables.
#' @param ... lavaan fitted objects
#' @param what Character vector of fit indices as given in \pkg{lavaan} `fitMeasures()`
#' @examples data("HolzingerSwineford1939", package="lavaan")
#' m1 = cfa("F1 =~ x1 + x2 + x3 + x4 + x5", estimator= "mlr", HolzingerSwineford1939)
#' m2 = cfa("F1 =~ x1 + x2 + x3 + x4 + x5; x4 ~~ x5", estimator= "mlr", HolzingerSwineford1939)
#' lav_compare(m1, m2, what = c("cfi.scaled", "rmsea.scaled") )
#'
#'
#' @export
lav_compare = function(..., what = c("cfi", "tli", "rmsea", "srmr", "bic", "df"), LRT = F, print  = T) {


  if(length(what)<1) warning("Please choose at least one fit statistic to report.")
  if(length(list(...))==1 & "list" %in% class(list(...)[[1]]) ) {
    modellist = list(...)[[1]]
    modelnames <- names(modellist)
  } else {
    modellist = list(...)
    if(is.null(names(modellist)))
      modelnames <- as.character(substitute(...()) )
    else
      modelnames <- names(modellist)
  }


  out2<- t(sapply(modellist,  function(x) {
    all.fit= fitmeasures(x)
    sapply(what, function(y) ifelse(any(names(all.fit) == y), all.fit[[y]], NA))
  }))
  diffs <- apply(out2, 2, function(v) v - c(NA, v[-length(v)]))
  out2 <- cbind(out2, diffs)



  rownames(out2)<-modelnames

  out = out2

  if(LRT) {
    # LRT <- do.call(lavaan::lavTestLRT, append(modellist, list(model.names = modelnames)))
    # above does not work properly
    args.for.lav.LRT = append(modellist, list(modelnames))
    names(args.for.lav.LRT) <- c("object", rep("", length(modellist)-1), "model.names")
    LRT <- do.call(lavaan::lavTestLRT, args.for.lav.LRT)

    if(print) print(LRT)
    out = list(fit=out2, LRT = LRT)
  }

  if(print) {
    cat("\n")
    print(round(out2, 3), digits=3, row.names = TRUE, na.print = "" )
  }
  invisible(out)
}








#' Plot latent means
#' @param fit lavaan object containing an MGCFA with latent means
#' @return Returns a list of ggplot objects
#' @export
plot_latent_means <- function(fit) {
  ov.names <- colnames(lavInspect(fit, "data")[[1]])
  ests <- parameterEstimates(fit)
  mean.tbl <- ests[ests$op=="~1" & !ests$lhs  %in% ov.names,]

  mean.tbl$country <-  lavInspect(fit, "group.label")[mean.tbl$group]

  lapply(unique(mean.tbl$lhs), function(lat.var) {
    d.gg <- mean.tbl[mean.tbl$lhs==lat.var,]
    d.gg$country <- factor(d.gg$country, levels= d.gg$country[order(d.gg$est)])
    ggplot(d.gg, aes(country, est))+
      geom_point()+
      geom_errorbar(aes(ymin= est - 1.96*se, ymax = est + 1.96*se), width=.3)+
      coord_flip()+
      theme_minimal()+labs(x="", y=lat.var)

  })

}



#' SEM tab
#' @param ... Fitted lavaan models as separate arguments or a list of models
#' @param what Kinds of parameters to report. Possible values "loadings", "intercepts",  "slopes", "covariances", and "others"
#' @export
sem_tab <- function(..., what = c("loadings", "intercepts", "slopes", "covariances", "others"), std = F, ci = F) {

  if(length(list(...))==1 & class(list(...)[[1]]) == "list")
    modellist = list(...)[[1]]
  else
    modellist = list(...)



  if(is.null(names(modellist))) {
    modelnames <- as.character(substitute(...()) )
    print(modelnames)
  } else {
    modelnames <- names(modellist)
  }

  #modellist = list(IH.pool.3bif = IH.pool.3bif, IH.pool.three = IH.pool.three)

  names(modellist) <- modelnames

  #str(modellist, 1)
  #print(names(modellist))

  paramlist <- lapply(setNames(nm=modelnames),
                      function(m) {
                        if(std) {
                          pr.tb = standardizedSolution(modellist[[m]])
                          pr.tb = dplyr::rename(pr.tb, "est" = "est.std")
                          pr.tb$model = m
                          pr.tb
                        } else {
                          pr.tb = parameterEstimates(modellist[[m]])
                          pr.tb$model = m
                          pr.tb
                        }
                      })

  #print(paramlist)
  allprms = Reduce(rbind, paramlist)



  allprms %>% mutate(kind = ifelse(op == "=~", "loadings",
                                   ifelse(op == "~1", "intercepts",
                                          ifelse(op == "~~", "covariances",
                                                 ifelse(op == "~", "slopes", "other"))))) %>%
    filter(kind %in% what) %>%
    mutate(est_star  = eststar(est, pvalue, ci.low = if(ci) ci.lower else NA, ci.hi = if(ci) ci.upper else NA )) %>%
    reshape2::dcast(lhs + rhs + kind ~ model, value.var = "est_star")

}

# sem_tab <- function(m, elements = "loadings", std = F) {
#
#   extract_pars <- function(model, std = std) {
#     if(std)  {
#       pars = standardizedSolution(model) %>%
#         dplyr::rename(est = "est.std")
#     } else {
#       pars = parameterEstimates(model)
#     }
#
#     if( ("loadings" %in% elements)) {
#       if(lavInspect(model, "ngroups")>1 ) {
#         tab1 = pars %>% dplyr::filter(op == "=~") %>%
#           dplyr::mutate(eststar = eststar(est, pvalue)) %>%
#           dplyr::select(lhs, rhs, group, eststar) %>%
#           reshape2::dcast(lhs + rhs ~ group, value.var = "eststar")
#       } else {
#         tab1 = pars %>% dplyr::filter(op == "=~") %>%
#           dplyr::mutate(eststar = eststar(est, pvalue)) %>%
#           dplyr::select(lhs, rhs, eststar)
#       }
#
#
#     }
#     return(tab1)
#   }
#
#
#   if(is.list(m)) {
#     Reduce(dplyr::full_join, sapply(m, function(x) extract_pars(x)))
#   } else {
#     extract_pars(m)
#   }
#
#
# }
#
#' Add all possible indirect and total effects to the `lavaan` formula
#' @param lav.formula  `lavaan` formula
#' @param lab.sep How labels should be separated
#' @param ngroups Nuber of groups for multiple group data
#' @export
#'
add_all_indir_and_tot <- function(lav.formula, lab.sep = "__", ngroups = 1) {

  mtab = lavaanify(lav.formula)
  paths = dplyr::filter(mtab, op == "~")
  mediators = unique(paths[paths$lhs %in% paths$rhs,"lhs"])
  outcome.vars = unique(paths[!paths$lhs %in% paths$rhs,"lhs"])
  exog.vars = unique(paths[!paths$rhs %in% paths$lhs,"rhs"])
  covs = dplyr::filter(mtab, op == "~~" & lhs != rhs)

  statemtns.list <-
    sapply(exog.vars, function(exog) {
      c(
        # exog -> mediator
        sapply(mediators, function(med) {
          paste0(med, " ~ ", paste0(med, lab.sep, exog, "*"), exog)
        }),

        # exog -> outcome [direct effs]
        sapply(outcome.vars, function(out) {
          paste0(out,  " ~ ", paste0(out, lab.sep, exog, "*"), exog)
        }),

        # exog -> mediator -> outcome
        sapply(mediators, function(med) {
          sapply(outcome.vars, function(out) {

            c(
              #[indirect]
              paste0("INDIRECT_", paste(out, med, exog, sep = lab.sep),  " := ",
                     paste0(out, lab.sep, med), "*", paste0(med, lab.sep, exog)),
              #[direct - mediator-specific]
              paste0("TOTAL_", paste(out, med, exog, sep = lab.sep),  " := ",
                     paste0(out, lab.sep, med), "*", paste0(med, lab.sep, exog), " + ",
                     paste0(out, lab.sep, exog))
            )
          })}),


        #[total: exog -> mediator1 -> outcome + exog -> mediator2 -> outcome]
        sapply(outcome.vars, function(out) {
          paste0("TOTAL_", paste(out,
                                 paste(mediators, collapse="_WITH_"),
                                 exog, sep = lab.sep),  " := ",
                 paste(
                   sapply(mediators, function(med) {
                     paste0(
                       paste0(out, lab.sep, med), "*", paste0(med, lab.sep, exog))
                   }), collapse = " + "), " + ",
                 paste0(out, lab.sep, exog))
        })


      )
    })
  # mediator -> outcome
  statemtns.list = append(statemtns.list,
                          sapply(mediators, function(med) {
                            sapply(outcome.vars, function(out) {

                              paste0(out, " ~ ", paste0(out, lab.sep, med), "*", med) })}))

  # add covs
  # statemtns.list  = append(statemtns.list, apply(covs, 1, function(x) paste(x[['lhs']], "~~", x[['rhs']])))

  # output
  paste(statemtns.list, collapse = ";\n")

}
