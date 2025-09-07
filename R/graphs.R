###Graphs and generic functions for comparative analysis##
require('ggplot2')
require('sjmisc')
require('ggrepel')
require('arm')
require('car')
require("scales")

#' Convenience function for logging the code
#'
#' @param ... Character strings.
#' @details Prints text and objects in order to log and debug the code.
verb <- function(...) {
  print(paste(...))
}


##Stacked Bars ####
#' Quick stacked bar plot
#'
#' @details Computes proportions cross-table and plots it in a nice way. ggplot-based, so `+theme()` and other arguments can be added directly to transform the plot.
#'
#' @param variable Variable to be plotted.
#' @param group Grouping variable to cross-tabulate and compute.
#' @param sort.cat Category to sort the stacked chart.
#' @param colors Vector of colors distinguishing groups.
#' @param include.na Logical. Should NAs be a separate level or should they be excluded? FALSE by default.
#' @param labs Logical. Should percents labels be added to the plot. TRUE by default.
#' @param label.col In case labels are added, vector of labels colors.
#' @param label_col Same as label.col, saved for legacy.
#' @param format.label Character of one digit standing for decimal places and one symbol added in the end, for example "1\%" would result in "99.9\%" whereas "2$" would lead to "99.99$".
#' @param weight If there is a survey weight, variable to use in crosstab computation.
#' @export
#'
stacked_bar<-function(variable, group, sort.cat=1, colors=NA, include.na=FALSE, labs=TRUE, label.col=NA, format.label="2%", weight=NA,
                      label_col=NA, flip = F, cat.rev = F) {
  requireNamespace("RColorBrewer")
  requireNamespace('ggplot2')
  requireNamespace('ggrepel')
  requireNamespace('scales')
  requireNamespace('reshape2')
  requireNamespace('sjmisc')
  requireNamespace('magrittr')
  requireNamespace('stringr')


  label_col=label.col


  if(any(is.na(colors))) {
    colors<-c('#ffffd9','#edf8b1','#c7e9b4',
              '#7fcdbb','#41b6c4','#1d91c0',
              '#225ea8','#253494','#081d58', '#CCCCCC')
  }

  var.label <- stringr::str_wrap(attr(variable, "label"), 50)
  #print(var.label)
  if(include.na) {
    variable <- factor(variable, levels=c(unique(variable), "Missing"))
    variable[is.na(variable)] <- "Missing"
  } else {
    variable<-sjmisc::to_label(variable)
  }



  if(length(weight)==1) {

    b <-  table(variable, group) %>%
      prop.table(2) %>%
      {
        if(any(!is.na(sort.cat)))
        .[, order(colSums(.[sort.cat,]))]
        else
          .
        } %>%
      as.data.frame %>% set_colnames(c("var", "group", "Freq"))
  } else {

    requireNamespace("survey")

    b <-  svydesign(id = ~0,
                    weights = ~ weight,
                    data = data.frame(var = variable, group=group, weight=weight),
                    strata=NULL,
                    fpc = NULL,
                    probs = NULL
    ) %>%
      svytable(~ var + group, .) %>%
      prop.table(2) %>%
      #.[, order(.[sort.cat,])] %>%
      { if(any(is.na(sort.cat))) .[, order(.[sort.cat,])] else . } %>%
      as.data.frame #%>% set_colnames(c("var", "group", "Freq"))
  }

  print(b)
  #print(reshape2::dcast(b,  group ~ var  ))
if(cat.rev) levels(b$var) <- rev(levels(b$var))


  g<- ggplot(b,aes(x = group, y = Freq, fill = rev(var) )) +
    geom_bar(position = "fill", stat = "identity", col="black", size=.1)+
    scale_y_continuous(name="", labels = percent_format()) +
    labs(x="", y="", fill="")+
    theme(legend.title=element_blank(), legend.position = "bottom")+
    theme_minimal()+
    scale_fill_manual(name="", values=colors[1:length(levels(variable))],
                      labels=rev(levels(variable)))+
    theme(panel.grid = element_blank())

  if(!flip) g <- g+coord_flip()

  if(!is.null(var.label)) g<- g + ggtitle(var.label)


  if(  length(labs)> 1 |
       (length(labs)==1 && labs) ) {

    b$label<- paste( format(round(b$Freq*100,
                                  as.numeric(strsplit(format.label, "")[[1]][1])  ), nsmall = as.numeric(strsplit(format.label, "")[[1]][1])) %>%
                       sapply(., function(x) if(as.numeric(x)<2) "<2" else x  ),
                     ifelse(is.na(strsplit(format.label, "")[[1]][2]), "", strsplit(format.label, "")[[1]][2]),
                     sep="")


    if(!any(is.na(label.col))) {
      b$label.col <-  rep(label.col, nrow(b)/length(label.col))
    } else {
      b$label.col <- rep("black", nrow(b))
    }

    #print("___________Removing excess labels________________")
    #verb("b$var", b$var)
    #verb("b$label", b$label)
    if( length(labs)>1 ) {
      b$label[!b$var %in% labs] <- NA
      if(length(label.col)>1) {
        for(i in 1:length(labs) ) b$label.col[b$var==labs[i] ] <- label.col[i]

      }
      #verb("b$label", b$label)

    }


    g+geom_text(
      aes(label = b$label ), col=b$label.col,
      position = position_stack(vjust = 0.5), size=2.5, family="Tahoma")

  } else {
    g
  }

}

#Example
#stacked_bar(d$SWL2, d$Country, 7)


stacked_bar_ntf<-function(var, group, sort.cat=0, flip=FALSE) {
  requireNamespace("RColorBrewer")
  requireNamespace(stringr)
  requireNamespace(ggplot2)
  requireNamespace(scales)
  requireNamespace(sjlabelled)
  dta.graph1<-as.data.frame.matrix(prop.table(table(to_label(group), var),2))

  #Total<-colSums(as.data.frame.matrix(table(to_label(group), var)))
  #dta.graph2<-rbind(dta.graph1, Total)

  if(sort.cat!=0) {
    dta.graph1<-dta.graph1[order(dta.graph1[sort.cat,])]
  }

  #dta.graph1<-dta.graph1/colSums(dta.graph1)
  datm <- melt(cbind(dta.graph1, ind = rownames(dta.graph1)), id.vars = c('ind'))
  datm$ind<-factor(datm$ind, levels=levels(to_factor(group)))

  datm$variable<-factor(str_wrap(datm$variable, width = 10), unique(str_wrap(datm$variable, width = 10)))

  g<-ggplot(datm,aes(x = variable,
                     y = value,
                     fill = rev(ind),
                     label=percent(round(value,2)))) +
    geom_bar(stat = "identity", colour="black", size=.1) +
    scale_y_continuous(name="", labels = percent_format()
    ) +
    xlab("")+
    #scale_x_discrete(labels=levels(datm$variable))+
    #theme(legend.title=attr(group, "label")  )+
    theme_minimal()+
    scale_fill_manual(name="",
                      values=brewer.pal(n=5, name="Greys"),
                      labels=rev(levels(datm$ind))
    )+
    #labs(title = gsub('(.{1,50})(\\s|$)', '\\1\n', get_label(group))) +
    geom_text(size = 3, position = position_stack(vjust = 0.5),
              col=rep(c("White", "White","Black","Black","Black"), nrow(datm)/5))+
    theme(panel.grid = element_blank())

  if(flip==TRUE) {
    g + coord_flip()
  }
  else {
    g
  }
}

stacked_bar_ntf_n<-function(var, group, sort.cat=0, flip=FALSE, leg=TRUE, wrap=15, five.categories=TRUE) {
  #  if(sort.cat!=0) {
  #    message("Sorting is not active currently!")
  #    sort.cat=0 }

  requireNamespace("RColorBrewer")
  requireNamespace(stringr)
  requireNamespace(ggplot2)
  requireNamespace(scales)
  requireNamespace(sjmisc)
  requireNamespace(reshape2)
  requireNamespace(sjlabelled)

  if(!is.factor(group)) group <- as.factor(group)

  dta.graph1<-as.data.frame.matrix(prop.table(table(to_label(group), var),2))

  Total.count<-colSums(as.data.frame.matrix(table(to_label(group), var)))
  dta.graph1<-rbind(Total=Total.count/(sum(Total.count))*0.2,dta.graph1)

  if(sort.cat!=0) {
    dta.graph1<-dta.graph1[,order(dta.graph1[sort.cat+1,])]
    Total.count<-Total.count[names(dta.graph1[1,])]
  }

  datm <- melt(cbind(dta.graph1, ind = rownames(dta.graph1)), id.vars = c('ind'))
  datm$ind<-factor(datm$ind, levels=rev(c(levels(to_factor(group)), "Total")))

  if(sort.cat!=0) {
    datm$variable<-factor(str_wrap(datm$variable, width = wrap), unique(str_wrap(datm$variable, width = wrap)))
  } else {
    datm$variable<-factor(str_wrap(datm$variable, width = wrap),
                          sort(unique(str_wrap(datm$variable, width = wrap)), T))
  }


  lbls<-percent(round(datm$value,2))
  lbls[datm$value < 0.004999]<-NA
  lbls[seq(1, length(lbls), length(levels(group))+1)]<-NA


  g<-ggplot(datm,aes(x = variable,
                     y = value,
                     #fill = ind,
                     label=lbls)) +
    geom_bar(aes(fill=ind), stat = "identity", colour="black", size=.1) +
    scale_y_continuous(name="", labels = percent_format()) +
    xlab("")+
    theme_minimal()+
    scale_fill_manual(name="",
                      values=c("#e34a33",brewer.pal(n=length(levels(group)),
                                                    name="Greys")),
                      breaks=levels(datm$ind),
                      labels=c("Number of responses", levels(datm$ind)[2:length(levels(datm$ind))])
    )+

    theme(panel.grid = element_blank(),
          legend.position=ifelse(leg==T, "right", "none"))

  if(five.categories) {
    g<- g+ geom_text(size = 3,
                     position = position_stack(vjust = 0.5),
                     col=rep(c("White", "White","Black","Black","Black", "Black"), nrow(datm)/6))
  } else {
    g<- g+ geom_text(size = 3, col="black",
                     position = position_stack(vjust = 0.5))
  }


  print(Total.count)

  if(flip==TRUE) {
    g + geom_text(data=datm[datm$ind=="Total",],
                  aes(y = value+1, label=Total.count), hjust=-0.5, size=2.5, fontface="bold")+coord_flip()
  }
  else {
    g + geom_text(data=datm[datm$ind=="Total",],
                  aes(y = value+1, label=Total.count), vjust=-0.5, size=2.5)
  }

}


stacked_bar_ntf_nm<-function(vars, group, sort.cat=0, flip=FALSE, leg=TRUE, wrap=15) {
  #  if(sort.cat!=0) {
  #    message("Sorting is not active currently!")
  #    sort.cat=0 }

  requireNamespace("RColorBrewer")
  requireNamespace(stringr)
  requireNamespace(ggplot2)
  requireNamespace(scales)
  dta.graph1<-sapply(names(vars), function(x) prop.table(table(to_label(group), vars[,x]),2)[,2])

  #dta.graph1<-as.data.frame.matrix(prop.table(table(to_label(group), vars[,1]),2)[,2])

  Total<-colSums(vars[, names(vars)] != 0, na.rm = T)
  dta.graph1<-rbind(Total=Total/(sum(Total))*0.2,dta.graph1)

  if(sort.cat!=0) {
    dta.graph1<-dta.graph1[,order(dta.graph1[sort.cat+1,])]
    Total<-Total[names(dta.graph1[1,])]
  }

  #dta.graph1<-dta.graph1/colSums(dta.graph1)
  datm <- melt(dta.graph1, id.vars = c(ind=rownames(dta.graph1)))
  names(datm)<-c("ind", "variable", "value")
  datm$ind<-factor(datm$ind, levels=rev(c(levels(to_factor(group)), "Total")))

  datm$variable <- gsub("`", "", datm$variable)
  datm$variable<-factor(str_wrap(datm$variable, width = wrap), unique(str_wrap(datm$variable, width = wrap)))

  #if(sort.cat!=0) {

  #datm$variable <- factor(datm$variable, levels = datm$variable)

  #datm$variable <- factor(datm$variable, levels=unique(as.character(datm$variable)) )
  #datm$variable <- transform(datm$variable, variable=reorder(datm$variable, datm$variable))
  #lvls<-levels(to_factor(var))[order(dta.graph1[sort.cat,])]

  #datm$variable<-factor(datm$variable, levels=datm$variable)

  #datm$variable <- transform(datm$variable, variable=reorder(datm$variable, -order(dta.graph1[sort.cat,])) )

  #  reorder(datm$variable, levels(to_factor(var))[order(dta.graph1[sort.cat,])])
  #}

  lbls<-percent(round(datm$value,2))
  lbls[seq(1, length(lbls), 6)]<-Total

  g<-ggplot(datm,aes(x = variable,
                     y = value,
                     fill = ind,
                     label=lbls)) +
    geom_bar(stat = "identity", colour="black", size=.1) +
    scale_y_continuous(name="", labels = percent_format()) +
    xlab("")+
    #scale_x_discrete(labels=levels(datm$variable))+
    #theme(legend.title=attr(group, "label")  )+
    theme_minimal()+
    scale_fill_manual(name="",
                      values=c("#e34a33",brewer.pal(n=length(levels(group)),
                                                    name="Greys")),
                      breaks=levels(datm$ind),
                      labels=c("Number of responses", levels(datm$ind)[2:6])
    )+
    #labs(title = gsub('(.{1,50})(\\s|$)', '\\1\n', get_label(group))) +
    geom_text(size = 3,
              #position = position_stack(),
              position = position_stack(vjust = 0.5),
              #vjust=c(rep(c(rep(0.5,5),0), nrow(datm)/6)),
              col=rep(c("White", "White","Black","Black","Black", "Blue"), nrow(datm)/6))+
    theme(panel.grid = element_blank(),
          legend.position=ifelse(leg==T, "right", "none"))


  if(flip==TRUE) {
    g + coord_flip()
  }
  else {
    g
  }

  #  ifelse(flip==T, g, g+coord_flip())


}



#Graph means w/CIs####
#' Graph means w/CIs
#'
#' Returns ggplot of means by group with 95% confidence intervals. Modifiable with standard ggplot geoms, scales, themes, etc.
#' @param x Variable to aggregate.
#' @param group Group variable. If length is 1, assumes it's a variable name in the `data`
#' @param highlight.group Character vector of groups make bold.
#' @param codes "print" or "caption"
#' @param type "means", "ridges", "heat"
#' @param use.labels Whether or not attempt to extract value labels. TRUE by default.
#' @param data Optional, used only if `X` or `group` have length 1.
#'
#' @examples d<-data.frame(v=1:100, group=rep(1:2, 50))
#' graph_means_ci(d$v, d$group)
#' with(d, graph_means_ci(v, group))
#' @export
graph_means_ci <- function(x,
                           group,
                           highlight.group=NA,
                           codes=c("none", "print", "caption"),
                           type=c("means", "ridges", "heat"),
                           use.labels=T,
                           data) {

  type = type[[1]]
  if(!type %in% c("means", "ridges", "heat")) {
    stop('Type argument should be one of the "means", "ridges", "heat"')
  }

  if(length(x)==1) x <- data[,x]
  if(length(group)==1) group <- data[,group]

  #warning("There is some error in labeling! Do not trust the results!")
  group <- lab_to_fac(group)

  # if(!is.null(attr(x, "labels"))) {
  #   lbl.tbl<-  data.frame(codes= attr(x, "labels"),
  #                         labels= names(attr(x, "labels")),
  #                         row.names = NULL)
  #   print(lbl.tbl)
  # }


  # Caption and codes
  if(!is.null(attr(x, "labels")) && use.labels) {
    g.caption <- paste(attr(x, "labels"),
                       attr(attr(x, "labels"), "names"), collapse="; \n")
  } else {
    #g.caption <- paste(unique(x), collapse="; \n")
    g.caption <- paste(deparse(substitute(x)), "by",
                       deparse(substitute(group))
                       )
  }

  # x labs
  if(!is.null(attr(x, "label"))  &&
     any(!attr(x, "label")==attr(x, "labels")) &&
     use.labels) {
    x.label <-  attr(x, "label")
  } else {
    x.label <-   deparse(substitute(x))
  }


  if(type[1]=="means") {

    dt<-data.frame(
      mean=tapply(x, group, function(x) mean(x, na.rm=T), simplify = T),
      sd=tapply(x, group, function(x) sd(x, na.rm=T), simplify = T),
      n=tapply(x, group, function(x) length(x), simplify = T)
    )
    dt$lower<-dt[,1]-dt[,2]*1.96/sqrt(dt[,3])
    dt$upper<-dt[,1]+dt[,2]*1.96/sqrt(dt[,3])
    dt<-na.omit(dt)
    dt$country<-factor(rownames(dt), levels=rownames(dt)[order(dt$mean)])
    dt$group.highlighted <- as.character(dt$country %in% highlight.group)

    # Main plot
    g<-ggplot(dt, aes(x=country, y=mean))+
      geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, colour="grey")+theme_minimal()+
      geom_point(aes(colour=group.highlighted), show.legend = F, alpha=1 )+
      geom_text(aes(label=format(mean,digits=1,nsmall=1)), nudge_x=.4, size=2, color="black")+
      xlab("") +
      scale_y_continuous(name="",
                         #breaks=2:7,
                         minor_breaks=NULL) +
      coord_flip()+
      labs(title = gsub('(.{1,50})(\\s|$)', '\\1\n', g.caption))+
      theme(axis.line = element_line(colour = "black"),
            panel.grid = element_blank(),
            plot.caption = element_text(size=5, hjust=0))

    # Highlights
    if(!is.null(highlight.group)) {
      bold.highlight <- rep("plain", length(dt$country))
      bold.highlight[dt$highlight.group] <- "bold"

      g<-g+theme(axis.text.y=element_text(face= bold.highlight ))+
        geom_vline(xintercept=(1:length(levels(dt$country)))[levels(dt$country) %in% highlight.group], linetype="dotted", color="black", size=.1)+
        scale_colour_manual(values = c("red", "black") )
    }




  } else if(type[1] == "ridges") {

    requireNamespace("ggridges")

    means<-tapply(x, lab_to_fac(group), function(x) mean(x, na.rm=T), simplify = T)

    dt<- data.frame(x=x,
                    group=factor(group, levels=names(means)[order(means)]))

    g<-ggplot(dt, aes(x=x, y=group))+
      ggridges::geom_density_ridges(aes(fill=group %in% highlight.group),
                          show.legend = F, quantiles=4, scale = 2, size=0.3)+theme_mr()+
      scale_fill_manual(values=c("skyblue", "#FF8FDA"))+
      geom_point(data=data.frame(x=means, y=names(means)), aes(x, y), shape=19, size=1, color="black", alpha=.5)+
      geom_text(data=data.frame(x=means, y=names(means)),
                aes(x, y, label=format(x, digits=2, nsmall=2)), size=2, nudge_y = 0.5)+
      labs(x=x.label, y=NULL)

  } else if(type[1]=="heat") {

    freq.dt<-as.data.frame(prop.table(table(group, x, useNA="no"), margin=1))
    means<-tapply(x, lab_to_fac(group), function(y) mean(y, na.rm=T), simplify = T)
    means.dt<- data.frame(group=names(means), means=means)
    freq.dt$ones <- rep(1, nrow(freq.dt))
    freq.dt$group <- factor(freq.dt$group, levels=names(means)[order(means)])

    g<-ggplot(freq.dt, aes(group, ones, fill=Freq))+geom_col(position="stack")+coord_flip()+
      scale_fill_gradient2(low = "white", mid="#5FC768", high = "blue", midpoint = .5)+
      #scale_fill_distiller(type="seq", direction = 1, palette="GnBu")+
      geom_point(data=means.dt, aes(group, means, fill=NULL))+
      geom_text(data=means.dt, aes(group, means, fill=NULL, label=format(means, digits=2, nsmall=2)), size=2, nudge_y=.5)+
      scale_y_continuous(breaks=min(x,na.rm =T):max(x,na.rm =T) - 0.5, labels=min(x,na.rm =T):max(x,na.rm =T))+
      theme_mr()+labs(x="", y="", fill="Frequency")


    if(!is.null(highlight.group)) {
      bold.highlight <- rep("plain", length(levels(freq.dt$group)))
      bold.highlight[levels(freq.dt$group) %in% highlight.group] <- "bold"

      g<-g+theme(axis.text.y=element_text(face= bold.highlight ))+
        geom_vline(xintercept=(1:length(levels(freq.dt$group)))[levels(freq.dt$group) %in% highlight.group], linetype="dotted", color="black", size=.1)#+
      #scale_colour_manual(breaks = c("4", "6", "8"), values = c("red", "black") )
    }

  }



  if(codes[[1]]=="caption") {
    g+labs(caption = g.caption)
  } else if (codes[[1]]=="print") {
    cat(g.caption)
  } else if (codes[[1]]=="none") {
  }

  g

}

#Means and SEs####
#' Means and SEs
#'
#' @param x Variable for which mean, se, lower and upper bounds of confidence interval are returned.
#' @return Row with 4 columns (Mean, SE, Lower, Upper)
#' @export
mean_se_lower_upper<-function(x) {
  cbind(Mean=mean(x, na.rm = T),
        SE=sd(x, na.rm = T)/sqrt(length(x)),
        Lower=mean(x, na.rm = T)-1.96*sd(x, na.rm = T)/sqrt(length(x)),
        Upper=mean(x, na.rm = T)+1.96*sd(x, na.rm = T)/sqrt(length(x)))}

# Scatter by country #####
#' Computes means by group and plots them in as a scatterplot
#'
#' @param var1 Variable to plot on x axis
#' @param var2 Variable to plot on y axis
#' @param group Grouping variable
#' @param data Data frame
#' @param plot Logical. Should the plot be created?
#' @param print Logical. Should the result be printed in the console?
#'
#'
#' @details Recommended to use with `with` function, as in example.
#' @examples scatter_means_ci('alcfreq', 'HE', 'cntry', ESS, plot=T, print=TRUE)
#'
#' @export
scatter_means_ci <- function(variable1, variable2, group, data, plot=TRUE, print=TRUE, smooth.method = "lm", drop.groups = NULL) {
  require(ggrepel)

  var1 = data[,variable1]
  var2 = data[,variable2]
  grp =  data[,group]

  if(!is.null(attr(var1, "labels"))) {
    lbl.tbl1<-  data.frame(codes= attr(var1, "labels"),
                           labels= names(attr(var1, "labels")),
                           row.names = NULL)
    print(lbl.tbl1)
  }

  if(!is.null(attr(var2, "labels"))) {
    lbl.tbl2<-  data.frame(codes= attr(var2, "labels"),
                           labels= names(attr(var2, "labels")),
                           row.names = NULL)
    print(lbl.tbl2)

  }

  if(!is.null(drop.groups)) {
    var1 <- var1[!grp %in% drop.groups]
    var2 <- var2[!grp %in% drop.groups]
    grp<- grp[!grp %in% drop.groups]
    if(any(class(grp) %in% "factor")) grp<- droplevels(grp)
  }


  dt<-data.frame(
    mean1=tapply(as.vector(var1), lab_to_fac(grp), function(x) mean(x, na.rm=T), simplify = T),
    mean2=tapply(as.vector(var2), lab_to_fac(grp), function(x) mean(x, na.rm=T), simplify = T)
  )
  #names.dt<-c(deparse(substitute(var1)),deparse(substitute(var2)))
  dt1<-dt

  # colnames(dt1) <- names.dt
  colnames(dt1) <- c(variable1, variable2)
  if(print)  print(dt1)

  if(plot) {

    ggplot(dt, aes(mean1, mean2, label=row.names(dt)))+
      geom_point()+geom_text_repel()+
      labs(x=variable1, y=variable2)+geom_smooth(method = smooth.method, se = FALSE)+
      labs(caption=c(round(cor(dt$mean1, dt$mean2, use="pairwise.complete.obs"),2), "n=", nrow(na.omit(cbind(dt$mean1, dt$mean2))) ))+
      theme_mr()
  } else {
    dt
  }
}


# Plot random effects #####
#' Plot random effects and interactions for lmer objects
#'
#'Takes object produced by lme4::lmer() function, extracts random effects and interactions, and returns ggplot2-based plots to examine.
#'
#' @param lmer.fit A mer object
#' @param optional.names A named vector of random effects, where names indicate terms and values are used as axis x labels in correspomding plot. The length  should be equal to a number of random effects. Usually it includes "(Intercepts)" and names of variables whose effects are made random.
#' @param facets Logical. Should the random effects be plotted in facets or as a series of single plots?
#' @param scatter Logical. Should the scatterplots of the cross-level interactions be plotted? If TRUE, `facets` argument corresponds to scatterplots. This option is under development...
#' @param print If the plots should be actually printed, valid only if facets = F
#' @return Returns one or several ggplots. In case one plot is returned it can be appended with `theme`, `geom_`, etc.
#' @examples random_plot(lmr, optional.names=c(`(Intercepts)`="Intercepts", gndr="Female", s.Age = "Effects of age"), facets=TRUE)
#'@export
#'
random_plot <- function(lmer.fit, optional.names=NA, facets=FALSE, print = T) { #optional.names should be a named vector


  group.name<-names(getME(lmer.fit, "flist"))

  ses<-as.data.frame(arm::se.coef(lmer.fit)[[group.name]])
  #dt<-subset(coef(lmer.fit)[[group.name]], select=names(ses))
  dt<-coef(lmer.fit)[[group.name]][names(ses)]

  if(length(optional.names)==1 && is.na(optional.names)) {
    optional.names<-names(ses)
    names(optional.names)<-names(ses)
  }


  requireNamespace("grid")
  glist<-grid::gList()
  for(i in names(ses)) {

    est.ci<- data.frame(group    = row.names(dt),
                        lower.ci = dt[,i]-ses[,i]*1.96,
                        estimate = dt[,i],
                        upper.ci = dt[,i]+ses[,i]*1.96,
                        stringsAsFactors = F)

    est.ci$group <- factor(est.ci$group, levels=est.ci$group[order(est.ci$estimate)]  )
    est.ci$significant <- abs(dt[,i]/ses[,i])>1.96
    #print(est.ci)

    g<-ggplot(est.ci, aes(group, estimate, ymin=lower.ci, ymax=upper.ci))+
      geom_errorbar(aes(alpha=significant), size=.5, width=.5, colour="black", show.legend = F)+geom_point(aes(alpha=significant), show.legend = F)+
      scale_alpha_manual(values=c("FALSE"=0.3, "TRUE"=1))+
      geom_hline(aes(yintercept=0), linetype="dashed"  )+coord_flip()+
      labs(x="", y=paste("", optional.names[i] ))+theme_minimal()+
      theme(axis.line = element_line(colour = "black"),
            panel.grid = element_blank(),
            plot.caption = element_text(size=10, hjust=0))

    # if(!facets) {
    #   print(g)
    # } else {
    #   glist[[i]] <- g
    # }

    glist[[i]] <- g
  }

  if(!facets)  {
    if(print ==T) {
      for(i in 1:length(glist))  print( glist[[i]] )
    }
    cat(length(names(ses)), "graphs were created.")
    invisible(glist)

  } else {
    gridExtra::grid.arrange( grobs=glist, ncol=length(glist), nrow=1)

  }


}




# Cross-level interaction plot ####
#' Plot cross-level interactions for mer objects
#'
#'Takes an object produced by lme4::lmer() function, and returns interaction plot(s).
#'
#' @param x A mer object produced by `lmer` function
#' @param real Logical. Whether real groups should be used to predict random effects, or extrapolate using means and standard deviations?
#' @param z.levels String, indicating what levels of moderating group-level variable should be computed. Can take (only) the following values:
#' * "1SD". Default. Computes group-level mean and standard deviation of moderating variable and finds groups with similar values, so prediction of slopes and values is made based on three real  groups.
#' * "1SD" Computes group-level mean and standard deviation of moderating variable.
#' * "2SD" Computes group-level mean and doubled standard deviation of moderating variable.
#' * "12SD" Computes group-level mean and plots both standard deviation and doubled standard deviation of moderating variable.
#' @param scatter Logical. Should scatterplot be created in addition?
#' @param ... Arguments passed to `effect` function of `effects` package.
#' @param labs List of arguments passed to `+labs()` PLUS elements named  "line1", "line2"... which are used to name lines on the plot. If there are more or less "line" elements than lines on the plot, they are ignored.
#' @param x.levels desired x-axis coordinates, individual-level term. If NULL (default) are defined automatically.
#' @return Returns one or several ggplots. In case one plot is returned it can be appended with `+theme()`, `+geom_()`, etc.
#' @examples data("Orthodont",package="nlme")
#' m1 = lmer(distance ~ age*Sex + (age|Subject), data=Orthodont)
#' random_interaction2(m1)
#' @details It somehow repeats functionality of 'sjPlot::sjp.int', but differs in being able to select real groups close to +/- 1 sd and mean of moderating variable; makes prettier and customizable plots.
#' @seealso \link{random_plot} \link{lmer_table}
#' @md
#'
#' @export
random_interaction <- function( x,
                                real=TRUE,
                                z.levels=c("1SD", "2SD", "1.2SD", "all"),
                                scatter=FALSE,
                                labs=NULL,
                                x.levels=NULL,
                                silent = T,
                                ... ) {


  # Works only with cross-level two-way interactions
  # Test it with factor predictors
  # Test it with multiple interactions +
  # Test it with non-cross-level interactions
  # Add scatterlot of random effects vs explanatory variable


  loadNamespace("effects")
  loadNamespace("stringr")
  loadNamespace("lme4")
  loadNamespace("grid")
  loadNamespace("ggrepel")


  z.levels<-z.levels[1]
  if(z.levels=="1.2SD") z.levels="12SD"

  all.fix.terms <- names(fixef(x))
  mf <- model.frame(x)
  mm <- model.matrix(x)

  # Interaction terms
  interaction.terms <- all.fix.terms[grep(":", all.fix.terms)]

  # Are there any interactions
  if(length(interaction.terms)==0) stop("Interactions were not found!")

  # Grouping variable name
  group.name <- names(ranef(x))
  if(length(group.name)>1) stop("Cannot deal with more than one grouping variable.")




  # Refit if any 'labelled'
  if(any(sapply(mf, function(x) any(class(x) %in% "labelled")))) {
    # Remove class 'labelled'
    for(v in 1:ncol(mf))
      if(any(class(mf[[v]]) %in% "labelled"))
        class(mf[[v]]) <- class(mf[[v]])[class(mf[[v]])!= "labelled"]

    message(paste("There are",is.there.any.labelled.variables, "variable(s) of class 'labelled', with which package 'effects' doesn't work, so refitting to a data with 'labelled' converted to 'numeric'. " ))
    x<-lmer(formula(x), data=mf)
  }




  interaction.terms.split<- str_split(interaction.terms, ":")
  if(!silent) verb("interaction.terms.split", interaction.terms.split)



  glist<-grid::gList()
  for(i in 1:length(interaction.terms.split)) {


    #Check if one of the variables in an interaction is a group-level variable
    sum.var1<- sum(aggregate(model.matrix(x)[ ,interaction.terms.split[[i]][1] ],
                             getME(x, "flist"), sd, na.rm=T)[,2])
    sum.var2<- sum(aggregate(model.matrix(x)[ ,interaction.terms.split[[i]][2] ],
                             getME(x, "flist"), sd, na.rm=T)[,2])

    if(sum.var1==0) {
      group.eff.name <- interaction.terms.split[[i]][1]

    } else if(sum.var2==0){
      group.eff.name <- interaction.terms.split[[i]][2]

    } else {
      warning("The interaction '", paste(interaction.terms.split[[i]], collapse=" X "), "' is a first-level interaction. You might use 'effects' package.")
      next
    }

    # Check if variables are factors
    possibly.factors <- unlist(sapply(interaction.terms.split[[i]], function(v) v[!v %in% colnames(mf)]))
    if(length(possibly.factors)>0) {
    possibly.factor <- names(mf)[sapply(names(mf),function(n) grepl(paste0("^",n), possibly.factors))]
    if(length(possibly.factor)>1) stop("Cannot deal with an interaction of two factors.")
    factor.var <- ifelse(class(mf[,possibly.factor])=="factor", possibly.factor, NA)

      warning("One of the terms is factor, using all the levels: ", factor.var)
      z.levels = "all"
      if(any(group.eff.name %in% possibly.factors)) group.eff.name = factor.var
      interaction.terms[[i]] = sub(possibly.factors,factor.var, interaction.terms[[i]])

    }

    ind.eff <- sub(":", "", sub(group.eff.name, "", interaction.terms[[i]]))
    # Find which one is a random effect
    if( sum( interaction.terms.split[[i]] %in% names(ranef(x)[[group.name]]) )>0 ) {

      random.eff.name <- interaction.terms.split[[i]][interaction.terms.split[[i]] %in% names(ranef(x)[[group.name]])]
      if(!silent) verb("Attempting to plot interaction ", random.eff.name, " X ", group.eff.name, ".", sep="")

    } else {
      warning("Not ", paste(interaction.terms.split[[i]], collapse=" nor ") , " are random effects. Ignoring this interaction.")
      next
    }

    if(!silent) {
    message("Interaction term is ", interaction.terms[[i]],
      "\n factor variable is ", ifelse(length(possibly.factors)>0,factor.var,"none"),
      "\n group-level effect is ", group.eff.name,
      "\n random effect term is ", random.eff.name,
      "\n ind term is ", ind.eff
       )
    }


  # Constructing the moderator levels and labels
    if(z.levels == "1SD") {
      noflines <- 1:3
    } else if (z.levels == "2SD") {
      noflines <- c(1,4,5)
    } else if(any(z.levels %in% c("12SD"))) {
      noflines <- 1:5
    } else if(z.levels == "all") {
      #noflines <- 1:nrow(group.var.means)
      message("Plotting all the groups.")
      real=TRUE
    } else {
      "Incorrect z.levels argument."
    }



    if(z.levels == "all") {

      #moderator.levels.to.plot <- list(name1= group.var.means[,2])
      #index.groups.needed <- 1:nrow(group.var.means)

      moderator.levels.to.plot <- list(as.character(unique(mf[ ,group.eff.name ])))

    } else {
      group.var.means <- aggregate(mf[ ,group.eff.name ],
                                   getME(x, "flist"),
                                   mean, na.rm=T)
      mean.level.group.eff <- mean(group.var.means[,2], na.rm=T)
      sd.level.group.eff <- sd(group.var.means[,2], na.rm=T)

    if(real==FALSE) {

      moderator.levels.to.plot <- list(name1=c(
        mean.level.group.eff,
        mean.level.group.eff+sd.level.group.eff,
        mean.level.group.eff-sd.level.group.eff,
        mean.level.group.eff+2*sd.level.group.eff,
        mean.level.group.eff-2*sd.level.group.eff)[noflines])

    } else {


        index.groups.needed <- c(
          which.min(abs(group.var.means[,2] - mean.level.group.eff)),
          which.min(abs(group.var.means[,2] - (mean.level.group.eff + sd.level.group.eff))),
          which.min(abs(group.var.means[,2] - (mean.level.group.eff - sd.level.group.eff))),
          which.min(abs(group.var.means[,2] - (mean.level.group.eff + 2*sd.level.group.eff))),
          which.min(abs(group.var.means[,2] - (mean.level.group.eff - 2*sd.level.group.eff))))

        index.groups.needed <- index.groups.needed[noflines]
        moderator.levels.to.plot <- list(name1= group.var.means[index.groups.needed,2])

      }}


    names(moderator.levels.to.plot) <- gsub("scale|)|\\(", "", group.eff.name)

    # assign x axis levels if they are defined by user
    if(!any(is.null(x.levels)))  {
      #moderator.levels.to.plot[[length(moderator.levels.to.plot)+1]] <-  x.levels
      #names(moderator.levels.to.plot)[length(moderator.levels.to.plot)]<- strsplit(interaction.terms[i], ":")[[1]][1]
      moderator.levels.to.plot <- append(moderator.levels.to.plot, list(x.levels))
      names(moderator.levels.to.plot)[[2]]<- ind.eff
    }

    if(!silent) print(moderator.levels.to.plot)

    # Compute effects

    if(length(possibly.factors)>0) {


     # int.terms2 <- interaction.terms.split[[1]]
     # which.int.1 <- which(grepl(possibly.factors1, int.terms2))
     # which.int.2 <- which(!grepl(possibly.factors1, int.terms2))
     # non.fact.term <- int.terms2[[which.int.2]]

     # itt <- paste0(c(possibly.factors1,non.fact.term)[c(which.int.1,which.int.2)], collapse=":")

      ploff<-suppressWarnings(effect(term=interaction.terms[[i]], mod=x, xlevels=moderator.levels.to.plot,...))
      db<- data.frame(ploff)

    } else {
      ploff<-suppressWarnings(effect(term=interaction.terms[[i]],
                    mod=x,
                    xlevels=moderator.levels.to.plot, ...))
      if(!silent) print(ploff)

      db<- data.frame(ploff)
      db[,group.eff.name]<-factor(round(db[,group.eff.name], 2))
      #ind.str=interaction.terms.split[[i]][!interaction.terms.split[[i]] %in% group.eff.name]
    }


    if(!silent) print(db)

    #Plot effects


    g <-
      ggplot(db,
             aes_string(
               x = ind.eff,
               y = "fit",
               col = group.eff.name,
               group = group.eff.name
             )) +
      geom_line(aes_string(linetype = group.eff.name)) +
      geom_ribbon(
        aes_string(
          ymin = "lower",
          ymax = "upper",
          fill = group.eff.name
        ),
        alpha = 0.1,
        col = NA
      ) +
      theme_minimal() + theme(
        legend.position = "none",
        axis.line = element_line(colour = "grey"),
        panel.grid = element_blank()
      )



    if(z.levels == "1SD") {
      col.scale.values <- c("red", "black", "blue")
      linetype.scale <- c("dashed", "solid", "dashed")
      labels <- paste(group.eff.name,  c("mean", "+1SD", "-1SD"))
      labels <- paste(labels, round(moderator.levels.to.plot[[1]],2)  )

    } else if(z.levels == "2SD") {
      col.scale.values <- c("red", "black", "blue")
      linetype.scale <- c("dashed", "solid", "dashed")
      labels <- paste(group.eff.name,  c("mean", "+2SD", "-2SD"))
      labels <- paste(labels, round(moderator.levels.to.plot[[1]],2)  )

    } else if (z.levels == "12SD") {
      col.scale.values <- c("magenta", "red", "black", "blue", "skyblue")
      linetype.scale <-   c("dotted", "dashed", "solid", "dashed", "dotted")
      labels <- paste(group.eff.name,  c("mean", "+1SD", "-1SD", "+2SD","-2SD"))
      labels <- paste(labels, round(moderator.levels.to.plot[[1]],2)  )
    } else {
      #labels <- group.var.means[,1]
      labels <- moderator.levels.to.plot[[1]]
    }

    #verb("moderator.levels.to.plot", moderator.levels.to.plot)




    if(real & z.levels!="all") {
      labels <- paste(labels, "; b=", round(coef(x)[[group.name]][,random.eff.name][index.groups.needed],2), sep="")
      labels <- sapply(1:length(labels),
                         function(u) paste(labels[u], group.var.means[,1][index.groups.needed][u], sep=","))

    } else if(real & z.levels=="all")  {


    ranef.w.group.eff.names <-  merge(aggregate(mf[,c(group.eff.name )],
                                                list(mf[,group.name]),
                                                unique),
                                      coef(x)[[group.name]],
                                      by.x="Group.1",
                                      by.y="row.names")

     average.ranef.by.group.eff <- aggregate(ranef.w.group.eff.names[,random.eff.name],
                                             list(group.eff.names=ranef.w.group.eff.names$x),
                                             median)
     labels <- paste(labels, "; b=", format(average.ranef.by.group.eff[match(average.ranef.by.group.eff$group.eff.names, labels), 2],nsmall=2), sep="")


    } else {
      if(!silent) warning("Here computations of slopes should be")
    }


    if(z.levels!="all") {
      g<- g + scale_color_manual(values=col.scale.values)+
        scale_fill_manual(values=col.scale.values)+
        scale_linetype_manual(values=linetype.scale)
    }


    if (is.null(labs)) {
      g <- g +
        geom_text_repel(data = db[db[,ind.eff] == max(db[,ind.eff]), ],
                        aes(label = labels),
                        box.padding = unit(0.5, "lines"))+
        labs(y = ploff$response,
             caption = paste("Predicted slopes conditioned on group-level variable",
                             group.eff.name)
        )
    } else {

      l <- labs#[[length(glist)+1]]
      l.nonlines <- l[!grepl("line", names(l))]


      if(length(l.nonlines)>0)
        g <- g + structure(l.nonlines, class = "labels")

    if(sum(grepl("line", names(l))) != length(noflines) ) {
      warning("Labels contain less/more 'lines' labels than specified in z.levels argument. Ignoring line labels.")
    } else {
      if(sum(grepl("line", names(l)))>0)
        g <-
        g + geom_text_repel(data = db[ db[,ind.eff] == max(db[,ind.eff]), ],
                            aes(label = l[grep("line", names(l))]),
                            box.padding = unit(0.5, "lines"))
    }
    }




    if(scatter) {
      scatter <- qplot(group.var.means[,2], coef(x)[[group.name]][group.var.means[,1],random.eff.name],
                       label= group.var.means[,1],
                       geom=c( "text"),
                       xlab=group.eff.name, ylab=paste("Effect of", random.eff.name, "on", as.character(x@call$formula[2])))

      g <- gridExtra::grid.arrange( scatter, g, nrow=2, ncol=1)
    }

    #rm(new.data, envir=.GlobalEnv)
    glist[[length(glist)+1]] <- g
  }

  #  g
  if(length(glist)==1) {
    #plot(glist[[1]])
    g
  } else if(length(glist)>1){
    verb(length(glist), " ggplots have been created.")
    gridExtra::grid.arrange( grobs=glist, ncol=length(glist), nrow=1)
  } else {
    message("Nothing was created, because interactions weren't found (or for some other reason")
  }

}

#' Clean and precise theme
#'
#' @export
theme_mr <- function(...) {
  theme_minimal()+
    do.call("theme", append(list(
      panel.grid = element_blank(), #legend.position = "none",
      axis.line.x = element_line(color="black", size = 0.5),
      axis.line.y = element_line(color="black", size = 0.5),
      axis.title.x = element_text(face="bold", size=14),
      axis.title.y = element_text(face="bold", size=14)),
      list(...))
    )
}



#' Simple interaction plot with nice defaults
#'
#'
#'@param modl Model fitted with lm or lmer.
#'@param eff.id Order number of interaction term, integer.
#'@param x.var x.var argument of effect function.
#'@param ... Arguments passed to effect
#'
#' @export
plef <-   function(modl, eff.id=1, x.var=1,...) {
  predeff<- effects::effect(term=names(fixef(modl))[grepl(":", names(fixef(modl)))][eff.id], mod=modl, ...)
  plot(predeff, multiline=T, x.var=x.var, use.splines=T, confint=list(style="bands"), ticks=NULL,
       rug=F, colors=c("pink", "red", "black", "blue", "skyblue"))
}

#' Prepares data for significance staples on ggplot
#'
#'
#'@param df Aggregated data frame
#'@param compare.var Indicator of categories compared
#'@param y.var Means or other compared quantities (used to define the ends of the staples)
#'@param group.var Any additional variable in the dataset (e.g., groupuing variable)
#'@param heights Numeric vector. Height or width of each staple.
#'@param ends Numeric vector. Fixed ends for even staple graphs, in case specified, `y.var` is ignored.
#'
#' @export
sig_seg <- function(df, compare.var, y.var, group.var, heights, ends) {
  df <-untibble(df)
  df = df[,c(compare.var, y.var, group.var)]
  df[,compare.var]<-as.character(df[,compare.var])

  all.links = expand.grid(unique(df[,compare.var]),
                          unique(df[,compare.var]),
                          stringsAsFactors = F) %>%
    mutate(x = 1:9) %>%
    dcast(Var1 ~ Var2, value.var = "x") %>%
    set_rownames(.$Var1)

  segment.id = all.links[,-1][lower.tri(all.links[,-1])]
  segments = melt(all.links, id.vars = "Var1") %>% filter(value %in% segment.id) %>%
    mutate(variable = as.character(variable))

  framework = lapply(1:nrow(segments), function(x)
    as.data.frame.matrix(matrix(unlist(segments[x, c(1,1,1,2,2,2)]), nrow = 3, byrow = T)) %>% mutate(segm.id = paste(segments[x, c(1,2)], collapse = "_")  ))

  if(is.null(ends)) ends <- d[,y.var]

  segments.paths =
    lapply(df[,group.var], function(pop) {
      a = dplyr::filter(df,df[,group.var]==pop)
      lapply(1:length(framework), function(x) {
        fr = framework[[x]]
        fr$x1 = heights[[x]]
        fr$x2 = heights[[x]]
        if(is.null(ends))  {
          fr[1, "x1"]<- a[,y.var][a[,compare.var] == fr[1,1]]
          fr[3, "x2"]<- a[,y.var][a[,compare.var] == fr[3,1]]
        } else {
          fr[1, "x1"]<- ends[[x]]
          fr[3, "x2"]<- ends[[x]]
        }
        cbind(fr, pop_groups = pop)
      })})

  segm.df = Reduce(rbind, lapply(segments.paths, function(x) Reduce(rbind, x))) %>%
    set_names(c("y", "yend", "segm.id", "x", "xend", group.var))
  segm.df
}




#' Make a quick network graph
#'
#'
#' @param df Data frame
#' @param cv Correlation matrix (if df is NULL)
#' @param title  Title of the plot
#' @param legend  Legend position
#' @param gamma Gamma parameter for the model selection function, see `ggmModSelect`
#' @param layout  Default is "fr". Network layout. See more at `ggraph::ggraph`
#' @param start  Default is "glasso", see `ggmModSelect`
#' @param stepwise Default is TRUE, see `ggmModSelect`
#' @param seed  Attempt to make plots replicable.
#' @param group.id Vector of regex expressions to apply to variable names and use it to group variables.
#' @param item_label Function that converts the item names to node labels.
#' @param return.df Logical, if true, returns tidygraph dataset instead of the plot.

#' @examples
#' data("HolzingerSwineford1939", package = "lavaan")
#' get_net(HolzingerSwineford1939[, 7:15],
#'        group.id = c(first.factor = "1|2|3",
#'                     second = "4|5|6",
#'                     third = "7|8|9"),
#'        legend = "bottom"
#' )
#'
#' @export
get_net <- function(df = NULL, cv = NULL, n = NULL,
                    title = "",
                    legend = "none",
                    gamma = .3,
                    layout = "fr",
                    start = "glasso",
                    stepwise = T,
                    seed = 1,
                    group.id = NA,
                    item_label = function(x) gsub("\\D", "", x),
                    return.df = F, ...) {
  require(qgraph)
  require(tidygraph)
  require(ggraph)

  if(is.null(cv)) {
    cv <- cor(df, use = "pair")
    n <- nrow(na.omit(df))
  }

  group.key.df <- data.frame(varnames = colnames(cv))
  group.key.df$groupnames <- "none"

  if(all(!is.na(group.id))) {
    for(i in names(group.id)) {
      group.key.df$groupnames = ifelse(grepl(group.id[[i]], group.key.df$varnames), i, group.key.df$groupnames)
    }
  }



  ggm.out <- ggmModSelect(cv, n = n, stepwise=stepwise, gamma = gamma, start = start, nCores = 6, ...)

  gr.tbl <- ggm.out$graph
  gr.tbl[lower.tri(gr.tbl, diag = T)] <-NA
  gr.tbl2 <- gr.tbl %>%
    reshape2::melt(.) %>%
    filter(value>.05 & !duplicated(.))
  gr.tbl2$Var1 <- as.character(gr.tbl2$Var1)
  gr.tbl2$Var2 <- as.character(gr.tbl2$Var2)
  gr.tbl3 <- as_tbl_graph(gr.tbl2, directed = F)

  gr.tbl3 %<>%
    activate(nodes) %>%
    mutate(q.id = item_label(name)) %>%
    mutate(factor.lab = group.key.df[match(name, group.key.df$varnames), "groupnames"]) %>%
    mutate(name = gsub("\\D", "", name) )

if(!return.df) {
  set.seed(seed)
  ggraph(gr.tbl3, layout = layout, weights = value) +
    geom_edge_fan(aes(edge_width=value), alpha=.5, show.legend = F) +
    geom_node_circle(aes(r=.4, fill = factor.lab), show.legend = T)+
    geom_node_text(aes(label = q.id), size = 3)+
    scale_edge_width(range = c(0.0001, 3), guide = "none")+
    theme_void()+
    ggtitle(title)+
    theme(legend.position = legend)+
    coord_equal()+
    labs(fill = "")+
    guides(fill=guide_legend(ncol=3, title.position = "bottom",
                             title.theme =element_text(size=10),
                             override.aes = list(alpha = 1)))
} else {
  return(gr.tbl3)
}

}


