###Graphs and generic functions for comparative analysis##
require(ggplot2)
require(sjmisc)
require(ggrepel)
require(arm)
require(car)

#' Convenience function for logging the code
#'
#' @param ... Character strings.
#' @details Prints text and objects in order to log and debug the code.
#'
#' @export
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
                      label_col=NA) {
  library("RColorBrewer")
  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(reshape2)
  library(sjmisc)
  library(magrittr)
  library(stringr)


  label_col=label.col


  if(is.na(colors)) {
    colors<-c('#ffffd9','#edf8b1','#c7e9b4',
              '#7fcdbb','#41b6c4','#1d91c0',
              '#225ea8','#253494','#081d58', '#CCCCCC')
  }

  var.label <- str_wrap(attr(variable, "label"), 50)
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
      .[, order(.[sort.cat,])] %>%
      as.data.frame %>% set_colnames(c("var", "group", "Freq"))
  } else {

    library(survey)

    b <-  svydesign(id = ~0,
                    weights = ~ weight,
                    data = data.frame(var = variable, group=group, weight=weight),
                    strata=NULL,
                    fpc = NULL,
                    probs = NULL
    ) %>%
      svytable(~ var + group, .) %>%
      prop.table(2) %>%
      .[, order(.[sort.cat,])] %>%
      as.data.frame #%>% set_colnames(c("var", "group", "Freq"))
  }

  print(b)
  #print(reshape2::dcast(b,  group ~ var  ))



  g<- ggplot(b,aes(x = group, y = Freq, fill = rev(var) )) +
    geom_bar(position = "fill", stat = "identity", col="black", size=.1)+
    scale_y_continuous(name="", labels = percent_format()) +
    labs(x="", y="", fill="")+
    coord_flip() +
    theme(legend.title=element_blank(), legend.position = "bottom")+
    theme_minimal()+
    scale_fill_manual(name="", values=colors[1:length(levels(variable))],
                      labels=rev(levels(variable)))+
    theme(panel.grid = element_blank())

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
  library("RColorBrewer")
  library(stringr)
  library(ggplot2)
  library(scales)
  library(sjlabelled)
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

  library("RColorBrewer")
  library(stringr)
  library(ggplot2)
  library(scales)
  library(sjmisc)
  library(reshape2)
  library(sjlabelled)

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

  library("RColorBrewer")
  library(stringr)
  library(ggplot2)
  library(scales)
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
#' @param var Variable to aggregate.
#' @param group Group variable.
#' @param highlight.group Character vector of groups make bold.
#' @param codes "print" or "caption"
#' @param type "means", "ridges", "heat"
#'
#' @examples d<-data.frame(v=1:100, group=rep(1:2, 50))
#' graph_means_ci(d$v, d$group)
#' with(d, graph_means_ci(v, group))
#' @export
graph_means_ci <- function(var, group, highlight.group=NA, codes=c("print", "caption") , type=c("means", "ridges", "heat")) {

  group <- lab_to_fac(group)

  # if(!is.null(attr(var, "labels"))) {
  #   lbl.tbl<-  data.frame(codes= attr(var, "labels"),
  #                         labels= names(attr(var, "labels")),
  #                         row.names = NULL)
  #   print(lbl.tbl)
  # }


  # Caption and codes
  if(!is.null(attr(var, "labels"))) {
    g.caption <- paste(attr(var, "labels"),
                       attr(attr(var, "labels"), "names"), collapse="; \n")
  } else {
    g.caption <- paste(unique(var), collapse="; \n")
  }

  # X labs
  if(!is.null(attr(var, "label"))  &&  any(!attr(var, "label")==attr(var, "labels")) ) {
    x.label <-  attr(var, "label")
  } else {
    x.label <-   deparse(substitute(var))
  }


  if(type[1]=="means") {

    dt<-data.frame(
      mean=tapply(var, group, function(x) mean(x, na.rm=T), simplify = T),
      sd=tapply(var, group, function(x) sd(x, na.rm=T), simplify = T),
      n=tapply(var, group, function(x) length(x), simplify = T)
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
      labs(title = gsub('(.{1,50})(\\s|$)', '\\1\n', x.label))+
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

    library(ggridges)

    means<-tapply(var, lab_to_fac(group), function(x) mean(x, na.rm=T), simplify = T)

    dt<- data.frame(var=var,
                    group=factor(group, levels=names(means)[order(means)]))

    g<-ggplot(dt, aes(x=var, y=group))+
      geom_density_ridges(aes(fill=group %in% highlight.group),
                          show.legend = F, quantiles=4, scale = 2, size=0.3)+theme_mr()+
      scale_fill_manual(values=c("skyblue", "#FF8FDA"))+
      geom_point(data=data.frame(x=means, y=names(means)), aes(x, y), shape=19, size=1, color="black", alpha=.5)+
      geom_text(data=data.frame(x=means, y=names(means)),
                aes(x, y, label=format(x, digits=2, nsmall=2)), size=2, nudge_y = 0.5)+
      labs(x=x.label, y=NULL)

  } else if(type[1]=="heat") {

    freq.dt<-as.data.frame(prop.table(table(group, var, useNA="no"), margin=1))
    means<-tapply(var, lab_to_fac(group), function(x) mean(x, na.rm=T), simplify = T)
    means.dt<- data.frame(group=names(means), means=means)
    freq.dt$ones <- rep(1, nrow(freq.dt))
    freq.dt$group <- factor(freq.dt$group, levels=names(means)[order(means)])

    g<-ggplot(freq.dt, aes(group, ones, fill=Freq))+geom_col(position="stack")+coord_flip()+
      scale_fill_gradient2(low = "white", mid="#5FC768", high = "blue", midpoint = .5)+
      #scale_fill_distiller(type="seq", direction = 1, palette="GnBu")+
      geom_point(data=means.dt, aes(group, means, fill=NULL))+
      geom_text(data=means.dt, aes(group, means, fill=NULL, label=format(means, digits=2, nsmall=2)), size=2, nudge_y=.5)+
      scale_y_continuous(breaks=min(var,na.rm =T):max(var,na.rm =T) - 0.5, labels=min(var,na.rm =T):max(var,na.rm =T))+
      theme_mr()+labs(x="", y="", fill="Frequency")


    if(!is.null(highlight.group)) {
      bold.highlight <- rep("plain", length(levels(freq.dt$group)))
      bold.highlight[levels(freq.dt$group) %in% highlight.group] <- "bold"

      g<-g+theme(axis.text.y=element_text(face= bold.highlight ))+
        geom_vline(xintercept=(1:length(levels(freq.dt$group)))[levels(freq.dt$group) %in% highlight.group], linetype="dotted", color="black", size=.1)#+
        #scale_colour_manual(breaks = c("4", "6", "8"), values = c("red", "black") )
    }

  }


  if(codes[1]=="caption") {
    g+labs(caption = g.caption)
  } else if (codes[1]=="print") {
    cat(g.caption)
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
#' @param plot Logical. Should the plot be created?
#' @param print Logical. Should the result be printed in the console?
#'
#'
#' @details Recommended to use with `with` function, as in example.
#' @examples scatter_means_ci(ess$alcfreq, ess$HE, ess$cntry, plot=T, print=TRUE)
#'
#' @export
scatter_means_ci <- function(var1, var2, group, plot=TRUE, print=TRUE) {
  require(ggrepel)
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



  dt<-data.frame(
    mean1=tapply(var1, lab_to_fac(group), function(x) mean(x, na.rm=T), simplify = T),
    mean2=tapply(var2, lab_to_fac(group), function(x) mean(x, na.rm=T), simplify = T)
  )
  names.dt<-c(deparse(substitute(var1)),deparse(substitute(var2)))
  dt1<-dt; names(dt1) <- names.dt;
  if(print)  print(dt1)

  if(plot) {

    ggplot(dt, aes(mean1, mean2, label=row.names(dt)))+
      geom_point()+geom_text_repel()+
      labs(x=names.dt[1], y=names.dt[2])+geom_smooth(method = "lm", se = FALSE)+
      labs(caption=c(round(cor(dt$mean1, dt$mean2, use="pairwise.complete.obs"),2), "n=", nrow(na.omit(cbind(dt$mean1, dt$mean2))) ))+
      theme_mr()
    } else {
      dt
}
}


# Plot random effects #####
#' Plot random effects and interactoions for mer objects
#'
#'Takes object produced by lme4::lmer() function, extracts random effects and interactions, and returns ggplot2-based plots to examine.
#'
#' @param lmer.fit A mer object
#' @param optional.names A named vector of random effects, where names are used as titles of the output plots. The length  should be equal to a number of random effects. Usually it includes "(Intercepts)" and names of variables whose effects are made random.
#' @param facets Logical. Should the random effects be plotted in facets or as a series of single plots?
#' @param scatter Logical. Should the scatterplots of the cross-level interactions be plotted? If TRUE, `facets` argument corresponds to scatterplots. This option is under development...
#' @return Returns one or several ggplots. In case one plot is returned it can be appended with `theme`, `geom_`, etc.
#' @examples random_plot(lmr, optional.names=c(Intercept="(Intercepts)", Female="gndr"), facets=TRUE)
#' @export
#'
random_plot <- function(lmer.fit, optional.names=NA, facets=FALSE) { #optional.names should be a named vector


  group.name<-names(getME(lmer.fit, "flist"))

  ses<-as.data.frame(arm::se.coef(lmer.fit)[[group.name]])
  #dt<-subset(coef(lmer.fit)[[group.name]], select=names(ses))
  dt<-coef(lmer.fit)[[group.name]][,names(ses)]

  if(length(optional.names)==1 && is.na(optional.names)) {
    optional.names<-names(ses)
    names(optional.names)<-names(ses)
  }


  library(grid)
  glist<-gList()
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
      geom_errorbar(aes(alpha=significant), size=.5, width=.5, colour="black", show.legend = F)+geom_point()+
      scale_alpha_manual(values=c("FALSE"=0.3, "TRUE"=1))+
      geom_hline(aes(yintercept=0), linetype="dashed"  )+coord_flip()+
      labs(x="", y=paste("", optional.names[i] ))+theme_minimal()+
      theme(axis.line = element_line(colour = "black"),
            panel.grid = element_blank(),
            plot.caption = element_text(size=10, hjust=0))

    if(!facets) {
      print(g)
    } else {
      glist[[i]] <- g
    }
  }

  if(!facets)  {
    cat(length(names(ses)), "graphs were created.")

  } else {
    gridExtra::grid.arrange( grobs=glist, ncol=length(glist), nrow=1)

  }


}


# Cross-level interaction plot ####
#' Plot cross-level interactions for mer objects
#'
#'Takes object produced by lme4::lmer() function, and returns interaction plot(s).
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
#' @param labs List of length equal to a number of cross-level interactions, each element should contain sublist with names "x", "y", "line1", "line2", "line3", optionally "line4", "line5", "caption".
#' @param x.levels desired x-axis coordinates, individual-level term. If NULL (default) are defined automatically.
#' @return Returns one or several ggplots. In case one plot is returned it can be appended with `+theme()`, `+geom_()`, etc.
#' @examples random_interaction(lmr, "model.1SD.2SD")
#' @details It somehow repeats functionality of [sjPlot::sjp.int], but differs in being able to select real groups close to +/- 1 sd and mean of moderating variable; makes prettier and customizable plots.
#' @seealso \link{random_plot} \link{good_table}
#' @md
#'
#' @export
random_interaction <- function( x,
                                real=TRUE,
                                z.levels=c("1SD", "2SD", "1.2SD", "all"),
                                scatter=TRUE, labs=NULL,
                                x.levels=NULL,
                                ... ) {

  # Works only with cross-level two-way interactions
  # Test it with factor predictors
  # Test it with multiple interactions +
  # Test it with non-cross-level interactions
  # Add scatterlot of random effects vs explanatory variable


  library("effects")
  library("stringr")
  library("lme4")
  library("grid")

  z.levels<-z.levels[1]


  interaction.terms <- names(fixef(x))[grep(":", names(fixef(x)))]
  #verb(interaction.terms)

  # Are there any interactions
  if(length(interaction.terms)==0) stop("Interactions were not found!")
  #verb("interaction.terms", str(interaction.terms))

  new.data<-model.frame(x)
  is.there.any.labelled.variables <-0

  glist<-gList()



  for(i in names(fixef(x))[-1]) {
    if(class(model.frame(x)[[i]])=="labelled")  {
      is.there.any.labelled.variables <-is.there.any.labelled.variables+1
      class(new.data[[i]]) <- "numeric"
    }}

  if(is.there.any.labelled.variables>0) {
    message(paste("There are",is.there.any.labelled.variables, "variable(s) of class 'labelled', with which package 'effects' doesn't work, so refitting to a data with 'labelled' converted to 'numeric'. " ))
    x<-lmer(formula(x), data=new.data, weights=`(weights)`)
  }

  group.name<-names(getME(x, "flist"))
  #verb("Group name is", group.name)

  interaction.terms.split<- str_split(interaction.terms, ":")
  verb("interaction.terms.split", interaction.terms.split)

  for(i in 1:length(interaction.terms.split)) {


    #Check if one of the variables in an interaction is a group-level interaction
   sum.var1<- sum(aggregate(model.frame(x)[ ,interaction.terms.split[[i]][1] ], list(model.frame(x)[,group.name]), sd, na.rm=T)[,2])
   sum.var2<- sum(aggregate(model.frame(x)[ ,interaction.terms.split[[i]][2] ], list(model.frame(x)[,group.name]), sd, na.rm=T)[,2])

    if(sum.var1==0) {
      group.eff.name <- interaction.terms.split[[i]][1]
      #verb("Variable ", interaction.terms.split[[i]][1], "is group-level, as its variance within group is 0.")

    } else if(sum.var2==0){
      group.eff.name <- interaction.terms.split[[i]][2]
      #verb("Variable ", interaction.terms.split[[i]][2], "is group-level, as its variance within group is 0.")

    } else {
      verb("The interaction '", paste(interaction.terms.split[[i]], collapse=" X "), "' is a first-level interaction. Currently I ignore it.")
      next
    }


  if( sum( interaction.terms.split[[i]] %in% names(ranef(x)[[group.name]]) )>0 ) {

    random.eff.name <- interaction.terms.split[[i]][interaction.terms.split[[i]] %in% names(ranef(x)[[group.name]])]
verb("Attempting to plot interaction", random.eff.name, " X ", group.eff.name, ".", sep="")

  } else {
    warning("Not", paste(interaction.terms.split[[i]], collapse=" nor ") , "is a random effect. Ignore this interaction.")
    next
  }


   group.var.means <- aggregate(model.frame(x)[[group.eff.name]], list(model.frame(x)[[group.name]]), mean, na.rm=T)

   mean.level.group.eff <- mean(group.var.means[,2], na.rm=T)
   sd.level.group.eff <- sd(group.var.means[,2], na.rm=T)

   if(z.levels == "1SD") {
     noflines <- 1:3
   } else if (z.levels == "2SD") {
     noflines <- c(1,4,5)
   } else if(z.levels == "12SD") {
     noflines <- 1:5
   } else if(z.levels == "all") {
     #noflines <- 1:nrow(group.var.means)
     message("Plotting all the groups.")
     real=TRUE
   } else {
     "Incorrect z.levels argument."
   }

if(real==FALSE) {

      moderator.levels.to.plot <- list(name1=c(
        mean.level.group.eff,
        mean.level.group.eff+sd.level.group.eff,
        mean.level.group.eff-sd.level.group.eff,
        mean.level.group.eff+2*sd.level.group.eff,
        mean.level.group.eff-2*sd.level.group.eff)[noflines])

  } else {
    if(z.levels == "all") {

      moderator.levels.to.plot <- list(name1= group.var.means[,2])
      index.groups.needed <- 1:nrow(group.var.means)
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
      moderator.levels.to.plot[[length(moderator.levels.to.plot)+1]] <-  x.levels
      names(moderator.levels.to.plot)[length(moderator.levels.to.plot)]<- strsplit(interaction.terms[i], ":")[[1]][1]
    }

print("Meow")
print(moderator.levels.to.plot)

# Compute effects

    assign("new.data", new.data, envir=.GlobalEnv)
    ploff<-effect(term=interaction.terms[i], xlevels=moderator.levels.to.plot, mod=x, ...)
    print(ploff)
    db<- data.frame(ploff$x, ploff$fit, ploff$lower, ploff$upper)
    real.names<-names(db)
    names(db)<-c("dep", "moderator", "ind", "lower", "upper")
    db$moderator <- as.factor(round(db$moderator, 2))
#print(db)


#Plot effects
    g<- ggplot(db, aes(dep, ind, col=moderator, group=moderator))+
      geom_line(aes(linetype=moderator))+
      geom_ribbon(aes(ymin=lower, ymax=upper, fill=moderator), alpha=0.1, col=NA)+
      labs(x=real.names[1], y=ploff$response, #col=real.names[2], fill=real.names[2], linetype=real.names[2],
           caption=paste("Predicted slopes conditioned on group-level variable", group.eff.name)  )+
      theme_minimal()+theme(legend.position = "none", axis.line = element_line(colour = "grey"),
                            panel.grid = element_blank())



      if(length(moderator.levels.to.plot[[1]])==3) {
        col.scale.values <- c("red", "black", "blue")
        linetype.scale <- c("dashed", "solid", "dashed")
        labels <- paste(group.eff.name,  c("mean", "plus SD", "minus SD"))

      } else if (length(moderator.levels.to.plot[[1]])==5) {
        col.scale.values <- c("magenta", "red", "black", "blue", "skyblue")
        linetype.scale <-   c("dotted", "dashed", "solid", "dashed", "dotted")
        labels <- paste(group.eff.name,  c("mean", "plus 1SD", "minus 1SD", "plus 2SD","minus 2SD"))
      } else {
        labels <- group.var.means[,1]
      }

    #verb("moderator.levels.to.plot", moderator.levels.to.plot)

    labels <- paste(labels, round(moderator.levels.to.plot[[1]],2)  )


    if(real) {
    labels <- paste(labels, "; b=", round(coef(x)[[group.name]][,random.eff.name][index.groups.needed],2), sep="")
    if(z.levels!="all") {
    labels <- sapply(1:length(labels),
                     function(u) paste(labels[u], group.var.means[,1][index.groups.needed][u], sep=","))
    }
    } else {
      warning("Here computations of slopes should be")
    }


    if(length(moderator.levels.to.plot[[1]])<=5) {
      g<- g + scale_color_manual(values=col.scale.values)+
        scale_fill_manual(values=col.scale.values)+
        scale_linetype_manual(values=linetype.scale)
    }

    library(ggrepel)
    if(is.null(labs)) {
    g<-g+geom_text_repel(data=db[db$dep==max(db$dep),], aes(label=labels))
    } else {
      l<-labs[[length(glist)+1]]
      g<-g+labs(x=l[["x"]], y=l[["y"]], caption=ifelse(is.null(l[["caption"]]),"", l[["caption"]]))+
        geom_text_repel(data=db[db$dep==max(db$dep),], aes(label=c(l[["line1"]],l[["line2"]],l[["line3"]])  ),
                        box.padding = unit(0.5, "lines"))
    }



#Construct labels
    #
    # if(real=FALSE) {
    # #Predicted slopes conditioned on the interaction effect
    # main.effect<-fixef(x)[interaction.terms.split[[i]]][random.eff.name]
    # group.effect<-fixef(x)[interaction.terms.split[[i]]][group.eff.name]
    # interaction.effect<-fixef(x)[interaction.terms[i]]
    #
    # # For exmaple, we have
    # # y = a + 5x + 2z + 0.5xz, where x is variable with random effect and z is a group-level variable.
    # # For mean(z):
    # #  y = a + 5*x + 2*mean(z) + 0.5*(x)*mean(z)
    # slopes <- c(
    #   main.effect - (mean.level.group.eff + 2*sd.level.group.eff)*interaction.effect,
    #   main.effect - (mean.level.group.eff + sd.level.group.eff)*interaction.effect,
    #   main.effect + mean.level.group.eff*interaction.effect,
    #   main.effect + (mean.level.group.eff + sd.level.group.eff)*interaction.effect,
    #   main.effect + (mean.level.group.eff + 2*sd.level.group.eff)*interaction.effect
    # )
    #
    # } else if(real=TRUE) {
    #
    #     slopes <- ranef(x)[[group.name]][,random.eff.name]
    #     names(slopes)<-rownames(ranef(x)[[group.name]])
    #
    #      if(z.levels!="all") {
    #
    #        slopes <- slopes[rownames(slopes)==c(group.close.to.minus2SD[,1],
    #                                             group.close.to.minusSD[,1],
    #                                             group.close.to.mean[,1],
    #                                             group.close.to.plusSD[,1],
    #                                             group.close.to.plus2SD[,1]),]
    #
    #        label.group <-  paste()
    #
    #        } else {
    #        label.group <- rownames(slopes)
    #
    #   }
    #
    #
    #
    #
    #
    # if(z.levels == "1SD") {
    #
    #   label.group <- c(
    #     paste(group.close.to.minusSD, ", \n",
    #           group.eff.name, "(~—1sd)=", round(moderator.levels.to.plot[[1]][1], 2), sep=""),
    #     paste(group.close.to.mean,    ", \n",
    #           group.eff.name, "(~mean)=", round(moderator.levels.to.plot[[1]][2], 2), sep=""),
    #     paste(group.close.to.plusSD,  ", \n",
    #           group.eff.name, "(~+1sd)=", round(moderator.levels.to.plot[[1]][3], 2), sep=""))
    #
    # } else if(z.levels == "real.2SD") {
    #
    #   label.group <- c(
    #     paste(group.close.to.minus2SD, ", \n",
    #           group.eff.name, "(-2sd)=", round(moderator.levels.to.plot[[1]][1], 2), sep=""),
    #     paste(group.close.to.mean,    ", \n",
    #           group.eff.name, "(mean)=", round(moderator.levels.to.plot[[1]][2], 2), sep=""),
    #     paste(group.close.to.plus2SD,  ", \n",
    #           group.eff.name, "(+2sd)=", round(moderator.levels.to.plot[[1]][3], 2), sep=""))
    #
    # } else if(z.levels=="model.1SD") {
    #
    #   label.group <- c(
    #     paste(group.eff.name, "(-1sd)=", round(moderator.levels.to.plot[[1]][1], 2),  "; b=", round(slope.minus1,2) , sep=""),
    #     paste(group.eff.name, "(mean)=", round(moderator.levels.to.plot[[1]][2], 2), "; b=", round(slope0,2), sep=""),
    #     paste(group.eff.name, "(+1sd)=", round(moderator.levels.to.plot[[1]][3], 2), "; b=", round(slope.plus1,2), sep=""))
    #
    # } else if( z.levels=="model.2SD") {
    #
    #   label.group <- c(
    #     paste(group.eff.name, "(-2sd)=", round(moderator.levels.to.plot[[1]][1], 2),  "; b=", round(slope.minus2,2) , sep=""),
    #     paste(group.eff.name, "(mean)=", round(moderator.levels.to.plot[[1]][2], 2), "; b=", round(slope0,2), sep=""),
    #     paste(group.eff.name, "(+2sd)=", round(moderator.levels.to.plot[[1]][3], 2), "; b=", round(slope.plus2,2), sep=""))
    #
    #
    # } else if(z.levels=="model.1SD.2SD") {
    #
    #   label.group <- c(
    #     paste(group.eff.name, "(-2sd)=", round(moderator.levels.to.plot[[1]][1], 2),  "; b=", round(slope.minus2,2) , sep=""),
    #     paste(group.eff.name, "(-1sd)=", round(moderator.levels.to.plot[[1]][2], 2),  "; b=", round(slope.minus1,2) , sep=""),
    #     paste(group.eff.name, "(mean)=", round(moderator.levels.to.plot[[1]][3], 2), "; b=", round(slope0,2), sep=""),
    #     paste(group.eff.name, "(+1sd)=", round(moderator.levels.to.plot[[1]][4], 2), "; b=", round(slope.plus1,2), sep=""),
    #     paste(group.eff.name, "(+2sd)=", round(moderator.levels.to.plot[[1]][5], 2), "; b=", round(slope.plus2,2), sep=""))
    #
    #
    # }
    #

if(scatter) {
    scatter <- qplot(group.var.means[,2], coef(x)[[group.name]][group.var.means[,1],random.eff.name],
                     label= group.var.means[,1],
                     geom=c( "text"),
                     xlab=group.eff.name, ylab=paste("Effect of", random.eff.name, "on", as.character(x@call$formula[2])))

    g <- gridExtra::grid.arrange( scatter, g, nrow=2, ncol=1)
  }

    rm(new.data, envir=.GlobalEnv)
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
theme_mr <- function() {
  theme_minimal()+
    theme(panel.grid = element_blank(), #legend.position = "none",
      axis.line.x = element_line(color="black", size = 0.5),
      axis.line.y = element_line(color="black", size = 0.5),
      axis.title.x = element_text(face="bold", size=14),
      axis.title.y = element_text(face="bold", size=14))
}



#' Simple interaction plot with nice defaults
#'
#'
#'@param modl Model fitted with lm or lmer.
#'@param eff.id Order number of interaction term, integer.
#'@param ... Arguments passed to effect
#'
#' @export
plef <-   function(modl, eff.id=1, ...) {
  predeff<- effects::effect(term=names(fixef(modl))[grepl(":", names(fixef(modl)))][eff.id], mod=modl, ...)
  plot(predeff, multiline=T, x.var=1, use.splines=T, confint=list(style="bands"), ticks=NULL,
       rug=F, colors=c("pink", "red", "black", "blue", "skyblue"))
}


# Sys.setenv(TZ="Europe/Lisbon")
# Sys.getenv("TZ")
