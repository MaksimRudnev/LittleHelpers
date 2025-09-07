#' Correlation table with stars
#'
#' @description Produces data frame of correlations with stars standing for significance
#' Create Correlation Table with Significance Stars
#'
#' Creates a formatted correlation matrix with optional significance stars.
#' Handles tibble inputs and provides flexible output formats.
#'
#' @param d A data frame or tibble containing numeric variables
#' @param method Character string indicating correlation method. One of "pearson",
#'   "kendall", or "spearman" (default)
#' @param star Logical. If TRUE (default), adds significance stars to correlations
#' @param raw Logical. If TRUE, returns raw correlation matrix without formatting
#'
#' @return A matrix containing formatted correlations with significance stars (if requested)
#'   or raw correlation values
#'
#' @details Significance levels: * p < 0.05, ** p < 0.01, *** p < 0.001
#'
#' @importFrom dplyr select
#' @importFrom reshape2 dcast
#' @importFrom magrittr set_rownames
#' @importFrom stats cor cor.test
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' data <- data.frame(x = rnorm(100), y = rnorm(100), z = rnorm(100))
#' cor_table(data)                          # With stars
#' cor_table(data, star = FALSE)           # Without stars
#' cor_table(data, raw = TRUE)             # Raw correlations
#' cor_table(data, method = "pearson")     # Pearson method
#' }
#'
#' @export
cor_table <- function(d,
                      method = "spearman",
                      star = TRUE,
                      raw = FALSE) {

  # Handle tibble input
  if(any(class(d) == "tbl")) {
    d <- LittleHelpers::drop_labs(LittleHelpers::untibble(d))
  }

  # Calculate correlation matrix
  r <- stats::cor(d, use = "pairwise.complete.obs", method = method)

  # Calculate sample sizes (for potential future use)
  # n <- sapply(1:ncol(d), function(a) {
  #   sapply(1:ncol(d), function(b) {
  #     v <- !is.na(d[, c(a, b)]) %>% rowSums(.) == 2
  #     sum(v)
  #   })
  # })
  # rownames(n) <- colnames(n) <- names(d)

  # Return raw correlations if requested
  if(raw) {
    return(r)
  }

  if(star) {
    # Calculate p-values
    varpairs <- t(expand.grid(names(d), names(d)))
    ps <- rbind(varpairs,
                p = apply(varpairs, 2, function(x) {
                  rrr <- stats::cor.test(as.numeric(d[, x[[1]]]),
                                         as.numeric(d[, x[[2]]]),
                                         use = "pairwise.complete.obs",
                                         method = method)
                  rrr$p.value
                })) %>%
      t %>%
      as.data.frame %>%
      reshape2::dcast(Var1 ~ Var2, value.var = "p") %>%
      tibble::set_rownames(., .$Var1) %>%
      dplyr::select(-Var1) %>%
      as.matrix

    ps <- ps[rownames(r), colnames(r)]

    # Create significance stars
    starz <- matrix(rep("", ncol(d)^2), nrow = ncol(d))
    starz[as.numeric(ps) < 0.05] <- "*"
    starz[as.numeric(ps) < 0.01] <- "**"
    starz[as.numeric(ps) < 0.001] <- "***"
    diag(starz) <- ""

    # Format correlations with stars
    m <- paste(format(round(r, 2), digits = 2, nsmall = 2), starz, sep = "")
    m <- as.data.frame(matrix(m, nrow = ncol(d)))

  } else {
    # Format correlations without stars
    m <- format(round(r, 2), digits = 2, nsmall = 2)
    m <- as.data.frame(matrix(m, nrow = ncol(d)))
  }

  # Set names and return as matrix
  names(m) <- names(d)
  rownames(m) <- names(d)
  as.matrix(m)
}

#' Standard Error of Mean
#'
#' Calculates the standard error of the mean for a numeric variable,
#' removing NA values automatically.
#'
#' @param variable A numeric vector
#'
#' @return Standard error of the mean (numeric scalar)
#'
#' @importFrom stats na.omit var
#'
#' @examples
#' se(c(1, 2, 3, 4, 5))
#' se(c(1, 2, NA, 4, 5))  # NAs removed automatically
#'
#' @export
se <- function(variable) {
  variable <- stats::na.omit(variable)
  sqrt(stats::var(variable) / length(variable))
}

#' Convert P-values to Significance Stars
#'
#' Converts a vector of p-values to significance stars using standard thresholds.
#'
#' @param pvalues Numeric vector of p-values
#' @param na Character string to return for NA values (default: "")
#'
#' @return Character vector with significance stars
#'
#' @details Significance levels: *** p < 0.001, ** p < 0.01, * p < 0.05
#'
#' @examples
#' pvalue_to_stars(c(0.001, 0.01, 0.05, 0.1))
#' pvalue_to_stars(c(0.001, NA, 0.05), na = "?")
#'
#' @export
pvalue_to_stars <- function(pvalues, na = "") {
  ifelse(is.na(pvalues), na,
         ifelse(pvalues < 0.001, "***",
                ifelse(pvalues < 0.01, "**",
                       ifelse(pvalues < 0.05, "*", " "))))
}

#' Format Estimates with Significance Stars and Optional Confidence Intervals
#'
#' Creates formatted strings combining point estimates, optional confidence intervals,
#' and significance stars from p-values.
#'
#' @param est Numeric vector of point estimates
#' @param pvalue Numeric vector of p-values
#' @param digits Number of decimal places for rounding (default: 2)
#' @param na Character string to return for NA estimates (default: "")
#' @param ci.low Numeric vector of confidence interval lower bounds (optional)
#' @param ci.hi Numeric vector of confidence interval upper bounds (optional)
#'
#' @return Character vector of formatted estimate strings
#'
#' @details When confidence intervals are provided, format is: "est[ci.low ci.hi]***"
#'   When not provided, format is: "est***"
#'
#' @examples
#' eststar(1.234, 0.001)
#' eststar(1.234, 0.001, digits = 3)
#' eststar(c(1.2, 2.3), c(0.01, 0.1))
#' eststar(1.234, 0.001, ci.low = 0.8, ci.hi = 1.7)
#'
#' @export
eststar <- function(est, pvalue, digits = 2, na = "", ci.low = NULL, ci.hi = NULL) {

  # Vectorized number formatting with NA handling
  fmt <- function(x) ifelse(is.na(x), na, formatC(x, format = "f", digits = digits))

  # Build CI component if both bounds provided
  ci_part <- if (!is.null(ci.low) && !is.null(ci.hi)) {
    paste0("[", fmt(ci.low), " ", fmt(ci.hi), "]")
  } else ""

  # Combine estimate + CI + stars
  paste0(fmt(est), ci_part, pvalue_to_stars(pvalue, na))

}


#' Combine Named Vectors into Data Frame by Matching Names
#'
#' Creates a data frame by matching names across multiple vectors. Unnamed elements
#' (NA names) are preserved as separate rows for each vector.
#'
#' @param ... Named vectors, or a single list of named vectors
#'
#' @return Data frame with matched names as rows and vectors as columns
#'
#' @details Named elements are matched by name across all vectors. Unnamed elements
#'   (with NA names) are added as separate rows, identified by vector source.
#'   Column names are taken from argument names or generated as V1, V2, etc.
#'
#' @examples
#' v1 <- c(a = 1, b = 2, 3)        # 3 has no name
#' v2 <- c(a = 10, c = 30, 40)     # 40 has no name
#' v3 <- c(b = 200, d = 400, 500)  # 500 has no name
#' match_named_vectors(v1, v2, v3)
#'
#' # With custom names
#' match_named_vectors(first = v1, second = v2, third = v3)
#'
#' # With a list
#' match_named_vectors(list(v1 = v1, v2 = v2, v3 = v3))
#'
#' @export
match_named_vectors <- function(...) {

  # Handle both ... and list input
  if (length(list(...)) == 1 && is.list(list(...)[[1]]) &&
      !is.null(names(list(...)[[1]]))) {
    vectors <- list(...)[[1]]
  } else {
    vectors <- list(...)
    # Generate column names if not provided
    arg_names <- names(vectors)
    if (is.null(arg_names)) {
      names(vectors) <- paste0("V", seq_along(vectors))
    } else {
      unnamed_idx <- arg_names == "" | is.na(arg_names)
      names(vectors)[unnamed_idx] <- paste0("V", which(unnamed_idx))
    }
  }

  n_vectors <- length(vectors)
  col_names <- names(vectors)

  # Process each vector to separate named and unnamed elements
  processed <- lapply(seq_along(vectors), function(i) {
    v <- vectors[[i]]
    v_names <- if (is.null(names(v))) rep(NA, length(v)) else names(v)

    list(
      named_elements = v[!is.na(v_names)],
      named_names = v_names[!is.na(v_names)],
      unnamed_elements = v[is.na(v_names)],
      col_name = col_names[i]
    )
  })

  # Get all unique names across all vectors
  all_names <- unique(unlist(lapply(processed, function(x) x$named_names)))

  # Create data frame for matched names
  df_named <- data.frame(
    matrix(NA, nrow = length(all_names), ncol = n_vectors,
           dimnames = list(all_names, col_names))
  )

  # Fill in named elements
  for (i in seq_along(processed)) {
    p <- processed[[i]]
    if (length(p$named_elements) > 0) {
      df_named[p$named_names, i] <- p$named_elements
    }
  }

  # Add unnamed elements
  for (i in seq_along(processed)) {
    p <- processed[[i]]
    if (length(p$unnamed_elements) > 0) {
      # Create rows for unnamed elements
      n_unnamed <- length(p$unnamed_elements)
      unnamed_rows <- matrix(NA, nrow = n_unnamed, ncol = n_vectors,
                             dimnames = list(
                               paste0(p$col_name, "_unnamed_", seq_len(n_unnamed)),
                               col_names
                             ))
      unnamed_rows[, i] <- p$unnamed_elements

      # Add to main data frame
      df_named <- rbind(df_named, as.data.frame(unnamed_rows))
    }
  }

  df_named
}




#' Prints the number and percent of NAs in a dataframe
#'@param data Data.frame
#'@param missing.code List of values that should be considered missing.
#'@param separately If `TRUE`, reports missing values separately for each value in `missing` argument. If `FALSE`, reports total number of missing values. Only makes sense if `missing` is longer than 1.
#'@param viewer If `TRUE``, show in Rstudio viewer, if FALSE returns a data.frame
#'@importFrom scales percent
#'@export
na_tab <- function(data, missing.code = c(NA, ""),
                   separately = F,
                   viewer = T) {


  lst <- apply(data, 2, function(x) {

    mis.sum = numeric(0)

    if(any(is.na(missing.code))) mis.sum = c("NA" = sum(is.na(x)))

    if(length(na.omit(missing.code))>0)  {
      mis.codes = sapply(na.omit(missing.code), function(code) sum(x == code, na.rm = T))
      names(mis.codes) = paste0("string_'", na.omit(missing.code), "'")
      mis.sum = c(mis.sum, mis.codes)
    }

    if(separately) {

      mis.sum.percentage = scales::percent(mis.sum / nrow(data))
      names(mis.sum.percentage) = paste0(names(mis.sum), "_percent")

      mis.sum.rows = c(mis.sum,  mis.sum.percentage)
      mis.sum.rows = mis.sum.rows[sort(names(mis.sum.rows))]

    } else {
      mis.sum.rows = c(
        Missing_total = sum(mis.sum),
        Missing_percent = scales::percent(sum(mis.sum) / nrow(data))
      )
    }
    mis.sum.rows
  })


  if(viewer) df_to_viewer(t(lst), kable = T) else return(lst)

}



#' Easy cross-tabulation with labels
#' @param rows Character, variable name to put in rows
#' @param cols Character, variable name to put in columns
#' @param data Data.frame containing variables
#' @param margin If any proportions should be computed, might be `row`, `col`, or `none`.
#' @param useNA How to deal with NAs, passed to `table`.
#' @param drop.empty Remove empty categories?
#' @examples
#'\dontrun{
#'    crosstab("country", "frequency", wvs6, "row")
#' }
#'

#'
#' @export
crosstab <- function(rows, cols, data, margin="row", useNA="always", drop.empty = T) {

  row = lab_to_fac(data[,rows])
  col = lab_to_fac(data[,cols])

  if(drop.empty) {
    if(is.factor(row)) row <- droplevels(row)
    if(is.factor(col)) col <- droplevels(col)
  }


  tb <- table(row,col, useNA=useNA)

  if(margin=="col") {
    tb <- prop.table(tb, 2)
  } else if (margin == "row") {
    tb <- prop.table(tb, 1)
  }

  rownames(tb)[is.na(rownames(tb))]<-".NA"


  if("tbl_df" %in% class(data)) {
    title = paste("[", rows, "]",  attr(lab_to_fac(data[,rows]), "header"), " BY ",
                  "[", cols, "]", attr(lab_to_fac(data[,cols]), "header"), sep="")

  } else {
    title = paste("[", rows, "]",  "[", cols, "]")
  }

  print(title)
  print(tb)

    df_to_viewer(
      as.data.frame.matrix(tb),
      digits=2,
      kable.options=c(caption = title)
    )


}


#' Format the number quickly and remove zero before dot
#'
#' @param x any numeric vector or value.
#' @param digits number of decimals.
#' @returns Character of the formatted
#' @export
f = function(x, digits=3) {
  x = as.numeric(x)
  ifelse(grepl("^-", x),
         sub("^-0\\.", "-.", sprintf(paste0("%.", digits, "f"), x)),
         sub("^0\\.", ".", sprintf(paste0("%.", digits, "f"), x))
  )
}


#' Reverse values and value labels
#'
#' @description  Returns same variable with reversed values. Reversing makes use of observed values rather than stored factor levels or attributes.
#'
#' This function is deprecated and will be removed in future versions. Consider using `car::Recode()` or similar functions for reversing values.
#'
#' @param var variable to reverse.
#' @param preserve.labels Attempt saving labels.
#' @export
reverse <-function(var, preserve.labels = T) {

  straight<-sort(unique(var), F)
  reversed<-sort(unique(var), T)

  #new.var<-sapply(var, function(x) reversed[straight==x][1] )

  # Faster version
  new.var <- rep(NA, length(var))
  for(i in 1:length(straight) ) new.var[var==straight[i]] <- reversed[i]


  if(length(attr(var, "labels", exact = T))!=0 & preserve.labels) {
    old.labels <- attr(var, "labels", exact = T)
    names(reversed) <- sapply(straight, function(x)  names(old.labels)[old.labels==x]  )
    attr(new.var, "labels") <- reversed
    attr(new.var, "label") <- attr(var, "label", exact = T)
  }
  new.var
}


