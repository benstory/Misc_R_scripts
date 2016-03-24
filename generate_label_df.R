require(multcompView)
require(ggplot2)
require(plyr)
source("~/Code/R_scripts/factorially_paste_lists.R")
source("~/Code/R_scripts/factorially_paste_rows.R")

generate_label_df <- function(orig.data, ggplot.obj, aov.eq, compare.panels=FALSE){
    ggplot.data <- ggplot.obj$data
    ggplot.build <- ggplot_build(ggplot.obj)
    ggplot.build.data <- as.data.frame(ggplot.build$data[[1]])
    num.panels <-  max(as.numeric(ggplot.build$panel$layout$PANEL), na.rm=TRUE)
    measure <- gsub(" ", "", strsplit(aov.eq, "~", fixed=TRUE)[[1]][1])
    aov.factors <- strsplit(gsub(" ", "", strsplit(aov.eq, "~", fixed=TRUE)[[1]][2]), "*", fixed=TRUE)[[1]]
    if(length(aov.factors) < 2) {
        factor.levels <- levels(eval(parse(text=paste("orig.data$", aov.factors, sep=""))))
    } else {
        factor.levels <- sapply(orig.data[,aov.factors], levels)
    }
    
    if(is.list(factor.levels)==TRUE) {
        factor.levels <- as.list(factor.levels)
        contrasts.order <- factorially_paste_lists(factor.levels)
    } else {
        factor.levels <- as.data.frame(factor.levels)
        contrasts.order <- factorially_paste_rows(factor.levels)
    }
    ult.contrasts <- as.data.frame(do.call(paste, c(ggplot.data[aov.factors], sep=":")))
    ult.contrasts[,1] <- factor(ult.contrasts[,1], levels=contrasts.order)
    ult.contrasts.name <- paste(aov.factors, collapse=":")
    names(ult.contrasts) <- ult.contrasts.name
    ggplot.data <- cbind(ggplot.data, ult.contrasts)
    if (compare.panels==FALSE) {
        if (num.panels > 1) {
            labels.df <- NULL
            aov.list <- list()
            facet.vars <- as.data.frame(ggplot.build$panel$layout[, !names(ggplot.build$panel$layout) %in% c("PANEL", 
                                                                                                             "ROW", 
                                                                                                             "COL", 
                                                                                                             "SCALE_X", 
                                                                                                             "SCALE_Y", 
                                                                                                             "AXIS_X", 
                                                                                                             "AXIS_Y")])
            facet.names <- names(ggplot.build$panel$layout)[!names(ggplot.build$panel$layout) %in% c("PANEL", 
                                                                                                     "ROW", 
                                                                                                     "COL", 
                                                                                                     "SCALE_X", 
                                                                                                     "SCALE_Y", 
                                                                                                     "AXIS_X", 
                                                                                                     "AXIS_Y")]
            names(facet.vars) <- facet.names
            panel <- 0
            for (i in c(1:length(facet.vars[,1]))) {
                # i <- 1
                # print(i)
                sub.ggplot.data <- ggplot.data
                panel <- panel + 1
                for (facet.name in facet.names) {
                    sub.ggplot.data <- subset(sub.ggplot.data, 
                                              eval(parse(text=paste(facet.name, 
                                                                    ' == "', 
                                                                    facet.vars[i, facet.name], 
                                                                    '"', 
                                                                    sep=""))))
                }
                sub.ggplot.aov <- aov(eval(parse(text=aov.eq)), data=sub.ggplot.data)
                aov.list[[panel]] <- sub.ggplot.aov
                sub.ggplot.tukey <- TukeyHSD(sub.ggplot.aov)
                final.level <- names(sub.ggplot.tukey[length(sub.ggplot.tukey)])
                final.level.tick <- paste("`", final.level, "`", sep="")
                Tukey.levels <- sub.ggplot.tukey[[final.level]][,4]
                if(length(names(Tukey.levels))==0) {
                    names(Tukey.levels) <- row.names(sub.ggplot.tukey[[final.level]])
                }
                Tukey.labels <- multcompLetters(Tukey.levels, reversed=TRUE)['Letters']
                Tukey.labels$Letters <- Tukey.labels$Letters[contrasts.order]
                Tukey.labels$Letters <- Tukey.labels$Letters[!is.na(Tukey.labels$Letters)]
                plot.labels <- names(Tukey.labels[['Letters']])
                
                max.range <- max(eval(parse(text=paste("sub.ggplot.data$", measure, sep=""))), na.rm=TRUE) - 
                    min(eval(parse(text=paste("sub.ggplot.data$", measure, sep=""))), na.rm=TRUE)
                buffer1 <- 10*(max.range^2)
                buffer2 <- 0.1*max.range
                if (buffer1 < buffer2) {
                    buffer <- buffer1
                } else {
                    buffer <- buffer2
                }
                boxplot.df <- ddply(sub.ggplot.data, 
                                    final.level.tick, 
                                    function (x) {
                                        max(eval(parse(text=paste("x$", measure, sep=""))), na.rm=TRUE) + 
                                            buffer
                                    })
                sub.ggplot.build.data <- subset(ggplot.build.data, PANEL == panel)
                x.coords <- (sub.ggplot.build.data$xmin + sub.ggplot.build.data$xmax)/2
                
                plot.levels <- data.frame(plot.labels, 
                                          sub.ggplot.data[c(1:length(x.coords)), facet.names], 
                                          x.coords, 
                                          labels = Tukey.labels[['Letters']], 
                                          stringsAsFactors = FALSE)
                names(plot.levels) <- c("contrast", facet.names, "x.coords", "labels")
                facet.labels.df <- merge(plot.levels, 
                                         boxplot.df, 
                                         by.x = "contrast", 
                                         by.y = 1, 
                                         sort = FALSE)
                labels.df <- rbind(labels.df, facet.labels.df)
            }
        } else {
            ggplot.aov <- aov(eval(parse(text=aov.eq)), data=ggplot.data)
            aov.list <- list(ggplot.aov)
            ggplot.tukey <- TukeyHSD(ggplot.aov)
            final.level <- names(ggplot.tukey[length(ggplot.tukey)])
            final.level.tick <- paste("`", final.level, "`", sep="")
            Tukey.levels <- ggplot.tukey[[final.level]][,4]
            if(length(names(Tukey.levels))==0) {
                names(Tukey.levels) <- row.names(ggplot.tukey[[final.level]])
            }
            Tukey.labels <- multcompLetters(Tukey.levels, reversed=TRUE)['Letters']
            Tukey.labels$Letters <- Tukey.labels$Letters[contrasts.order]
            plot.labels <- names(Tukey.labels[['Letters']])
            
            max.range <- max(eval(parse(text=paste("ggplot.data$", measure, sep=""))), na.rm=TRUE) - 
                min(eval(parse(text=paste("ggplot.data$", measure, sep=""))), na.rm=TRUE)
            buffer1 <- 10*(max.range^2)
            buffer2 <- 0.1*max.range
            if (buffer1 < buffer2) {
                buffer <- buffer1
            } else {
                buffer <- buffer2
            }
            boxplot.df <- ddply(ggplot.data, 
                                final.level.tick, 
                                function (x) {
                                    max(eval(parse(text=paste("x$", measure, sep=""))), na.rm=TRUE) + 
                                        buffer
                                })
            x.coords <- (ggplot.build.data$xmin + ggplot.build.data$xmax)/2
            
            plot.levels <- data.frame(plot.labels, 
                                      x.coords, 
                                      labels = Tukey.labels[['Letters']], 
                                      stringsAsFactors = FALSE)
            labels.df <- merge(plot.levels, 
                               boxplot.df, 
                               by=1, 
                               sort = TRUE)
        }
    } else {
        facet.vars <- as.data.frame(ggplot.build$panel$layout[, !names(ggplot.build$panel$layout) %in% c("PANEL", 
                                                                                                         "ROW", 
                                                                                                         "COL", 
                                                                                                         "SCALE_X", 
                                                                                                         "SCALE_Y", 
                                                                                                         "AXIS_X", 
                                                                                                         "AXIS_Y")])
        facet.names <- names(ggplot.build$panel$layout)[!names(ggplot.build$panel$layout) %in% c("PANEL", 
                                                                                                 "ROW", 
                                                                                                 "COL", 
                                                                                                 "SCALE_X", 
                                                                                                 "SCALE_Y", 
                                                                                                 "AXIS_X", 
                                                                                                 "AXIS_Y")]
        names(facet.vars) <- facet.names
        ggplot.aov <- aov(eval(parse(text=aov.eq)), data=ggplot.data)
        aov.list <- list(ggplot.aov)
        ggplot.tukey <- TukeyHSD(ggplot.aov)
        final.level <- names(ggplot.tukey[length(ggplot.tukey)])
        final.level.tick <- paste("`", final.level, "`", sep="")
        Tukey.levels <- ggplot.tukey[[final.level]][,4]
        if(length(names(Tukey.levels))==0) {
            names(Tukey.levels) <- row.names(ggplot.tukey[[final.level]])
        }
        Tukey.labels <- multcompLetters(Tukey.levels, reversed=TRUE)['Letters']
        Tukey.labels$Letters <- Tukey.labels$Letters[contrasts.order]
        plot.labels <- names(Tukey.labels[['Letters']])
        
        max.range <- max(eval(parse(text=paste("ggplot.data$", measure, sep=""))), na.rm=TRUE) - 
            min(eval(parse(text=paste("ggplot.data$", measure, sep=""))), na.rm=TRUE)
        buffer1 <- 10*(max.range^2)
        buffer2 <- 0.1*max.range
        if (buffer1 < buffer2) {
            buffer <- buffer1
        } else {
            buffer <- buffer2
        }
        boxplot.df <- ddply(ggplot.data, 
                            final.level.tick, 
                            function (x) {
                                max(eval(parse(text=paste("x$", measure, sep=""))), na.rm=TRUE) + 
                                    buffer
                            })
        x.coords <- (ggplot.build.data$xmin + ggplot.build.data$xmax)/2
        panel.names <- facet.vars[ggplot.build.data$PANEL, ,drop=FALSE]
        
        plot.levels <- data.frame(plot.labels, 
                                  panel.names,
                                  x.coords, 
                                  labels = Tukey.labels[['Letters']], 
                                  stringsAsFactors = FALSE)
        labels.df <- merge(plot.levels, 
                           boxplot.df, 
                           by=1, 
                           sort = TRUE)
    }
    return(list("labels.df"=labels.df, "aov.list"=aov.list))
}
