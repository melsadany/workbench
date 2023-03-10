## ----style, echo=FALSE, results="asis", message=FALSE-------------------------
knitr::opts_chunk$set(tidy = FALSE,
		   message = FALSE)

## ----echo=FALSE, results="hide", message=FALSE--------------------------------
library("ggplot2")
library("cowplot")
library("grid")
library("vcd")
library("lattice")
library("colorspace")
library("ggimage")

library("ggplotify")
theme_set(theme_grey())

## ----warning=FALSE------------------------------------------------------------
library("grid")
library("ggplotify")

p1 <- as.grob(~barplot(1:10))
p2 <- as.grob(expression(plot(rnorm(10))))
p3 <- as.grob(function() plot(sin))

library("vcd")
data(Titanic)
p4 <- as.grob(~mosaic(Titanic))

library("lattice")
data(mtcars)
p5 <- as.grob(densityplot(~mpg|cyl, data=mtcars))

## ----fig.width=7, fig.height=7------------------------------------------------
grid.newpage()
grid.draw(p1)
vp = viewport(x=.35, y=.75, width=.35, height=.3)
pushViewport(vp)
grid.draw(p2)
upViewport()

## ----warning=FALSE------------------------------------------------------------
library(ggplot2)
p1 <- as.ggplot(~barplot(1:10)) +
    annotate("text", x = .6, y = .5,
             label = "Hello Base Plot", size = 5,
             color = 'firebrick', angle=45)

p2 <- as.ggplot(expression(plot(rnorm(10))))
p3 <- as.ggplot(function() plot(sin))

p4 <- as.ggplot(~mosaic(Titanic))

p5 <- as.ggplot(densityplot(~mpg|cyl, data=mtcars))

## ----fig.width=14, fig.height=12----------------------------------------------
library(cowplot)

library(colorspace)
col <- rainbow_hcl(3)
names(col) <- unique(iris$Species)

color <- col[iris$Species]
p6 <- as.ggplot(~plot(iris$Sepal.Length, iris$Sepal.Width, col=color, pch=15))

p7 <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color=Species)) +
    geom_point(shape=15) + scale_color_manual(values=col, name="")

legend <- get_legend(p7)

## also able to annotate base or other plots using ggplot2
library(ggimage)
p8 <- p6 + geom_subview(x=.7, y=.78, subview=legend)


p9 <- as.ggplot(~image(volcano))

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol=3, labels=LETTERS[1:9])

