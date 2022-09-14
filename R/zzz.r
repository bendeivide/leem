# About menu: auxiliar functions

.contact <- function() {
  tclServiceMode(FALSE)
  contact <- tktoplevel(width = 800,
                        height = 800)
  # Not propagate
  tkpack.propagate(contact, FALSE)

  tkwm.resizable(contact, FALSE, FALSE)
  tkwm.title(contact,
             gettext("Contact", domain = "R-leem"))
  # Text
  tkimage.create("photo", "::image::toolkit", file = system.file("etc", "toolkit.png", package = "leem"))
  label1 <- tklabel(contact, image = "::image::toolkit")
  tkgrid(label1, row = 1, column = 1, sticky = "w")
  label2 <- tklabel(contact, text = gettext("To contact the maintainer, \n please send an email to \n ben.deivide@ufsj.edu.br", domain = "R-leem"))
  # Para entrar em contato com o mantenedor, \n envie um e-mail para ben.deivide@ufsj.edu.br
  tkgrid(label2, row = 1, column = 2, sticky = "w")
  tclServiceMode(TRUE)
}


.author <- function() {
  tclServiceMode(FALSE)
  author <- tktoplevel(width = 400,
                        height = 400)
  # Not propagate
  tkpack.propagate(author, FALSE)

  tkwm.resizable(author, FALSE, FALSE)
  tkwm.title(author,
             gettext("Author", domain = "R-leem"))
  # Text
  tkimage.create("photo", "::image::ufsj", file = system.file("etc", "ufsj.png", package = "leem"))
  label1 <- tklabel(author, image = "::image::ufsj")
  tkpack(label1, side = "top", fill = "both", anchor = "center")
  label2 <- tklabel(author, text = gettext("\n\n Juliane Nassarala Almeida (PROFMAT/UFSJ) \n julianenassaralla@gmail.com", domain = "R-leem"))
  label3 <- tklabel(author, text = gettext("\n\n Ben Deivide de Oliveira Batista (UFSJ) \n ben.deivide@ufsj.edu.br", domain = "R-leem"))
  label4 <- tklabel(author, text = gettext("\n\n Alexandre Celesino Leite Almeida (UFSJ)\n celestino@ufsj.edu.br", domain = "R-leem"))
  # Para entrar em contato com o mantenedor, \n envie um e-mail para ben.deivide@ufsj.edu.br
  tkpack(label2, label3, label4, side = "top", fill = "both", anchor = "center")
  tclServiceMode(TRUE)
}

.leem <- function() {
  tclServiceMode(FALSE)
  leem <- tktoplevel(width = 400,
                       height = 400)
  # Not propagate
  tkpack.propagate(leem, FALSE)

  tkwm.resizable(leem, FALSE, FALSE)
  tkwm.title(leem,
             gettext("leem", domain = "R-leem"))
  # Text
  tkimage.create("photo", "::image::leem-history", file = system.file("etc", "leem-history.png", package = "leem"))
  label1 <- tklabel(leem, image = "::image::leem-history")
  tkpack(label1, side = "top", fill = "both", anchor = "center")
  tclServiceMode(TRUE)
}

.onLoad <- function(libname, pkgname){
  if (!requireNamespace("tkRplotR", quietly = TRUE)) install.packages("tkRplotR")
  if (!requireNamespace("manipulate", quietly = TRUE)) install.packages("manipulate", repos="http://cran.rstudio.com/", dependencies=TRUE)
}

