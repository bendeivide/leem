#' Graphical User Interface for leem package
#'
#' \code{leem} A Graphical User Interface (GUI) for
#'     the leem package
#' @param gui Logical argument, \code{TRUE} or \code{FALSE}. The default is \code{TRUE}
#' @return \code{leem} presents GUI with various problems for the teaching of statistics and mathematics. The idea is to use this package to learn these subjects without necessarily programming in R
#' @examples
#' # Loading package
#' library(leem)
#' if (interactive()) {
#'   leem(gui = FALSE)
#' }
#' @import "tcltk" "tkrplot" "grDevices"
#' @importFrom "graphics"  "abline"  "curve"  "grid"  "lines"  "par"  "points" "rect"  "text"
#' @importFrom "stats"  "D"
#' @importFrom "utils"  "edit"  "read.table"
#' @export
leem <- function(gui = TRUE) {
  # Language
  # # portugues
  # Sys.setenv(LANG = "pt_BR")
  #
  # # Ingles
  # Sys.setenv(LANG = "en")
  # Environment of package
  envss <- new.env(parent = base::emptyenv())
  assign("dat", NULL, envir = envss)



  if (gui == TRUE) {
    # Insert images
    tkimage.create("photo", "::image::logo",
                   file = system.file("etc", "leem.png", package = "leem"))
    tkimage.create("photo", "::image::iconleem",
                   file = system.file("etc", "leem-icon.png", package = "leem"))
    tkimage.create("photo", "::image::edit",
                   file = system.file("etc", "edit.gif", package = "leem"))
    tkimage.create("photo", "::image::directory",
                   file = system.file("etc", "directory.gif", package = "leem"))
    tkimage.create("photo", "::image::open",
                   file = system.file("etc", "open.gif", package = "leem"))


    # ##########################
    # # Configuration of widgets
    # ##########################
    # #Clear the configurations of the option function
    .Tcl("option clear")
    #
    # Button and TButton configurations
    .Tcl("option add *Button.Pady 2
          #option add *Button.Background #e1eff7
          #option add Button.Foreground #e1eff7
          #option add *Button.Foreground black
          option add *Button.Cursor draft_small 46
          option add *TButton.Cursor draft_small 46")
    #
    # # Label configurations
    # .Tcl("option add *Label.Background #e1eff7")
    #
    # # Chackbutton configurations
    # .Tcl("option add *Checkbutton.Background #e1eff7")
    #
    # # Frame configurations
    # .Tcl("option add *Frame.Background #e1eff7")
    #
    #
    # Style TFrame
    #tcl("ttk::style" , "configure" , "Toolbar.TFrame", relief = "solid", expand = TRUE)
    .Tcl("ttk::style configure Toolbar.TFrame -relief solid")
    #
    # # Style LabelFrame
    # .Tcl("ttk::style configure Toolbar.TLabelframe -background #e1eff7
    #       ttk::style configure Toolbar.TLabelframe.Label -background #e1eff7")
    #
    #
    # # Style PanedWindow
    # .Tcl("ttk::style configure Toolbar.TPanedwindow -background #e1eff7")
    #
    # # Class disabled/Enabled
    .Tcl("option add *Ativado.Entry.state normal 81
          option add *Ativado.Label.state normal 81
          option add *Ativado.Button.state normal 81
          option add *Desativado.Entry.state disabled 81
          option add *Desativado.Label.state disabled 81
          option add *Desativado.Button.state disabled 81")


    # Disabled GUI (Type I)
    oldmode <- tclServiceMode(FALSE)

    # Top-level window
    wid <- 1019
    hei <- 700

    topwinstat <- tktoplevel(
      width = wid,
      height = hei
      #background = "blue"
    )
    # Disabled GUI (Type II)
    #tkwm.state(topwinstat, "withdraw")

    ####################################
    # Configurations of top-level window
    ####################################

    # Title
    tkwm.title(topwinstat,
               gettext("leem package", domain = "R-leem"))

    #Icon main toplevel window
    tcl("wm", "iconphoto", topwinstat, "-default", "::image::iconleem")

    # Not propagate
    tkpack.propagate(topwinstat, FALSE)

    # Initial screen
    tkpack(quadroinicial <- tkframe(topwinstat), expand = TRUE, fill = "both")
    tkpack(telainicial <- tklabel(parent = quadroinicial, image = "::image::logo"),
           expand = TRUE, fill = "both")


    # Auxiliar functions
    f.read <- NULL
    f.read <- function(file) {
      if (grepl("\\.txt$", file)) {
        if (tclvalue(group_cbox_1_resp) == "TRUE") {
          return(read.table(file, header = TRUE, dec = ",", sep = tclvalue(group_cbox_2_resp)))
        }
        if (tclvalue(group_cbox_1_resp) == "FALSE") {
          return(read.table(file, header = TRUE, sep = tclvalue(group_cbox_2_resp)))
        }
      }
      if (grepl("\\.csv$", file)) {
        if (tclvalue(group_cbox_1_resp)) {
          return(read.table(file, header = TRUE, dec = ",", sep = tclvalue(group_cbox_2_resp)))
        }
        if (tclvalue(group_cbox_1_resp) == FALSE) {
          return(read.table(file, header = TRUE, sep = tclvalue(group_cbox_2_resp)))
        }
      }
    }
    group_cbox_1_resp <- tclVar("FALSE")
    group_cbox_2_resp <- tclVar("")
    openfile <- function(...) {
      tclServiceMode(FALSE)
      if (exists("confdata")) {
        tkwm.deiconify(confdata)
      } else{
        confdata <- tktoplevel()
        tkwm.resizable(confdata, FALSE, FALSE)
        tkwm.title(confdata,
                   gettext("Configurations of the data", domain = "R-leem"))
      }
      # Group of buttons
      tkpack(group_cbox <- tkframe(parent = confdata),
             expand = TRUE, fill = "x", pady = "1m")
      #tkpack.configure(group_cbox, expand = TRUE, fill = "both")

      # Checkbox
      tkpack(group_cbox_1 <- tkcheckbutton(parent = group_cbox,
                                           text = gettext("Comma as decimal points", domain = "R-leem"),
                                           variable = group_cbox_1_resp,
                                           onvalue = "TRUE",
                                           offvalue = "FALSE"),
             anchor = "nw", padx = "1m", side = "left")

      ##
      ##Separator
      tkpack(ttkseparator(parent = group_cbox, orient = "vertical"),
             fill = "both", side = "left")
      ##
      tkpack(tklabel(parent = group_cbox,
                     text = gettext("Separator of variables:",
                                    domain = "R-leem")),
             side = "left", anchor = "nw", padx = "1m"
      )
      ##
      tkpack(group_cbox_2 <- tkentry(textvariable = group_cbox_2_resp,
                                     parent = group_cbox,
                                     width = 5),
             side = "left", anchor = "nw", padx = "1m"
      )

      ##Separator
      tkpack(ttkseparator(parent = confdata, orient = "horizontal"),
             fill = "x")
      tkpack(bconfdata <- ttkbutton(parent = confdata,
                                    text = gettext("Enter the data", domain = "R-leem")), anchor = "e")

      tclServiceMode(TRUE)
      tkfocus(bconfdata)
      funcbconfdata <- function(...){
        filetemp <- tkgetOpenFile(filetypes = paste(
          "{{txt files} {.txt} }" ,
          "{{csv files} {.csv}}" ,
          sep = " "))
        start_dir <- tclvalue(filetemp)
        if (file.exists(start_dir)) {
          tkwm.withdraw(confdata)
          envss$dat <- f.read(start_dir)
        }
        if (file.exists(start_dir) == FALSE) {
          tkwm.withdraw(confdata)
          tkmessageBox(message = gettext("No data set has been entered!", domain = "R-leem"))
        }
      }
      tkbind(bconfdata, "<ButtonRelease>", funcbconfdata)
      tkbind(confdata, "<Return>", funcbconfdata)
      tkbind(confdata, "<Escape>", function(){
        tkwm.withdraw(confdata)
      })


    }
    # Menu
    menu_bar <- tkmenu(topwinstat)
    tkconfigure(topwinstat, menu = menu_bar)


    # File menu
    file_menu <- tkmenu(menu_bar, tearoff = FALSE)
    chosdir <- function(...) {
      dir_name <- tkchooseDirectory()
      if (nchar(dir_name <- tclvalue(dir_name))) {
        dir_name <- setwd(dir_name)
        on.exit(setwd(dir_name)) # Return initial directory
      }
    }
    tkadd(file_menu, 'command', label = gettext('Choose directory...', domain = "R-leem"),
          accelerator = 'Ctrl+Shift+H', command = chosdir,
          image = "::image::directory", compound = "left")
    tkadd(menu_bar, 'cascade', label = gettext('File', domain = "R-leem"), menu = file_menu)
    tkadd(file_menu, 'command', label = gettext('Open file (.txt or .csv)...', domain = "R-leem"),
          accelerator = 'Ctrl+O', command = openfile, image = "::image::open", compound = "left")
    restartscreen <- function(){
      aux <- as.character(tkwinfo("children", quadroinicial))
      sapply(aux, function(W) tcl("destroy", W))
      #tkdestroy(telainicial)
      tkpack(telainicial <- tklabel(parent = quadroinicial, image = "::image::logo"),
             expand = TRUE, fill = "both")
    }
    tkadd(file_menu, 'command', label = gettext('Restart', domain = "R-leem"),
          accelerator = 'Ctrl+R', command = restartscreen)
    ## Edit menu
    # This variable is important in the event of the "bentry" button

    fedit <- function(...) {
      if (is.null(envss$dat)) {
        tkmessageBox(message = gettext("No data set has been entered!", domain = "R-leem"))
      } else{
        envss$dat <- edit(envss$dat)
      }
    }
    edit_menu <- tkmenu(menu_bar, tearoff = FALSE)
    tkadd(menu_bar, "cascade", label = gettext("Edit", domain = "R-leem"), menu = edit_menu)
    tkadd(edit_menu, "command", label = gettext("Data set...", domain = "R-leem"),
          accelerator = "Ctrl+E", command = fedit,
          image = "::image::edit", compound = "left")


    tkbind(topwinstat, "<Control-O>", openfile)
    tkbind(topwinstat, "<Control-o>", openfile)
    tkbind(topwinstat, "<Control-E>", fedit)
    tkbind(topwinstat, "<Control-e>", fedit)
    tkbind(topwinstat, "<Control-Shift-H>", chosdir)
    tkbind(topwinstat, "<Control-Shift-h>", chosdir)
    tkbind(topwinstat, "<Control-R>", restartscreen)
    tkbind(topwinstat, "<Control-r>", restartscreen)


    # Menu Mathematics
    math_menu <- tkmenu(menu_bar, tearoff = FALSE)
    tkadd(menu_bar, "cascade", label = gettext("Mathematics", domain = "R-leem"), menu = math_menu)
    tkadd(math_menu, "command", label = gettext("Elementary school", domain = "R-leem"),
          accelerator = "Ctrl+S", command = .plot1st2st)
    tkadd(math_menu, "command", label = gettext("Higher education", domain = "R-leem"),
          accelerator = "Ctrl+H", command = function()  tkdestroy(topwinstat))


    # Menu Statistics
    stat_menu <- tkmenu(menu_bar, tearoff = FALSE)
    tkadd(menu_bar, "cascade", label = gettext("Statistics", domain = "R-leem"), menu = stat_menu)
    #Menu Demonstrations
    stat_menudemo <- tkmenu(stat_menu, tearoff = FALSE)
    tkadd(stat_menu, "cascade", label = gettext("Demonstrations", domain = "R-leem"), menu = stat_menudemo)
    tkadd(stat_menudemo, "command", label = gettext("Normal distribution", domain = "R-leem"), accelerator = "Ctrl+V", command = .demolocsca)
    # Menu m.position.
    tkadd(stat_menu, "command", label = gettext("Measures of position", domain = "R-leem"),accelerator = "Ctrl+P", command = .mposition)
    # Menu m.dispersion.
    tkadd(stat_menu, "command", label = gettext("Measures of dispersion", domain = "R-leem"), accelerator = "Ctrl+D", command = .mdispersion)
    #Menu probability.
    stat_menuprob <- tkmenu(stat_menu, tearoff = FALSE)
    tkadd(stat_menu, "cascade", label = gettext("Probability distributions", domain = "R-leem"), menu = stat_menuprob)
    tkadd(stat_menuprob, "command", label = gettext("Beta", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuprob, "command", label = gettext("Binomial", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuprob, "command", label = gettext("Exponential", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuprob, "command", label = gettext("Geometric", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuprob, "command", label = gettext("Gumbel", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuprob, "command", label = gettext("Hypergeometric", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuprob, "command", label = gettext("Negative Binomial", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuprob, "command", label = gettext("Normal", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuprob, "command", label = gettext("Poisson", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuprob, "command", label = gettext("T-Student", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    #Menu Quantitative
    stat_menuquant <- tkmenu(stat_menu, tearoff = FALSE)
    tkadd(stat_menu, "cascade", label = gettext("Quantitative distributions", domain = "R-leem"), menu = stat_menuprob)
    tkadd(stat_menuquant, "command", label = gettext("Beta", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuquant, "command", label = gettext("Binomial", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuquant, "command", label = gettext("Exponential", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuquant, "command", label = gettext("Geometric", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuquant, "command", label = gettext("Gumbel", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuquant, "command", label = gettext("Hypergeometric", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuquant, "command", label = gettext("Negative Binomial", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuquant, "command", label = gettext("Normal", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuquant, "command", label = gettext("Poisson", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    tkadd(stat_menuquant, "command", label = gettext("T-Student", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))
    #Menu tabfreq
    tkadd(stat_menu, "command", label = gettext("Table of frequêncy", domain = "R-leem"),
          accelerator = "Ctrl+V", command = function()  tkdestroy(topwinstat))


    # Menu about
    about_menu <- tkmenu(menu_bar, tearoff = FALSE)
    tkadd(menu_bar, "cascade", label = gettext("About", domain = "R-leem"), menu = about_menu)
    tkadd(about_menu, "command", label = gettext("leem", domain = "R-leem"),
          command = .leem)
    tkadd(about_menu, "command", label = gettext("Authors", domain = "R-leem"),
          command = .author)
    tkadd(about_menu, "command", label = gettext("Contact", domain = "R-leem"),
          command = .contact)


    # Activate GUI
    finish <- tclServiceMode(oldmode)

  }
}
