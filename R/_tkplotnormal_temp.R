# library(tcltk)
# library(tkrplot)
#
# ##########################
# # Configuration of widgets
# ##########################
# #Clear the configurations of the option function
# #.Tcl("option clear")
#
# # # Geral configurations
# .Tcl("option add *Background #e1eff7")
#
# # # Button and TButton configurations
# # .Tcl("option add *Button.Pady 2
# #           option add *Button.Background #e1eff7
# #           #option add Button.Foreground #e1eff7
# #           #option add *Button.Foreground black
# #           option add *Button.Cursor draft_small 46
# #           option add *TButton.Cursor draft_small 46")
# #
# # # Label configurations
# # .Tcl("option add *Label.Background #e1eff7")
# #
# # # Chackbutton configurations
# # .Tcl("option add *Checkbutton.Background #e1eff7")
# #
# # # Frame configurations
# # .Tcl("option add *Frame.Background #e1eff7")
#
#
# # Main window
# tt <- tktoplevel()
# tkwm.title(tt, "Normal Distribution")
# tkconfigure(tt, background = "#e1eff7")
#
# # =========================
# # Variables
# # =========================
#
# q_var <- tclVar(0)
# mu_var <- tclVar(0)
# sigma_var <- tclVar(1)
# text_var <- tclVar(1)
#
# comma_var <- tclVar(0)
# segment_var <- tclVar(0)
# title_var <- tclVar(1)
#
# # =========================
# # Plot function
# # =========================
#
# plot_function <- function() {
#
#   q <- as.numeric(tclvalue(q_var))
#   mu <- as.numeric(tclvalue(mu_var))
#   sigma <- as.numeric(tclvalue(sigma_var))
#
#   curve(
#     dnorm(x, mu, sigma),
#     from = mu - 4*sigma,
#     to = mu + 4*sigma,
#     lwd = 2,
#     ylab = "f(x)",
#     xlab = "x",
#     main = "Normal Distribution"
#   )
#
#   abline(v = q, col = "red", lty = 2)
# }
#
# # =========================
# # Left panel
# # =========================
#
# left_panel <- tkframe(tt)
# right_panel <- tkframe(tt)
#
# tkpack(left_panel, side = "left", padx = 10, pady = 10)
# tkpack(right_panel, side = "right", fill = "both", expand = TRUE)
#
# # =========================
# # Sliders
# # =========================
#
#
#
# q_slider <- tkscale(
#   left_panel,
#   from = -4,
#   to = 4,
#   resolution = 0.1,
#   orient = "horizontal",
#   variable = q_var,
#
#   label = gettext(
#     "Quantile",
#     domain = "R-leem"
#   ),
#
#   command = function(...) tkrreplot(img)
# )
#
#
#
# mu_slider <- tkscale(
#   left_panel,
#   from = -4,
#   to = 4,
#   resolution = 0.1,
#   orient = "horizontal",
#   variable = mu_var,
#
#   label = gettext(
#     "Mean",
#     domain = "R-leem"
#   ),
#
#   command = function(...) tkrreplot(img)
# )
#
#
#
# sigma_slider <- tkscale(
#   left_panel,
#   from = 0.1,
#   to = 5,
#   resolution = 0.1,
#   orient = "horizontal",
#   variable = sigma_var,
#
#   label = gettext(
#     "Standard Deviation",
#     domain = "R-leem"
#   ),
#
#   command = function(...) tkrreplot(img)
# )
#
# # =========================
# # Checkboxes
# # =========================
#
# cb1 <- tkcheckbutton(
#   left_panel,
#
#   text = gettext(
#     "Vertical Title",
#     domain = "R-leem"
#   ),
#
#   variable = title_var,
#   command = function() tkrreplot(img)
# )
#
# cb2 <- tkcheckbutton(
#   left_panel,
#
#   text = gettext(
#     "Long Segment",
#     domain = "R-leem"
#   ),
#
#   variable = segment_var,
#   command = function() tkrreplot(img)
# )
#
# cb3 <- tkcheckbutton(
#   left_panel,
#
#   text = gettext(
#     "Comma",
#     domain = "R-leem"
#   ),
#
#   variable = comma_var,
#   command = function() tkrreplot(img)
# )
#
#
#
# # =========================
# # Export buttons
# # =========================
#
# save_png <- function() {
#
#   file <- tclvalue(
#     tkgetSaveFile(
#       defaultextension = ".png"
#     )
#   )
#
#   if (file != "") {
#
#     png(file, width = 2000, height = 1400, res = 300)
#
#     plot_function()
#
#     dev.off()
#   }
# }
#
# png_button <- tkbutton(
#   left_panel,
#
#   text = gettext(
#     "Export PNG",
#     domain = "R-leem"
#   ),
#
#   command = save_png
# )
#
# # =========================
# # Pack widgets
# # =========================
#
# tkpack(q_slider, fill = "x")
# tkpack(mu_slider, fill = "x")
# tkpack(sigma_slider, fill = "x")
#
# tkpack(cb1, anchor = "w")
# tkpack(cb2, anchor = "w")
# tkpack(cb3, anchor = "w")
#
# tkpack(png_button, pady = 10)
#
# # =========================
# # Plot area
# # =========================
#
# img <- tkrplot(
#   right_panel,
#   fun = plot_function,
#   hscale = 1.5,
#   vscale = 1.5
# )
#
# tkpack(img, fill = "both", expand = TRUE)
