#' candelabra class
#'
#' Each candelabra is made up of 1 or more candles
#'
#' @return an object with class 'candelabra'
#'
#' @import plotly
#'
#' @export
stage <- R6::R6Class(
  classname = "stage",
  public = list(
    #' @field name an alias (human-readable) name for the stage
    name = NULL,

    #' @field height the default height of candelabras on the stage (in cm)
    height = 1000,

    #' @field width the width of the stage (from wing to wing) in cm
    width = NULL,

    #' @field depth the depth of the stage (front to back) in cm
    depth = NULL,

    #' @field unit_scaling the size (in cm) of each stage unit (which will have
    #' lux individually calculated). Unit scaling of 1 means each 1cm square will
    #' have its lux calculated (lower number will result in larger computation times)
    unit_scaling = NULL,

    #' @field max_lux a fixed comparison level of lux to ensure plots are always comparable to
    max_lux = NULL,

    #' @description create a new stage object
    #' @param n_candelabras the number of candelabras to initiate on the stage
    #' @param width the width of the stage in cm
    #' @param depth the depth of the stage in cm
    #' @param name a human-readable alias for the stage
    #' @param max_lux a comparison level of lux to always be able to compare to
    #' @param unit_scaling the square size in cm of each stage unit
    initialize = function(n_candelabras = 0,
                          width = 1200,
                          depth = 800,
                          name = "new_stage",
                          max_lux = 10,
                          unit_scaling = default("unit_scaling")) {

      private$.id <- uuid::UUIDgenerate()

      self$unit_scaling <- unit_scaling
      self$width <- width
      self$depth <- depth
      self$max_lux <- max_lux

      private$.candelabras <- lapply(seq_len(n_candelabras),
                            \(x) {
                              candelabra$new(
                                stage = self,
                                n_candles = default("candelabra_candles"),
                                centre_position = c(0,0),
                                radius = default("candelabra_radius"),
                                candle_lumens = default("candle_lumens"),
                                candle_height = default("candle_height"),
                                height = self$height
                              )
                            })

      # Make class reactive for use in shiny
      private$reactiveDep <- function(x) NULL

      invisible(self)
    },

    #' @description return a reactive version of the object that can be used in
    #' a shiny app
    reactive = function() {
      # Ensure the reactive stuff is initialized.
      if (is.null(private$reactiveExpr)) {
        private$reactiveDep <- reactiveVal(0)
        private$reactiveExpr <- reactive({
          private$reactiveDep()
          self
        })
      }
      private$reactiveExpr
    },

    #' @description invalidate the object, in a shiny setting
    #' This should be run whenever something happens that the reactive shiny
    #' context needs to be aware of
    invalidate = function() {
      private$count <- private$count + 1
      private$reactiveDep(private$count)
      invisible()
    },

    #' @description change the name of the stage
    #' @param new_name name to change the stage name to
    change_name = function(new_name) {
      self$name <- new_name
      self$invalidate()
    },

    #' @description add a candelabra to the stage
    #' @param x the x coordinate of the centre of the new candelabra
    #' @param y the y coordinate of the centre of the new candelabra
    #' @param n the number of candles in the new candelabra
    #' @param r the radius in cm of the new candelabra
    #' @param l the lumens of each candle in the new candelabra
    #' @param h the height of the new candelabra
    #' @param ch the height of each candle in the candelabra in cm
    #' @return adds a new candelabra object to the stage
    add_candelabra = function(x, y, n = 10, r = 30, l = 10, ch = 15, h = 1000) {
      private$.candelabras[[length(private$.candelabras)+1]] <- candelabra$new(
        stage = self,
        n_candles = n,
        centre_position = c(x,y),
        radius = r,
        candle_lumens = l,
        height = h,
        candle_height = ch
      )

      self$invalidate()
    },

    #' @description move a candelabra on the stage
    #' @param i the index of the candelabra
    #' @param new_x the new x coordinate of the candelabra
    #' @param new_y the new y coordinate of the candelabra
    move_candelabra = function(i, new_x, new_y) {
      private$.candelabras[[i]]$move_to(new_x, new_y)
      self$invalidate()
    },

    #' @description remove a candelabra from the stage
    #' @param elem the number of the candelabra (i.e. 1 is the first candelabra
    #' that was placed on the stage, 2 the second and so on)
    #' @return removes the specified candelabra from the stage
    remove_candelabra = function(elem) {
      private$.candelabras[[elem]] <- NULL
      private$.candelabras <- purrr::compact(private$.candelabras)

      self$invalidate()
    },

    #' @description plot candelabra positions
    #' @return a plotly plot of the stage with the candelabra and candle positions marked
    #' on it
    plot = function() {
      # Make sure candelabra can fit on plot
      neg_lim <- -(as.numeric(default("candelabra_radius")*2)-5)
      ggplot(self$candle_positions) +
        geom_point(aes(x = x, y = y, color = clb_id, fill = "red")) +
        scale_y_continuous(limits = c(neg_lim, self$depth)) +
        scale_x_continuous(limits = c(neg_lim, self$width))
    },

    #' @description plot the stage lighting
    #' @param show_candelabras logical, if TRUE the candelabra positions will
    #' be shown on the lux plot. If FALSE they will not be visible (but the light
    #' they produce still will be)
    #' @return a plotly plot of the stage with each stage unit showing how much lux
    #' it has
    plot_light = function(show_candelabras = TRUE) {
      plot <- plot_ly(self$lux_grid,
                      width = self$width,
                      height = self$depth) %>%
        add_trace(
          x = ~ux,
          y = ~uy,
          z = ~lux,
          type = "heatmap",
          text = ~lux_hover_label,
          hoverinfo = "text",
          zmin = 0,
          zmax = max(self$max_lux, max(self$lux_grid$lux)),
          zauto = FALSE,
          colors = colorRamp(c("black", "yellow"))
        )
      if (length(private$.candelabras) && show_candelabras) {
        plot <- plot %>%
          add_trace(
            data = self$candle_positions,
            x = ~x,
            y = ~y,
            colors = "red",
            customdata = ~clb_num,
            text = ~lumens,
            type = "scatter",
            mode = "markers",
            hovertemplate = paste0("Candelabra: <i>%{customdata}</i><br>",
                                   "Lumens from candle: <b>%{text}</b>",
                                   "<extra></extra>")
          )
      }
      plot %>%
        layout(annotations = list(
          list(
            x = self$width/2,
            y = -30,
            text = "Downstage",
            xanchor = "below",
            yanchor = "below",
            showarrow = F,
            font = list(size = 20)
          ),
          list(
            x = -80,
            y = self$depth/2,
            text = "Stage\nRight",
            xanchor = "below",
            yanchor = "below",
            showarrow = F,
            font = list(size = 20)
          ),
          list(
            x = self$width+80,
            y = self$depth/2,
            text = "Stage\nLeft",
            xanchor = "below",
            yanchor = "below",
            showarrow = F,
            font = list(size = 20)
          ),
          list(
            x = self$width/2,
            y = self$depth+30,
            text = "Upstage",
            xanchor = "above",
            yanchor = "above",
            showarrow = F,
            font = list(size = 20)
          )),
          xaxis = list(title = "",
                       showgrid = FALSE,
                       showticklabels = FALSE,
                       zeroline = FALSE,
                       ticks = ""),
          yaxis = list(title = "",
                       showgrid = FALSE,
                       showticklabels = FALSE,
                       zeroline = FALSE,
                       ticks = ""),
          autosize = FALSE)
    }
  ),
  private = list(
    .id = NULL,
    .candelabras = NULL,
    .stage_units = NULL,
    reactiveDep = NULL,
    reactiveExpr = NULL,
    count = 0
  ),
  active = list(

    #' @field id a unique generated ID for the stage
    id = function(value) {
      if (missing(value)) {
        private$.id
      } else {
        stop("Cannot set stage id, automatically generated")
      }
    },

    #' @field alias the name of the stage if it has one, otherwise the unique ID
    alias = function(value) {
      if (missing(value)) {
        if (length(self$name)) {
          self$name
        } else {
          private$.id
        }
      } else {
        stop("Cannot set alias directly, set $name instead")
      }
    },

    #' @field candelabras a list of the candelabras currently on the stage
    candelabras = function(value) {
      if (missing(value)) {
        private$.candelabras
      } else {
        stop("Cannot manually overwrite candelabras, use $add_candelabra, $move_candelabra etc.")
      }
    },

    #' @field candle_positions a tibble giving the position of each candle currently
    #' on the stage along with the candelabra it sits in and the lumens it is giving off
    candle_positions = function(value) {
      if (missing(value)) {
        purrr::imap(self$candelabras,
                    \(clb, clb_i) purrr::imap_dfr(clb$candles,
                                    \(cnd, cnd_i) list("x" = cnd$x_position,
                                                "y" = cnd$y_position,
                                                "z" = clb$height + cnd$candle_height,
                                                "clb_id" = clb$id,
                                                "clb_num" = clb_i,
                                                "cnd_num" = cnd_i,
                                                "lumens" = cnd$lumens))) %>%
          bind_rows()
      } else {
        stop("Cannot set candle positions manually")
      }
    },

    #' @field lux_grid a tibble giving the amount of lux at each stage unit and a
    #' label indicating how much each candelabra is contributing to the total. The key
    #' tibble used for plotting
    lux_grid = function(value) {
      if (missing(value)) {
        if (!length(private$.candelabras)) {
          tidyr::expand_grid(ux = seq(from = 0,
                                      to = self$width,
                                      by = self$unit_scaling),
                             uy = seq(from = 0,
                                      to = self$depth,
                                      by = self$unit_scaling),
                             uz = 0,
                             lux = 0,
                             lux_hover_label = paste0(
                               "Location: (", ux, ",", uy, ")<br>",
                               "Total Lux: <b>0</b><br>",
                               "<i>No candelabras added to stage yet</i>"
                             ))
        } else {
          tidyr::expand_grid(ux = seq(from = 0,
                                      to = self$width,
                                      by = self$unit_scaling),
                             uy = seq(from = 0,
                                      to = self$depth,
                                      by = self$unit_scaling),
                             uz = 0,
                             self$candle_positions) %>%
            mutate(dist = v_sld(ux,uy,uz,x,y,z),
                   lux = v_calc_lux(lumens, dist)) %>%
            group_by(ux,uy,uz,clb_num) %>%
            summarise(across(lux, sum)) %>%
            group_by(ux,uy,uz) %>%
            mutate(contrib_text = paste("Contribution from candelabra", clb_num, ":", round(lux, 2), "lx",
                           collapse = "<br>")) %>%
            summarise(across(lux, sum),
                      lux_hover_label = paste0(
                        "Location: (", ux, ",", uy, ")<br>",
                        "Total Lux: <b>", round(lux,2), "</b><br>",
                        first(contrib_text)
                      ))
        }
      } else {
        stop("Cannot set lux grid directly")
      }
    }
  )
)

#' @exportS3Method
print.stage <- function(x) {
  cli::cli_inform("{.cls lumos::stage}")
  cli::cli_inform("{.emph {x$alias}} [{ {x$width}}x{ {x$depth}}]")
  cli::cli_inform("{.val {length(x$candelabras)}} candelabras")
  invisible(x)
}
