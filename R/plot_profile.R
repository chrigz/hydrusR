
#' Plot profile generic
#'
#' @param project.path project.path
#' @param output default: "Head"
#'
#' @return ggplot object of profile
#' @export
#'
plot_profile.last <- function(project.paths, output = "Head", measured){

        h1d_output = list()
        for (path in project.paths) {
                temp_output = hydrusR::read.nod_inf(path,
                                                    out.file = "Nod_Inf.out",
                                                    output = output)
                temp_output = dplyr::filter(temp_output, Time == max(temp_output$Time))

                temp_output$model = gsub(".{,20}/","",path)

        h1d_output = append(h1d_output, list(temp_output))
        }

        ggplot2::ggplot(data = h1d_output[[1]],
                        mapping = ggplot2::aes(x = !!rlang::ensym(output),
                                               y = Depth)
                        ) ->
                plot_obj


        fit_data <- tibble::tibble(r_squared = 0, x_pos = 0, .rows = length(h1d_output))

        for (i in 1:length(h1d_output)) {
                # calculate r squared
                # approx(x = tibble::deframe(dplyr::select(h1d_output[[i]], Depth)),
                approx(x = h1d_output[[i]]$Depth,
                       y = tibble::deframe(dplyr::select(h1d_output[[i]], !!rlang::ensym(output))),
                       xout = measured$Depth, rule = 2) ->
                        model_values

                cor(model_values$y,
                    dplyr::select(measured, !!rlang::ensym(output)),
                    use = "na.or.complete")^2 ->
                        fit_data$r_squared[i]

                mean(model_values$y) ->
                        fit_data$x_pos[i]


                plot_obj +
                        ggplot2::geom_path(data = h1d_output[[i]], mapping = ggplot2::aes(linetype = model)) ->
                        plot_obj




        }

        dplyr::mutate(fit_data, y_pos = mean(measured$Depth),
                      label = paste0("R2 = ", round(r_squared, digits = 2))) ->
                fit_data

        plot_obj +
                ggplot2::geom_point(data = measured) +
                ggplot2::geom_text(data = fit_data, mapping = ggplot2::aes(x = x_pos,
                                                                       y = y_pos,
                                                                       label = label)) +
                ggplot2::theme_bw()



}



#' Plot pressure profile
#'
#' @param project.path project.path
#' @param output default: "Head"
#'
#' @return plot pressure profile
#' @export
#'
plot_pressure.profile <- function(project.path, output = "Head"){

      h1d_output = read.nod_inf(project.path, out.file = "Nod_Inf.out", output = output)
      Depth = unique(h1d_output$Depth)

 output_split = split(x = h1d_output, f = h1d_output$Time)

 total_times = length(output_split)

 tindex = seq(0, by = 20, total_times)

 output_in = output_split[tindex]

 output_in = do.call("cbind", output_in)

 output_names = names(output_in)

 output_names_p = grep(paste(output, collapse = "|"), output_names, value = T)

output_p = as.matrix(output_in[, ..output_names_p])

plot_range = range(max(output_p), -(max(output_p)))

matplot(x = output_p, y = Depth, xlim = plot_range, type = "l", xlab = "Hean (L)")

graphics::abline(v = 0, lwd = 2, col = "grey40")

}

#' Plot moisture profile
#'
#' @param project.path project.path
#' @param output default: "Moisture"
#'
#' @return plot moisture profile
#' @export
#' @importFrom graphics abline matplot
plot_moisture.profile <- function(project.path, output = "Moisture"){

      h1d_output = read.nod_inf(project.path, out.file = "Nod_Inf.out", output = output)
      Depth = unique(h1d_output$Depth)

      output_split = split(x = h1d_output, f = h1d_output$Time)

      total_times = length(output_split)

      tindex = seq(0, by = 20, total_times)

      output_in = output_split[tindex]

      output_in = do.call("cbind", output_in)

      output_names = names(output_in)

      output_names_p = grep(paste(output, collapse = "|"), output_names, value = T)

      output_p = as.matrix(output_in[, ..output_names_p])

      plot_range = range(0, (max(output_p)))

      graphics::matplot(x = output_p, y = Depth, xlim = plot_range, type = "l",
      xlab = parse(text = "Theta~(L^3~L^-3)"))
}
