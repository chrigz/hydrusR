#' Write profile data to profile.dat
#'
#' @param project.path Path of the HYDRUS1D project
#' @param parameters named list of single numbers or vectors to write to profile data section
#'
#' @return Write profile data to "profile.dat"
#' @export
#'
#'
write.profile.data <- function(project.path, parameters) {

        # Read file and separate into components
        file.profile.dat = file.path(project.path, "PROFILE.DAT")
        def_profile_data = readLines(con = file.profile.dat, n = -1L, encoding = "unknown")
        profile_summary = def_profile_data[1:5]

        profile_header = def_profile_data[6]
        profile_header_vec = unlist(strsplit(trimws(profile_header), "[ ]+"))
        num_nodes = as.numeric(profile_header_vec[1])
        profile_header_vec = profile_header_vec[5:length(profile_header_vec)]
        profile_header_vec = c("i", profile_header_vec)

        profile_body = def_profile_data[7:(7 + num_nodes - 1)]
        profile_data_split = strsplit(profile_body, split = " ")
        profile_data_split2 = sapply(profile_data_split, FUN = function(x) x[x!= ""])
        profile_data_new = t(profile_data_split2)
        profile_data_new = as.data.frame(profile_data_new)
        names(profile_data_new) <- profile_header_vec

        node_info_lines = def_profile_data[(num_nodes + 7):(length(def_profile_data))]


        # Add in new content and re-format the profile data block
        value_format_list = list(i = "%d", x = "%.6e", h = "%.6e", Mat = "%d", Lay = "%d", Beta = "%.6e", Axz = "%.6e", Bxz = "%.6e", Dxz = "%.6e", Temp = "%.6e", Conc = "%.6e")
        for (p in 1:length(parameters)) {
                fmt = value_format_list[[names(parameters)[p]]]
                profile_values_col = sprintf(fmt = fmt, parameters[[p]])
                if ("e" == substr(fmt, nchar(fmt), nchar(fmt))) {
                        profile_values_col = sub('([0-9])$', '0\\1',  profile_values_col)
                }

                profile_data_new[names(parameters)[p]] = profile_values_col
        }

        fmt_space = c(5, 15, 15, 5, 5, 15, 15, 15, 15, 15, 15)
        fmt_vec = paste("%", fmt_space, "s", sep = "")
        fmt_vec = fmt_vec[1:ncol(profile_data_new)]

        profile_data_fmt = profile_data_new
        for(n in 1:nrow(profile_data_new)){

                profile_data_fmt[n, ] = sprintf(fmt_vec, profile_data_new[n, ])
        }

        tspace = sprintf("%13s", "")
        profile_data_fmt2 = apply(profile_data_fmt, MARGIN = 1, FUN = paste, collapse = "")
        profile_data_fmt2 = paste(profile_data_fmt2, tspace)


        # Put everything together again and save
        profile_data_new = c(profile_summary, profile_header, profile_data_fmt2, node_info_lines)

        write(profile_data_new, file.profile.dat, append = FALSE)

}

