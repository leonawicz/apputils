# nolint start
.app_img_link <- function(app_url, img_url, title, subtitle, height = 200, img_style = NULL,
                          new_window = TRUE){
  p <- if(is.null(img_style)) "" else paste0('style="', img_style, '" ')
  tblank <- if(new_window) 'target="_blank"' else ''
  shiny::HTML(paste0(
    '<div class="img_link_wrap">
    <img class="img_app" src="', img_url, '" ', p, 'width="100%" height="', height, '"/>
    <a href="', app_url, '" style="color:white;" ', tblank, '
    <div class="img_hover_layer">
    <div class="img_hover">
    <h3><p>', title, '</p></h3>
    <h4><p class="img_hover">', subtitle, '</p></h4>
    </div>
    </div>
    </a>')) # contextual, must remove a closing div with mutliple inline calls
}
# nolint end

.app_img_links <- function(app_url, img_url, title, subtitle, label = NULL, drop = NULL,
                           height = 200, min_width = 300, max_width = 400, col_width = 4,
                           img_style = NULL, new_window = TRUE){
  apps <- basename(app_url)
  n <- length(apps)
  if(!is.null(drop) && !all(drop %in% apps))
    stop("Cannot drop apps that are not in the master list.")
  idx <- if(is.null(drop)) seq_along(apps) else seq_along(app_url)[-match(drop, apps)]

  if(is.null(img_style)){
    img_style <- vector("list", n)
  } else {
    if(!length(img_style) %in% c(1, n))
      stop("`img_padding` length must be one or length of `app_url`.")
    if(length(img_style) == 1) img_style <- rep(img_style, n)
  }
  if(is.null(label)){
    label <- vector("list", n)
  } else {
    if(length(label ) != n) stop("`label` length must match length of `app_url`.")
  }

  x <- purrr::map(
    idx, ~shiny::column(
      col_width, label[.x], .app_img_link(app_url[.x], img_url[.x], title[.x], subtitle[.x], height,
                                          img_style[[.x]], new_window = new_window),
      style = paste0("min-width: ", min_width, "px; max-width: ", max_width, "px; padding:5px;")))
  shiny::fluidRow(x, style = "padding: 10px;")
}

#' Generate app showcase content
#'
#' This function generates a showcase panel for Shiny apps.
#'
#' Despite being named for this \code{apputils} context, \code{app_showcase} can be applied to other types of content.
#'
#' The image padding argument is useful when multiple source images have different amounts of margin space embedded in them
#' and you need them to match each other better,
#' or when you need padding around an image that includes no margin around it.
#' For image padding, use a vector of length one or length equal to the length of \code{app_url}.
#'
#' If \code{label} is not \code{NULL}, it must be a vector with a label for every image link.
#' To force empty labels among non-empty labels, use \code{""}. E.g., \code{label = c("A", "", "C")}.
#' \code{label} can be styled by using html, e.g., \code{label = h4("Top Title")}.
#'
#' @param app_url character vector of app urls.
#' @param img_url character vector of image urls, same length as \code{app_url}.
#' @param title character, titles displayed on image links during mouseover.
#' @param subtitle character, subtitles displayed on image link during mouseover.
#' @param label character or \code{NULL}, optional fixed label place above an image panel. See details.
#' @param drop character vector of any apps to exclude from showcase. Useful for programmed app lists that may include self.
#' @param height numeric, height in pixels.
#' @param min_width numeric, minimum width in pixels.
#' @param max_width numeric, maximum width in pixels.
#' @param col_width integer, column width number for row, 1 through 12, defaults to 4.
#' @param img_style optional style for the image component of the widget. Useful for padding in particular, e.q., \code{style="padding:10px;"}.
#' Defaults to \code{NULL}. See details. If length is one, is repeated for length of \code{app_url}.
#' @param new_window logical, open the link in a new browser window. Defaults to \code{TRUE}.
#'
#' @return a shiny fluidRow containing organized and stylized app image links for reference.
#' @export
#'
#' @examples
#' #not run
app_showcase <- function(app_url, img_url, title, subtitle, label = NULL, drop = NULL, height = 200,
                         min_width = 300, max_width = 400, col_width = 4, img_style = NULL, new_window = TRUE){
  .app_img_links(app_url, img_url, title, subtitle, label, drop, height,
                 min_width, max_width, col_width, img_style, new_window)
}

#' Generate a recommended app citation
#'
#' Generate a sub section for an app's information page to provide a recommended citation.
#'
#' All character strings appear as is. For example, enter \code{author="First Last, Last, F. , Last, F"} if that is how
#' three authors should appear in the citation. Note that terminal periods (\code{.}) are implicit.
#'
#' @param author character string, appears as is.
#' @param year year of publication.
#' @param title citation title.
#' @param publisher the publisher.
#' @param url the url of the application.
#' @param heading the heading of the citation section, defaults to \code{"Recommended citation"}.
#' @param heading_size character, the heading tag, defaults to \code{"h3"}.
#'
#' @return a character string wrapped in \code{shiny::HTML}.
#' @export
#'
#' @examples
#' #not run
app_citation <- function(author, year, title, publisher, url,
                         heading = "Recommended citation", heading_size = "h3"){
  shiny::HTML(paste0("<", heading_size, ">", heading, "</", heading_size, ">\n",
              "<p style='text-align: justify;'>", author, ". ", year, ". ", title, ". ", publisher, ". ", url, ".\n"
  ))
}

#' Add contact information widget
#'
#' Add a contact information section that includes author name, role, a thumbnail photo and optional logos and links.
#'
#' A name, role and photo are required. The recommended photo size is 128 x 128 pixels.
#' For logos (multiple), width is full but height is forced constant.
#' \code{logo} may be a vector. \code{href} may be an equal-length vector if logos are image links.
#'
#' \code{links} is a separate named or unnamed list of author-related links.
#' For example, \code{links = list("GitHub pages" = "https://leonawicz.github.io", Twitter = "https://twitter.com/leonawicz")} will appear
#' as \code{Github pages | Twitter} in the output. If \code{links} is unnamed, each element must be a string  and will be wrapped in \code{shiny::HTML}.
#' This is useful for something like small icon links with no text, e.g.:
#'
#' \code{links = list('<a href="https://leonawicz.github.io" target="_blank"><i class="fa fa-github fa-lg"></i></a>')}
#'
#' \code{heading = NULL} will drop the heading. \code{footnote} is optional arbitrary text that will appear at the bottom of the widget in a paragraph tag.
#' Everything but logos remains tightly integrated on the left and is intended to be specific to the author.
#' Logos are floated to the right and are commonly used for branding. The default photo and image heights help keep elements nicely aligned for typical
#' sized web pages. The footnote should be short to keep the widget small, such as "For questions about this app, email...", rather than an author bio.
#' If \code{footnote} is a vector, note that each element will become a separate paragraph tag and will stack vertically with vertical white space as well.
#'
#' @param name author.
#' @param role i.e., job title.
#' @param photo author image url.
#' @param logo vector of additional image urls, i.e., funders.
#' @param href optional links to logos.
#' @param links separate list of text author-related links. See details.
#' @param heading optional heading.
#' @param heading_size character, defaults to \code{"h2"}.
#' @param footnote additional text.
#' @param logo_height numeric, set a fixed height in pixels for any logos.
#' @param photo_width numeric, author photo width in pixels.
#' @param photo_height numeric, author photo height in pixels.
#'
#' @return a shiny taglist.
#' @export
#'
#' @examples
#' #not run
contactinfo <- function(name, role, photo, logo = NULL, href = NULL, links = NULL,
                        heading = "Contact information", heading_size = "h2", footnote = NULL,
                        logo_height = 170, photo_width = 128, photo_height = 128){
  if(!inherits(photo, "character") || length(photo) != 1)
    stop("`photo` must be a path or url to a single image.")
  if(inherits(logo, "character")){
    if(inherits(href, "character") && length(href) != length(logo))
      stop("`logo` and `href` must be equal length if `href` is not NULL.")
    close_tag <- paste0('" style="float:right;height:', logo_height,
                        'px;margin:5px;" target="_blank">')
    if(!is.null(href)) close_tag <- paste0(close_tag, "</a>")
    if(is.null(href))
      x <- purrr::map2(logo, ~paste0('<img src="', .x, close_tag))
    if(!is.null(href))
      x <- purrr::map2(logo, href, ~paste0('<a href="', .y, '"><img src="', .x, close_tag))
    x <- paste(unlist(x), collapse = "")
  } else {
    x <- ""
  }
  id <- paste0('<div style="clear: left;"><img src="', photo, # nolint start
               '" alt="" style="float: left; margin-right:5px; width:', photo_width,
               '; height:', photo_height, ';" /></div><p>', name, '<br/>', role, '<br/>')
  if(inherits(links, "list")){
    if(is.null(names(links))){
      links <- paste(links, collapse = "")
    } else {
      links <- purrr::map2(links, names(links),
                           ~paste0('<a href="', .x, '" target="_blank">', .y, '</a>')) %>% # nolint end
        unlist() %>% paste(collapse = " | ")
    }
    id <- paste0(id, links)
  }
  if(!is.null(heading))
    heading <- shiny::HTML(paste0("<", heading_size, ">", heading, "</", heading_size, ">"))
  if(!is.null(footnote))
    footnote <- shiny::HTML(paste(purrr::map_chr(footnote, ~as.character(shiny::p(.x))), collapse = ""))
  shiny::tagList(shiny::HTML(x), heading, shiny::HTML(paste0(id, "</p>")), footnote)
}

# nolint start
#' App launch information
#'
#' Show information about an app during launch such as a 'welcome/what to do' toast message.
#'
#' The list \code{toast_args} has the following defaults: \code{timeOut = 10000}, \code{extendedTimeOut = timeOut}, \code{position = "top-center"},
#' \code{closeButton = TRUE} and \code{preventDuplicates=TRUE}.
#' If another list is provided, but does not include some of these five arguments, they will
#' be retained as defaults. Note that this function depends heavily on associated custom CSS. For example, if your message body is
#' very large, it will not fit in the toast box unless additional css overrides are included in your app for size and position.
#' Use with care and see this following \href{https://gist.github.com/leonawicz/24ed656f63d4a889ad7043bc5436a641}{GitHub gist} for details.
#'
#' @param title character.
#' @param message plain character string or, typically, one wrapping an html snippet.
#' @param logo the \code{src} character string for an image.
#' @param logo_position character, \code{"title"} or \code{"message"}.
#' The logo is comes after and is floated to the right of either the title or the message body.
#' @param type whether to return a \code{"toast"} message using \code{shinytoastr} (default) or plain \code{"html"} as a character string.
#' @param toast_type if \code{type = "toast"}, the toast background color is set by specifying one of \code{"info"} (default),
#' \code{"success"}, \code{"warning"} or \code{"error"}.
#' @param heading_size character, wrap the title in html heading tags, defaults to \code{"h2"}.
#' @param toast_args a list of additional arguments (after \code{title} and \code{message}) to pass to \code{shinytoastr::toast_*}.
#'
#' @return a \code{shiny::toastr::toast_*} object by default. Alternatively, a character string of html.
#' @export
#'
#' @examples
#' #not run
appintro <- function(title, message, logo = NULL, logo_position = "title", type = "toast",
                     toast_type = "info", heading_size = "h2",
                     toast_args = list(
                       timeOut = 10000, extendedTimeOut = 10000, position = "top-center",
                       closeButton = TRUE, preventDuplicates = TRUE)){
  title <- paste0("<", heading_size, ">", title, "</", heading_size, ">")
  if(!is.null(logo)){
    close_tag <- "' style='float:right; width:200px; padding-left:15px;'/>"
    if(logo_position == "title"){
      title <- paste0("<img src='", logo, close_tag, "\n", title, "\n")
    } else {
      message <- paste0(message, "\n<img src='", logo, close_tag)
    }
  }
  if(type == "html") return(paste0(title, message, collapse = "\n"))
  if(!toast_type %in% c("info", "success", "warning", "error"))
    stop("Invalid toast type. Must be info, success, warning or error.")
  make_toast <- paste0("toastr_", toast_type)
  if(is.null(toast_args$timeOut)) toast_args$timeOut <- 10000
  if(is.null(toast_args$extendedTimeOut)) toast_args$extendedTimeOut <- toast_args$timeOut
  if(is.null(toast_args$position)) toast_args$position <- "top-center"
  if(is.null(toast_args$closeButton)) toast_args$closeButton <- TRUE
  if(is.null(toast_args$preventDuplicates)) toast_args$preventDuplicates <- TRUE
  do.call(make_toast, c(message = message, title = title, toast_args))
}
# nolint end

#' Generate a Frequently Asked Questions (FAQ) widget
#'
#' Generate a widget that displays FAQs in an information/about section of an app.
#'
#' \code{faq} creates an FAQ widget from a named list.
#' It returns a Bootstrap Collapse menu FAQ format by default. Changing to \code{format = "list"}
#' simply passes through the original \code{faqlist}.
#'
#' Generally, an FAQ list is a named list that is prepared in advance and passed to \code{faqlist}.
#' The names of the list elements are used for indexing the list with \code{id}.
#' It is designed this way in the context of reusable FAQ list dictionaries, where each use does not require all entries.
#' Each list element in \code{faqlist} is a length-2 list.
#' The first element is the question: a character string. The second element
#' is the answer: any text or HTML content, e.g., multiple tags wrapped in a tag list.
#'
#' A key difference with the \code{open} argument that can be passed in a list to \code{bscollapse_args}
#' is that it must simply match a single \emph{dictionary entry} \code{id} (e.g., \code{open = "apps"}),
#' not a collapse panel question title or id.
#'
#' @param faqlist a prepared list of FAQs. See details.
#' @param id character, the id for the \code{shinyBS::bsCollapse} element.
#' @param format character, \code{"bscollapse"} (default) or alternatively, \code{"list"}.
#' @param bscollapse_args a list of arguments passed to bsCollapse. Defaults to \code{list(id = "faq", open = NULL, multiple = FALSE)}.
#'
#' @return a \code{shinyBS::bsCollapse} object or a list.
#' @export
#'
#' @examples
#' #not run
faq <- function(faqlist, id, format = "bscollapse",
                bscollapse_args = list(id = "faq", open = NULL, multiple = FALSE)){
  if(is.null(names(faqlist))) stop("`faqlist` must be a named list.")
  if(!all(id %in% names(faqlist))) stop("All 'id' values must match to FAQ dictionary entries.")
  open <- bscollapse_args$open
  bscollapse_args$open <- if(length(open) == 1 && open %in% names(faqlist)) faqlist[[open]][[1]] else NULL
  faqlist <- faqlist[id]
  if(format == "list") return(faqlist)
  if(format == "bscollapse")
    return(
      do.call(shinyBS::bsCollapse,
              as.list(
                c(bscollapse.args,
                  purrr::map(faqlist, ~shinyBS::bsCollapsePanel(.x[[1]], .x[[2]], style = "info"))
                )
              )
            )
    )
  stop("invalid FAQ format.")
}

#' Add footer to Shiny dashboard sidebar
#'
#' Add a foot to a Shiny dashboard sidebar that includes a logo image link and a label.
#'
#' \code{style} pertains to the logo and label whereas \code{width} is for the logo specifically and only \code{height}
#' pertains directly to the footer tag.
#'
#' @param href url, will open in new window.
#' @param src image url.
#' @param label character, should be short.
#' @param width width of logo.
#' @param height height of footer, defaults to \code{"160px"}.
#' @param italic logical, for label text.
#' @param bold logical, for label text.
#' @param style character, option style string for logo and label. See function for defaults.
#'
#' @return an html footer tag.
#' @export
#'
#' @examples
#' #not run
dashboard_footer <- function(href, src, label = "", width = "100%", height = "160px", italic = TRUE, bold = TRUE,
                             style = "text-align:center; align: center; padding: 0px; margin: 0px;"){
  if(italic) label <- shiny::em(label)
  if(bold) label <- shiny::strong(label)
  shiny::tags$footer(
    shiny::a(href = href, target = "_blank", shiny::tags$img(src=src, width = width), style = style),
    shiny::p(label, style = style),
    style=paste0("position:absolute; align: center; bottom:0px; width:100%; height:",
                 height, "; color: white; padding: 5px;")
  )
}

# rintrojs calback function helpers
.stepEquals <- function(i) paste0("this._currentStep==", i-1, collapse=" || ")

.dv <- function(x, quote=TRUE){
  x <- paste0("a[data-value=\"", x, "\"]")
  if(quote) x <- paste0("'", x, "'")
  x
}

.rmClass <- function(x) paste0(paste0(
  "$(", .dv(x), ").removeClass('active');", collapse="\n"), "\n")

.goClass <- function(x){
  if(length(x) > 1) stop("Only add and trigger one class at a time.")
  paste0("$(", .dv(x), ").addClass('active');\n$(", .dv(x), ").trigger('click');\n")
}

.stepcb <- function(condition, action){
  paste0("if (", condition, ") {", paste0(action, collapse="\n"), "}")
}

#' Generate a JS callback string for rintrojs tour
#'
#' This function generates an JS callback character string to change remove an active class
#' and add and trigger a new class in its place.
#'
#' This function is useful on rintrojs tours in order to, for example, ensure that the active tab panel
#' changes accordingly depending on tour content that is required to be visible during particular steps.
#' Note that only one active class should be added at a time. Call twice to add two active classes for a common step.
#'
#'
#' @param step numeric, a vector of steps to apply the change to.
#' @param from character, a vector of the the active classes to be removed.
#' @param to character, a single class to add as active and trigger with a click.
#'
#' @return a character string.
#' @export
#'
#' @examples
#' #not run
tour_changeClass <- function(step, from, to){
  .stepcb(.stepEquals(step), c(.rmClass(from), .goClass(to)))
}

#' Read paragraphs from file
#'
#' Read paragraphs from a text file based on markdown format.
#'
#' The text file does not need to be a \code{.md} file. The function merely relies on the assumption that
#' paragraphs are separated by an empty line, based on markdown formatting. This allows for typing up
#' paragraphs in a plain text file using this convenient formatting, to be read into R wherever paragraphs of
#' text are needed in an app.
#'
#' Common uses include returning a vector of strings to be used as text for each tour step description
#' in an introjs tour (\code{ptag=FALSE} and \code{collapse=FALSE}, the default)
#' or a collapsed string of html text (\code{ptag=TRUE} and \code{collapse=TRUE}) for an app description.
#'
#' @param file character, file to read text from.
#' @param ptag logical, wrap paragraphs in html paragraph tag an include full justification.
#' @param collapse logical, collapse vector of paragraphs into a single element.
#'
#' @return a vector of character strings.
#' @export
#'
#' @examples
#' #not run
read_md_paragraphs <- function(file, ptag = FALSE, collapse = FALSE){
  x <- readLines(file) %>% unlist
  idx <- which(x == "")
  l <- length(idx) + 1
  text <- vector("list", l)
  if(l > 1){
    idx <- unique(c(0, idx))
    for(i in 1:l){
      text[[i]] <- if(i < l) x[(idx[i] + 1):(idx[i + 1] - 1)] else x[(idx[i] + 1):length(x)]
    }
  } else text <- list(x)
  text <- purrr::map_chr(text, ~paste(.x, collapse = " "))
  if(ptag) text <- purrr::map_chr(text, ~paste0("<p style='text-align: justify;'>", .x, "</p>"))
  if(collapse) text <- paste(text, collapse="\n")
  text
}
