#' Grab gist urls.
#'
#' @param id shorthand character id refering to entry in gist_url dictionary.
#'
#' @return a character string.
#' @export
#'
#' @examples
#' #not run
gist_url <- function(id){
  switch(id,
    css="https://gist.githubusercontent.com/leonawicz/24ed656f63d4a889ad7043bc5436a641/raw/050538f0c78616ac53a03ebebe9c256d33f9053f/shiny_app_styles.css",
    showcase="https://gist.githubusercontent.com/leonawicz/fb34c7d835e546c3790e7d0d6aa8bdeb/raw/f53040823f222df76d26e2828f4dcfe6da022ad3/shiny_app_showcase.R"
  )
}

.app_img_link <- function(app_url, img_url, title, subtitle, height=200){
  shiny::HTML(paste0(
    '<div class="img_link_wrap">
    <img class="img_app" src="', img_url, '" width="100%" height="', height, '"/>
    <a href="', app_url, '" style="color:white;" target="_blank"
    <div class="img_hover_layer">
    <div class="img_hover">
    <h3><p>', title, '</p></h3>
    <h4><p class="img_hover">', subtitle, '</p></h4>
    </div>
    </div>
    </a>')) # contextual, must remove a closing div with mutliple inline calls
}

.app_img_links <- function(app_url, img_url, title, subtitle, drop=NULL, height=200, min.width=300, max.width=400, col.width=4){
  apps <- basename(app_url)
  if(!is.null(drop) && !all(drop %in% apps)) stop("Cannot drop apps that are not in the master list.")
  idx <- if(is.null(drop)) seq_along(apps) else seq_along(app_url)[match(drop, apps)]
  x <- purrr::map(idx,
                  ~shiny::column(col.width, .app_img_link(app_url[.x], img_url[.x], title[.x], subtitle[.x], height),
                          style=paste0("min-width: ", min.width, "px; max-width: ", max.width, "px; padding:5px;")))
  shiny::fluidRow(x, style="padding: 10px;")
}

#' Genrate app showcase content
#'
#' @param drop character vector of any apps to exclude from showcase.
#'
#' @return a shiny fluidRow containing organized and stylized app image links for reference.
#' @export
#'
#' @examples
#' #not run
app_showcase <- function(drop=NULL){
  args <- list(
    app_url=c(
      "http://shiny.snap.uaf.edu/climdist",
      "https://uasnap.shinyapps.io/jfsp-v10",
      "https://uasnap.shinyapps.io/ar5eval",
      "https://uasnap.shinyapps.io/cc4liteFinal",
      "https://uasnap.shinyapps.io/ex_leaflet",
      "https://uasnap.shinyapps.io/nwtapp",
      "http://shiny.snap.uaf.edu/standage"
    ),
    img_url=c(
      "https://raw.githubusercontent.com/leonawicz/jfsp/master/_jfsp_small.png",
      "https://raw.githubusercontent.com/leonawicz/ar5eval/master/_ar5eval_small.png",
      "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/cc4liteFinal.jpg",
      "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/ex_leaflet.jpg",
      "https://github.com/ua-snap/shiny-apps/raw/master/_images/small/nwtapp.jpg",
      "https://raw.githubusercontent.com/leonawicz/agedist/master/_agedist_small.png",
      "https://raw.githubusercontent.com/leonawicz/dash/master/images/_climdist_small.png"
    ),
    title=c(
      "CMIP5 Regional Climate", "Alaska Wildfire Projections", "Climate Model Analysis",
      "Communities & Climate", "Interactive Documents", "Northwest Territories",
      "Alaska Vegetation Changes"
    ),
    subtitle=c(
      "Full distributions", "ALFRESCO model output", "CMIP5 GCM evaluation",
      "Alaska & western Canada", "Leaflet + Shiny observers", "Climate projections",
      "Tree stand age projections"
    )
  )
  do.call(.app_img_links, c(args, drop=drop))
}

#' Add contact information widget to app.
#'
#' @return a shiny taglist.
#' @export
#'
#' @examples
#' #not run
contactinfo <- function(){
  shiny::tagList(
    shiny::h2("Contact information"),
    shiny::HTML('
         <div style="clear: left;"><img src="https://www.gravatar.com/avatar/5ab20ebc3829054f8af7b1ea4a317269?s=128"
         alt="" style="float: left; margin-right:5px" /></div>
         <p>Matthew Leonawicz<br/>
         Statistician | useR<br/>
         <a href="http://leonawicz.github.io" target="_blank">Github.io</a> |
         <a href="http://blog.snap.uaf.edu" target="_blank">Blog</a> |
         <a href="https://twitter.com/leonawicz" target="_blank">Twitter</a> |
         <a href="http://www.linkedin.com/in/leonawicz" target="_blank">Linkedin</a> <br/>
         <a href="http://www.snap.uaf.edu/", target="_blank">Scenarios Network for Alaska and Arctic Planning</a>
         </p>'
    ),
    shiny::p("For questions about this application, please email mfleonawicz@alaska.edu")
  )
}
