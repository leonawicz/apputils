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

app_img_links <- function(app_url, img_url, title, subtitle, height=200, min.width=300, max.width=400, col.width=4){
  x <- purrr::map(seq_along(app_url),
                  ~shiny::column(col.width, .app_img_link(app_url[.x], img_url[.x], title[.x], subtitle[.x], height),
                          style=paste0("min-width: ", min.width, "px; max-width: ", max.width, "px; padding:5px;")))
  shiny::fluidRow(x, style="padding: 10px;")
}
