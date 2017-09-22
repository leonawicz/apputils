# apputils 0.4.8 (Release date: 2017-09-22)

* Generalized contact info, adding a number of arguments to give more control over appearance and content. Migrated  a new SNAP-specific version to `snaputils`.

# apputils 0.4.7 (Release date: 2017-09-01)

* Updated collection of functions. Some are still SNAP-specific and in the process of being migrated to `snaputils`.
* Other functions may migrate to `maputils`.
* Added documentation, web pages using `pkgdown`.
* Package development now more fully integrated and streamlined with SNAPverse practices and themes.
* Initial scaffolding in place for vignettes and other materials.

# apputils 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

contactinfo <- function(id = "leonawicz", snap, iarc, uaf){
  logo <- c(uaf, iarc, snap)
  href <- c("http://www.uaf.edu", "https://web.iarc.uaf.edu", "https://www.snap.uaf.edu")
  
  if(id == "leonawicz"){
    links <- list(
      GitHub.io = "https://leonawicz.github.io",
      LinkedIn = "http://www.linkedin.com/in/leonawicz",
      Twitter = "https://twitter.com/leonawicz",
      Blog = "https://leonawicz.github.io/blog"
    )
    info <- apputils::contactinfo(
      name = "Matthew Leonawicz",
      role = "Statistician | R Developer",
      photo = "https://www.gravatar.com/avatar/5ab20ebc3829054f8af7b1ea4a317269?s=128",
      logo = logo, href = href, 
      links = links, 
      header = "Contact information",
      footnote = "For questions about this application, please email mfleonawicz@alaska.edu", 
      logo_height = 170, photo_width = 128, photo_height = 128
    )
  }
  info
}
