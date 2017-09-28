# apputils 0.4.9 (Release date: 2017-09-28)

* Generalized `faq`, adding a number of arguments to give more control over appearance and content. Migrated a new SNAP-specific version to `snaputils`.
* BREAKING CHANGES: updated several argument names for various functions.
* Unnamed list can now be passed to `links` in `contactinfo`. It will take strings and wrap them in `shiny::HTML`.
* Added heading size argument to `contactinfo`.
* Added `color_indexer` helper function.

# apputils 0.4.8 (Release date: 2017-09-22)

* Generalized `contactinfo`, adding a number of arguments to give more control over appearance and content. Migrated a new SNAP-specific version to `snaputils`.

# apputils 0.4.7 (Release date: 2017-09-01)

* Updated collection of functions. Some are still SNAP-specific and in the process of being migrated to `snaputils`.
* Other functions may migrate to `maputils`.
* Added documentation, web pages using `pkgdown`.
* Package development now more fully integrated and streamlined with SNAPverse practices and themes.
* Initial scaffolding in place for vignettes and other materials.

# apputils 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.
