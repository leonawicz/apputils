This Shiny app demonstrates the use of custom image files in place of icons for value boxes in Shiny Dashboard.

### Motivation

Libraries like font awesome and glyphicon cannot be expected to provide a substantial suite of icons tailored to probability and statistics or many other subjects. Examples here use custom icons for inspiration, which are simply tiny png files of native R plots, included in the `apputils` package.

### Details

Custom image file icons are made possible by overriding two functions: `icon` from the `shiny` package and `valueBox` from the `shinydashboard` package
with new versions in `apputils`. Each function adds minimal, specific additional handling of image files. Note that custom css must be included so that value boxes can display the icons. `apputils` includes this CSS when you call `use_apputils` in the UI code of your app.

For more information, see the `apputils` help documentation. Also consult the [package website](https://leonawicz.github.io/apputils/).
There is also a [blog post](https://leonawicz.github.io/apputils/) regarding these icons.
