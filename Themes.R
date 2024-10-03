#the default colours in the shiny dashboard theme come from adminlte_color
mytheme <- fresh::create_theme(
  fresh::adminlte_color(
    light_blue = "#005EB8", #NHS Blue
    blue = "#003087", #NHS Dark Blue
    purple = "#330072" #NHS Purple
    
  )
)

url1 <- a("https://github.com/rbrown02", href = "https://github.com/rbrown02")
url2 <- a("https://github.com/ropensci/fingertipsR", href = "https://github.com/ropensci/fingertipsR")