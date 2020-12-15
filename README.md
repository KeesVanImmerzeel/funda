# Package "funda"

<!-- badges: start -->
<!-- badges: end -->

Functions to explore the website 'funda.nl'.

## Installation

to install this package:

`install_github("KeesVanImmerzeel/funda")`

Then load the package:

`library("funda")` 

## Functions in this package

- `fd_create_house_htmls'(): Create a list of htmls with information on houses for sale in 'gemeenten'.
- `fd_addresses'(): Extract addresses of houses in a list of html documents.
- `fd_type'(): Extract type of construction from a list of html documents.
- `fd_plot_area'(): Extract Plot area of houses from a list of html documents. 
- `fd_gemeenten'(): Extract 'gemeenten' from a list of html documents.
- `fd_description'(): Extract 'description' of houses from a list of html documents.
- `fd_descr_match'(): Check if pattern matches description of house in a list of html documents.


## Get help

To get help on the functions in this package type a question mark before the function name, like `?fd_create_house_htmls()`.

## Examples

```
# Create a data frame of information on houses for sale in the selected 'gemeenten'.

gemeenten <- c("Aalten","Arnhem","Doesburg","Doetinchem","Duiven", "Lochem","Rheden","Rozendaal", "Westervoort", "Winterswijk",
               "Zevenaar", "Zutphen", "Oude IJsselstreek", "Oost Gelre", "Berkelland", "Bronckhorst", "Montferland")
house_htmls <- fd_create_house_htmls(gemeenten)
pattern <-
      "puntstuk|bron|waterbron|beregeningsbron|put|waterput|welput"
df <- data.frame(
      gemeente=fd_gemeenten(house_htmls),
      adres=fd_addresses(house_htmls),
      perceel_opp=fd_plot_area(house_htmls),
      type_bouw=fd_type(house_htmls),
      beschrijving=fd_description(house_htmls),
      puntstuk=fd_descr_match(house_htmls, pattern = "puntstuk|bron|waterbron|beregeningsbron|put|waterput|welput")
)
```
