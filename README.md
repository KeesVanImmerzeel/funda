# Package "funda"

<!-- badges: start -->
<!-- badges: end -->

Functions to explore the website 'funda.nl'.

![The R-code for producing this image is available in the folder: data-raw/example](https://user-images.githubusercontent.com/16401251/102477519-be5f0800-405c-11eb-8642-bee15a3a2c40.PNG)

[Interactive version of the map above.](https://rpubs.com/KeesVanImmerzeel/Puntstukken-15-12-2020)

## Installation

To install this package:

`install_github("KeesVanImmerzeel/funda")`

To load the package:

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
check the folder data-raw/example for a more elaborated example.
