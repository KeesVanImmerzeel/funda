#' Return number of web pages associated with url of 'gemeente'
#'
#' @param url url of webpage (character)
#' @return number of web pages associated with url of 'gemeente' (integer)
# @examples
# nr_of_pages("Doesburg")
# @export
nr_of_pages <- function(url) {
      x <- xml2::read_html(url) %>%
            rvest::html_node(".pagination")  %>%
            rvest::html_text() %>%
            gsub("\r\n", "", .) %>%
            gsub(" ", "", .) %>%
            gsub("Pagina", " ", .)  %>%
            gsub("van", " ", .)  %>%
            gsub("Volgende", "", .) %>%
            gsub("^ ", "", .) %>%
            gsub("  ", " ", .) %>%
            strsplit( " ") %>%
            unlist() %>%
            as.numeric() %>%
            max()
      return(x)
}

#' Replace spaces and leave out characters 'Gemeente' in names of 'gemeente'  in order to create a valid url.
#'
#' @param gemeente Name of 'gemeente' (character)
#' @return Name of gemeente in correct format to use in an url (character)
# @examples
# gemeente <- "Gemeente Oost Gelre"
# adj_gemeente_str(gemeente)
adj_gemeente_str <- function(gemeente) {
      gemeente %<>% gsub("Gemeente", "", .) %>%
            gsub("^ ", "", .) %>%
            stringr::str_replace_all(., " ", "-")
      return(gemeente)
}


#' Create valid url for 'gemeente'.
#'
#' @inheritParams adj_gemeente_str
#' @return valid url for 'gemeente' (character)
# @examples
# url <- "Doesburg"
# create_gemeente_url(gemeente)
create_gemeente_url <- function(gemeente) {
      gemeente %<>% adj_gemeente_str()
      return(
            paste0("https://www.funda.nl/koop/gemeente-",
                   gemeente,"/"))
}

#' Create valid urls to all web pages of 'gemeente'.
#'
#' @inheritParams adj_gemeente_str
#' @return valid urls for 'gemeente' (character vector)
# @examples
# url <- "Doesburg"
# create_gemeente_urls(gemeente)
create_gemeente_urls <- function(gemeente){
      url <- create_gemeente_url(gemeente)
      n <- nr_of_pages(url)
      s <- rep(url,n)
      i <- 1:n %>% as.character() %>% paste0("p",.,"/")
      i[1] <- ""
      return(paste0(s,i))
}

#' Create list of html documents of all web pages of 'gemeente'.
#'
#' @inheritParams adj_gemeente_str
#' @return valid html documents of 'gemeente' (character vector)
# @examples
# url <- "Doesburg"
# x <- create_gemeente_htmls(gemeente)
create_gemeente_htmls <- function(gemeente) {
      urls <- create_gemeente_urls(gemeente)
      return(apply(urls %>% as.array(), 1, xml2::read_html))
}

#' TRUE if html attr (as read with html_attr(.,"href")) refers to a house for sale.
#'
#' @param html_attr html attr (as read with html_attr(.,"href")) (character)
#' @return TRUE if html attr (as read with html_attr(.,"href")) refers to a house for sale (boolean)
is_house_info_link <- function(html_attr) {
      return(length(grep("^/koop/.*/huis-\\d{8}-.*/$", html_attr)) == 1)
}

#'  Use html document to extract the core of urls of houses for sale.
#'
#' @param html_doc a html document (as read with function 'create_gemeente_htmls()')
#' @return All core urls to houses for sale from html document (character vector)
# url <- "Doesburg"
# x <- create_gemeente_htmls(gemeente)
# extract_urls_from_html(x[[1]])
extract_core_urls_from_html <- function(html_doc) {
      x <-
            html_doc %>% rvest::html_nodes(., "a") %>%
            rvest::html_attr(., "href") %>%
            as.array()
      is_addr <- x %>%
            apply(1,is_house_info_link)
      x <- x[is_addr]
      x %<>% unique() %>% paste0("funda.nl",.)
      return(x)
}

#'  Extract url's of houses in 'gemeente'.
#'
#' @inheritParams adj_gemeente_str
#' @return url's of houses in 'gemeente' (character vector)
#' examples
#' create_house_urls("Doesburg" )
create_house_urls <- function(gemeente) {
      x <- create_gemeente_htmls(gemeente)
      lapply(x, extract_core_urls_from_html) %>% unlist() %>% paste0("https://www.",.)
}

#' create a list of htmls with information on houses for sale.
#'
#' @inheritParams adj_gemeente_str
#' @return list of htmls with information on houses for sale.
#' examples
#' x <- create_house_htmls("Doesburg" )
#' @export
create_house_htmls <- function(gemeente) {
      urls <- create_house_urls(gemeente)
      return(apply(urls %>% as.array(), 1, xml2::read_html))
}
