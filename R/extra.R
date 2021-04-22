#' Plot number of missing values by class
#'
#' This function returns a dataframe with a multilevel structure. It generates a dataframe using a varying
#' intercepts/varying slopes linear regression with a single target variable y.
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_bar coord_flip facet_wrap labs theme_bw
#' @param df dataframe with missing values
#' @param class name of the variable containing classes
#' @return A barplot with the number of missing values by class, by variable
#' @export
#'
#' @examples
#' data(data.example)
#' missing.plot(data.example, "class")
missing.plot <- function(df, class){

  missClust <- df %>% dplyr::group_by(class) %>%
    dplyr::summarise_if(is.numeric, ~sum(is.na(.x))) %>%
    tidyr::gather(var, value, -class)

  g <- ggplot(missClust) +
    geom_bar(aes(x = class, y = value), stat="identity") +
    facet_wrap(~var)+
    labs(x = "Class", y ="Number of missing values") +
    theme_bw()
  g
}

#' Plot pattern of missing values by class
#'
#' This function returns a dataframe with a multilevel structure. It generates a dataframe using a varying
#' intercepts/varying slopes linear regression with a single target variable y.
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes coord_flip facet_wrap labs theme_bw scale_fill_continuous geom_tile
#' @param df dataframe with missing values
#' @param class name of the variable containing classes
#' @return A plot with the patter of missing values by class, by variable
#' @export
#'
#' @examples
#' data(data.example)
#' pattern.plot(data.example, "class")
pattern.plot <- function(df, class){

  df_na <- df %>% dplyr::group_by(class) %>%
    dplyr::mutate(obs = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tidyr::gather(var, value, -c(class, obs))

  g <- ggplot(df_na, aes(class, obs, fill= value)) +
    facet_wrap(~var, ncol = 1)+
    geom_tile(colour = "black") +
    scale_fill_continuous(high = "gray", na.value = 'white') +
    theme_bw() +
    labs( y ="")
  g

}

#' Plot pattern of missing values by class
#'
#' This function returns a dataframe with a multilevel structure. It generates a dataframe using a varying
#' intercepts/varying slopes linear regression with a single target variable y.
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes coord_flip facet_wrap labs theme_bw scale_fill_continuous geom_tile
#' @param df dataframe with missing values
#' @param y target variable
#' @param class name of the variable containing classes
#' @return A boxplot for each class of the target variable
#' @importFrom ggplot2 ggplot aes geom_boxplot theme_bw

#' @export
#'
#' @examples
#' data(data.example)
#' target.boxplot(data.example, y, "class")
target.boxplot <- function(df, y, class){

  ggplot(df %>% tidyr::drop_na(), aes(class, y)) +
    geom_boxplot()+
    theme_bw()
}









