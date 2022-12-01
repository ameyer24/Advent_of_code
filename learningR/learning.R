install.packages("tidyverse")
library(tidyverse)
## installing other packages too
install.packages(c("nycflights13", "gapminder", "Lahman"))
dput(mtcars)
mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg)
?mpg

ggplot(data = mpg)

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
