

# Run these lines of code. To run a line of code, you can do one of three options: 
# 1. Click your cursor anywhere on a given line and press Control+Return (Windows) or Command+Return (Mac)
# 2. Highlight one or more lines with your mouse, then press Control+Return (Windows) or Command+Return (Mac)
# 3. Highlight one or more lines iwth your mouse, then press the "Run" button at the top of the window


# Run these lines. They should run without generating an "error" message. 
# If a pop-up comes on to ask about compilation, click "yes"
install.packages("tidyverse")
install.packages("lubridate")
install.packages("viridis")
install.packages("rnoaa")
install.packages("lfstat")
install.packages("dataRetrieval")

# Run these lines. They should run without generating an "error" message.
library(tidyverse)
library(lubridate)
library(viridis)
library(rnoaa)
library(lfstat)
library(dataRetrieval)

# Run these two lines together. A plot should appear in your "plots" window.
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point()
