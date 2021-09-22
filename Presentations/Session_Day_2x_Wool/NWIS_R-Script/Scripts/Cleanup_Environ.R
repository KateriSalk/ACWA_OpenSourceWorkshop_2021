## Any object in the global environ that DOES NOT START with KEEP will be deleted

rm(list = grep("^Keep|^PLOT", ls(), value = TRUE, invert = TRUE))


