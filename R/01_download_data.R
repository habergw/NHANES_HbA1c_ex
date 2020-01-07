# Create NHANES_Data directory
dir.create("NHANES_Data")

# Download non-DXA data
url <- "https://wwwn.cdc.gov/Nchs/Nhanes/"

years <- c("1999-2000/", "2001-2002/", "2003-2004/", "2005-2006/")

filenames <- list(paste0(c("DEMO", "BMX", 
                           "DIQ", "ALQ", "SMQ", "PAQ", 
                           "PAQIAF", "UC", "LAB10"), ".XPT"),
                  paste0(c("DEMO", "BMX", 
                            "DIQ", "ALQ", "SMQ", "PAQ", 
                            "PAQIAF", "UC", "L10"), "_B.XPT"), 
                  paste0(c("DEMO", "BMX", 
                            "DIQ", "ALQ", "SMQ", "PAQ", 
                            "PAQIAF", "UC", "L10"), "_C.XPT"),
                  paste0(c("DEMO", "BMX", 
                            "DIQ", "ALQ", "SMQ", "PAQ", 
                            "PAQIAF", "UCPREG", "GHB"), "_D.XPT")) 

lapply(1:4, function(x)
       lapply(filenames[[x]], function(y) download.file(paste0(url, years[x], y),
                                           paste0("NHANES_Data/", y))))

# Download DXA data
url2 <- "https://wwwn.cdc.gov/nchs/data/nhanes/dxa/"

filenames_dxa <- paste0("dxx", c("", "_B", "_C", "_D"), ".xpt")

lapply(filenames_dxa, function(x)
       download.file(paste0(url2, x), paste0("NHANES_Data/", x)))
