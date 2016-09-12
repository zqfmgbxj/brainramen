require("dplyr")
require("tidyr")
require("psych")

refine <- read.csv("refine_original.csv")

#1. clean up brand names

refine$company <- sub(".*ps$", "phillips", refine$company, ignore.case = TRUE); 
refine$company <- sub("^a.*", "akzo", refine$company, ignore.case = TRUE); 
refine$company <- sub("^v.*", "van houten", refine$company, ignore.case = TRUE); 
refine$company <- sub("^u.*", "unilever", refine$company, ignore.case = TRUE);

#2. separate product code and number 
#Separate the product code and product number into separate columns 
#i.e. add two new columns called product_code and product_number, 
#containing the product code and number respectively

refine <- refine %>% mutate(temppcn = Product.code...number) 
refine <- separate(data = refine, col = temppcn, into = c("product_code", "product_number"), sep ="-")

#3. Add product categories
#You learn that the product codes actually represent the following product categories:
#    p = Smartphone
#    v = TV
#    x = Laptop
#    q = Tablet

#In order to make the data more readable, add a column with the product category for each record.

product_category <- function(productcode) {
  if (productcode == "p") {
    return ("Smartphone")
  }
  else if (productcode == "v") {
    return ("TV")
  }
  else if (productcode == "x") {
    return ("Laptop")
  }
  else if (productcode == "q") {
    return ("Tablet")
  }
}

refine <- refine %>% mutate(product_category = sapply(product_code, product_category))

#4: Add full address for geocoding
#Create a new column full_address that concatenates the three address fields (address, city, country), separated by commas.

refine <- refine %>% unite(full_address, address:country, sep = ", ", remove = FALSE)

#5: Create dummy variables for company and product category
#Both the company name and product category are categorical variables i.e. they take only a fixed set of values. 
#Create dummy binary variables for each of them with the prefix company_ and product_ i.e.
    #Add four binary (1 or 0) columns for company: company_philips, company_akzo, company_van_houten and company_unilever
    #Add four binary (1 or 0) columns for product category: product_smartphone, product_tv, product_laptop and product_tablet

dummy <- dummy.code(refine$company)
refinetemp <- data.frame(refine, dummy)
colnames(refinetemp)[11:14] <- paste("company_", colnames(refinetemp[ c(11:14)]), sep = "")

dummy2 <- dummy.code(refinetemp$product_category)
refinal <- data.frame(refinetemp, dummy2)
colnames(refinal)[15:18] <- paste("product_", colnames(refinal[ c(15:18)]), sep = "")

write.csv(refinal, "refine_new.csv")
