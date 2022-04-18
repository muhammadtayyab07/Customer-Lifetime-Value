#tayyab
library(tidyverse)
library(lubridate)
df_sales_data <-
  read.csv("df_sales_data.csv")

#average purchase frequency
df_sales_data |>
  mutate(
    invoicedate = strptime(InvoiceDate, format = "%m/%d/%Y %H:%M"),
    month = lubridate::month(invoicedate, label = TRUE, abbr = TRUE),
    year = lubridate::year(invoicedate)) |>
  drop_na() |>
  group_by(CustomerID, month, year) |>
  summarise(n_transaction = n_distinct(InvoiceNo)) |>
  ungroup() |> 
  summarise(avg_purchase_freq = mean(n_transaction)) -> avg_purchase_frequency

#average purchase value
df_sales_data |>
  mutate(subtotal = Quantity * UnitPrice) |>
  group_by(InvoiceNo) |>
  summarise(total_value = sum(subtotal)) |>
  summarise(avg_purchase_value = mean(total_value)) -> avg_purchase_value

#average lifespan
df_sales_data |>
  mutate(invoice_no = strptime(InvoiceDate, format = "%m/%d/%Y %H:%M")) |>
  group_by(CustomerID) |>
  summarise(
    last_purchase = max(invoice_no),
    first_purchase = min(invoice_no),
    lifespan = difftime(last_purchase, first_purchase, units = "days")
  ) |>
  mutate(monthly_lifespan = as.integer((lifespan) / 30)) |>
  summarise(average_lifespan = mean(monthly_lifespan)) -> avg_lifespan

CLV <- avg_purchase_frequency * avg_purchase_value * avg_lifespan * (0.3)
print(CLV)

