library(RODBC)
cn <-odbcDriverConnect(connection="Driver={SQL Server Native Client 11.0};server=vigi-dw01;database=Datawarehouse;trusted_connection=yes;")
res <- sqlQuery(cn, 'select * from [DataWarehouse].[dbo].[DimDate]')


