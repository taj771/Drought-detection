# Databricks notebook source
# Count distinct ERA5 pixels per country (for scaling estimate vs Benin).
from pyspark.sql import SparkSession
spark = SparkSession.builder.getOrCreate()
df = spark.sql("""
  SELECT country, COUNT(DISTINCT CONCAT(x,'_',y)) AS n_pixels
  FROM projects.xx_drought_index.drought_index_dailysummary
  WHERE country IN ('Benin','Argentina','Thailand','Greece','Mexico','South Africa')
  GROUP BY country
  ORDER BY n_pixels
""")
rows = df.collect()
out = [f"{r['country']}={r['n_pixels']}" for r in rows]
total = sum(r['n_pixels'] for r in rows)
out.append(f"TOTAL_6={total}")
dbutils.notebook.exit(" || ".join(out))
