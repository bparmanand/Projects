# -*- coding: utf-8 -*-
"""
##################################################
#
# QMB 6358: Software Tools for Business Analytics
#
# Assignment 8
#
# Name: Brandon Parmanand
# College of Business Administration
# University of Central Florida
#
# Date: 11/30/2021
#
#
"""

##################################################
# Import Packages
##################################################
import os 
from csv import reader
import sqlite3

##################################################
# Set Working Directory.
##################################################
# Find out the current directory.
os.getcwd()
# Change to a new directory.
git_path = 'C:\\Users\\brand\\OneDrive\\Documents\\GitHub\\Brandon-QMB6358-Solutions\\Assignment_08'
os.chdir(git_path)
# Check that the change was successful.
os.getcwd()
##################################################



con = sqlite3.connect('TractorDataBasePython.db')

cur = con.cursor()

with open (os.path.join(git_path, 'tractor_sales.csv'), 'r')as read_file:
    csv_reader = reader(read_file)
    list_of_rows = list (csv_reader)

# Create Table for Tractor Sales

cur.execute('''CREATE TABLE TractorSales(
    SaleID TEXT,
    Saleprice INTEGER NOT NULL,
    Age INTEGER NOT NULL,
    Enghours INTEGER NOT NULL,
    JohnDeere INTEGER NOT NULL,
    Spring INTEGER NOT NULL,
    Summer INTEGER NOT NULL,
    Winter INTEGER NOT NULL,
    PRIMARY KEY (SaleID))''')
con.commit()

# Insert Data

for row in list_of_rows:
    cur.execute('INSERT INTO TractorSales VALUES (?,?,?,?,?,?,?,?)', row)
    con.commit()
    
# Check Contents
   
cur.execute('SELECT * FROM TractorSales')
for row in cur.fetchall():
    print(row)
    
# John Deere

cur.execute('''SELECT 
            johndeere,
            MIN(Saleprice) AS MinPrice,
            MAX(Saleprice) AS MaxPrice,
            AVG(Saleprice) AS AvgPrice
            FROM
            TractorSales
            GROUP BY 
            JohnDeere''')
            
for row in cur.fetchall():
    print(row)

# Create Table for Tractor Specs

with open('tractor_specs.csv','r') as read_specs:
    file = reader(read_specs)
    list_specs = list(file)
    print(list_specs)

 
cur.execute('''CREATE TABLE TractorSpecs ( 
SaleID TEXT,
Horsepower INTEGER NOT NULL,
Diesel INTEGER NOT NULL,
Fwd INTEGER NOT NULL, 
Manual INTEGER NOT NULL,
PRIMARY KEY (saleID))''')
con.commit()

for rows in list_specs:
    cur.execute('INSERT INTO TractorSpecs VALUES (?, ?, ?, ?, ?)', rows)
con.commit()

cur.execute('SELECT * FROM TractorSpecs')
for rows in cur.fetchall():
    print(rows)
    
    
# Join the data in TractorSales with TractorSpecs; add a binary variable to the
# joined table to indicate whether each tractor has an enlosed cab or not   
  
cur.execute('''SELECT
TractorSales.*,
TractorSpecs.Horsepower,
TractorSpecs.Diesel,
TractorSpecs.Fwd,
TractorSpecs.Manual
From
TractorSales as TractorSales
INNER JOIN 
TractorSpecs AS TractorSpecs
ON TractorSales.saleID = TractorSpecs.SaleID''')
con.commit()

# Create Table with Cabs

with open('tractors_with_cabs.csv','r') as read_cabs:
    file = reader(read_cabs)
    list_cabs = list(file)
    print(list_cabs)
    
cur.execute('''CREATE TABLE TractorCabs (
SaleID TEXT, 
Cab INTEGER NOT NULL,
PRIMARY KEY (saleID)
)''')
con.commit()

for rows2 in list_cabs:
    cur.execute('INSERT INTO TractorCabs VALUES (?, ?)', rows2)
con.commit()

cur.execute('SELECT * FROM TractorCabs')
for rows2 in cur.fetchall():
    print(rows2)

        
# Create Full Table
cur.execute('''CREATE TABLE TractorFull (
  SaleID        TEXT ,
  Saleprice     INTEGER NOT NULL  ,
  Age           INTEGER NOT NULL ,
  Enghours      INTEGER NOT NULL ,
  JohnDeere     INTEGER NOT NULL ,
  Spring	    INTEGER NOT NULL ,
  Summer	    INTEGER NOT NULL ,
  Winter	    INTEGER NOT NULL ,
  Horsepower    INTEGER NOT NULL ,
  Diesel        INTEGER NOT NULL ,
  Fwd	        INTEGER NOT NULL ,
  Manual	    INTEGER NOT NULL ,
  Cab           INTEGER NOT NULL ,
  PRIMARY KEY (saleID)
)''')
con.commit()

# Join into full table
 
cur.execute('''INSERT INTO TractorFull
SELECT
TractorSales.*,
TractorSpecs.Horsepower,
TractorSpecs.Diesel,
TractorSpecs.Fwd,
TractorSpecs.Manual,
TractorCabs.Cab
From
TractorSales as TractorSales
INNER JOIN 
TractorSpecs AS TractorSpecs
ON TractorSales.saleID = TractorSpecs.saleID
INNER JOIN 
TractorCabs AS TractorCabs ON TractorSales.saleID = TractorCabs.saleID''')
con.commit()

cur.execute('SELECT * FROM TractorFull')
for allrows in cur.fetchall():
    print(allrows)
 

# Tabulate with Different Variables

# Diesel

cur.execute('''SELECT 
MIN(horsepower)     AS MinHorsepower , 
AVG(horsepower)     AS AvgHorsepower , 
MAX(horsepower)     AS MaxHorsepower ,
MIN(saleprice)     AS MinPrice , 
AVG(saleprice)     AS AvgPrice , 
MAX(saleprice)     AS MaxPrice 
FROM
TractorFull
GROUP BY Diesel''')
            
for Diesel in cur.fetchall():
    print(Diesel)

# FWD?
    
cur.execute('''SELECT 
MIN(horsepower)     AS MinHorsepower , 
AVG(horsepower)     AS AvgHorsepower , 
MAX(horsepower)     AS MaxHorsepower ,
MIN(saleprice)     AS MinPrice , 
AVG(saleprice)     AS AvgPrice , 
MAX(saleprice)     AS MaxPrice 
FROM
TractorFull
GROUP BY Fwd''')
            
for Fwd in cur.fetchall():
    print(Fwd)

# Manual?

cur.execute('''SELECT 
MIN(horsepower)     AS MinHorsepower , 
AVG(horsepower)     AS AvgHorsepower , 
MAX(horsepower)     AS MaxHorsepower ,
MIN(saleprice)     AS MinPrice , 
AVG(saleprice)     AS AvgPrice , 
MAX(saleprice)     AS MaxPrice 
FROM
TractorFull
GROUP BY Manual''')
            
for Manual in cur.fetchall():
    print(Manual)
    
# JohnDeere?
 
cur.execute('''SELECT 
MIN(horsepower)     AS MinHorsepower , 
AVG(horsepower)     AS AvgHorsepower , 
MAX(horsepower)     AS MaxHorsepower ,
MIN(saleprice)     AS MinPrice , 
AVG(saleprice)     AS AvgPrice , 
MAX(saleprice)     AS MaxPrice 
FROM
TractorFull
GROUP BY JohnDeere''')
            
for JohnDeere in cur.fetchall():
    print(JohnDeere)
    
# Cab?

cur.execute('''SELECT 
MIN(horsepower)     AS MinHorsepower , 
AVG(horsepower)     AS AvgHorsepower , 
MAX(horsepower)     AS MaxHorsepower ,
MIN(saleprice)     AS MinPrice , 
AVG(saleprice)     AS AvgPrice , 
MAX(saleprice)     AS MaxPrice 
FROM
TractorFull
GROUP BY Cab''')
            
for Cab in cur.fetchall():
    print(Cab)


####################################################################
#End
####################################################################

