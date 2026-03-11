import pandas as pd

df=pd.read_csv("logistics_orders.csv")

df2=pd.read_csv("logistics_shipments.csv")

df3=pd.read_csv('logistics_vehicle_tracking.csv')

df4=pd.read_csv("logistics_inventory.csv")

#Lọc dữ liệu có order_value > 0
# order_value_cleaned=df[df['order_value']>0]

# order_value_cleaned.to_csv("logistics_orders_cleaned.csv",index=False)

# delivery_date_cleaned=df2[(df2['delivery_days']>0) & (df2['shipping_cost']>0)]

# delivery_date_cleaned.to_csv("logistics_shipments_cleaned.csv",index=False)

# vehicle_cleaned=df3[(df3['speed_kmh']>0) & (df3['fuel_consumption']>0)]

# vehicle_cleaned.to_csv('logistics_vehicle_tracking_cleaned.csv',index=False)

stock1_cleaned_data=df4[df4['stock_level'].notna()]

stock1_cleaned_data.to_csv("logistics_inventory_cleaned.csv",index=False)