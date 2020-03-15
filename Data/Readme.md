## Data used for the replication study

### Contents:

The project focuses on new housing construction in Allegheny county in Pennsylvania, which contains the metropolitan area of Pittsburgh. The city of Pittsburgh used a two rate property taxation system from 1913-2001. The data comes from the Allegheny County Web site, which is maintained by the Office of Property Assessments. The Web site provides access to a database with detailed information about all properties, both residential and commercial, in the entire county. The complete database lists 561,174 properties. This is not a commercial data as it only lists the properties by a numerical property ID and does not contain any identifiable information about its owners.

### Data:
1.	Pittsburgh_post1995.txt:
This is the primary dataset used for the replication study. It contains data for 6362 residential real estate       properties within the Allegheny county, Pittsburgh built post 1995 in a comma-delimited text file format. The level of the data is ‘ObjectID’ which refers to unique real estate units built. It also captures some important metrics for the houses such as the price of land, value per unit land, plot area and its geo-location (latitude and longitude).

2.	Pitsburgh_Commercial_Properties.txt:
This file contains the commercial real estate data for Pittsburgh post 1995. It lists 992 commercial units built post 1995.



### Explaination of Data:
<pre>
<b>Pittsburgh_post1995.txt:</b>
ObjectID : Index representing unique residential real estate units built in Pittsburgh
pland    : Price of the land on which the house is built
v        : Value per unit of land
tcog     : Travel time from the plot to the designated city center traffic zone
luc      : Land Use Code
yearbuilt: The year when the property was built
muni     : municipality taxation category of the properties
lotarea  : The sq.ft area of the plot on which the property is built
X        : latitude
Y        : longitude


<b>Pitsburgh_Commercial_Properties.txt:</b>
lotarea  : The sq.ft area of the plot on which the property is built
pland    : Price of the land on which the property is built
v        : Value per unit of land
zip      : zip code of the location where the property is located within Pittsburgh
</pre>
