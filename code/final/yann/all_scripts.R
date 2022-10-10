source('code/final/yann/load_alternative_actor_names.R')

source('code/final/yann/make_directed_network.R')

# Next import files to Gephi to produce node positions, then export to JSON to the data folder.
# Next script imports the coordinates and creates layouts for ggraph. 

# Gephi settings, resize nodes by degree, 
# ForceAtlas2 with stronger gravity, gravity .1, scaling 50, prevent overlap, edge weight influence .1.

source('code/final/yann/import_gephi_positions.R')

# Draw pdf versions of network

source('code/final/yann/draw_static_networks.R')

# Make interactive Visnetwork versions

source('code/final/yann/generate_visNetwork.R')

#Create the info table

source('code/final/yann/generate_network_table.R')

source('code/final/mikko/yann_authors_article.R')

source('code/final/mikko/yann_publication_places_article.R') 

source('code/final/mikko/yann_key_scottish_works.R')

source('code/final/mikko/yann_topics_article.R')
