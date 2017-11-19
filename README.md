# reserve_price
Code and data used in the paper "How reserve price affects auction result. Case of Ukrainian public procurement market"

Short version: http://cep.kse.org.ua/article/Chomu-zavyshchena-ochikuvana%20vartist-pohirshuye-rezultat-auktsionu-i-yak-z-tsym-borotysya/

Full version of the article: link

Using the analysis of more than two thousands public procurements of natural gas, paper A4 and eggs conducted by the Ukrainian public institutions and companies in 2016 and 2017, I showed that increase in the reserve price on average worthens the result of the zero round of the ProZorro auction but does not affect number of the auction participants or competition during the dynamic phase of the auction.

Files in the repository:

  Short description of the variables included in the three datasets:
  
    list_of_variables.txt - 
    
  Datasets:
  
    data_eggs.csv
    
    data_gas.csv
    
    data_paper.csv

  Analysis of each of the three markets:
  
    analysis_eggs.R
    
    analysis_gas.R
    
    analysis_paper.R

  Functions used in the analysis:
  
    function_describe_variable.R
    
    function_draw_ribbon.R
    
    function_draw_scatter.R
    
    function_mode.R
