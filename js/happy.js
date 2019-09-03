'use strict';

let count2 = 0;

let happyArray = ['Harim + Betsy = H', 
                    'Harim + Betsy = Ha', 
                    'Harim + Betsy = Hap', 
                    'Harim + Betsy = Happ', 
                    'Harim + Betsy = Happi', 
                    'Harim + Betsy = Happin', 
                    'Harim + Betsy = Happine', 
                    'Harim + Betsy = Happines', 
                    'Harim + Betsy = Happiness', 
                    'Harim + Betsy = Happiness ', 
                    'Harim + Betsy = Happiness F', 
                    'Harim + Betsy = Happiness Fo', 
                    'Harim + Betsy = Happiness For', 
                    'Harim + Betsy = Happiness Fore', 
                    'Harim + Betsy = Happiness Forev', 
                    'Harim + Betsy = Happiness Foreve', 
                    'Harim + Betsy = Happiness Forever', 
                    'Harim + Betsy = Happiness Forever!', 
                    'Harim + Betsy = Happiness Forever! :D',
                    'Harim + Betsy = Happiness Forever! :D',
                    'Harim + Betsy = Happiness Forever! :D',
                    'Harim + Betsy = Happiness Forever! :D',
                    'Harim + Betsy = Happiness Forever! :D'];

setInterval(function() {
    count2 = count2 + 1;

    if(count2 == happyArray.length){
        count2 = 0;
    }
    
    happy.text(happyArray[count2]);

}, 200);