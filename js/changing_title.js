'use strict';

let count = 0;
let wordsArray = ["I Love You, Betsy ", "Te Amo, Mi Corazonsito ", "Te Quiero Muchisimo ", "To My Future Wife ", "You're The Love Of My Life ", "My One And Only "];

let title = $(".title");

setInterval(function() {
    count = count + 1;

    if(count == wordsArray.length){
        count = 0;
    }
    
    title.text(wordsArray[count]);

}, 2000);