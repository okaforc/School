// function printStatement() { console.log("Will the function delayed for 0 milliseconds be first") } 
// setTimeout(printStatement, 0) 
// console.log("Or, will I be executed first?")


// function sleep(ms) {
//     return new Promise(resolve => setTimeout(resolve, ms));
// }

// async function demo() {
//     for (let i = 25; i >= 0; i--) {
//         console.log(i);
//         await sleep(1000);
//     }
//     // console.log('Done');
// }

// demo();



// trying out promises 
"use strict"; 
function callmebackin(s,subject) { 
    return new Promise ( (resolve, reject) => { 
        setTimeout( () => {
            resolve("Returned call subject:"+subject)
        }, s*1000) 
    }) 
} 
// console.log("First I call you about a dog") 
// let p1 = callmebackin (5,"about a dog") 
// let p2 = callmebackin(3,"about another dog") 
// p1.then( (subject) => console.log("Got called back: "+subject)) 
// p2.then( (subject) => console.log("you called me back:"+subject))

// trying out Async Await 
async function waitaround() { 
    let topic1 = await callmebackin(5,"about a dog"); 
    console.log("Got called back:"+topic1) 
    let topic2 = await callmebackin(3,"about another dog"); 
    console.log("you called me back:"+topic2) 
} 
waitaround() 
console.log("Hoping to get some calls")