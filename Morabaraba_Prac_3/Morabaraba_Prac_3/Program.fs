// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

//defining data structures
open System
open System.IO

type Player = 
|X
|Y

type Cell =
| X
| Y
| Blank

type GameBoard =
| Board of (Cell*Cell*Cell)* (*1,4,7*)
           (Cell*Cell*Cell)* (*2,4,6*)
           (Cell*Cell*Cell)* (*3,4,5*)
           (Cell*Cell*Cell*Cell*Cell*Cell)* (*1,2,3*)(*5,6,7*)
           (Cell*Cell*Cell)*
           (Cell*Cell*Cell)*
           (Cell*Cell*Cell)

type Result = 
|Ongoing of GameBoard
|Mill of Cell * GameBoard
|BreakMill 
|Draw

type totalCows = { X1 :int; Y1: int }
type Cows = { X1 : int; Y1 : int}

type cowOption= 
|Some of Cows
|None

type GameState =
|PlacingPhase 
|MovementPhase of GameBoard*List<string>*List<string> 
|Drawn
|Won of Player

//defining functions

let blankBoard =
    let blankRow = Blank, Blank, Blank
    let blankRow1 = Blank, Blank, Blank, Blank, Blank, Blank
    Board (blankRow, blankRow, blankRow,blankRow1, blankRow, blankRow,blankRow)

let swapPlayer x =
    match x with 
    |X -> Y
    |Y -> X
    |Blank -> failwith "YOU SHOULD NOT BE HERE!!!"
    
let drawBoardGame (Board(rowA,rowB,rowC,rowD,rowE,rowF,rowG))=
    let reMapCell cell= 
        match cell with 
        |X -> 'x'
        |Y -> 'o'
        |Blank -> ' '
    (*System.Console.BackgroundColor *)
    printfn "                      1  2   3        4        5   6  7"
    let (x,y,z) = rowA
    printfn "                    a %c---------------%c---------------%c " (reMapCell x) (reMapCell y) (reMapCell z)
    let (x,y,z) = rowB
    printfn "                      |\              |              /|    "
    printfn "                    b |  %c------------%c------------%c  |" (reMapCell x) (reMapCell y) (reMapCell z)
    printfn "                      |  |\           |           /|  |  "
    let (x,y,z) = rowC
    printfn "                    c |  |   %c--------%c--------%c   |  |" (reMapCell x) (reMapCell y) (reMapCell z)
  //  printfn "                      |  |   |        |          |    |  |"
    printfn "                      |  |   |                 |   |  |  "
    let (a,b,c,d,e,f) = rowD
    printfn "                    d %c--%c---%c                 %c---%c--%c" (reMapCell a) (reMapCell b) (reMapCell c) (reMapCell d) (reMapCell e) (reMapCell f)
    printfn "                      |  |   |                 |   |  |  "
    let (x,y,z) = rowE
    printfn "                    e |  |   %c--------%c--------%c   |  |" (reMapCell x) (reMapCell y) (reMapCell z)
    printfn "                      |  | /          |          \ |  |   "
    let (x,y,z) = rowF
    printfn "                    f |  %c------------%c------------%c  |" (reMapCell x) (reMapCell y) (reMapCell z)
    printfn "                      | /             |             \ |    "
    let (x,y,z) = rowG
    printfn "                    g %c---------------%c---------------%c" (reMapCell x) (reMapCell y) (reMapCell z)

let isBlank board position = 
    match position, board with 
    |('a', '1'), Board ((Blank,_,_),_,_,_,_,_,_)
    |('a','4'), Board ((_,Blank,_),_,_,_,_,_,_)
    |('a','7'), Board ((_,_,Blank),_,_,_,_,_,_)
    |('b', '2'), Board (_,(Blank,_,_),_,_,_,_,_)
    |('b','4'), Board (_,(_,Blank,_),_,_,_,_,_)
    |('b','6'), Board (_,(_,_,Blank),_,_,_,_,_)
    |('c','3'), Board (_,_,(Blank,_,_),_,_,_,_)
    |('c','4'), Board (_,_,(_,Blank,_),_,_,_,_)
    |('c','5'), Board (_,_,(_,_,Blank),_,_,_,_)
    |('d','1'), Board (_,_,_,(Blank,_,_,_,_,_),_,_,_)
    |('d','2'), Board (_,_,_,(_,Blank,_,_,_,_),_,_,_)
    |('d','3'), Board (_,_,_,(_,_,Blank,_,_,_),_,_,_)
    |('d','5'), Board (_,_,_,(_,_,_,Blank,_,_),_,_,_)
    |('d','6'), Board (_,_,_,(_,_,_,_,Blank,_),_,_,_)
    |('d','7'), Board (_,_,_,(_,_,_,_,_,Blank),_,_,_)
    |('e','3'), Board (_,_,_,_,(Blank,_,_),_,_)
    |('e','4'), Board (_,_,_,_,(_,Blank,_),_,_)
    |('e','5'), Board (_,_,_,_,(_,_,Blank),_,_)
    |('f','2'), Board (_,_,_,_,_,(Blank,_,_),_)
    |('f','4'), Board (_,_,_,_,_,(_,Blank,_),_)
    |('f','6'), Board (_,_,_,_,_,(_,_,Blank),_)
    |('g','1'), Board (_,_,_,_,_,_,(Blank,_,_))
    |('g','4'), Board (_,_,_,_,_,_,(_,Blank,_))
    |('g','7'), Board (_,_,_,_,_,_,(_,_,Blank)) -> true   
    | _ -> false   

let changeMillState (list: int list) indx =
    match list.[indx] with 
    |0 -> 1
    |_ -> 1 //this is when the state of the board is already in Mill for that particular position


let currentStatus (list:int list) indx =
    match list.[indx] with
    | 0 -> true  //if the given index is O then it means this is a new Mill that is being formed
    | _ -> false //if the given index is 1 then it means  then it means that the given position is an old Mill
let updateList (list:int list) index  =      
    let rec someF list counter out =
        match list with
        |[] -> List.rev out
        |head::tail -> match counter=index with
                       | true -> match list.Head with 
                                    |0 -> someF tail (counter + 1) (1::out) 
                                    |_ -> someF tail (counter + 1) (head::out)
                       | _ -> someF tail (counter + 1) (head::out)
            
    someF list 0  []
let checkY_mill board list =   
    let (Board (r1,r2,r3,r4,r5,r6,r7)) = board
    let a1 = match r1,r2,r3,r4,r5,r6,r7  with 
             |(Y,Y,Y),_,_,_,_,_,_ -> currentStatus list 0 
             | _ -> false

    let a2 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,(Y,Y,Y),_,_,_,_,_ -> currentStatus list 1            
             | _ -> false
    let a3 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,(Y,Y,Y),_,_,_,_ -> currentStatus list  2            
             | _ -> false
    let a4 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,(Y,Y,Y,_,_,_),_,_,_ -> currentStatus list  3            
             | _ -> false
    let a5 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,(_,_,_,Y,Y,Y),_,_,_ -> currentStatus list  4             
             | _ -> false
    let a6 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,(Y,Y,Y),_,_ -> currentStatus list  5             
             | _ -> false
    let a7 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,_,(Y,Y,Y),_ -> currentStatus list  6            
             | _ -> false
    let a8 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,_,_,(Y,Y,Y) -> currentStatus list  7
             | _ -> false
    
    match (a1 || a2 || a3 || a4 || a5 || a6|| a7 || a8) with
    | true -> true
    | _ -> false  
    
let checkY2_mill board list =  
    let (Board (r1,r2,r3,r4,r5,r6,r7)) = board
    let a1 = match r1,r2,r3,r4,r5,r6,r7  with 
             |(Y,_,_),_,_,(Y,_,_,_,_,_),_,_,(Y,_,_) -> currentStatus list 8 
             | _ -> false

    let a2 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,(Y,_,_),_,(_,Y,_,_,_,_),_,(Y,_,_),_ -> currentStatus list 9            
             | _ -> false
    let a3 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,(Y,_,_),(_,_,Y,_,_,_),(Y,_,_),_,_ -> currentStatus list  10            
             | _ -> false
    let a4 = match r1,r2,r3,r4,r5,r6,r7  with
             |(_,Y,_),(_,Y,_),(_,Y,_),_,_,_,_ -> currentStatus list  11            
             | _ -> false
    let a5 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,(_,Y,_),(_,Y,_),(_,Y,_) -> currentStatus list 12             
             | _ -> false   
    
    match ( a1 || a2 || a3 || a4 || a5 ) with
    | true -> true
    | _ -> false  

let checkY3_mill board list =
    let (Board (r1,r2,r3,r4,r5,r6,r7)) = board
    let a1 = match r1,r2,r3,r4,r5,r6,r7  with 
             |_,_,(_,_,Y),(_,_,_,Y,_,_),(_,_,Y),_,_ -> currentStatus list 13 
             | _ -> false

    let a2 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,(_,_,Y),_,(_,_,_,_,Y,_),_,(_,_,Y),_ -> currentStatus list 14            
             | _ -> false
    let a3 = match r1,r2,r3,r4,r5,r6,r7  with
             |(_,_,Y),_,_,(_,_,_,_,_,Y),_,_,(_,_,Y) -> currentStatus list  15            
             | _ -> false
    let a4 = match r1,r2,r3,r4,r5,r6,r7  with
             |(Y,_,_),(Y,_,_),(Y,_,_),_,_,_,_ -> currentStatus list  16            
             | _ -> false
    let a5 = match r1,r2,r3,r4,r5,r6,r7  with
             |(_,_,Y),(_,_,Y),(_,_,Y),_,_,_,_ -> currentStatus list  17             
             | _ -> false
    let a6 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,(Y,_,_),(Y,_,_),(Y,_,_) -> currentStatus list  18             
             | _ -> false
    let a7 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,(_,_,Y),(_,_,Y),(_,_,Y) -> currentStatus list  19            
             | _ -> false    
    
    match (a1 || a2 || a3 || a4 || a5 || a6|| a7 ) with
    | true -> true
    | _ -> false  
    
let updateY_mill_list board (list:int list) =
    let (Board (r1,r2,r3,r4,r5,r6,r7)) = board
    let a1 = match r1,r2,r3,r4,r5,r6,r7  with 
             |(Y,Y,Y),_,_,_,_,_,_ -> updateList list 0
             | _ -> list

    let a2 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,(Y,Y,Y),_,_,_,_,_ -> updateList a1 1
             | _ -> a1
             
    let a3 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,(Y,Y,Y),_,_,_,_ -> updateList a2  2
             | _ -> a2

    let a4 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,(Y,Y,Y,_,_,_),_,_,_ -> updateList a3  3
             | _ -> a3
        
    let a5 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,(_,_,_,Y,Y,Y),_,_,_ -> updateList a4  4
             | _ -> a4

    let a6 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,(Y,Y,Y),_,_ -> updateList a5  5
             | _ -> a5

    let a7 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,_,(Y,Y,Y),_ -> updateList a6  6
             | _ -> a6

    let a8 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,_,_,(Y,Y,Y) -> updateList a7  7
             | _ -> a7
             
    let a9 = match r1,r2,r3,r4,r5,r6,r7  with 
             |(Y,_,_),_,_,(Y,_,_,_,_,_),_,_,(Y,_,_) -> updateList a8 8
             | _ -> a8                             
                                                      
    let a10 = match r1,r2,r3,r4,r5,r6,r7  with        
              |_,(Y,_,_),_,(_,Y,_,_,_,_),_,(Y,_,_),_ -> updateList a9 9           
              | _ -> a9  
              
    let a11 = match r1,r2,r3,r4,r5,r6,r7  with        
              |_,_,(Y,_,_),(_,_,Y,_,_,_),(Y,_,_),_,_ -> updateList a10 10            
              | _ -> a10

    let a12 = match r1,r2,r3,r4,r5,r6,r7  with
              |(_,Y,_),(_,Y,_),(_,Y,_),_,_,_,_ -> updateList a11 11           
              | _ -> a11

    let a13 = match r1,r2,r3,r4,r5,r6,r7  with
              |_,_,_,_,(_,Y,_),(_,Y,_),(_,Y,_) -> updateList a12 12           
              | _ -> a12  
              
    let a14 = match r1,r2,r3,r4,r5,r6,r7  with 
              |_,_,(_,_,Y),(_,_,_,Y,_,_),(_,_,Y),_,_ -> updateList a12 13 
              | _ -> a13

    let a15 = match r1,r2,r3,r4,r5,r6,r7  with
              |_,(_,_,Y),_,(_,_,_,_,Y,_),_,(_,_,Y),_ -> updateList a12 14            
              | _ -> a14
    let a16 = match r1,r2,r3,r4,r5,r6,r7  with
              |(_,_,Y),_,_,(_,_,_,_,_,Y),_,_,(_,_,Y) -> updateList a12 15            
              | _ -> a15
    let a17 = match r1,r2,r3,r4,r5,r6,r7  with
              |(Y,_,_),(Y,_,_),(Y,_,_),_,_,_,_ -> updateList a12 16           
              | _ -> a16
    let a18 = match r1,r2,r3,r4,r5,r6,r7  with
              |(_,_,Y),(_,_,Y),(_,_,Y),_,_,_,_ -> updateList a12 17             
              | _ -> a17
    let a19 = match r1,r2,r3,r4,r5,r6,r7  with
              |_,_,_,_,(Y,_,_),(Y,_,_),(Y,_,_) -> updateList a12 18             
              | _ -> a18
    let a20 = match r1,r2,r3,r4,r5,r6,r7  with
              |_,_,_,_,(_,_,Y),(_,_,Y),(_,_,Y) -> updateList a12 19            
              | _ -> a19 
    updateList a20 20




let checkX_mill board (list:int list)=
    let (Board (r1,r2,r3,r4,r5,r6,r7)) = board
    let a1 = match r1,r2,r3,r4,r5,r6,r7  with 
             |(X,X,X),_,_,_,_,_,_ -> currentStatus list 0 
             | _ -> false

    let a2 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,(X,X,X),_,_,_,_,_ -> currentStatus list 1            
             | _ -> false
    let a3 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,(X,X,X),_,_,_,_ -> currentStatus list  2            
             | _ -> false
    let a4 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,(X,X,X,_,_,_),_,_,_ -> currentStatus list  3            
             | _ -> false
    let a5 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,(_,_,_,X,X,X),_,_,_ -> currentStatus list  4             
             | _ -> false
    let a6 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,(X,X,X),_,_ -> currentStatus list  5             
             | _ -> false
    let a7 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,_,(X,X,X),_ -> currentStatus list  6            
             | _ -> false
    let a8 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,_,_,(X,X,X) -> currentStatus list  7
             | _ -> false
    
    match (a1 || a2 || a3 || a4 || a5 || a6|| a7 || a8) with
    | true -> true
    | _ -> false     

let checkX1_mill board list =   
    let (Board (r1,r2,r3,r4,r5,r6,r7)) = board
    let a1 = match r1,r2,r3,r4,r5,r6,r7  with 
             |(X,_,_),_,_,(X,_,_,_,_,_),_,_,(X,_,_) -> currentStatus list 8 
             | _ -> false

    let a2 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,(X,_,_),_,(_,X,_,_,_,_),_,(X,_,_),_ -> currentStatus list 9            
             | _ -> false
    let a3 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,(X,_,_),(_,_,X,_,_,_),(X,_,_),_,_ -> currentStatus list  10            
             | _ -> false
    let a4 = match r1,r2,r3,r4,r5,r6,r7  with
             |(_,X,_),(_,X,_),(_,X,_),_,_,_,_ -> currentStatus list  11            
             | _ -> false
    let a5 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,(_,X,_),(_,X,_),(_,X,_) -> currentStatus list  12             
             | _ -> false   
    
    match ( a1 || a2 || a3 || a4 || a5 ) with
    | true -> true
    | _ -> false        

let checkX3_mill board list =   
    let (Board (r1,r2,r3,r4,r5,r6,r7)) = board
    let a1 = match r1,r2,r3,r4,r5,r6,r7  with 
             |_,_,(_,_,X),(_,_,_,X,_,_),(_,_,X),_,_ -> currentStatus list 13 
             | _ -> false

    let a2 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,(_,_,X),_,(_,_,_,_,X,_),_,(_,_,X),_ -> currentStatus list 14            
             | _ -> false
    let a3 = match r1,r2,r3,r4,r5,r6,r7  with
             |(_,_,X),_,_,(_,_,_,_,_,X),_,_,(_,_,X) -> currentStatus list  15            
             | _ -> false
    let a4 = match r1,r2,r3,r4,r5,r6,r7  with
             |(X,_,_),(X,_,_),(X,_,_),_,_,_,_ -> currentStatus list  16            
             | _ -> false
    let a5 = match r1,r2,r3,r4,r5,r6,r7  with
             |(_,_,X),(_,_,X),(_,_,X),_,_,_,_ -> currentStatus list  17             
             | _ -> false
    let a6 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,(X,_,_),(X,_,_),(X,_,_) -> currentStatus list  18             
             | _ -> false
    let a7 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,(_,_,X),(_,_,X),(_,_,X) -> currentStatus list  19            
             | _ -> false    
    
    match (a1 || a2 || a3 || a4 || a5 || a6|| a7 ) with
    | true -> true
    | _ -> false   
    


let updateX_mill_list board (list:int list) =
    let (Board (r1,r2,r3,r4,r5,r6,r7)) = board
    let a1 = match r1,r2,r3,r4,r5,r6,r7  with 
             |(X,X,X),_,_,_,_,_,_ -> updateList list 0
             | _ -> list

    let a2 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,(X,X,X),_,_,_,_,_ -> updateList a1 1
             | _ -> a1
             
    let a3 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,(X,X,X),_,_,_,_ -> updateList a2  2
             | _ -> a2

    let a4 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,(X,X,X,_,_,_),_,_,_ -> updateList a3  3
             | _ -> a3
        
    let a5 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,(_,_,_,X,X,X),_,_,_ -> updateList a4  4
             | _ -> a4

    let a6 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,(X,X,X),_,_ -> updateList a5  5
             | _ -> a5

    let a7 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,_,(X,X,X),_ -> updateList a6  6
             | _ -> a6

    let a8 = match r1,r2,r3,r4,r5,r6,r7  with
             |_,_,_,_,_,_,(X,X,X) -> updateList a7  7
             | _ -> a7
             
    let a9 = match r1,r2,r3,r4,r5,r6,r7  with 
             |(X,_,_),_,_,(X,_,_,_,_,_),_,_,(X,_,_) -> updateList a8 8
             | _ -> a8                             
                                                      
    let a10 = match r1,r2,r3,r4,r5,r6,r7  with        
              |_,(X,_,_),_,(_,X,_,_,_,_),_,(X,_,_),_ -> updateList a9 9           
              | _ -> a9  
              
    let a11 = match r1,r2,r3,r4,r5,r6,r7  with        
              |_,_,(X,_,_),(_,_,X,_,_,_),(X,_,_),_,_ -> updateList a10 10            
              | _ -> a10

    let a12 = match r1,r2,r3,r4,r5,r6,r7  with
              |(_,X,_),(_,X,_),(_,X,_),_,_,_,_ -> updateList a11 11           
              | _ -> a11

    let a13 = match r1,r2,r3,r4,r5,r6,r7  with
              |_,_,_,_,(_,X,_),(_,X,_),(_,X,_) -> updateList a12 12           
              | _ -> a12  
              
    let a14 = match r1,r2,r3,r4,r5,r6,r7  with 
              |_,_,(_,_,X),(_,_,_,X,_,_),(_,_,X),_,_ -> updateList a12 13 
              | _ -> a13

    let a15 = match r1,r2,r3,r4,r5,r6,r7  with
              |_,(_,_,X),_,(_,_,_,_,X,_),_,(_,_,X),_ -> updateList a12 14            
              | _ -> a14
    let a16 = match r1,r2,r3,r4,r5,r6,r7  with
              |(_,_,X),_,_,(_,_,_,_,_,X),_,_,(_,_,X) -> updateList a12 15            
              | _ -> a15
    let a17 = match r1,r2,r3,r4,r5,r6,r7  with
              |(X,_,_),(X,_,_),(X,_,_),_,_,_,_ -> updateList a12 16           
              | _ -> a16
    let a18 = match r1,r2,r3,r4,r5,r6,r7  with
              |(_,_,X),(_,_,X),(_,_,X),_,_,_,_ -> updateList a12 17             
              | _ -> a17
    let a19 = match r1,r2,r3,r4,r5,r6,r7  with
              |_,_,_,_,(X,_,_),(X,_,_),(X,_,_) -> updateList a12 18             
              | _ -> a18
    let a20 = match r1,r2,r3,r4,r5,r6,r7  with
              |_,_,_,_,(_,_,X),(_,_,X),(_,_,X) -> updateList a12 19            
              | _ -> a19 
    updateList a20 20

let (millList: GameBoard list) = []  

let gameCheck board list list1=    
    //match board with
    match checkX_mill board list || checkX1_mill board list || checkX3_mill board list with
    |true-> Mill (X,board)         
    |false->
        match (checkY_mill board list1)||(checkY2_mill board list1)||(checkY3_mill board list1) with 
        |true -> Mill (Y,board)                 
        |_->          
            let f = isBlank board
            match (f ('a','1') ||f ('a','4') ||f ('a','7') ||f ('b','2') ||f ('b','4') ||f ('b','6') ||f ('c','3') ||f ('c','4') ||f ('c','5') ||f ('d','1') ||f ('d','2') ||f ('d','3') ||f ('d','5') ||
                                f (   'd','6') ||f ('d','7') ||f ('e','3') ||f ('e','4') ||f ('e','5') ||f ('f','2') ||f ('f','4') ||f ('f','6') ||f ('g','1') ||f ('g','4') ||f ('g','7')) with
                    |true -> Ongoing board
                    |_ -> Draw
        
let removeCow (* (player: Cell)*) (Board(r1,r2,r3,r4,r5,r6,r7)) position= 
    let newBoard =
        let changeColumn col (a,b,c)=
            match col with 
            |1 -> Blank,b,c
            |2 -> a,Blank,c
            |3 -> a,b,Blank
            |_ -> failwith "Kallas!!!"
        let changeColumnX col (a,b,c,d,e,f)=
            match col with 
            |1 -> Blank,b,c,d,e,f
            |2 -> a,Blank,c,d,e,f
            |3 -> a,b,Blank,d,e,f
            |4 -> a,b,c,Blank,e,f
            |5 -> a,b,c,d,Blank,f
            |6 -> a,b,c,d,e,Blank
            |_ -> failwith "Kallas!!!"
        let data =
            match position with   
            |('a' , '1')->changeColumn 1 r1,r2,r3,r4,r5,r6,r7 
            |('a' , '4')->changeColumn 2 r1,r2,r3,r4,r5,r6,r7 
            |('a' , '7')->changeColumn 3 r1,r2,r3,r4,r5,r6,r7
            |('b', '2') -> r1,changeColumn 1 r2,r3,r4,r5,r6,r7               
            |('b', '4') -> r1,changeColumn 2 r2,r3,r4,r5,r6,r7                   
            |('b', '6') -> r1,changeColumn 3 r2,r3,r4,r5,r6,r7                                   
            |('c', '3') -> r1,r2,changeColumn 1 r3,r4,r5,r6,r7                   
            |('c','4') ->  r1,r2,changeColumn 2 r3,r4,r5,r6,r7                  
            |('c','5') ->  r1,r2,changeColumn 3 r3,r4,r5,r6,r7 
            |('d','1') -> r1,r2,r3,changeColumnX 1 r4,r5,r6,r7                 
            |('d','2') -> r1,r2,r3,changeColumnX 2 r4,r5,r6,r7              
            |('d','3') -> r1,r2,r3,changeColumnX 3 r4,r5,r6,r7             
            |('d','5') -> r1,r2,r3,changeColumnX 4 r4,r5,r6,r7                    
            |('d','6') -> r1,r2,r3,changeColumnX 5 r4,r5,r6,r7                   
            |('d','7') -> r1,r2,r3,changeColumnX 6 r4,r5,r6,r7
            |('e','3' )-> r1,r2,r3,r4,changeColumn 1 r5,r6,r7                   
            |('e','4') -> r1,r2,r3,r4,changeColumn 2 r5,r6,r7                  
            |('e','5') -> r1,r2,r3,r4,changeColumn 3 r5,r6,r7                   
            |('f','2') -> r1,r2,r3,r4,r5,changeColumn 1 r6,r7                  
            |('f','4') -> r1,r2,r3,r4,r5,changeColumn 2 r6,r7                    
            |('f','6') -> r1,r2,r3,r4,r5,changeColumn 3 r6,r7                    
            |('g','1') -> r1,r2,r3,r4,r5,r6,changeColumn 1 r7                  
            |('g','4') -> r1,r2,r3,r4,r5,r6,changeColumn 2 r7                   
            |('g','7') -> r1,r2,r3,r4,r5,r6,changeColumn 3 r7                  
        Board data     
    newBoard

    
    //Still to add conditions of when player wins.

let makeMove (player: Cell) (Board(r1,r2,r3,r4,r5,r6,r7)) position list list1= 
    let newBoard =
        let changeColumn col (a,b,c)=
            match col with 
            |1 -> player,b,c
            |2 -> a,player,c
            |3 -> a,b,player
            |_ -> failwith "Kallas!!!"
        let changeColumnX col (a,b,c,d,e,f)=
            match col with 
            |1 -> player,b,c,d,e,f
            |2 -> a,player,c,d,e,f
            |3 -> a,b,player,d,e,f
            |4 -> a,b,c,player,e,f
            |5 -> a,b,c,d,player,f
            |6 -> a,b,c,d,e,player
            |_ -> failwith "Kallas!!!"
        let data =
            match position with   
            |('a' , '1')->changeColumn 1 r1,r2,r3,r4,r5,r6,r7 
            |('a' , '4')->changeColumn 2 r1,r2,r3,r4,r5,r6,r7 
            |('a' , '7')->changeColumn 3 r1,r2,r3,r4,r5,r6,r7
            |('b', '2') -> r1,changeColumn 1 r2,r3,r4,r5,r6,r7               
            |('b', '4') -> r1,changeColumn 2 r2,r3,r4,r5,r6,r7                   
            |('b', '6') -> r1,changeColumn 3 r2,r3,r4,r5,r6,r7                                   
            |('c', '3') -> r1,r2,changeColumn 1 r3,r4,r5,r6,r7                   
            |('c','4') ->  r1,r2,changeColumn 2 r3,r4,r5,r6,r7                  
            |('c','5') ->  r1,r2,changeColumn 3 r3,r4,r5,r6,r7 
            |('d','1') -> r1,r2,r3,changeColumnX 1 r4,r5,r6,r7                 
            |('d','2') -> r1,r2,r3,changeColumnX 2 r4,r5,r6,r7              
            |('d','3') -> r1,r2,r3,changeColumnX 3 r4,r5,r6,r7             
            |('d','5') -> r1,r2,r3,changeColumnX 4 r4,r5,r6,r7                    
            |('d','6') -> r1,r2,r3,changeColumnX 5 r4,r5,r6,r7                   
            |('d','7') -> r1,r2,r3,changeColumnX 6 r4,r5,r6,r7
            |('e','3' )-> r1,r2,r3,r4,changeColumn 1 r5,r6,r7                   
            |('e','4') -> r1,r2,r3,r4,changeColumn 2 r5,r6,r7                  
            |('e','5') -> r1,r2,r3,r4,changeColumn 3 r5,r6,r7                   
            |('f','2') -> r1,r2,r3,r4,r5,changeColumn 1 r6,r7                  
            |('f','4') -> r1,r2,r3,r4,r5,changeColumn 2 r6,r7                    
            |('f','6') -> r1,r2,r3,r4,r5,changeColumn 3 r6,r7                    
            |('g','1') -> r1,r2,r3,r4,r5,r6,changeColumn 1 r7                  
            |('g','4') -> r1,r2,r3,r4,r5,r6,changeColumn 2 r7                   
            |('g','7') -> r1,r2,r3,r4,r5,r6,changeColumn 3 r7                  
        Board data 
    gameCheck newBoard list list1
    
let numCows = { X1=12; Y1=12} //Maximum number of cows for each player to place
let xMovesList = []
let yMovesList = []


let checkCowAvailable Player numCows = 
    match Player with 
    |X -> 
        let ans = {numCows with X1 = numCows.X1 - 1}
        match ans with
        |{X1= -1} -> None 
        |_ -> Some ans
    |Y ->
        let ans = {numCows with Y1 = numCows.Y1 - 1}   
        match ans with 
        |{Y1= -1} -> None             
        |_-> Some ans

let isValidInput (input:string) =
    match input.[0] with 
    |'a'|'b'|'c'|'d'|'e'|'f'|'g' -> 
        match  input.[1] with 
        | '1' | '2' |'3'| '4' |'5' | '6'| '7'-> true
    |_-> false

//let millFound Mill 
let shootPlayer board choice =
    //let removeThis = System.Console.ReadLine()
    //let choice = removeThis.[0], removeThis.[1] 
    //runPlacementPhase (swapPlayer Player) (removeCow Player board choice) Cows ""
    removeCow board choice

let addMove list move = 
    match List.exists (fun x -> x = move) list with
    |true -> list
    |false -> List.rev (move:: list)

let MillFound (millList:'a List) board player = 
    printfn "Mill Found! Choose which cow to shoot"
    let shootCoordinates = System.Console.ReadLine()
    let choice = shootCoordinates.[0], shootCoordinates.[1]
    shootPlayer board choice
    
    System.Console.Clear()
    drawBoardGame board
    
let rec runPlacementPhase (Player : Cell) board numCows eraTxt  xMovesList yMovesList list list1=
    System.Console.Clear()
    drawBoardGame board
    printfn ""
    printfn ""
    printfn ""
    printfn ""
    printfn "%s" eraTxt
    printfn "%A's turn.  Type the cell reference of the cell that you want to place cow into." Player
    let n = (System.Console.ReadLine ())
    //Making a (char*char) tuple of the cell referece given by player.
    let playerChoice = n.[0], n.[1]
    match isValidInput n with
    |true->
            match isBlank board playerChoice with 
            |true -> 
                    match Player with
                    |X -> let new_Xmoves = addMove xMovesList n
                          match checkCowAvailable Player numCows with 
                          |Some Cows -> 
                              match (makeMove Player board playerChoice list list1) with 
                              |Ongoing board -> runPlacementPhase (swapPlayer Player) board Cows "" new_Xmoves yMovesList list list1
                              |Mill (currentPlayer,board)-> 
                                  let newboardX = updateList list
                                  let newboardY = updateList list1
                                  System.Console.Clear()
                                  drawBoardGame board
                                  printfn "Mill Found! Choose which cow to shoot"
                                  let removeThis = System.Console.ReadLine()
                                  match isValidInput removeThis with
                                  |true -> 
                                      let choice = removeThis.[0], removeThis.[1]                                             
                                      runPlacementPhase (swapPlayer Player) (removeCow (*Player*) board choice) Cows "" xMovesList yMovesList list list1
                              |Draw -> Drawn
                                  //printf "Draw!!!"   
                          |None -> 
                                  MovementPhase (board,xMovesList, yMovesList)
                    |Y -> let new_YMoves = addMove yMovesList n
                          match checkCowAvailable Player numCows with 
                          |Some Cows -> 
                              match (makeMove Player board playerChoice list list1) with 
                              |Ongoing board -> runPlacementPhase (swapPlayer Player) board Cows ""  xMovesList new_YMoves list list1
                              |Mill (currentPlayer,board)-> 
                                  
                                  System.Console.Clear()
                                  drawBoardGame board
                                  printfn "Mill Found! Choose which cow to shoot"
                                  let removeThis = System.Console.ReadLine()
                                  match isValidInput removeThis with
                                  |true -> 
                                      let choice = removeThis.[0], removeThis.[1]                                             
                                      runPlacementPhase (swapPlayer Player) (removeCow (*Player*) board choice) Cows "" xMovesList new_YMoves list list1
                              |Draw -> Drawn
                                  //printf "Draw!!!"   
                          |None ->                                     
                                   MovementPhase (board,xMovesList, yMovesList)                                                      
                    
                        //runPlacementPhase Player board numCows "You have exhausted the number of cows you can place; Make your move on the board"
                    //MovingPhase (board)
            |_ -> 
                  runPlacementPhase Player board numCows "*******Position Occupied! Make Another Move*********" xMovesList yMovesList list list1
        |_-> runPlacementPhase Player board numCows "Enter Valid Column Number"  xMovesList yMovesList list list1

    |_-> runPlacementPhase Player board numCows "Enter Valid Row Letter"  xMovesList yMovesList list list1
   


let isYourPiece  xMovesList yMovesList currentplayer  moveSelected =
    match currentplayer with 
    |X->
        match List.exists (fun x -> x = moveSelected) xMovesList with
        |true -> true
        |_-> false
    |Y-> match List.exists (fun x -> x = moveSelected) yMovesList with
         |true -> true
         |_-> false
         
let isAdjustWith fromPlace toPlace board = 
    match fromPlace with 
    |"a1" -> match toPlace with
             |"a4" |"d1"| "b2"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                 |true->true
                                 |_-> false
             |_-> false
    |"a4" ->match toPlace with
             |"a1" |"a7"|"b4"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                |true->true
                                |_-> false
             |_-> false         
    |"a7" ->match toPlace with
             |"a4" |"d7"| "b6"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                 |true->true
                                 |_-> false
             |_-> false          
    |"b2" ->match toPlace with
             |"a1" |"d2"| "b4" |"c3"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                       |true->true
                                       |_-> false
             |_-> false
    |"b4" ->match toPlace with
             |"a4" |"b2"| "b6" |"c4"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                       |true->true
                                       |_-> false
             |_-> false
    |"b6" ->match toPlace with
             |"a7" |"b4"| "d6" |"c5"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                       |true->true
                                       |_-> false
             |_-> false
    |"c3" ->match toPlace with
             |"b2" |"b4"| "d3"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                 |true->true
                                 |_-> false
             |_-> false
    |"c4" ->match toPlace with
             |"b4" |"c5"| "c3" -> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                  |true->true
                                  |_-> false
             |_-> false
    |"c5" ->match toPlace with
            |"b6" |"d5"| "c4" -> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                 |true->true
                                 |_-> false
            |_-> false

    |"d1" ->match toPlace with
             |"a1" |"g1"| "d2" -> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                  |true->true
                                  |_-> false
             |_-> false
    |"d2" ->match toPlace with
                |"d1" |"b2"| "f2"|"d3" -> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                          |true->true
                                          |_-> false
                |_-> false
    |"d3" ->match toPlace with
               |"c3" |"e3"| "d2"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                   |true->true
                                   |_-> false
               |_-> false  
    |"d5" ->match toPlace with
               |"c5" |"e5"| "d6"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                   |true->true
                                   |_-> false
               |_-> false
               
    |"d6" ->match toPlace with
               |"b6" |"d7"| "f6"|"d5"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                        |true->true
                                        |_-> false
               |_-> false 
    |"d7" ->match toPlace with
               |"a7" |"g7"| "d6"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                   |true->true
                                   |_-> false
               |_-> false 
    |"e3" ->match toPlace with
               |"d3" |"f2"| "e4"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                   |true->true
                                   |_-> false
               |_-> false
    |"e4" ->match toPlace with
               |"e3" |"e5"| "f4"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                   |true->true
                                   |_-> false
               |_-> false
    |"e5" ->match toPlace with
               |"d5" |"f6"| "e4"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                   |true->true
                                   |_-> false
               |_-> false  
    |"f2" ->match toPlace with
               |"e3" |"g1"| "f4"|"d2"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                        |true->true
                                        |_-> false
               |_-> false 
    |"f4" ->match toPlace with
               |"e4" |"g4"| "f2"|"f6"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                        |true->true
                                        |_-> false
               |_-> false 
    |"f6" ->match toPlace with
               |"e5"|"g7"|"d6"|"f4"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                      |true->true
                                      |_-> false
               |_-> false
    |"g1" ->match toPlace with
               |"d1" |"f2"| "g4"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                   |true->true
                                   |_-> false
               |_-> false
    |"g4" ->match toPlace with
               |"f4" |"g1"| "g7"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                   |true->true
                                   |_-> false
               |_-> false 
    |"g7" ->match toPlace with
               |"g4" |"f6"| "d7"-> match isBlank board (toPlace.[0],toPlace.[1]) with 
                                   |true->true
                                   |_-> false
               |_-> false 

    |_->false
let rec run board currentplayer xMovesList yMovesList list list1=
    Console.Clear()
    drawBoardGame board
    printfn "\n\n\n\n\n"
    match currentplayer with
    |X -> printfn "Player X "
    |Y-> printf "Player Y "
    printf "Enter Coodinates:\n"
    let removeThis = System.Console.ReadLine()
    let choice = removeThis.[0], removeThis.[1]
     
    
    match isValidInput removeThis with
        |true->
            match isBlank board choice with 
            |true -> printf "Place is blank. Renter coodinates\n"
                     run board currentplayer xMovesList yMovesList list list1
                    
            |_-> match isYourPiece xMovesList yMovesList  currentplayer removeThis with
                 |true ->
                        printfn "%s to which position?\n" removeThis
                        let moveTo = System.Console.ReadLine()
                        let newPosition = moveTo.[0],moveTo.[1]
                        match (isValidInput moveTo) = (isAdjustWith removeThis moveTo board)  with
                        |true-> printfn "Moving %s to %s\n" removeThis moveTo
                                Console.Clear()
                                Console.ForegroundColor <- ConsoleColor.Green
                                                      
                                let newboard = makeMove currentplayer (removeCow board choice) newPosition list list1
                                match newboard with
                                |Mill (currentplayer,newBoard)-> 
                                                                  printfn "Mill Found! Choose which cow to shoot"
                                                                  let removeThis = System.Console.ReadLine()
                                                                  match isValidInput removeThis with
                                                                  |true -> 
                                                                      let choice = removeThis.[0], removeThis.[1]                                             
                                                                      run (removeCow (*Player*) board choice) (swapPlayer currentplayer) xMovesList yMovesList list list1
                                                                      
                                                                   
                                |Draw -> printf "Draw!!"
                                |Ongoing board -> drawBoardGame board
                                                  run board (swapPlayer currentplayer) xMovesList yMovesList list list1
                        |_-> 
                             System.Console.ReadLine()
                             run board ( currentplayer) xMovesList yMovesList list list1
                 |_-> printfn "Choice selectedd is not your move"
                         
                       
            
        |_-> printfn "Coodinates do not exist!\n"
             printfn "Row invalid, rows only exist for a..g\n Reenter Coodinates.........\n"
             run board currentplayer xMovesList yMovesList list list1

let rec runGame currentPlayer board  xMovesList yMovesList list list1 =
    
    match runPlacementPhase currentPlayer board numCows ""  xMovesList yMovesList list list1 with
    |Drawn -> 
         printf "Draw!!!"
    |MovementPhase (board, xMovesList, yMovesList)  -> 
        Console.Clear()
        Console.ForegroundColor <- ConsoleColor.DarkCyan
        drawBoardGame board
        printf "You have exhausted the number of cows you can place\n"
        printf "Enter coodinates of Cow to move \n"
        run board currentPlayer  xMovesList yMovesList list list1
    
[<EntryPoint>]
let main argv = 
    Console.ForegroundColor <- ConsoleColor.Yellow
    let xMill_List = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0]
    let yMill_List = [0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0]
    runGame X blankBoard  xMovesList yMovesList xMill_List yMill_List
    
    System.Console.ReadLine()
   
    0 // return an integer exit code
