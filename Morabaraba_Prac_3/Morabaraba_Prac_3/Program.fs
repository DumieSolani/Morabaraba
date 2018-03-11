// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

//defining data structures

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
        |Y -> 'y'
        |Blank -> 'o'
    (*System.Console.BackgroundColor *)
    printfn "            1  2   3        4        5   6  7"
    let (x,y,z) = rowA
    printfn "          a %c---------------%c---------------%c" (reMapCell x) (reMapCell y) (reMapCell z)
    let (x,y,z) = rowB
    printfn "          b |  %c------------%c------------%c  |" (reMapCell x) (reMapCell y) (reMapCell z)
    let (x,y,z) = rowC
    printfn "          c |  |   %c--------%c--------%c   |  |" (reMapCell x) (reMapCell y) (reMapCell z)
    let (a,b,c,d,e,f) = rowD
    printfn "          d %c--%c---%c                 %c---%c--%c" (reMapCell a) (reMapCell b) (reMapCell c) (reMapCell d) (reMapCell e) (reMapCell f)
    let (x,y,z) = rowE
    printfn "          e |  |   %c--------%c--------%c   |  |" (reMapCell x) (reMapCell y) (reMapCell z)
    let (x,y,z) = rowF
    printfn "          f |  %c------------%c------------%c  |" (reMapCell x) (reMapCell y) (reMapCell z)
    let (x,y,z) = rowG
    printfn "          g %c---------------%c---------------%c" (reMapCell x) (reMapCell y) (reMapCell z)

let isBlank board position = 
    
    match position with 
    |'a', y ->
        match y with
        |'1' -> 
            match board with 
            |Board ((Blank,_,_),_,_,_,_,_,_) -> true
            |_ -> false
        |'4' ->
            match board with 
            |Board ((_,Blank,_),_,_,_,_,_,_) -> true
            |_ -> false
        |'7' ->
            match board with 
            |Board ((_,_,Blank),_,_,_,_,_,_) -> true
            |_ -> false
        |_ -> false
    |'b', y ->
        match y with
        |'2' -> 
            match board with 
            |Board (_,(Blank,_,_),_,_,_,_,_) -> true
            |_ -> false
        |'4' ->
             match board with 
             |Board (_,(_,Blank,_),_,_,_,_,_) -> true
             |_ -> false
        |'6' ->
             match board with 
             |Board (_,(_,_,Blank),_,_,_,_,_) -> true
             |_ -> false
        |_ -> false
    |'c', y ->
        match y with
        |'3' -> 
            match board with 
            |Board (_,_,(Blank,_,_),_,_,_,_) -> true
            |_ -> false
        |'4' ->
            match board with 
            |Board (_,_,(_,Blank,_),_,_,_,_) -> true
            |_ -> false
        |'5' ->
            match board with 
            |Board (_,_,(_,_,Blank),_,_,_,_) -> true
            |_ -> false
        |_ -> false
    |'d', y ->
        match y with
        |'1' -> 
            match board with 
            |Board (_,_,_,(Blank,_,_,_,_,_),_,_,_) -> true
            |_ -> false
        |'2' ->
            match board with 
            |Board (_,_,_,(_,Blank,_,_,_,_),_,_,_) -> true
            |_ -> false
        |'3' ->
            match board with 
            |Board (_,_,_,(_,_,Blank,_,_,_),_,_,_) -> true
            |_ -> false
        |'5' -> 
            match board with 
            |Board (_,_,_,(_,_,_,Blank,_,_),_,_,_) -> true
            |_ -> false
        |'6' ->
            match board with 
            |Board (_,_,_,(_,_,_,_,Blank,_),_,_,_) -> true
            |_ -> false
        |'7' ->
            match board with 
            |Board (_,_,_,(_,_,_,_,_,Blank),_,_,_) -> true
            |_ -> false
        |_ -> false
    |'e', y ->
        match y with 
        |'3' -> 
            match board with 
            |Board (_,_,_,_,(Blank,_,_),_,_) -> true
            |_ -> false
        |'4' ->
            match board with 
            |Board (_,_,_,_,(_,Blank,_),_,_) -> true
            |_ -> false
        |'5' ->
            match board with 
            |Board (_,_,_,_,(_,_,Blank),_,_) -> true
            |_ -> false
        |_ -> false
    |'f', y ->
        match y with
        |'2' -> 
            match board with 
            |Board (_,_,_,_,_,(Blank,_,_),_) -> true
            |_ -> false
        |'4' ->
             match board with 
             |Board (_,_,_,_,_,(_,Blank,_),_) -> true
             |_ -> false
        |'6' ->
             match board with 
             |Board (_,_,_,_,_,(_,_,Blank),_) -> true
             |_ -> false
        |_ -> false      
    |'g', y ->        
        match y with
        |'1' -> 
            match board with 
            |Board (_,_,_,_,_,_,(Blank,_,_)) -> true
            |_ -> false
        |'4' ->
            match board with 
            |Board (_,_,_,_,_,_,(_,Blank,_)) -> true
            |_ -> false
        |'7' ->
            match board with 
            |Board (_,_,_,_,_,_,(_,_,Blank)) -> true
            |_ -> false
        |_ -> false  

let checkX_mill board=
    let (Board (r1,r2,r3,r4,r5,r6,r7)) = board
    match r1,r2,r3,r4,r5,r6,r7 with 
    |(X,X,X),_,_,_,_,_,_ 
    |_,(X,X,X),_,_,_,_,_
    |_,_,(X,X,X),_,_,_,_
    |_,_,_,(X,X,X,_,_,_),_,_,_
    |_,_,_,(_,_,_,X,X,X),_,_,_
    |_,_,_,_,(X,X,X),_,_
    |_,_,_,_,_,(X,X,X),_
    |_,_,_,_,_,_,(X,X,X)->true
    |_-> false

let checkX1_mill board=
    let (Board (r1,r2,r3,r4,r5,r6,r7)) = board
    match r1,r2,r3,r4,r5,r6,r7 with 
    |(X,_,_),_,_,(X,_,_,_,_,_),_,_,(X,_,_)
    |_,(X,_,_),_,(_,X,_,_,_,_),_,(X,_,_),_
    |_,_,(X,_,_),(_,_,X,_,_,_),(X,_,_),_,_
    |(_,X,_),(_,X,_),(_,X,_),_,_,_,_
    |_,_,_,_,(_,X,_),(_,X,_),(_,X,_)->true
    |_-> false
let checkX3_mill board=
    let (Board (r1,r2,r3,r4,r5,r6,r7)) = board
    match r1,r2,r3,r4,r5,r6,r7 with
    |_,_,(_,_,X),(_,_,_,X,_,_),(_,_,X),_,_
    |_,(_,_,X),_,(_,_,_,_,X,_),_,(_,_,X),_
    |(_,_,X),_,_,(_,_,_,_,_,X),_,_,(_,_,X)
    |(X,_,_),(X,_,_),(X,_,_),_,_,_,_ 
    |(_,_,X),(_,_,X),(_,_,X),_,_,_,_  
    |_,_,_,_,(X,_,_),(X,_,_),(X,_,_)   
    |_,_,_,_,(_,_,X),(_,_,X),(_,_,X) ->true
    |_-> false
let checkY_mill board =

    let (Board(r1,r2,r3,r4,r5,r6,r7)) =board
    match r1,r2,r3,r4,r5,r6,r7 with
    |(Y,Y,Y),_,_,_,_,_,_
    |_,(Y,Y,Y),_,_,_,_,_
    |_,_,(Y,Y,Y),_,_,_,_
    |_,_,_,(Y,Y,Y,_,_,_),_,_,_
    |_,_,_,(_,_,_,Y,Y,Y),_,_,_
    |_,_,_,_,(Y,Y,Y),_,_
    |_,_,_,_,_,(Y,Y,Y),_
    |_,_,_,_,_,_,(Y,Y,Y)->true
    |_-> false

let checkY2_mill board =
    let (Board(r1,r2,r3,r4,r5,r6,r7)) =board
    match r1,r2,r3,r4,r5,r6,r7 with
    |(Y,_,_),_,_,(Y,_,_,_,_,_),_,_,(Y,_,_)
    |_,(Y,_,_),_,(_,Y,_,_,_,_),_,(Y,_,_),_
    |_,_,(Y,_,_),(_,_,Y,_,_,_),(Y,_,_),_,_
    |(_,Y,_),(_,Y,_),(_,Y,_),_,_,_,_
    |_,_,_,_,(_,Y,_),(_,Y,_),(_,Y,_)
    |_-> false
let checkY3_mill board =
    let (Board(r1,r2,r3,r4,r5,r6,r7)) =board
    match r1,r2,r3,r4,r5,r6,r7 with
    |_,_,(_,_,Y),(_,_,_,Y,_,_),(_,_,Y),_,_
    |_,(_,_,Y),_,(_,_,_,_,Y,_),_,(_,_,Y),_
    |(_,_,Y),_,_,(_,_,_,_,_,Y),_,_,(_,_,Y)
    |(Y,_,_),(Y,_,_),(Y,_,_),_,_,_,_ 
    |(_,_,Y),(_,_,Y),(_,_,Y),_,_,_,_
    |_,_,_,_,(Y,_,_),(Y,_,_),(Y,_,_) 
    |_,_,_,_,(_,_,Y),(_,_,Y),(_,_,Y) -> true
    |_-> false
let gameCheck board=
    //match board with
    match checkX_mill board || checkX1_mill board || checkX3_mill board with
    |true-> Mill (X,board)
    |false->
        match (checkY_mill board)||(checkY2_mill board)||(checkY3_mill board) with 
        |true -> Mill (Y,board)
        |_-> Ongoing board


let removeCow (player: Cell) (Board(r1,r2,r3,r4,r5,r6,r7)) position= 
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
            |'a', y ->
                match y with
                |'1' -> changeColumn 1 r1,r2,r3,r4,r5,r6,r7                       
                |'4' -> changeColumn 2 r1,r2,r3,r4,r5,r6,r7                      
                |'7' -> changeColumn 3 r1,r2,r3,r4,r5,r6,r7                       
            |'b', y ->
                match y with
                |'2' -> r1,changeColumn 1 r2,r3,r4,r5,r6,r7                  
                |'4' -> r1,changeColumn 2 r2,r3,r4,r5,r6,r7                    
                |'6' -> r1,changeColumn 3 r2,r3,r4,r5,r6,r7                    
            |'c', y ->
                match y with
                |'3' ->  r1,r2,changeColumn 1 r3,r4,r5,r6,r7                   
                |'4' ->  r1,r2,changeColumn 2 r3,r4,r5,r6,r7                  
                |'5' ->  r1,r2,changeColumn 3 r3,r4,r5,r6,r7 
            |'d', y ->
                match y with
                |'1' -> r1,r2,r3,changeColumnX 1 r4,r5,r6,r7                 
                |'2' -> r1,r2,r3,changeColumnX 2 r4,r5,r6,r7              
                |'3' -> r1,r2,r3,changeColumnX 3 r4,r5,r6,r7             
                |'5' -> r1,r2,r3,changeColumnX 4 r4,r5,r6,r7                    
                |'6' -> r1,r2,r3,changeColumnX 5 r4,r5,r6,r7                   
                |'7' -> r1,r2,r3,changeColumnX 6 r4,r5,r6,r7
            |'e', y ->
                match y with 
                |'3' -> r1,r2,r3,r4,changeColumn 1 r5,r6,r7                   
                |'4' -> r1,r2,r3,r4,changeColumn 2 r5,r6,r7                  
                |'5' -> r1,r2,r3,r4,changeColumn 3 r5,r6,r7                   
            |'f', y ->
                match y with
                |'2' -> r1,r2,r3,r4,r5,changeColumn 1 r6,r7                  
                |'4' -> r1,r2,r3,r4,r5,changeColumn 2 r6,r7                    
                |'6' -> r1,r2,r3,r4,r5,changeColumn 3 r6,r7                    
            |'g', y ->        
                match y with
                |'1' -> r1,r2,r3,r4,r5,r6,changeColumn 1 r7                  
                |'4' -> r1,r2,r3,r4,r5,r6,changeColumn 2 r7                   
                |'7' -> r1,r2,r3,r4,r5,r6,changeColumn 3 r7                  
        Board data     
    newBoard

    
    //Still to add conditions of when player wins.

let makeMove (player: Cell) (Board(r1,r2,r3,r4,r5,r6,r7)) position= 
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
            |'a', y ->
                match y with
                |'1' -> changeColumn 1 r1,r2,r3,r4,r5,r6,r7                       
                |'4' -> changeColumn 2 r1,r2,r3,r4,r5,r6,r7                      
                |'7' -> changeColumn 3 r1,r2,r3,r4,r5,r6,r7                       
            |'b', y ->
                match y with
                |'2' -> r1,changeColumn 1 r2,r3,r4,r5,r6,r7                  
                |'4' -> r1,changeColumn 2 r2,r3,r4,r5,r6,r7                    
                |'6' -> r1,changeColumn 3 r2,r3,r4,r5,r6,r7                    
            |'c', y ->
                match y with
                |'3' ->  r1,r2,changeColumn 1 r3,r4,r5,r6,r7                   
                |'4' ->  r1,r2,changeColumn 2 r3,r4,r5,r6,r7                  
                |'5' ->  r1,r2,changeColumn 3 r3,r4,r5,r6,r7 
            |'d', y ->
                match y with
                |'1' -> r1,r2,r3,changeColumnX 1 r4,r5,r6,r7                 
                |'2' -> r1,r2,r3,changeColumnX 2 r4,r5,r6,r7              
                |'3' -> r1,r2,r3,changeColumnX 3 r4,r5,r6,r7             
                |'5' -> r1,r2,r3,changeColumnX 4 r4,r5,r6,r7                    
                |'6' -> r1,r2,r3,changeColumnX 5 r4,r5,r6,r7                   
                |'7' -> r1,r2,r3,changeColumnX 6 r4,r5,r6,r7
            |'e', y ->
                match y with 
                |'3' -> r1,r2,r3,r4,changeColumn 1 r5,r6,r7                   
                |'4' -> r1,r2,r3,r4,changeColumn 2 r5,r6,r7                  
                |'5' -> r1,r2,r3,r4,changeColumn 3 r5,r6,r7                   
            |'f', y ->
                match y with
                |'2' -> r1,r2,r3,r4,r5,changeColumn 1 r6,r7                  
                |'4' -> r1,r2,r3,r4,r5,changeColumn 2 r6,r7                    
                |'6' -> r1,r2,r3,r4,r5,changeColumn 3 r6,r7                    
            |'g', y ->        
                match y with
                |'1' -> r1,r2,r3,r4,r5,r6,changeColumn 1 r7                  
                |'4' -> r1,r2,r3,r4,r5,r6,changeColumn 2 r7                   
                |'7' -> r1,r2,r3,r4,r5,r6,changeColumn 3 r7                  
        Board data 
    gameCheck newBoard        

let rec run (Player : Cell) board eratxt =
    System.Console.Clear()
    drawBoardGame board
    printfn ""
    printfn ""
    printfn ""
    printfn ""
    printfn "%s" eratxt
    printfn "%A's turn.  Type the cell reference of the cell that you want to place cow into." Player
    let n = (System.Console.ReadLine ())
    //Making a (char*char) tuple of the cell referece given by player.
    let playerChoice = n.[0], n.[1]   
    //
    match isBlank board playerChoice with 
    |true -> makeMove Player board playerChoice        
    |_ -> 
         run Player board "*******Position Occupied! Make Another Move*********"


(*let elliminate player board playerChoice =
    match isBlank board playerChoice with
    |true->run player board "No Cow Here!"
    //|_-> removeCow player board playerChoice *)


let rec runGame currentPlayer board=
    match run currentPlayer board "" with 
    |Ongoing newBoard-> runGame (swapPlayer currentPlayer) newBoard
    |Mill (currentPlayer,board)-> 
                                System.Console.Clear()
                                drawBoardGame board
                                printfn "Mill Found! Choose which cow to shoot"
                                let removeThis = System.Console.ReadLine()
                                let choice = removeThis.[0], removeThis.[1] 
                                
                                runGame (swapPlayer currentPlayer) (removeCow currentPlayer board choice) 

    
[<EntryPoint>]
let main argv = 

    runGame X blankBoard
 
    System.Console.ReadLine()
   
    0 // return an integer exit code
