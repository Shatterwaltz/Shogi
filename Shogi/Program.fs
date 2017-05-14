
//Define types of pieces
type Piece = 
    | Empty
    | Pawn
    | Lance
    | Knight
    | Silver
    | Gold
    | Rook
    | Bishop
    | King

let pieceToString (p:Piece) = 
    match p with
    | Empty -> 
        " "
    | Pawn -> 
        "P"
    | Lance ->
        "L"
    | Knight ->
        "N"
    | Silver ->
        "S"
    | Gold ->
        "G"
    | Rook ->
        "R"
    | Bishop ->
        "B"
    | King ->
        "K"

//Create board in initial state
let createBoard = 
    let board = [|for i in 0..8->[|for j in 0..8->Empty|]|]
    for i in 0..8 do
        board.[i].[2] <- Pawn
        board.[i].[6] <- Pawn
        match i with
        | 0 | 8 -> 
            board.[i].[0] <- Lance
            board.[i].[8] <- Lance
        | 1 | 7 ->
            board.[i].[0] <- Knight
            board.[i].[8] <- Knight
            if i = 1
            then board.[i].[1] <- Rook
                 board.[i].[7] <- Bishop
            else board.[i].[1] <- Bishop
                 board.[i].[7] <- Rook
        | 2 | 6-> 
            board.[i].[0] <- Silver
            board.[i].[8] <- Silver
        | 3 | 5 ->
            board.[i].[0] <- Gold
            board.[i].[8] <- Gold
        | 4 ->
            board.[i].[0] <- King
            board.[i].[8] <- King
        | _ ->
            printfn "Board init error, iterated to impossible number"
    board    

let printBoard (board:Piece array array) = 
    let rec pbHelper (board:Piece array array) i j str = 
        if i = 8
        then if j = 8
             then str + (pieceToString board.[i].[j]+"|")
             else pbHelper board 0 (j+1) (str + pieceToString board.[i].[j] + "|\n|" )
        else pbHelper board (i+1) (j) (str + pieceToString board.[i].[j]+"|")
    pbHelper board 0 0 "|"        


[<EntryPoint>]
let main argv = 
    printfn "%s" (createBoard |> printBoard)
    0 // return an integer exit code
