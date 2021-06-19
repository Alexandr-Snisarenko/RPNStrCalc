//Программа вычисляет значение выражения записанного в обратной польской нотации
//Проверка синтаксиса - только на недопустимые символы
//контроля правильности построения выражения - нет

open System

//меняем запятую на точку
let rplcDot (s:string) = s.Replace(',', '.')

//проверяем на допустимые символы
let checkStr (s:string) = 
    for chr in s do 
        match chr with
        |'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'0'|'+'|'-'|'*'|'/'|'.'|','|' ' -> chr |> ignore
        |_ -> raise (System.ArgumentException("Unexpected symbol in expression: " + string (chr)))

//вычисляем операцию по текущему операнду 
let calcPoint  (A:string[]) i = 
    match A.[i] with
    |"+" -> string (float  A.[i-2] + float A.[i-1] )
    |"-" -> string (float  A.[i-2] - float A.[i-1] )
    |"*" -> string (float  A.[i-2] * float A.[i-1])
    |"/" -> 
            match float A.[i-1] with
            |0.0  -> raise (System.ArgumentException("Devide by zero! ")) 
            |_ -> string (float  A.[i-2] / float A.[i-1])
    |_ -> raise (System.ArgumentException("Unexpected symbol in operator"))


//формируем массив после вычисления текущего операнда
let makeNewArrey A i result = 
    (Array.sub A 0 (i-2), [|result|]) ||> Array.append |> Array.append <| Array.skip (i+1) A

//вічисляем віражение в RPN нотации
let rec CalcExpression (A: string[]) i = 
 if A.Length <= 1 then A
 else 
       match A.[i] with
       |"+"|"-"|"*"|"/" ->  calcPoint A i |> makeNewArrey A i |> CalcExpression <|0
       |_ ->  CalcExpression A (i + 1)


[<EntryPoint>]
let main argv =
  let mutable toExit = true

  while toExit  do
    try
        Console.Clear()  
        printfn "Calculate an RPN expression. Warning! Expression must by correct. " 
        printf "Input RPN expression: " 
        let rpnExpr = Console.ReadLine() |> rplcDot

        checkStr rpnExpr
    
        printfn "\nInputed RPN Expression %s" rpnExpr

        let rpnArray = rpnExpr.Split([|" "|], StringSplitOptions.None) |> Array.filter (fun s -> s <> "")
        
        //printfn "Normalized RPN Array:  %A" rpnArray
    
        //// TO DO: Syntax Check

        CalcExpression rpnArray 0 |> Array.item 0 |> printfn "\nResult : %s"

    with
        | ex -> printfn "Exception! %s " (ex.Message)

    
    printfn "\nPress Esc to Exit. Press any key to continue. " 

    toExit <- (Console.ReadKey().Key <> ConsoleKey.Escape)


  0 // return an integer exit code