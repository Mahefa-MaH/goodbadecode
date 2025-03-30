// Good Code: Functional and concise
let goodCode (numbers: int list) =
    numbers |> List.filter (fun x -> x % 2 = 0) |> List.map (fun x -> x * 2)

// Bad Code: Imperative and less readable
let mutable badCodeResult = []
let badCode (numbers: int list) =
    for number in numbers do
        if number % 2 = 0 then
            badCodeResult <- number * 2 :: badCodeResult
    badCodeResult

