(* This program is an evaluator for friendship bracelet patterns. *)
module ProgramInterpreter
open ProgramParser
open SVG

(* Evaluates a Knot *)
let keval e s i =
    // remove number at end of color names for printing
    let justcolor s i =
        let x:string = List.item(i) s
        x.[..x.Length - 2]

    match e with // print corresponding symbol for each knot
    | RR _ -> (justcolor s i) + " >> "
    | LL _ -> (justcolor s (i+1)) + " << "
    | RL _ -> (justcolor s i) + " > "
    | LR _ -> (justcolor s (i+1)) + " < "
    | SKIP _ -> " _ "

// update list of colors to match string locations
let rec swap acc s i =
    if (List.length acc) < i then // keep strings up to i in the same order
        let newacc = List.append acc [List.head s]
        let news = List.tail s
        swap newacc news i
    else // swap strings at i and i + 1
        let temp1 = [List.head s]
        let news1 = List.tail s
        let temp2 = [List.head news1]
        let news2 = List.tail news1
        List.append (List.append acc (List.append temp2 temp1)) news2

// only swap strings if needed (aka. RR or LL knot)
let kstringshelperhelper e s i =
    match e with
    | RR _ -> swap [] s i
    | LL _ -> swap [] s i
    | RL _ -> s
    | LR _ -> s
    | SKIP _ -> s

// update list of knot positions
let rec updatepos i row knot pre post ktype =
    if (List.length pre) < i then // keep list same up to pos i
        let newPre = List.append pre [List.head post]
        let newPost = List.tail post
        updatepos i row knot newPre newPost ktype
    else // update pos at i
        let oldPos = List.head post
        let pos = (row |> string) + "," + (knot |> string) + " " + ktype
        let newPos = 
            match oldPos with
            | "" -> [oldPos + pos] // list was empty before
            | _ -> // list wasn't empty, so remove previous l/r indication
                [oldPos.[..oldPos.Length - 2] + pos]
        let restOfList = List.tail post
        let newlist = List.append (List.append pre newPos) restOfList
        newlist

// help update knot positions
let poshelper s i num pos ktype =
    let string1:string = List.item(i) s
    let s1num = string1.[string1.Length - 1] |> string |> int
    let string2:string = List.item(i + 1) s
    let s2num = string2.[string2.Length - 1] |> string |> int
    let update1 = updatepos (s1num - 1) num i [] pos (fst ktype)
    updatepos (s2num - 1) num i [] update1 (snd ktype)

// return updated list of strings 
let rec kstringshelper row s i output num pos =
    if (List.isEmpty row) then // finished the row so return results
        (s, output, pos)
    else // process the next knot
        let k = List.head row
        let news = kstringshelperhelper k s i // update strings
        let knot = keval (List.head row) s i 
        let newrow = List.tail row 
        match k with // see if pos list should be updated
        | RR _ | LL _ -> // string at i will end up on right, string at i+1 will end up on left
            let newpos = poshelper s i num pos ("r", "l")
            kstringshelper newrow news (i+1) (output + knot) num newpos
        | RL _ | LR _ -> // string at i will end up on left, string at i+1 will end up on right
            let newpos = poshelper s i num pos ("l", "r")
            kstringshelper newrow news (i+1) (output + knot) num newpos
        | SKIP _ -> // no changes needed so just keep going
            kstringshelper newrow news (i+1) (output + knot) num pos

(* Evaluates a Row *)
let reval e s num pos =
    match e with
    | Row r ->
        // error check
        if (List.length r) <> (List.length s) - 1 then
            failwith "Each row must have exactly (number of strings - 1) knots."
        else 
            let newstrings, output, newpos = kstringshelper r s 0 "" num pos
            (newstrings, "\n" + (num |> string) + "   " + output, newpos)

(* Evaluates a Block *)
let rec beval (e: Row list) s output num pos =
    if (e.IsEmpty) then // finished the block so return results
        (s, output, num, pos)
    else
        let tup = reval e.Head s num pos // process a row
        match tup with
        | (newcolors, news, newpos) ->
            beval e.Tail newcolors (List.append output [news]) (num + 1) newpos

(* Evaluates a Component *)
let rec ceval e s num pos =
    match e with
    | Block b -> // found a block
        let helper = beval b s [] num pos
        match helper with
        | (colors, output, newnum, newpos) ->
            let out = List.fold (+) "" output
            (colors, out, newnum, newpos)
    | Repeat (n,c) -> // found a repeat operation
        repeathelper n c s "" num pos

// perform repeat operation
and repeathelper n e s acc num pos =
    if n = 0 then // finished so return results
        (s, acc, num, pos)
    else // repeat once and recurse
        let result = clisthelper e s "" num pos
        match result with
        | (colors, output, newnum, newpos) ->
            let out = acc + output
            repeathelper (n-1) e colors out newnum newpos
 
// evaluates list of components
and clisthelper e s acc num pos =
    if (e.IsEmpty) then // no more components in the list
        (s, acc, num, pos)
    else // process next component
        let result = ceval e.Head s num pos
        let newe = e.Tail
        match result with
        | (colors, output, newnum, newpos) ->
            let out = acc + output
            clisthelper newe colors out newnum newpos

(* Evaluates a Strings *)
let seval e =
    match e with
    | Strings (n, strings) ->
        // error check
        if (n |> int) <> (List.length strings) then
            failwith "Number of colors must match number of strings."
        else
            let rec addnum s i news = // add number of string to end of each color to differentiate
                if i = List.length s then 
                    news
                else
                    let str = (List.item(i) s) + ((i + 1) |> string)
                    addnum s (i + 1) (List.append news [str])
            addnum strings 0 [] // return the list of strings/colors in order

(* Evaluates a Name *)
let neval e =
    match e with
    | Name name ->
        let n = "Pattern Name: " + name + "\n"
        (n, name)

// generate empty pos list
let rec makePosList i stop sofar =
    if i = stop then // list has same # of elements as # of strings in pattern so we're done
        sofar
    else // keep going
        makePosList (i + 1) stop (List.append sofar [""])

(* Evaluates a Pattern *)
let eval e d =
    match e with
    | Pattern (name, strings, components) ->
        let nres = neval name
        match nres with
        | (n, justname) ->
            let s = seval strings
            let pos = makePosList 0 (List.length s) []
            // evaluate the components in the pattern
            let comps = clisthelper components s "" 1 pos
            match comps with
            | (colors, fincomp, num, pos) -> // put together name and evaluated components for fully evaluated pattern
                let result = n + "\nRow" + fincomp
                makeSVG d justname s result (num - 1) pos // make SVG file
                result