(* This program produces SVG visualizations of friendship bracelet patterns. *)
module SVG
open System
open System.IO

// tab helpers
let tab1 = "\n\t"
let tab2 = "\n\t\t"
let tab3 = "\n\t\t\t"
let tab4 = "\n\t\t\t\t"
let tab5 = "\n\t\t\t\t\t"
let tab6 = "\n\t\t\t\t\t\t"

// initalize SVG
let startSVG dir name =
    let prefix = dir + "/output/" // where the files will go
    let path = prefix + name + ".html"

    let setup = // html header
        "<!DOCTYPE html>" + 
        "\n<html lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">" +
        "\n<head>" +
        "\n\t<meta charset=\"utf-8\" />" +
        tab1 + "<title>" + name + "</title>" +
        tab1 + "<link href=\"https://fonts.googleapis.com/css2?family=Roboto&display=swap\" rel=\"stylesheet\">" +
        "\n</head>"
    
    File.WriteAllText(path, setup)
    path

// set up <body> part of html doc
let startBody name s rows path =
    let numStrings = List.length s
    let width = 100 * (numStrings + 1)
    let height = 100 * (rows + 1)

    // compile necessary svg defs
    let svgdefs = 
        "\n\n<body>" +
        tab1 + "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"" + (width |> string) + "\" height=\"" + (height |> string) + "\">" +
        tab2 + "<defs>" +
        tab3 + "<marker id=\"arrowhead\" markerWidth=\"5\" markerHeight=\"5\" refX=\"0\" refY=\"2\" orient=\"auto\">" +
        tab4 + "<polygon points=\"0 0, 4 2, 0 4\" fill=\"white\" />" +
        tab3 + "</marker>" +
        tab3 + "<symbol id=\"rarrow\">" +
        tab4 + "<line x1=\"30\" y1=\"40\" x2=\"60\" y2=\"40\" stroke=\"white\" stroke-width=\"3\" marker-end=\"url(#arrowhead)\" />" +
        tab3 + "</symbol>" +
        tab3 + "<symbol id=\"knot\">" +
        tab4 + "<circle cx=\"50\" cy=\"50\" r=\"40\" />" +
        tab3 + "</symbol>" +
        tab3 + "<symbol id=\"twoarrows\">" +
        tab4 + "<g>" +
        tab5 + "<use xlink:href=\"#rarrow\" />" +
        tab5 + "<use xlink:href=\"#rarrow\" transform=\"translate(0 20)\" />" +
        tab4 + "</g>" +
        tab3 + "</symbol>" +
        tab3 + "<symbol id=\"fliparrows\">" +
        tab4 + "<g>" +
        tab5 + "<use xlink:href=\"#rarrow\" transform=\"scale(0.5,0.5)\" />" +
        tab5 + "<use xlink:href=\"#rarrow\" transform=\"translate(50 10) scale(-0.5,0.5)\" />" +
        tab4 + "</g>" +
        tab3 + "</symbol>" +
        tab3 + "<symbol id=\"rr\">" +
        tab4 + "<g>" +
        tab5 + "<use xlink:href=\"#knot\" fill=\"var(--color)\" transform=\"scale(0.5,0.5)\" />" +
        tab5 + "<use xlink:href=\"#twoarrows\" transform=\"scale(0.5, 0.5)\" />" +
        tab4 + "</g>" +
        tab3 + "</symbol>" +
        tab3 + "<symbol id=\"ll\">" +
        tab4 + "<use xlink:href=\"#rr\" transform=\"translate(50 0) scale(-1,1)\" />" +
        tab3 + "</symbol>" +
        tab3 + "<symbol id=\"rl\">" +
        tab4 + "<g>" +
        tab5 + "<use xlink:href=\"#knot\" fill=\"var(--color)\" transform=\"scale(0.5,0.5)\" />" +
        tab5 + "<use xlink:href=\"#fliparrows\" />" +
        tab4 + "</g>" +
        tab3 + "</symbol>" +
        tab3 + "<symbol id=\"lr\">" +
        tab4 + "<use xlink:href=\"#rl\" transform=\"translate(50 0) scale(-1,1)\" />" +
        tab3 + "</symbol>" +
        tab3 + "<symbol id=\"skip\" display=\"none\">" +
        tab4 + "<use xlink:href=\"#rr\" />" +
        tab3 + "</symbol>" +
        tab2 + "</defs>" +
        tab2 + "<style>" +
        tab3 + ".title {" +
        tab4 + "font: bold 32px 'Roboto';" +
        tab4 + "letter-spacing: 0.3em;" +
        tab4 + "text-transform: uppercase;" +
        tab3 + "}" +
        tab3 + ".row {" +
        tab4 + "font: bold 20px 'Roboto';" +
        tab3 + "}" +
        tab2 + "</style>" +
        tab2 + "<text x=\"50%\" y=\"25\" class=\"title\" text-anchor=\"middle\">" + name + "</text>" +
        tab2 + "<g id=\"rowhead\" transform=\"translate(0 70)\">" +
        tab3 + "<text x=\"0\" y=\"20\" class=\"row\">ROW</text>"

    // build the header row (shows original order of strings in their respective colors)
    let rowHead i = 
        let color = List.item(i) s
        let xoffset = 118 + i * 100
        let stringnum = i + 1
        tab3 + "<svg class=\"string" + color + "\">" +
        tab4 + "<text x=\"" + (xoffset |> string) + "\" y=\"20\" class=\"row\" fill=\"var(--color)\">" + (stringnum |> string) + "</text>" +
        tab3 + "</svg>"

    // add header row to the file
    let rec listMaker acc i = 
        if i = numStrings then
            acc
        else
            listMaker (List.append acc [i]) (i + 1)
     
    let headList = listMaker [] 0

    let allText = 
        (List.fold(fun acc elem -> acc + (rowHead elem)) svgdefs headList) + 
        tab2 + "</g>"
        
    File.AppendAllText(path, allText)

// add strings to SVG
let addStrings s path =
    // create 1 string style element
    let addString color =
        let styleElem = 
            tab1 + ".string" + color + " {" +
            tab2 + "--color: " + color + ";" +
            tab1 + "}"
        styleElem

    let uniqueColors = List.distinct s // list that stores only keeps 1 copy of each color used in the pattern
    let startStyle = 
        "\n\n<style>" +
        tab1 + "html {" +
        tab2 + "font-family: 'Roboto', sans-serif;" +
        tab1 + "}"
    let allStyle = 
        (List.fold (fun acc elem -> acc + (addString elem)) startStyle uniqueColors) +
        "\n</style>"

    File.AppendAllText(path, allStyle)

// add each row to the svg file
let addRows strings (res: string) path =
    let startRow = tab2 + "<g id=\"rows\" transform=\"translate(0 130)\">"
    let numStrings = List.length strings
    
    let addKnot s i = // add a knot
        let xOffset = (100 * (i - 1)) |> string
        match s with
        | ">>" -> 
            let text = 
                tab6 + "<use xlink:href=\"#rr\" x=\"" + xOffset + "\" />" +
                tab5 + "</svg>"
            (i+1, text)
        | "<<" -> 
            let text =
                tab6 + "<use xlink:href=\"#ll\" x=\"" + xOffset + "\" />" +
                tab5 + "</svg>"
            (i+1, text)
        | ">" -> 
            let text = 
                tab6 + "<use xlink:href=\"#rl\" x=\"" + xOffset + "\" />" +
                tab5 + "</svg>"
            (i+1, text)
        | "<" -> 
            let text =
                tab6 + "<use xlink:href=\"#lr\" x=\"" + xOffset + "\" />" +
                tab5 + "</svg>"
            (i+1, text)
        | "_" -> 
            let text =
                tab5 + "<svg>" +
                tab6 + "<use xlink:href=\"#skip\" x=\"" + xOffset + "\" />" +
                tab5 + "</svg>"
            (i+1, text)
        | _ -> // color
            let text = tab5 + "<svg class=\"string" + s + "\">"
            (i, text)

    let oneRow (row: string) = // add each row
        let split = row.Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList // get each individual item
        let rnum = List.head split // get row num
        let v = rnum |> int
        let yOffset = 100 * (v - 1)
        // start row
        let text = 
            tab3 + "<g id=\"row" + rnum + "\" transform=\"translate(0 " + (yOffset |> string) + ")\">" +
            tab4 + "<text x=\"0\" y=\"30\">" + rnum + "</text>" +
            tab4 + "<g transform=\"translate(150 0)\">"
        let knots = List.tail split // rest of items should be knots
        let total = (List.length knots) - 1

        let rec knotHelper itemInd knotInd =
            let s = List.item(itemInd) knots
            let res = addKnot s knotInd
            match res with
            | (n, str) ->
                if n = numStrings then // done with row
                    str
                else // keep going
                    str + (knotHelper (itemInd + 1) n)

        let addOn = knotHelper 0 1

        let endText =
            tab4 + "</g>" +
            tab3 + "</g>"
        
        text + addOn + endText
    
    // extract knots from string
    let arr = res.Split([|"Row\n"|], StringSplitOptions.RemoveEmptyEntries)
    let rows = arr.[1] // should be in second part of split string
    let rlist = rows.Split([|"\n"|], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList // extract rows with "/n"

    let endRow = 
        (List.fold (fun acc elem -> acc + (oneRow elem)) startRow rlist) +
        tab2 + "</g>" +
        tab1 + "</svg>"

    File.AppendAllText(path, endRow)

// finish SVG 
let endSVG path = 
    let finishdoc = 
        "\n</body>" +
        "\n</html>"

    File.AppendAllText(path, finishdoc)

// generate SVG file
let makeSVG dir name strings result rows =
    let path = startSVG dir name // get file path
    addStrings strings path 
    startBody name strings rows path
    addRows strings result path
    printfn "Your pattern has been saved to %s\n" path
    endSVG path