namespace tests

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open Parser
open ProgramParser
open ProgramInterpreter

[<TestClass>]
type TestClass () =

    // for file reading later
    let prefix = 
           Directory.GetParent(Directory.GetCurrentDirectory()).Parent.Parent.Parent.Parent.FullName

    (* INDIVIDUAL PARSER TESTS *)

    [<TestMethod>]
    // test if knot parser parses correctly
    member this.KnotParsesCorrectly () =
        let input = "A"
        let expected = RR('A')
        let result = knot (prepare input)

        match result with
        | Success(res, _) -> 
            Assert.AreEqual(expected, res)
        | Failure _ -> 
            Assert.IsTrue false

    [<TestMethod>]
    // test if row parser parses correctly
    member this.RowParsesCorrectly () =
        let input = "ABCD"
        let expected = Row[RR('A'); LL('B'); RL('C'); LR('D')]
        let result = row (prepare input)

        match result with
        | Success(res, _) -> 
            Assert.AreEqual(expected, res)
        | Failure _ -> 
            Assert.IsTrue false
    
    [<TestMethod>]
    // test if block parser parses correctly
    member this.BlockParsesCorrectly () =
        let input = "A_A_ BCDD"
        let expected = Block[Row[RR('A'); SKIP('_'); RR('A'); SKIP('_')]; Row[LL('B'); RL('C'); LR('D'); LR('D')]]
        let result = block (prepare input)

        match result with
        | Success(res, _) -> 
            Assert.AreEqual(expected, res)
        | Failure _ -> 
            Assert.IsTrue false

    [<TestMethod>]
    // test if name parser parses correctly
    member this.NameParsesCorrectly () =
        let input = "(name TEST)"
        let expected = Name("TEST")
        let result = name (prepare input)

        match result with
        | Success(res, _) -> 
            Assert.AreEqual(expected, res)
        | Failure _ -> 
            Assert.IsTrue false

    [<TestMethod>]
    // test if strings parser parses correctly
    member this.StringsParsesCorrectly () =
        let input = "(strings 3 pink purple blue)"
        let expected = Strings(3, ["pink"; "purple"; "blue"])
        let result = strings (prepare input)

        match result with
        | Success(res, _) -> 
            Assert.AreEqual(expected, res)
        | Failure _ -> 
            Assert.IsTrue false

    [<TestMethod>]
    // test if repeat parser parses correctly
    member this.RepeatParsesCorrectly () =
        let input = "(repeat 3 AAAA)"
        let expected = Repeat(3, [Block[Row[RR('A'); RR('A'); RR('A'); RR('A')]]])
        let result = repeat (prepare input)

        match result with
        | Success(res, _) -> 
            Assert.AreEqual(expected, res)
        | Failure _ -> 
            Assert.IsTrue false
    
    [<TestMethod>]
    // test if repeat parser with nested repeat parses correctly
    member this.NestedRepeatParsesCorrectly () =
        let input = "(repeat 3 AAAA (repeat 2 B_CD))"
        let expected = 
            Repeat(3, [Block[Row[RR('A'); RR('A'); RR('A'); RR('A')]]; 
            Repeat(2, [Block[Row[LL('B'); SKIP('_'); RL('C'); LR('D')]]])])
        let result = repeat (prepare input)

        match result with
        | Success(res, _) -> 
            Assert.AreEqual(expected, res)
        | Failure _ -> 
            Assert.IsTrue false

    (* COMPLETE PATTERN PARSER TESTS *)

    [<TestMethod>]
    // test if a pattern with 1 row parses correctly
    member this.SingleRowPatternParsesCorrectly () =
        let input = "(name SINGLEROW) (strings 3 red blue green) AA"
        let expected = 
            Pattern(
                Name "SINGLEROW", Strings(
                    3, ["red"; "blue"; "green"]), 
                    [Block[Row[RR('A'); RR('A')]]])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if a pattern with 2 rows parses correctly
    member this.TwoRowsPatternParsesCorrectly () =
        let input = "(name TWOROWS) (strings 3 black gray white) AB CD"
        let expected = 
            Pattern(
                Name "TWOROWS", Strings(
                    3, ["black"; "gray"; "white"]), 
                    [Block[Row[RR('A'); LL('B')]; Row[RL('C'); LR('D')]]])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false
    
    [<TestMethod>]
    // test if a pattern with > 9 strings parses correctly (multidigit)
    member this.TenStringsPatternParsesCorrectly () =
        let input = "(name 10STRINGS) (strings 10 red red orange orange yellow yellow green green blue blue) AAAAAAAAA"
        let expected = 
            Pattern(
                Name "10STRINGS", Strings(
                    10, ["red"; "red"; "orange"; "orange"; "yellow"; "yellow"; "green"; "green"; "blue"; "blue"]), 
                    [Block[Row[RR('A'); RR('A'); RR('A'); RR('A'); RR('A'); RR('A'); RR('A'); RR('A'); RR('A');]]])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if a pattern with repeat parses correctly
    member this.RepeatPatternParsesCorrectly () =
        let input = "(name REPEAT) (strings 4 orange pink pink orange) (repeat 2 ABC)"
        let expected = 
            Pattern(
                Name "REPEAT", Strings(
                    4, ["orange"; "pink"; "pink"; "orange"]), 
                    [Repeat(
                        2, [Block[Row[RR('A'); LL('B'); RL('C')]]])])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if a pattern with > 9 repeat parses correctly (multidigit)
    member this.TenRepeatPatternParsesCorrectly () =
        let input = "(name 10REPEAT) (strings 4 orange pink pink orange) (repeat 10 ABC)"
        let expected = 
            Pattern(
                Name "10REPEAT", Strings(
                    4, ["orange"; "pink"; "pink"; "orange"]), 
                    [Repeat(
                        10, [Block[Row[RR('A'); LL('B'); RL('C')]]])])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if a pattern with a repeat followed by block parses correctly
    member this.RepeatAndBlockPatternParsesCorrectly () =
        let input = "(name REPEATANDBLOCK) (strings 4 orange pink pink orange) (repeat 2 BCD) _A_"
        let expected = 
            Pattern(
                Name "REPEATANDBLOCK", Strings(
                    4, ["orange"; "pink"; "pink"; "orange"]), 
                    [Repeat(
                        2, [Block[Row[LL('B'); RL('C'); LR('D')]]]);
                    Block[Row[SKIP('_'); RR('A'); SKIP('_')]]])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if a pattern with a block followed by repeat parses correctly
    member this.BlockAndRepeatPatternParsesCorrectly () =
        let input = "(name BLOCKANDREPEAT) (strings 4 orange pink pink orange) _A_ (repeat 2 BCD)"
        let expected = 
            Pattern(
                Name "BLOCKANDREPEAT", Strings(
                    4, ["orange"; "pink"; "pink"; "orange"]), 
                    [Block[Row[SKIP('_'); RR('A'); SKIP('_')]]; 
                    Repeat(2, [Block[Row[LL('B'); RL('C'); LR('D')]]])])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if a pattern with a nested repeat parses correctly
    member this.NestedRepeatPatternParsesCorrectly () =
        let input = "(name NESTEDREPEAT) (strings 4 orange pink pink orange) (repeat 2 ABC (repeat 4 AAA))"
        let expected = 
            Pattern(
                Name "NESTEDREPEAT", Strings(
                    4, ["orange"; "pink"; "pink"; "orange"]),
                    [Repeat(
                        2, [Block[Row[RR('A'); LL('B'); RL('C')]];
                        Repeat(4, [Block[Row[RR('A'); RR('A'); RR('A')]]])])])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if example-1.fbp parses correctly
    member this.File1ParsesCorrectly () =
        let file = prefix + "/examples/example-1.fbp"
        let input = File.ReadAllText file
        let expected = 
            Pattern(
                Name "2ROWS", Strings(
                    6, ["CornflowerBlue"; "CornflowerBlue"; "Coral"; "Coral";
                    "BurlyWood"; "BurlyWood"]),
                    [Repeat(
                        6, [Block[Row[RR('A'); RR('A'); 
                        RR('A'); RR('A'); RR('A')]]])])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if example-2.fbp parses correctly
    member this.File2ParsesCorrectly () =
        let file = prefix + "/examples/example-2.fbp"
        let input = File.ReadAllText file
        let expected = 
            Pattern(
                Name "ARROW", Strings(
                    8, ["LightPink"; "LightSeaGreen"; "LightSeaGreen"; "LightPink";
                    "LightPink"; "LightSeaGreen"; "LightSeaGreen"; "LightPink"]),
                    [Repeat(
                        2, [Block
                        [Row [RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B'];
                        Row [SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; LL 'B'; SKIP '_']]])])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if example-3.fbp parses correctly
    member this.File3ParsesCorrectly () =
        let file = prefix + "/examples/example-3.fbp"
        let input = File.ReadAllText file
        let expected = 
            Pattern(
                Name "HEART", Strings(
                    8, ["PeachPuff"; "PaleVioletRed"; "PeachPuff"; "PaleVioletRed";
                    "PaleVioletRed"; "PeachPuff"; "PaleVioletRed"; "PeachPuff"]),
                    [Block
                        [Row [RL 'C'; SKIP '_'; RR 'A'; SKIP '_'; LL 'B'; SKIP '_'; LR 'D'];
                        Row [SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; LL 'B'; SKIP '_'];
                        Row [LL 'B'; SKIP '_'; RR 'A'; SKIP '_'; LL 'B'; SKIP '_'; RR 'A'];
                        Row [SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; LL 'B'; SKIP '_'];
                        Row [RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B'];
                        Row [SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; LL 'B'; SKIP '_'];
                        Row [RL 'C'; SKIP '_'; RR 'A'; SKIP '_'; LL 'B'; SKIP '_'; LR 'D'];
                        Row [SKIP '_'; LR 'D'; SKIP '_'; RR 'A'; SKIP '_'; RL 'C'; SKIP '_']]])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if example-4.fbp parses correctly
    member this.File4ParsesCorrectly () =
        let file = prefix + "/examples/example-4.fbp"
        let input = File.ReadAllText file
        let expected = 
            Pattern(
                Name "GRADIENTSQUARES", Strings(
                    16, ["gray"; "silver"; "indianred"; "lightblue"; "rosybrown"; "skyblue";
                    "silver"; "gray"; "gray"; "silver"; "skyblue"; "rosybrown"; "lightblue";
                    "indianred"; "silver"; "gray"]),
                    [Repeat
                        (2,
                        [Repeat
                            (4,
                            [Block
                                [Row
                                    [RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; RR 'A';
                                    SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_';
                                    LL 'B'];
                                Row
                                    [SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_';
                                    RR 'A'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B';
                                    SKIP '_']]]);
                        Block
                            [Row
                                [RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; RR 'A';
                                SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_';
                                LL 'B'];
                            Row
                                [SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_';
                                RR 'A'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D';
                                SKIP '_'];
                            Row
                                [LL 'B'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B';
                                SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_';
                                RR 'A'];
                            Row
                                [SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_';
                                RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; RR 'A';
                                SKIP '_'];
                            Row
                                [LL 'B'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_'; RR 'A';
                                SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_';
                                RR 'A'];
                            Row
                                [SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; RL 'C'; SKIP '_';
                                RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C';
                                SKIP '_'];
                            Row
                                [RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; LL 'B';
                                SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_';
                                LL 'B'];
                            Row
                                [SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_'; RR 'A'; SKIP '_';
                                LL 'B'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B'; SKIP '_'; LL 'B';
                                SKIP '_']]])])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false
            
    [<TestMethod>]
    // test if example-5.fbp parses correctly
    member this.File5ParsesCorrectly () =
        let file = prefix + "/examples/example-5.fbp"
        let input = File.ReadAllText file
        let expected = 
            Pattern(
                Name "ICECREAM", Strings(
                    15, ["darkslategray"; "peru"; "darkslategray"; "peru"; "darkslategray"; "peru";
                    "darkslategray"; "sienna"; "darkslategray"; "lightpink"; "darkslategray";
                    "lightpink"; "darkslategray"; "crimson"; "darkslategray"]),
                    [Repeat(
                        2,
                        [Block
                            [Row 
                                [RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C';
                                SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_'];
                            Row
                               [SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; RL 'C'; SKIP '_';
                                RL 'C'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D'];
                            Row
                               [RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D';
                                SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; RL 'C'; SKIP '_'];
                            Row
                               [SKIP '_'; LR 'D'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_';
                                RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; LR 'D'];
                            Row
                               [RL 'C'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D';
                                SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'];
                            Row
                               [SKIP '_'; LR 'D'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_';
                                RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; LR 'D'];
                            Row
                               [RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D';
                                SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; RL 'C'; SKIP '_'];
                            Row
                               [SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; RL 'C'; SKIP '_';
                                RL 'C'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D'];
                            Row
                               [RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C';
                                SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_'; RL 'C'; SKIP '_'];
                            Row
                               [SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_';
                                LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D'; SKIP '_'; LR 'D']]])])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    (* EVALUATOR TESTS *)

    [<TestMethod>]
    // test if single row evaluates correctly
    member this.SingleRowEvaluatesCorrectly () =
        let input = "(name SINGLEROW) (strings 3 red blue green) AA"
        let expected = 
            "Pattern Name: SINGLEROW\n\nRow" +
            "\n1   red >> red >> "

        match parse input with
            | Some ast -> 
                let result = eval ast prefix
                Assert.AreEqual(expected, result)
            | None    -> 
                Assert.IsTrue false

    [<TestMethod>]
    // test if pattern with > 9 strings evaluates correctly (multidigit)
    member this.TenStringsEvaluatesCorrectly () =
        let input = "(name 10STRINGS) (strings 10 red orange yellow green blue purple violet black gray white) AAAAAAAAA"
        let expected = 
            "Pattern Name: 10STRINGS\n\nRow" +
            "\n1   red >> red >> red >> red >> red >> red >> red >> red >> red >> "

        match parse input with
            | Some ast -> 
                let result = eval ast prefix
                Assert.AreEqual(expected, result)
            | None    -> 
                Assert.IsTrue false

    [<TestMethod>]
    // test if repeat evaluates correctly
    member this.RepeatEvaluatesCorrectly () =
        let input = "(name REPEAT) (strings 4 orange pink pink orange) (repeat 3 BBB)"
        let expected = 
            "Pattern Name: REPEAT\n\nRow" +
            "\n1   pink << pink << orange << " +
            "\n2   pink << orange << orange << " +
            "\n3   orange << orange << pink << "

        match parse input with
            | Some ast -> 
                let result = eval ast prefix
                Assert.AreEqual(expected, result)
            | None    -> 
                Assert.IsTrue false

    [<TestMethod>]
    // test if > 9 repeat evaluates correctly (multidigit)
    member this.TenRepeatEvaluatesCorrectly () =
        let input = "(name 10REPEAT) (strings 4 orange pink pink orange) (repeat 10 BBB)"
        let expected = 
            "Pattern Name: 10REPEAT\n\nRow" +
            "\n1   pink << pink << orange << " +
            "\n2   pink << orange << orange << " +
            "\n3   orange << orange << pink << " +
            "\n4   orange << pink << pink << " +
            "\n5   pink << pink << orange << " +
            "\n6   pink << orange << orange << " +
            "\n7   orange << orange << pink << " +
            "\n8   orange << pink << pink << " +
            "\n9   pink << pink << orange << " +
            "\n10   pink << orange << orange << "

        match parse input with
            | Some ast -> 
                let result = eval ast prefix
                Assert.AreEqual(expected, result)
            | None    -> 
                Assert.IsTrue false

    [<TestMethod>]
    // test if nestedrepeat evaluates correctly
    member this.NestedRepeatEvaluatesCorrectly () =
        let input = "(name NESTEDREPEAT) (strings 4 orange pink pink orange) (repeat 2 AAA (repeat 4 BBB))"
        let expected = 
            "Pattern Name: NESTEDREPEAT\n\nRow" +
            "\n1   orange >> orange >> orange >> " +
            "\n2   pink << orange << orange << " +
            "\n3   orange << orange << pink << " +
            "\n4   orange << pink << pink << " +
            "\n5   pink << pink << orange << " +
            "\n6   pink >> pink >> pink >> " +
            "\n7   orange << orange << pink << " +
            "\n8   orange << pink << pink << " +
            "\n9   pink << pink << orange << " +
            "\n10   pink << orange << orange << "

        match parse input with
            | Some ast -> 
                let result = eval ast prefix
                Assert.AreEqual(expected, result)
            | None    -> 
                Assert.IsTrue false

    [<TestMethod>]
    // test if example-1.fbp evaluates correctly
    member this.File1EvaluatesCorrectly () =
        let file = prefix + "/examples/example-1.fbp"
        let input = File.ReadAllText file
        let expected = 
            "Pattern Name: 2ROWS\n\nRow" +
            "\n1   CornflowerBlue >> CornflowerBlue >> CornflowerBlue >> CornflowerBlue >> CornflowerBlue >> " +
            "\n2   CornflowerBlue >> CornflowerBlue >> CornflowerBlue >> CornflowerBlue >> CornflowerBlue >> " +
            "\n3   Coral >> Coral >> Coral >> Coral >> Coral >> " +
            "\n4   Coral >> Coral >> Coral >> Coral >> Coral >> " +
            "\n5   BurlyWood >> BurlyWood >> BurlyWood >> BurlyWood >> BurlyWood >> " +
            "\n6   BurlyWood >> BurlyWood >> BurlyWood >> BurlyWood >> BurlyWood >> "

        match parse input with
            | Some ast -> 
                let result = eval ast prefix
                Assert.AreEqual(expected, result)
            | None    -> 
                Assert.IsTrue false

    [<TestMethod>]
    // test if example-2.fbp evaluates correctly
    member this.File2EvaluatesCorrectly () =
        let file = prefix + "/examples/example-2.fbp"
        let input = File.ReadAllText file
        let expected = 
            "Pattern Name: ARROW\n\nRow" +
            "\n1   LightPink >>  _ LightSeaGreen >>  _ LightSeaGreen <<  _ LightPink << " +
            "\n2    _ LightPink >>  _ LightSeaGreen >>  _ LightPink <<  _ " +
            "\n3   LightSeaGreen >>  _ LightPink >>  _ LightPink <<  _ LightSeaGreen << " +
            "\n4    _ LightSeaGreen >>  _ LightPink >>  _ LightSeaGreen <<  _ "

        match parse input with
            | Some ast -> 
                let result = eval ast prefix
                Assert.AreEqual(expected, result)
            | None    -> 
                Assert.IsTrue false

    [<TestMethod>]
    // test if example-3.fbp evaluates correctly
    member this.File3EvaluatesCorrectly () =
        let file = prefix + "/examples/example-3.fbp"
        let input = File.ReadAllText file
        let expected = 
            "Pattern Name: HEART\n\nRow" +
            "\n1   PeachPuff >  _ PeachPuff >>  _ PeachPuff <<  _ PeachPuff < " +
            "\n2    _ PaleVioletRed >>  _ PeachPuff >>  _ PaleVioletRed <<  _ " +
            "\n3   PaleVioletRed <<  _ PaleVioletRed >>  _ PaleVioletRed <<  _ PaleVioletRed >> " +
            "\n4    _ PeachPuff >>  _ PaleVioletRed >>  _ PeachPuff <<  _ " +
            "\n5   PaleVioletRed >>  _ PeachPuff >>  _ PeachPuff <<  _ PaleVioletRed << " +
            "\n6    _ PaleVioletRed >>  _ PeachPuff >>  _ PaleVioletRed <<  _ " +
            "\n7   PeachPuff >  _ PaleVioletRed >>  _ PaleVioletRed <<  _ PeachPuff < " +
            "\n8    _ PeachPuff <  _ PaleVioletRed >>  _ PeachPuff >  _ "

        match parse input with
            | Some ast -> 
                let result = eval ast prefix
                Assert.AreEqual(expected, result)
            | None    -> 
                Assert.IsTrue false

    [<TestMethod>]
    // test if example-4.fbp evaluates correctly
    member this.File4EvaluatesCorrectly () =
        let file = prefix + "/examples/example-4.fbp"
        let input = File.ReadAllText file
        let expected = 
            "Pattern Name: GRADIENTSQUARES\n\nRow" +
            "\n1   gray >>  _ indianred >>  _ rosybrown >>  _ silver >>  _ silver <<  _ rosybrown <<  _ indianred <<  _ gray << " +
            "\n2    _ gray >>  _ indianred >>  _ rosybrown >>  _ silver >>  _ rosybrown <<  _ indianred <<  _ gray <<  _ " +
            "\n3   silver >>  _ gray >>  _ indianred >>  _ rosybrown >>  _ rosybrown <<  _ indianred <<  _ gray <<  _ silver << " +
            "\n4    _ silver >>  _ gray >>  _ indianred >>  _ rosybrown >>  _ indianred <<  _ gray <<  _ silver <<  _ " +
            "\n5   lightblue >>  _ silver >>  _ gray >>  _ indianred >>  _ indianred <<  _ gray <<  _ silver <<  _ lightblue << " +
            "\n6    _ lightblue >>  _ silver >>  _ gray >>  _ indianred >>  _ gray <<  _ silver <<  _ lightblue <<  _ " +
            "\n7   skyblue >>  _ lightblue >>  _ silver >>  _ gray >>  _ gray <<  _ silver <<  _ lightblue <<  _ skyblue << " +
            "\n8    _ skyblue >>  _ lightblue >>  _ silver >>  _ gray >>  _ silver <<  _ lightblue <<  _ skyblue <<  _ " +
            "\n9   gray >>  _ skyblue >>  _ lightblue >>  _ silver >>  _ silver <<  _ lightblue <<  _ skyblue <<  _ gray << " +
            "\n10    _ gray >  _ skyblue >  _ lightblue >  _ silver >>  _ lightblue <  _ skyblue <  _ gray <  _ " +
            "\n11   gray <<  _ skyblue <<  _ lightblue <<  _ silver <<  _ silver >>  _ lightblue >>  _ skyblue >>  _ gray >> " +
            "\n12    _ skyblue <<  _ lightblue <<  _ silver <<  _ gray >>  _ silver >>  _ lightblue >>  _ skyblue >>  _ " +
            "\n13   skyblue <<  _ lightblue <<  _ silver <<  _ indianred >>  _ gray >>  _ silver >>  _ lightblue >>  _ skyblue >> " +
            "\n14    _ lightblue <  _ silver <  _ rosybrown >  _ indianred >  _ gray >  _ silver >  _ lightblue >  _ " +
            "\n15   skyblue >>  _ lightblue >>  _ silver >>  _ indianred <<  _ gray <<  _ silver <<  _ lightblue <<  _ skyblue << " +
            "\n16    _ skyblue >>  _ lightblue >>  _ silver >>  _ gray <<  _ silver <<  _ lightblue <<  _ skyblue <<  _ " +
            "\n17   gray >>  _ skyblue >>  _ lightblue >>  _ silver >>  _ silver <<  _ lightblue <<  _ skyblue <<  _ gray << " +
            "\n18    _ gray >>  _ skyblue >>  _ lightblue >>  _ silver >>  _ lightblue <<  _ skyblue <<  _ gray <<  _ " +
            "\n19   silver >>  _ gray >>  _ skyblue >>  _ lightblue >>  _ lightblue <<  _ skyblue <<  _ gray <<  _ silver << " +
            "\n20    _ silver >>  _ gray >>  _ skyblue >>  _ lightblue >>  _ skyblue <<  _ gray <<  _ silver <<  _ " +
            "\n21   rosybrown >>  _ silver >>  _ gray >>  _ skyblue >>  _ skyblue <<  _ gray <<  _ silver <<  _ rosybrown << " +
            "\n22    _ rosybrown >>  _ silver >>  _ gray >>  _ skyblue >>  _ gray <<  _ silver <<  _ rosybrown <<  _ " +
            "\n23   indianred >>  _ rosybrown >>  _ silver >>  _ gray >>  _ gray <<  _ silver <<  _ rosybrown <<  _ indianred << " +
            "\n24    _ indianred >>  _ rosybrown >>  _ silver >>  _ gray >>  _ silver <<  _ rosybrown <<  _ indianred <<  _ " +
            "\n25   gray >>  _ indianred >>  _ rosybrown >>  _ silver >>  _ silver <<  _ rosybrown <<  _ indianred <<  _ gray << " +
            "\n26    _ gray >  _ indianred >  _ rosybrown >  _ silver >>  _ rosybrown <  _ indianred <  _ gray <  _ " +
            "\n27   gray <<  _ indianred <<  _ rosybrown <<  _ silver <<  _ silver >>  _ rosybrown >>  _ indianred >>  _ gray >> " +
            "\n28    _ indianred <<  _ rosybrown <<  _ silver <<  _ gray >>  _ silver >>  _ rosybrown >>  _ indianred >>  _ " +
            "\n29   indianred <<  _ rosybrown <<  _ silver <<  _ skyblue >>  _ gray >>  _ silver >>  _ rosybrown >>  _ indianred >> " +
            "\n30    _ rosybrown <  _ silver <  _ lightblue >  _ skyblue >  _ gray >  _ silver >  _ rosybrown >  _ " +
            "\n31   indianred >>  _ rosybrown >>  _ silver >>  _ skyblue <<  _ gray <<  _ silver <<  _ rosybrown <<  _ indianred << " +
            "\n32    _ indianred >>  _ rosybrown >>  _ silver >>  _ gray <<  _ silver <<  _ rosybrown <<  _ indianred <<  _ "

        match parse input with
            | Some ast -> 
                let result = eval ast prefix
                Assert.AreEqual(expected, result)
            | None    -> 
                Assert.IsTrue false

    [<TestMethod>]
    // test if example-5.fbp evaluates correctly
    member this.File5EvaluatesCorrectly () =
        let file = prefix + "/examples/example-5.fbp"
        let input = File.ReadAllText file
        let expected = 
            "Pattern Name: ICECREAM\n\nRow" +
            "\n1   darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ " +
            "\n2    _ darkslategray <  _ darkslategray <  _ peru >  _ sienna >  _ darkslategray <  _ darkslategray <  _ darkslategray < " +
            "\n3   darkslategray >  _ darkslategray >  _ peru <  _ sienna <  _ lightpink <  _ lightpink <  _ darkslategray >  _ " +
            "\n4    _ darkslategray <  _ peru >  _ peru >  _ sienna >  _ lightpink >  _ lightpink >  _ darkslategray < " +
            "\n5   darkslategray >  _ peru <  _ peru <  _ sienna <  _ lightpink <  _ lightpink <  _ crimson <  _ " +
            "\n6    _ darkslategray <  _ peru >  _ peru >  _ sienna >  _ lightpink >  _ lightpink >  _ darkslategray < " +
            "\n7   darkslategray >  _ darkslategray >  _ peru <  _ sienna <  _ lightpink <  _ lightpink <  _ darkslategray >  _ " +
            "\n8    _ darkslategray <  _ darkslategray <  _ peru >  _ sienna >  _ darkslategray <  _ darkslategray <  _ darkslategray < " +
            "\n9   darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ " +
            "\n10    _ darkslategray <  _ darkslategray <  _ darkslategray <  _ darkslategray <  _ darkslategray <  _ darkslategray <  _ darkslategray < " +
            "\n11   darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ " +
            "\n12    _ darkslategray <  _ darkslategray <  _ peru >  _ sienna >  _ darkslategray <  _ darkslategray <  _ darkslategray < " +
            "\n13   darkslategray >  _ darkslategray >  _ peru <  _ sienna <  _ lightpink <  _ lightpink <  _ darkslategray >  _ " +
            "\n14    _ darkslategray <  _ peru >  _ peru >  _ sienna >  _ lightpink >  _ lightpink >  _ darkslategray < " +
            "\n15   darkslategray >  _ peru <  _ peru <  _ sienna <  _ lightpink <  _ lightpink <  _ crimson <  _ " +
            "\n16    _ darkslategray <  _ peru >  _ peru >  _ sienna >  _ lightpink >  _ lightpink >  _ darkslategray < " +
            "\n17   darkslategray >  _ darkslategray >  _ peru <  _ sienna <  _ lightpink <  _ lightpink <  _ darkslategray >  _ " +
            "\n18    _ darkslategray <  _ darkslategray <  _ peru >  _ sienna >  _ darkslategray <  _ darkslategray <  _ darkslategray < " +
            "\n19   darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ darkslategray >  _ " +
            "\n20    _ darkslategray <  _ darkslategray <  _ darkslategray <  _ darkslategray <  _ darkslategray <  _ darkslategray <  _ darkslategray < "

        match parse input with
            | Some ast -> 
                let result = eval ast prefix
                Assert.AreEqual(expected, result)
            | None    -> 
                Assert.IsTrue false