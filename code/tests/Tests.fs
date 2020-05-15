namespace tests

open System
open System.IO
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProgramParser

[<TestClass>]
type TestClass () =

    // for file reading later
    let prefix = 
           Directory.GetParent(Directory.GetCurrentDirectory()).Parent.Parent.Parent.Parent.FullName

    (* TEST STRING INPUT *)

    [<TestMethod>]
    // test if a pattern with 1 row parses correctly
    member this.SingleRowParsesCorrectly () =
        let input = "(name SINGLEROW) (strings 3 red blue green) AAA"
        let expected = 
            Pattern(
                Name "SINGLEROW", Strings(
                    3, ["red"; "blue"; "green"]), 
                    [Block[Row[RR('A'); RR('A'); RR('A')]]])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if a pattern with 2 rows parses correctly
    member this.TwoRowsParsesCorrectly () =
        let input = "(name TWOROWS) (strings 3 black gray white) ABA C_D"
        let expected = 
            Pattern(
                Name "TWOROWS", Strings(
                    3, ["black"; "gray"; "white"]), 
                    [Block[Row[RR('A'); LL('B'); RR('A')]; Row[RL('C'); SKIP('_'); LR('D')]]])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if a pattern with repeat parses correctly
    member this.RepeatParsesCorrectly () =
        let input = "(name REPEAT) (strings 4 orange pink pink orange) (repeat 2 ABCD)"
        let expected = 
            Pattern(
                Name "REPEAT", Strings(
                    4, ["orange"; "pink"; "pink"; "orange"]), 
                    [Repeat(
                        2, [Block[Row[RR('A'); LL('B'); RL('C'); LR('D')]]])])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if a pattern with a repeat followed by block parses correctly
    member this.RepeatAndBlockParsesCorrectly () =
        let input = "(name REPEATANDBLOCK) (strings 4 orange pink pink orange) (repeat 2 ABCD) _A_A"
        let expected = 
            Pattern(
                Name "REPEATANDBLOCK", Strings(
                    4, ["orange"; "pink"; "pink"; "orange"]), 
                    [Repeat(
                        2, [Block[Row[RR('A'); LL('B'); RL('C'); LR('D')]]]);
                    Block[Row[SKIP('_'); RR('A'); SKIP('_'); RR('A')]]])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if a pattern with a block followed by repeat parses correctly
    member this.BlockAndRepeatParsesCorrectly () =
        let input = "(name BLOCKANDREPEAT) (strings 4 orange pink pink orange) _A_A (repeat 2 ABCD)"
        let expected = 
            Pattern(
                Name "BLOCKANDREPEAT", Strings(
                    4, ["orange"; "pink"; "pink"; "orange"]), 
                    [Block[Row[SKIP('_'); RR('A'); SKIP('_'); RR('A')]]; 
                    Repeat(2, [Block[Row[RR('A'); LL('B'); RL('C'); LR('D')]]])])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
    // test if a pattern with a nested repeat parses correctly
    member this.NestedRepeatParsesCorrectly () =
        let input = "(name NESTEDREPEAT) (strings 4 orange pink pink orange) (repeat 2 ABCD (repeat 4 AAAA))"
        let expected = 
            Pattern(
                Name "NESTEDREPEAT", Strings(
                    4, ["orange"; "pink"; "pink"; "orange"]),
                    [Repeat(
                        2, [Block[Row[RR('A'); LL('B'); RL('C'); LR('D')]];
                        Repeat(4, [Block[Row[RR('A'); RR('A'); RR('A'); RR('A')]]])])])
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    (* TEST FILE INPUT *)
    [<TestMethod>]
    // test if example-1.fbp with a nested repeat parses correctly
    member this.Example1ParsesCorrectly () =
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