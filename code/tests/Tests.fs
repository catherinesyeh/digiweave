namespace tests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProgramParser

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.SingleRowParsesCorrectly () =
        let input = "AAA"
        let expected = Pattern[Block[Row[RR('A'); RR('A'); RR('A')]]]
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false
