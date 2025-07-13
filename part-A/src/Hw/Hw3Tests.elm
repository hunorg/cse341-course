module Hw.Hw3Tests exposing (..)

import Hw.Hw3 exposing (..)


onlyCapitalsTests =
    let
        test1 =
            onlyCapitals [ "Alice", "bob", "Zoe" ] == [ "Alice", "Zoe" ]

        test2 =
            onlyCapitals [ "x", "y", "z" ] == []

        test3 =
            onlyCapitals [] == []
    in
    [ test1, test2, test3 ]


longestString1Tests =
    let
        test1 =
            longestString1 [] == ""

        test2 =
            longestString1 [ "a", "bbb", "cc" ] == "bbb"

        test3 =
            longestString1 [ "a", "b", "c" ] == "a"
    in
    [ test1, test2, test3 ]


longestString2Tests =
    let
        test1 =
            longestString2 [] == ""

        test2 =
            longestString2 [ "a", "bbb", "cc" ] == "bbb"

        test3 =
            longestString2 [ "a", "b", "c" ] == "c"
    in
    [ test1, test2, test3 ]


longestString3Tests =
    let
        test1 =
            longestString3 [] == ""

        test2 =
            longestString3 [ "a", "bbb", "cc" ] == "bbb"

        test3 =
            longestString3 [ "a", "b", "c" ] == "a"
    in
    [ test1, test2, test3 ]


longestString4Tests =
    let
        test1 =
            longestString4 [] == ""

        test2 =
            longestString4 [ "a", "bbb", "cc" ] == "bbb"

        test3 =
            longestString4 [ "a", "b", "c" ] == "c"
    in
    [ test1, test2, test3 ]


longestCapitalizedTests =
    let
        test1 =
            longestCapitalized [] == ""

        test2 =
            longestCapitalized [ "apple", "Banana", "Cherry", "date" ] == "Banana"

        test3 =
            longestCapitalized [ "apple", "banana", "cherry" ] == ""
    in
    [ test1, test2, test3 ]


revStringTests =
    let
        test1 =
            revString "" == ""

        test2 =
            revString "a" == "a"

        test3 =
            revString "abc" == "cba"

        test4 =
            revString "Madam" == "madaM"

        test5 =
            revString "12345" == "54321"
    in
    [ test1, test2, test3, test4, test5 ]


firstAnswerTests =
    let
        f x =
            if x > 5 then
                Just (x * 2)

            else
                Nothing

        test1 =
            firstAnswer f [] == Nothing

        test2 =
            firstAnswer f [ 1, 2, 3 ] == Nothing

        test3 =
            firstAnswer f [ 3, 6, 2 ] == Just 12

        test4 =
            firstAnswer f [ 7, 8, 9 ] == Just 14
    in
    [ test1, test2, test3, test4 ]


allAnswersTests =
    let
        f1 n =
            if n > 0 then
                Just [ n, n * 2 ]

            else
                Nothing

        test1 =
            allAnswers f1 [] == Just []

        test2 =
            allAnswers f1 [ 1, 2, 3 ] == Just [ 1, 2, 2, 4, 3, 6 ]

        test3 =
            allAnswers f1 [ 1, -1, 3 ] == Nothing

        test4 =
            allAnswers f1 [ -1, -2 ] == Nothing
    in
    [ test1, test2, test3, test4 ]


countWildcardsTests =
    let
        test1 =
            countWildcards Wildcard == 1

        test2 =
            countWildcards (Variable "x") == 0

        test3 =
            countWildcards (TupleP [ Wildcard, Variable "y", Wildcard ]) == 2

        test4 =
            countWildcards (ConstructorP ( "Just", Wildcard )) == 1

        test5 =
            countWildcards
                (ConstructorP
                    ( "Pair"
                    , TupleP
                        [ Variable "a"
                        , ConstructorP ( "Box", Wildcard )
                        ]
                    )
                )
                == 1
    in
    [ test1, test2, test3, test4, test5 ]


countWildAndVariableLengthsTests =
    let
        test1 =
            countWildAndVariableLengths Wildcard == 1

        test2 =
            countWildAndVariableLengths (Variable "x") == 1

        test3 =
            countWildAndVariableLengths (Variable "abc") == 3

        test4 =
            countWildAndVariableLengths (TupleP [ Wildcard, Variable "hi", Wildcard ]) == 1 + 2 + 1

        test5 =
            countWildAndVariableLengths
                (ConstructorP
                    ( "Just"
                    , TupleP
                        [ Variable "long", Wildcard ]
                    )
                )
                == 4
                + 1
    in
    [ test1, test2, test3, test4, test5 ]


countSomeVarTests =
    let
        test1 =
            countSomeVar ( "x", Wildcard ) == 0

        test2 =
            countSomeVar ( "x", Variable "x" ) == 1

        test3 =
            countSomeVar ( "x", Variable "y" ) == 0

        test4 =
            countSomeVar
                ( "z"
                , TupleP [ Variable "z", Variable "a", Variable "z" ]
                )
                == 2

        test5 =
            countSomeVar
                ( "id"
                , ConstructorP
                    ( "Node"
                    , TupleP [ Variable "id", ConstructorP ( "Leaf", Variable "id" ) ]
                    )
                )
                == 2
    in
    [ test1, test2, test3, test4, test5 ]


checkPatTests =
    let
        test1 =
            checkPat Wildcard == True

        test2 =
            checkPat (Variable "x") == True

        test3 =
            checkPat (TupleP [ Variable "x", Variable "y" ]) == True

        test4 =
            checkPat (TupleP [ Variable "x", Variable "x" ]) == False

        test5 =
            checkPat (ConstructorP ( "C", TupleP [ Variable "a", ConstructorP ( "D", Variable "a" ) ] )) == False

        test6 =
            checkPat
                (ConstructorP
                    ( "Just"
                    , TupleP
                        [ Variable "a"
                        , ConstructorP ( "Box", TupleP [ Variable "b", Variable "c" ] )
                        ]
                    )
                )
                == True
    in
    [ test1, test2, test3, test4, test5, test6 ]


matchTests =
    let
        test1 =
            match ( Const 5, Wildcard ) == Just []

        test2 =
            match ( Const 10, Variable "x" ) == Just [ ( "x", Const 10 ) ]

        test3 =
            match ( Unit, Variable "u" ) == Just [ ( "u", Unit ) ]

        test4 =
            match ( Unit, UnitP ) == Just []

        test5 =
            match ( Const 42, ConstP 42 ) == Just []

        test6 =
            match ( Const 1, ConstP 2 ) == Nothing

        test7 =
            match ( Unit, ConstP 1 ) == Nothing

        test8 =
            match
                ( Tuple [ Const 1, Const 2 ]
                , TupleP [ Variable "a", Variable "b" ]
                )
                == Just [ ( "a", Const 1 ), ( "b", Const 2 ) ]

        test9 =
            match
                ( Tuple [ Const 1 ]
                , TupleP [ Variable "x", Variable "y" ]
                )
                == Nothing

        test10 =
            match
                ( Tuple [ Const 1, Unit ]
                , TupleP [ Variable "x", ConstP 2 ]
                )
                == Nothing

        test11 =
            match
                ( Constructor ( "Some", Const 5 )
                , ConstructorP ( "Some", Variable "v" )
                )
                == Just [ ( "v", Const 5 ) ]

        test12 =
            match
                ( Constructor ( "None", Unit )
                , ConstructorP ( "Some", UnitP )
                )
                == Nothing

        test13 =
            match
                ( Constructor ( "Some", Const 1 )
                , ConstructorP ( "Some", ConstP 2 )
                )
                == Nothing

        test14 =
            match
                ( Tuple [ Const 1, Constructor ( "Some", Const 2 ) ]
                , TupleP [ Variable "a", ConstructorP ( "Some", Variable "b" ) ]
                )
                == Just [ ( "a", Const 1 ), ( "b", Const 2 ) ]

        test15 =
            match ( Const 1, ConstP 1 ) == Just []

        test16 =
            match ( Unit, UnitP ) == Just []
    in
    [ test1
    , test2
    , test3
    , test4
    , test5
    , test6
    , test7
    , test8
    , test9
    , test10
    , test11
    , test12
    , test13
    , test14
    , test15
    , test16
    ]


firstMatchTests =
    let
        test1 =
            firstMatch (Const 42) [ ConstP 42, ConstP 100 ] == Just []

        test2 =
            firstMatch (Const 42) [ ConstP 100, ConstP 42 ] == Just []

        test3 =
            firstMatch (Const 42) [ ConstP 100 ] == Nothing

        test4 =
            firstMatch Unit [ ConstP 42, Wildcard ] == Just []

        test5 =
            firstMatch (Tuple [ Const 1, Const 2 ]) [ TupleP [ ConstP 1, Variable "y" ], Wildcard ]
                == Just [ ( "y", Const 2 ) ]

        test6 =
            firstMatch (Constructor ( "Just", Const 5 )) [ ConstructorP ( "Just", Variable "z" ) ]
                == Just [ ( "z", Const 5 ) ]

        test7 =
            firstMatch (Constructor ( "Nothing", Unit )) [] == Nothing
    in
    [ test1, test2, test3, test4, test5, test6, test7 ]


typecheckPatternsTests : List Bool
typecheckPatternsTests =
    let
        typeEnv =
            [ ( "Just", "Maybe", IntT )
            , ( "Nothing", "Maybe", Anything )
            , ( "Cons", "List", TupleT [ IntT, Datatype "List" ] )
            , ( "Nil", "List", Anything )
            ]

        test1 =
            typecheckPatterns typeEnv [ Variable "x" ] == Just Anything

        test2 =
            typecheckPatterns typeEnv [ Wildcard ] == Just Anything

        test3 =
            typecheckPatterns typeEnv [ UnitP ] == Just UnitT

        test4 =
            typecheckPatterns typeEnv [ ConstP 5 ] == Just IntT

        test5 =
            typecheckPatterns typeEnv [ TupleP [ Variable "x", Wildcard ] ] == Just (TupleT [ Anything, Anything ])

        test6 =
            typecheckPatterns typeEnv [ TupleP [ Wildcard, TupleP [ Wildcard, Wildcard ] ] ] == Just (TupleT [ Anything, TupleT [ Anything, Anything ] ])

        test7 =
            typecheckPatterns typeEnv [ ConstructorP ( "Just", Variable "y" ) ] == Just (Datatype "Maybe")

        test8 =
            typecheckPatterns typeEnv [ ConstructorP ( "Nothing", Wildcard ) ] == Just (Datatype "Maybe")

        test9 =
            typecheckPatterns typeEnv [ ConstructorP ( "Cons", TupleP [ Variable "x", Variable "xs" ] ) ] == Just (Datatype "List")

        test10 =
            typecheckPatterns typeEnv [ ConstructorP ( "Nil", Wildcard ) ] == Just (Datatype "List")

        test11 =
            typecheckPatterns typeEnv [ UnitP, ConstP 5 ] == Nothing
    in
    [ test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11 ]
