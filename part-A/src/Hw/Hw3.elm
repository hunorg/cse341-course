module Hw.Hw3 exposing (..)

import LectureNotes.VariableBindingsAndExpressions exposing (x)


type Pattern
    = Wildcard
    | Variable String
    | UnitP
    | ConstP Int
    | TupleP (List Pattern)
    | ConstructorP ( String, Pattern )


type Valu
    = Const Int
    | Unit
    | Tuple (List Valu)
    | Constructor ( String, Valu )


g : (() -> Int) -> (String -> Int) -> Pattern -> Int
g f1 f2 p =
    let
        r : Pattern -> Int
        r =
            g f1 f2
    in
    case p of
        Wildcard ->
            f1 ()

        Variable x ->
            f2 x

        TupleP ps ->
            List.foldl (\subP acc -> r subP + acc) 0 ps

        ConstructorP ( _, subP ) ->
            r subP

        _ ->
            0


onlyCapitals : List String -> List String
onlyCapitals xs =
    let
        startsWithUpper : String -> Bool
        startsWithUpper s =
            case String.toList s of
                [] ->
                    False

                y :: _ ->
                    Char.isUpper y
    in
    List.filter startsWithUpper xs


longestString1 : List String -> String
longestString1 xs =
    List.foldl
        (\x y ->
            if String.length x > String.length y then
                x

            else
                y
        )
        ""
        xs


longestString2 : List String -> String
longestString2 xs =
    List.foldl
        (\x y ->
            if String.length x >= String.length y then
                x

            else
                y
        )
        ""
        xs


longestStringHelper : (Int -> Int -> Bool) -> List String -> String
longestStringHelper f =
    List.foldl
        (\x y ->
            let
                lenX =
                    String.length x

                lenY =
                    String.length y
            in
            if f lenX lenY then
                x

            else
                y
        )
        ""


longestString3 : List String -> String
longestString3 =
    longestStringHelper (\lenX lenY -> lenX > lenY)


longestString4 : List String -> String
longestString4 =
    longestStringHelper (\lenX lenY -> lenX >= lenY)


longestCapitalized : List String -> String
longestCapitalized =
    onlyCapitals >> longestString3


revString : String -> String
revString =
    String.toList >> List.reverse >> String.fromList


firstAnswer : (a -> Maybe b) -> List a -> Maybe b
firstAnswer f xs =
    case xs of
        [] ->
            Nothing

        x :: xs_ ->
            if f x == Nothing then
                firstAnswer f xs_

            else
                f x



-- firstAnswer returns Maybe b cause there are no exceptions in Elm


allAnswers : (a -> Maybe (List b)) -> List a -> Maybe (List b)
allAnswers f xs =
    let
        aux ys acc =
            case ys of
                [] ->
                    Just acc

                y :: ys_ ->
                    case f y of
                        Just zs ->
                            aux ys_ (acc ++ zs)

                        _ ->
                            Nothing
    in
    aux xs []



{-
   The function g takes two functions and a pattern as arguments.
   The first function (f1) is applied to each Wildcard pattern,
   and the second function (f2) is applied to each Variable pattern’s name.
   g returns a number that combines the results of these applications
   over the entire pattern.
-}


countWildcards : Pattern -> Int
countWildcards =
    g (\() -> 1) (\_ -> 0)


countWildAndVariableLengths : Pattern -> Int
countWildAndVariableLengths =
    g (\() -> 1) (\s -> String.length s)


countSomeVar : ( String, Pattern ) -> Int
countSomeVar ( s, p ) =
    g (\() -> 0)
        (\var ->
            if s == var then
                1

            else
                0
        )
        p


checkPat : Pattern -> Bool
checkPat pat =
    let
        stringCollector : Pattern -> List String
        stringCollector p =
            case p of
                Variable x ->
                    [ x ]

                TupleP ps ->
                    List.foldl (\subP collected -> stringCollector subP ++ collected) [] ps

                ConstructorP ( _, ptn ) ->
                    stringCollector ptn

                _ ->
                    []

        hasRepeats : List a -> Bool
        hasRepeats xs =
            case xs of
                [] ->
                    False

                x :: xs_ ->
                    if List.any (\y -> y == x) xs_ then
                        True

                    else
                        hasRepeats xs_
    in
    stringCollector pat |> hasRepeats |> not


match : ( Valu, Pattern ) -> Maybe (List ( String, Valu ))
match ( v, p ) =
    case p of
        Wildcard ->
            Just []

        Variable name ->
            Just [ ( name, v ) ]

        UnitP ->
            if v == Unit then
                Just []

            else
                Nothing

        ConstP x ->
            case v of
                Const y ->
                    if x == y then
                        Just []

                    else
                        Nothing

                _ ->
                    Nothing

        TupleP ps ->
            case v of
                Tuple vs ->
                    if List.length ps == List.length vs then
                        allAnswers (\( x, y ) -> match ( x, y )) (List.map2 Tuple.pair vs ps)

                    else
                        Nothing

                _ ->
                    Nothing

        ConstructorP ( x, pat ) ->
            case v of
                Constructor ( y, val ) ->
                    if x == y then
                        match ( val, pat )

                    else
                        Nothing

                _ ->
                    Nothing


firstMatch : Valu -> List Pattern -> Maybe (List ( String, Valu ))
firstMatch v ps =
    firstAnswer (\p -> match ( v, p )) ps



-- CHALLENGE PROBLEM:


type Typ
    = Anything
    | UnitT
    | IntT
    | TupleT (List Typ)
    | Datatype String


typecheckPatterns : List ( String, String, Typ ) -> List Pattern -> Maybe Typ
typecheckPatterns typeEnvArg psArg =
    let
        getConstructorInfo : String -> List ( String, String, Typ ) -> Maybe ( String, Typ )
        getConstructorInfo cName typeEnv =
            case typeEnv of
                [] ->
                    Nothing

                ( constructorName, datatypeName, argumentType ) :: typeEnv_ ->
                    if cName == constructorName then
                        Just ( datatypeName, argumentType )

                    else
                        getConstructorInfo cName typeEnv_

        getCommonType : Typ -> Typ -> Maybe Typ
        getCommonType t1 t2 =
            case ( t1, t2 ) of
                ( Anything, _ ) ->
                    Just Anything

                ( _, Anything ) ->
                    Just Anything

                ( UnitT, UnitT ) ->
                    Just UnitT

                ( IntT, IntT ) ->
                    Just IntT

                ( TupleT ts1, TupleT ts2 ) ->
                    if List.length ts1 == List.length ts2 then
                        case allAnswers (\( typ1, typ2 ) -> getCommonType typ1 typ2 |> Maybe.map (\t -> [ t ])) (List.map2 Tuple.pair ts1 ts2) of
                            Just ts ->
                                Just (TupleT ts)

                            _ ->
                                Nothing

                    else
                        Nothing

                ( Datatype d1, Datatype d2 ) ->
                    if d1 == d2 then
                        Just (Datatype d1)

                    else
                        Nothing

                _ ->
                    Nothing

        getTyp : Pattern -> Maybe Typ
        getTyp p =
            case p of
                Wildcard ->
                    Just Anything

                Variable _ ->
                    Just Anything

                UnitP ->
                    Just UnitT

                ConstP _ ->
                    Just IntT

                TupleP ps ->
                    case allAnswers (\pat -> getTyp pat |> Maybe.map (\t -> [ t ])) ps of
                        Just ts ->
                            Just (TupleT ts)

                        _ ->
                            Nothing

                ConstructorP ( cName, pat ) ->
                    case ( getConstructorInfo cName typeEnvArg, getTyp pat ) of
                        ( Just ( datatypeName, argumentType ), Just patTyp ) ->
                            case getCommonType argumentType patTyp of
                                Just _ ->
                                    Just (Datatype datatypeName)

                                _ ->
                                    Nothing

                        _ ->
                            Nothing
    in
    case allAnswers (\p -> getTyp p |> Maybe.map (\t -> [ t ])) psArg of
        Just (t :: ts) ->
            List.foldl
                (\nextTyp accMaybeTyp ->
                    Maybe.andThen (\accTyp -> getCommonType accTyp nextTyp) accMaybeTyp
                )
                (Just t)
                ts

        _ ->
            Nothing
