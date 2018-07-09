module Calc exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (Maybe)
import Regex

type alias Stack n = List n

stackPush : Stack e -> e -> Stack e
stackPush stack e =
  e :: stack

stackPop : Stack e -> (Maybe e, Stack e)
stackPop stack =
  case stack of
    hd :: tail -> (Just hd, tail)
    _ -> (Nothing, stack)

type Token = TFloat Float | TChar Char

type alias Model =
  {
    input : String
  }

type Msg =
  Input String

apply : Char -> Float -> Float -> Float
apply op op1 op2 =
  case op of
    '+' -> op1 + op2
    '-' -> op1 - op2
    '*' -> op1 * op2
    '/' -> op1 / op2
    _ -> 0

parseInfix : List Token -> List Token
parseInfix tokens =
  let
    precedenceOf : Char -> Int
    precedenceOf c =
      case c of 
        '*' -> 2
        '/' -> 2
        _ -> 0

    isOperator : Char -> Bool
    isOperator c =
      String.toList "+-*/" |> List.member c

    parseToken token acc =
      case token of
        TFloat n ->
          { acc | outputQueue = acc.outputQueue ++ [token] }
        TChar '(' ->
          { acc | operatorStack = stackPush acc.operatorStack '(' }
        TChar ')' ->
          case stackPop acc.operatorStack of
            (Just '(', stack) ->
              { acc | operatorStack = stack }
            (Just op2, stack) ->
              parseToken token { acc | operatorStack = stack, outputQueue = acc.outputQueue ++ [TChar op2] }
            (Nothing, _) ->
              acc

        TChar op ->
          case stackPop acc.operatorStack of
            (Just op2, stack) ->
              if (precedenceOf op2) >= precedenceOf op && op2 /= '(' then
                parseToken token { acc | operatorStack = stack, outputQueue = acc.outputQueue ++ [TChar op2] }
              else
                { acc | operatorStack = stackPush acc.operatorStack op }
            (Nothing, _) ->
              { acc | operatorStack = stackPush acc.operatorStack op }

    parsedTokens =
      List.foldl parseToken { outputQueue = [], operatorStack = [] } tokens
  in
      parsedTokens.outputQueue ++ (List.map TChar parsedTokens.operatorStack)

eval : List Token -> Maybe Float
eval outputQueue =
  let rpnFold token stack =
      case stack of
        Just stack ->
          case token of
            TFloat n ->
              stackPush stack token
              |> Just
            TChar op ->
              case stack of
                (TFloat op2) :: (TFloat op1) :: tail ->
                  apply op op1 op2
                  |> TFloat
                  |> stackPush tail
                  |> Just
                _ -> Nothing

        _ -> Nothing

      stack = List.foldl rpnFold (Just []) outputQueue
  in
      case stack of
        Just ([TFloat n]) -> Just n
        _ -> Nothing

initialModel : Model
initialModel =
  { input = "" }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Input content -> { model | input = content }

resultInput : String -> Html Msg
resultInput str =
  input [ disabled False, value str, onInput Input ] []

btn : String -> Msg -> Html Msg
btn label msg =
  button [ onClick msg, href "javascript:void()" ] [ text label ]

br : Html Msg
br = Html.br [] []

tokenToString : Token -> String
tokenToString token =
  case token of
    TFloat c -> toString c
    TChar c -> String.fromChar c

view : Model -> Html Msg
view model =
  let
      result =
        model.input |> parseString |> parseInfix |> eval

      resultStr =
        case result of
          Just n -> n |> toString
          Nothing -> "0"
  in
      div
      []
      [
        resultInput model.input
      , br, resultInput resultStr
      ]

parseString : String -> List Token
parseString str =
  let
      elRegexStr =
        "(\\d+(?:\\.\\d+)?|\\s+|[*)(/]|\\-|\\+)"

      stripWhitespace : String -> String
      stripWhitespace str =
        let
            regex = Regex.regex "\\s+"
            strip m = ""
        in
            Regex.replace Regex.All regex strip str

      elRegex =
        Regex.regex elRegexStr

      findTokens =
        Regex.find Regex.All elRegex

      tokenize : String -> Maybe Token
      tokenize str =
        case String.toFloat str of
          Ok n -> Just (TFloat n)
          _ ->
            case String.toList str |> List.head of
              Just c -> Just (TChar c)
              Nothing -> Nothing
  in
     findTokens str
     |> List.map (\m -> stripWhitespace m.match)
     |> List.filter (String.isEmpty >> not)
     |> List.filterMap tokenize

main =
  Html.beginnerProgram
  {
    model = initialModel,
    view = view,
    update = update
  }
