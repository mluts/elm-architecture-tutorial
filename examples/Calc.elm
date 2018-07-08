module Calc exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (Maybe)

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
    currentNumber : Maybe Float,
    tokens : List Token
  }

type Msg =
  Digit Char | Op Char | Cmd Char


charIntoNumber : Char -> Float -> Maybe Float
charIntoNumber c currentNumber =
  let newnumberStr = (toString currentNumber) ++ (String.fromChar c)
  in
      case String.toFloat newnumberStr of
        Ok newnumber -> Just newnumber
        _ -> Nothing

updateCurrentNumber : Char -> Model -> Model
updateCurrentNumber c model =
  case model.currentNumber of
    Just n ->
      { model | currentNumber = (charIntoNumber c n) }
    Nothing ->
      case String.fromChar c |> String.toFloat of
        Ok n ->
          { model | currentNumber = Just n }
        _ ->
          model

precedenceOf : Char -> Int
precedenceOf c =
  case c of 
    '*' -> 2
    '/' -> 2
    _ -> 0

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
  let parseToken token acc =
        case token of
          TFloat n ->
            { acc | outputQueue = acc.outputQueue ++ [token] }
          TChar op ->
            case stackPop acc.operatorStack of
              (Just op2, stack) ->
                if (precedenceOf op2) >= precedenceOf op then
                  parseToken token { acc | operatorStack = stack, outputQueue = acc.outputQueue ++ [TChar op2] }
                else
                  { acc | operatorStack = stackPush acc.operatorStack op }
              (Nothing, _) ->
                { acc | operatorStack = stackPush acc.operatorStack op }
      parsedTokens = List.foldl parseToken { outputQueue = [], operatorStack = [] } tokens
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
  { tokens = [], currentNumber = Nothing }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Digit c ->
      updateCurrentNumber c model
    Op c ->
      case model.currentNumber of
        Just n ->
          { model | tokens = model.tokens ++ [TFloat n, TChar c], currentNumber = Nothing }
        Nothing ->
          { model | tokens = model.tokens ++ [TChar c] }
    Cmd 'D' ->
      case (model.currentNumber, List.isEmpty model.tokens) of
        (Just n, _) ->
          { model | currentNumber = Nothing }
        (Nothing, False) ->
          { model | tokens = List.take (List.length model.tokens - 1) model.tokens }
        _ -> model
    Cmd 'C' -> initialModel
    _ -> model

resultInput : String -> Html Msg
resultInput str =
  input [ disabled True, value str ] []

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
      tokens =
        case model.currentNumber of
          Just n -> model.tokens ++ [TFloat n]
          Nothing -> model.tokens

      tokensStr = List.map tokenToString tokens |> String.join ""

      result =
        case parseInfix tokens |> eval of
          Just n -> toString n
          Nothing -> ""
  in
      div
      []
      [
        resultInput tokensStr
      , br, resultInput result
      , br, btn "1" (Digit '1'), btn "2" (Digit '2'), btn "3" (Digit '3')
      , br, btn "4" (Digit '4'), btn "5" (Digit '5'), btn "6" (Digit '6')
      , br, btn "7" (Digit '7'), btn "8" (Digit '8'), btn "9" (Digit '9')
      , br, btn "0" (Digit '0')
      , br, btn "+" (Op '+'), btn "-" (Op '-')
      , br, btn "*" (Op '*'), btn "/" (Op '/')
      , br, btn "C" (Cmd 'C'), btn "Del" (Cmd 'D')
      ]

main =
  Html.beginnerProgram
  {
    model = initialModel,
    view = view,
    update = update
  }
