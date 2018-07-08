import Html exposing (Html, div, text, h1, img)
import Html.Attributes exposing (id, class, src, classList)
import Html.Events exposing (onClick)

urlPrefix =
  "http://elm-in-action.com/"

header : String -> Html msg
header str = h1 [] [ text str ]

view model =
  div [ class "content" ]
      [ header "Gallery"
      , thumbnails model.selectedUrl model.photos
      , bigPicture model.selectedUrl
      ]

bigPicture url =
  img [ class "large"
      , src (urlPrefix ++ "large/" ++ url)
      ]
      []

thumbnails selectedUrl photos =
  div [ id "thumbnails" ]
      (List.map (viewThumbnail selectedUrl) photos)

viewThumbnail selectedUrl thumbnail =
  img [ src (urlPrefix ++ thumbnail.url)
      , classList [ ("selected", selectedUrl == thumbnail.url) ]
      , onClick { operation = "SELECT_PHOTO", data = thumbnail.url }
      ]
      []

initialModel =
  {
    photos =
      [ { url = "1.jpeg" }
      , { url = "2.jpeg" }
      , { url = "3.jpeg" }
      ]
      , selectedUrl = "1.jpeg"
  }

update msg model =
  if msg.operation == "SELECT_PHOTO" then
    { model | selectedUrl = msg.data }
  else
    model

main =
  Html.beginnerProgram
  { model = initialModel
  , view = view
  , update = update
  }
