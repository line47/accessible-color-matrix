port module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Color.Convert exposing (colorToHex)

import Palette exposing (
  Palette, PaletteMsg, SerializedPalette, paletteUl,
  updatePalette, deserializePalette, serializePalette,
  arePaletteEditsValid
  )
import Matrix exposing (matrixDiv)

type Message =
  PaletteMessage PaletteMsg
  | LoadPalette SerializedPalette
  | StartEditing
  | FinishEditing
  | CancelEditing

type alias Model =
  { palette: Palette
  , isEditing: Bool
  , lastPalette: Palette
  }

port updateQs : SerializedPalette -> Cmd msg

port qsUpdated : (SerializedPalette -> msg) -> Sub msg

port updateFavicon : List String -> Cmd msg

defaultPalette : SerializedPalette
defaultPalette =
  [ ("white", "ffffff")
  , ("gray-lightest", "F1F1F1")
  , ("gray-lighter", "D6D7D9")
  , ("gray-light", "AEB0B5")
  , ("gray", "5B616B")
  , ("gray-dark", "323A45")
  , ("base", "212121")
  , ("primary", "0071BC")
  , ("primary-darker", "205493")
  , ("primary-alt-darkest", "046B99")
  , ("primary-alt-dark", "00A6D2")
  , ("primary-alt", "02BFE7")
  , ("primary-alt-light", "9BDAF1")
  , ("primary-alt-lightest", "E1F3F8")
  , ("success", "2E8540")
  , ("success-light", "4AA564")
  , ("success-lighter", "94BFA2")
  , ("success-lightest", "E7F4E4")
  , ("warn", "FDB81E")
  , ("warn-light", "F9C642")
  , ("warn-lighter", "FAD980")
  , ("warn-lightest", "FFF1D2")
  , ("error-darkest", "981B1E")
  , ("error-dark", "CD2026")
  , ("error", "E31C3D")
  , ("error-light", "E59393")
  , ("error-lighter", "EFB9B9")
  , ("error-lightest", "F9DEDE")
  ]

actions : Model -> Html Message
actions model =
  let
    edit =
      [ button [ onClick StartEditing ] [ text "Edit palette" ] ]

    -- TODO: If enter/esc is pressed in a field while editing, it
    -- should have the same effect as pressing the save/cancel buttons.
    -- Well, at least enter should, since it's easily undoable.
    saveOrCancel =
      [ button
        ([ onClick FinishEditing ] ++
          if arePaletteEditsValid model.palette
            then [] else [ disabled True, class "usa-button-disabled" ])
        [ text "Save changes" ]
      , button [ onClick CancelEditing
               , class "usa-button-secondary" ] [ text "Cancel" ]
      ]
  in
    div [ class "usa-grid-full usa-color-row" ]
      (if model.isEditing then saveOrCancel else edit)

view : Model -> Html Message
view model =
  div []
    [ h1 [] [ text "Accessible color palette builder" ]
    , Html.map (\m -> PaletteMessage m)
      (paletteUl model.palette model.isEditing)
    , actions model
    , h2 [] [ text "Accessible color combinations" ]
    , matrixDiv model.palette
    ]

updateFaviconFromPalette : Palette -> Cmd msg
updateFaviconFromPalette palette =
  List.map .color palette
    |> List.map colorToHex
    |> updateFavicon

update : Message -> Model -> (Model, Cmd msg)
update message model =
  case message of
    PaletteMessage msg ->
      let
        newPalette : Palette
        newPalette = updatePalette msg model.palette
      in
        ({model | palette = newPalette}, Cmd.none)
    LoadPalette palette ->
      let
        newPalette : Palette
        newPalette = getPaletteOrDefault palette
      in
        ({model | palette = newPalette
                , isEditing = False},
          updateFaviconFromPalette newPalette)
    StartEditing ->
      ({model | isEditing = True
              , lastPalette = model.palette}, Cmd.none)
    FinishEditing ->
      ({model | isEditing = False},
        Cmd.batch [ updateQs (serializePalette model.palette)
                  , updateFaviconFromPalette model.palette ])
    CancelEditing ->
      ({model | isEditing = False
              , palette = model.lastPalette}, Cmd.none)

getPaletteOrDefault : SerializedPalette -> Palette
getPaletteOrDefault palette =
  if List.length palette == 0
    then deserializePalette defaultPalette
    else deserializePalette palette

init : SerializedPalette -> (Model, Cmd msg)
init qsPalette =
  let
    palette = getPaletteOrDefault qsPalette
  in
    ({ palette = palette
     , isEditing = False
     , lastPalette = [] },
     updateFaviconFromPalette palette)

subscriptions : Model -> Sub Message
subscriptions model =
  qsUpdated LoadPalette

main =
  Html.programWithFlags
    { init = init
    , subscriptions = subscriptions
    , view = view
    , update = update }
