module Matrix exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, scope, style, title)
import Color exposing (white)

import Symbols exposing (symbols, badContrastSvg)
import Accessibility exposing (ariaHidden, role)
import ContrastRatio exposing (
  contrastRatio, areColorsIndistinguishable, humanFriendlyContrastRatio
  )
import Palette exposing (
  Palette, PaletteEntry, paletteEntryHex, squareBgStyle
  )

badContrastLegendText : String
badContrastLegendText = """
  Please don't use these color combinations; they do not meet a color
  contrast ratio of 4.5:1, so they do not conform with the standards of
  Section 508 for body text. This means that some people would have
  difficulty reading the text. Employing accessibility best practices
  improves the user experience for all users.
"""

badContrastText : PaletteEntry -> PaletteEntry -> Float -> String
badContrastText background foreground ratio =
  foreground.name ++ " text is not 508-compliant, with a failing contrast ratio of " ++
  (humanFriendlyContrastRatio ratio) ++ "."

goodContrastText : PaletteEntry -> PaletteEntry -> Float -> String
goodContrastText background foreground ratio =
  "The contrast ratio of " ++ foreground.name ++ " on " ++ background.name ++
    " is " ++ (humanFriendlyContrastRatio ratio) ++ "."

legend : Html msg
legend =
  div [ class "usa-matrix-legend" ]
    [ badContrastSvg ""
    , p [ class "usa-sr-invisible", ariaHidden True ]
        [ Html.text badContrastLegendText ]
    ]

capFirst : String -> String
capFirst str =
  (String.toUpper (String.left 1 str)) ++ (String.dropLeft 1 str)

matrixTableHeader : Palette -> Html msg
matrixTableHeader palette =
  let
    headerCell : PaletteEntry -> Html msg
    headerCell entry =
      li [ ]
        [ div [ class "usa-matrix-desc" ]
          [ text (capFirst entry.name)
          , text " text"
          , br [] []
          , small [] [ text (paletteEntryHex entry) ]
          ]
        
        ]
  in
    div []
      [ ul []
        ([ li [ ] [] ] ++ List.map headerCell palette)
      ]

matrixTableRow : Palette -> Html msg
matrixTableRow palette =
  let

    rowHeaderCell : PaletteEntry -> Html msg
    rowHeaderCell entry =
      li [ class "grid-item grid-item-main" ]
        [ div [ class "swatch", style (squareBgStyle entry) ]
           [ div [ class "usa-color-inner-content" ]
            [ text (capFirst entry.name)
            , br [] []
            , small [] [ text (paletteEntryHex entry) ]
            ]
          ]
        ]

    rowComboCell : PaletteEntry -> PaletteEntry -> Html msg
    rowComboCell background foreground =
      let
        ratio : Float
        ratio = contrastRatio background.color foreground.color

        validCell : Html msg
        validCell =
          li [ class "grid-item usa-matrix-valid-color-combo", style (squareBgStyle background) ]
            [ 
             div [ class "usa-matrix-color-combo-description", style [("color", paletteEntryHex foreground)]  ]
              [ strong [] [ text (capFirst foreground.name) ]
              , text " text on "
              , strong [] [ text (capFirst background.name) ]
              , text " background"
              , span [ class "" ]
                [ text " is 508-compliant, with a contrast ratio of "
                , text (humanFriendlyContrastRatio ratio)
                , text "."
                ]
              ]
            ]

        invalidCell : Html msg
        invalidCell =
          let
            desc = badContrastText background foreground ratio
          in
            li [ class "grid-item usa-matrix-invalid-color-combo" ]
              [ div [ role "presentation", title desc ]
                [ badContrastSvg "usa-matrix-square" ]
              , div [ class "" ] [ text desc ]
              ]
      in
        if ratio >= 4.5 then validCell else invalidCell

    row : Palette -> PaletteEntry -> Html msg
    row palette background =
      ul [ class "grid" ]
        ([ rowHeaderCell background ] ++
          List.map (rowComboCell background) palette)
  in
    div [] (List.map (row palette) (List.reverse palette))

matrixTable : Palette -> Html msg
matrixTable palette =
  div [ class "usa-table-borderless usa-matrix" ]
    [ matrixTableHeader palette
    , matrixTableRow palette
    ]

matrixDiv : Palette -> Html msg
matrixDiv palette =
  div []
    [ symbols
    , legend
    , matrixTable palette
    ]
