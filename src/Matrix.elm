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
  Please don't use these color combinations; they do not meet a 3.1 color
  contrast ratio. This means that some people would have
  difficulty reading the text. Employing accessibility best practices
  improves the user experience for all users.
"""

badContrastText : PaletteEntry -> PaletteEntry -> Float -> String
badContrastText background foreground ratio =
  foreground.name ++ " text is not compliant, with a failing contrast ratio of " ++
  (humanFriendlyContrastRatio ratio) ++ "."

goodContrastText : PaletteEntry -> PaletteEntry -> Float -> String
goodContrastText background foreground ratio =
  "The contrast ratio of " ++ foreground.name ++ " on " ++ background.name ++
    " is " ++ (humanFriendlyContrastRatio ratio) ++ "."

legend : Html msg
legend =
  div [ class "usa-matrix-legend" ]
    [  p [ class "usa-sr-invisible", ariaHidden True ]
        [ Html.text badContrastLegendText ]
    ]

capFirst : String -> String
capFirst str =
  (String.toUpper (String.left 1 str)) ++ (String.dropLeft 1 str)


matrixTableHeader palette =
  let
    headerCell : PaletteEntry -> Html msg
    headerCell entry =
      li [ ]
        [ div [ ]
          [ 
          ]
        
        ]
  in
    div []
      [ 
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
            , text " background "
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

        fourFiveValidCell : Html msg
        fourFiveValidCell =
          li [ class "grid-item usa-matrix-valid-color-combo valid-4-5", style (squareBgStyle background) ]
            [ 
             div [ class "usa-matrix-color-combo-description", style [("color", paletteEntryHex foreground)]  ]
              [ text (capFirst foreground.name) 
              , text ": "
              , span [] [ text (paletteEntryHex foreground) ]
              , span [class "tiny-swatch", style (squareBgStyle foreground)] [  ]
              , br [] []
              , text (capFirst background.name) 
              , text ": "
              , span [] [ text (paletteEntryHex background) ]
              , br [] []
              , span [ class "" ]
                [ text "Ratio: "
                , text (humanFriendlyContrastRatio ratio)
                ]
              ]
            ]

        threeOneValidCell : Html msg
        threeOneValidCell =
          li [ class "grid-item usa-matrix-valid-color-combo valid-3-1", style (squareBgStyle background) ]
            [ 
             div [ class "usa-matrix-color-combo-description", style [("color", paletteEntryHex foreground)]  ]
             [ text (capFirst foreground.name) 
              , text ": "
              , span [] [ text (paletteEntryHex foreground) ]
              , span [class "tiny-swatch", style (squareBgStyle foreground)] [  ]
              , br [] []
              , text (capFirst background.name) 
              , text ": "
              , span [] [ text (paletteEntryHex background) ]
              , br [] []
              , span [ class "" ]
                [ text "Ratio: "
                , text (humanFriendlyContrastRatio ratio)
                ]
              ]
            ]


        invalidCell : Html msg
        invalidCell =
          let
            desc = badContrastText background foreground ratio
          in
            li [ class "grid-item usa-matrix-invalid-color-combo invalid" ]
               [ text (capFirst foreground.name) 
              , text ": "
              , span [] [ text (paletteEntryHex foreground) ]
              , span [class "tiny-swatch", style (squareBgStyle foreground)] [  ]
              , br [] []
              , text (capFirst background.name) 
              , text ": "
              , span [] [ text (paletteEntryHex background) ]
              , br [] []
              , span [ class "" ]
                [ text "Ratio: "
                , text (humanFriendlyContrastRatio ratio)
                ]
              ]
      in
        if ratio >= 4.5 then 
            fourFiveValidCell  
          else if (ratio >= 3.1) && (ratio < 4.5) then 
            threeOneValidCell else 
          invalidCell

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
