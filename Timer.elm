import Color
import Graphics.Element as Element
import Graphics.Collage as Collage
import Keyboard
import Signal
import Text
import Time

-- MODEL

type alias Model =
    { total:Time.Time
    , elapsed:Time.Time
    , running:Bool
    } 

-- UPDATE

update : (Time.Time, Bool) -> Model -> Model
update (delta, running) model =
    { total = model.total
    , elapsed = model.elapsed + delta
    , running = running
    }

-- VIEW

faceRadius = 120
markerRadius = 10
totalDiameter = 2 * (faceRadius + markerRadius)

marker state =
    let fractionElapsed = state.elapsed / state.total
        angle = turns fractionElapsed
        location = fromPolar (faceRadius, angle)
        color = if state.running then Color.black else Color.darkGrey
    in Collage.circle markerRadius
       |> Collage.filled color
       |> Collage.move location

face state =
    Collage.collage totalDiameter totalDiameter
        [ Collage.filled Color.lightBlue (Collage.circle faceRadius)
        , marker state
        ]

view state = 
    Element.flow Element.down
        [ face state
        , Text.asText state
        ]

main =
    Signal.map view model

-- SIGNALS

initialState : Model
initialState =
    { total = 30 * Time.second
    , elapsed = 0 * Time.millisecond
    , running = False
    }

model : Signal Model
model =
    Signal.foldp update initialState timerUpdate

timerUpdate : Signal (Time.Time, Bool)
timerUpdate =
    Signal.map2
        (,)
        (Time.fpsWhen 60 timerRunning) -- TODO: does this do the right thing when the other signal happens?
        timerRunning

timerRunning : Signal Bool
timerRunning =
    Signal.foldp xor False Keyboard.space
