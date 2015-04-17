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

type Update = TimeDelta Time.Time | TimerState Bool

update : Update -> Model -> Model
update action model =
    case action of
        TimeDelta elapsed -> { model | elapsed <- model.elapsed + elapsed }
        TimerState on -> { model | running <- on }

-- VIEW

faceRadius = 120
markerRadius = 10
totalDiameter = 2 * (faceRadius + markerRadius)

marker : Model -> Collage.Form
marker state =
    let fractionElapsed = state.elapsed / state.total
        angle = turns fractionElapsed
        location = fromPolar (faceRadius, angle)
        color = if state.running then Color.black else Color.darkGrey
    in Collage.circle markerRadius
       |> Collage.filled color
       |> Collage.move location

face : Model -> Element.Element
face state =
    Collage.collage totalDiameter totalDiameter
        [ Collage.filled Color.lightBlue (Collage.circle faceRadius)
        , marker state
        ]

view : Model -> Element.Element
view state = 
    Element.flow Element.down
        [ face state
        , Text.asText state
        ]

main : Signal Element.Element
main =
    Signal.map view model

-- SIGNALS

timerUpdate : Signal Update
timerUpdate =
    let timerRunning = Signal.foldp xor False Keyboard.space
        timePassed = Time.fpsWhen 60 timerRunning
    in Signal.merge
       (Signal.map TimerState timerRunning)
       (Signal.map TimeDelta timePassed)

initialState : Model
initialState =
    { total = 30 * Time.second
    , elapsed = 0 * Time.millisecond
    , running = False
    }

model : Signal Model
model =
    Signal.foldp update initialState timerUpdate
