import Color
import Graphics.Element as Element
import Graphics.Collage as Collage
import Keyboard
import Signal
import Text
import Time
import Window

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
markerRadius = 20
totalDiameter = 2 * (faceRadius + markerRadius)

finished state =
    state.elapsed > state.total

marker : Model -> Collage.Form
marker state =
    let fractionElapsed = state.elapsed / state.total
        locationAngle = (degrees 90) - (turns fractionElapsed)
        location = fromPolar (faceRadius, locationAngle)
        markerAngle = (degrees 270) - (turns fractionElapsed)
        color = if finished state then base2 else base02
        shape = if state.running
                   then Collage.ngon 3 markerRadius
                   else Collage.ngon 6 (markerRadius / 2)
    in shape
       |> Collage.filled color
       |> Collage.rotate markerAngle
       |> Collage.move location

face : Model -> Element.Element
face state =
    Collage.collage totalDiameter totalDiameter
        [ Collage.filled blue (Collage.circle faceRadius)
        , marker state
        ]

view : (Int, Int) -> Model -> Element.Element
view (width, height) state =
    let backgroundColor = if finished state then base02 else base2
    in Element.container width height Element.middle (face state)
       |> Element.color backgroundColor

main : Signal Element.Element
main =
    Signal.map2 view Window.dimensions model

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

-- COLORS http://ethanschoonover.com/solarized

base03 = Color.rgb 00 43 54 -- dark primary background
base02 = Color.rgb 7 54 66 -- dark alternate background
base01 = Color.rgb 88 110 117 -- light alternate text
base00 = Color.rgb 101 123 131 -- light primary text
base0 = Color.rgb 131 148 150 -- dark primary text
base1 = Color.rgb 147 161 161 -- dark alternate text
base2 = Color.rgb 238 232 213 -- light alternate background
base3 = Color.rgb 253 246 227 -- light primary background
yellow = Color.rgb 181 137 0
orange = Color.rgb 203 75 22
red = Color.rgb 220 50 47
magenta = Color.rgb 211 54 130
violet = Color.rgb 108 113 196
blue = Color.rgb 38 139 210
cyan = Color.rgb 42 161 152
green = Color.rgb 133 153 0
