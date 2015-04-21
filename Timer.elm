module Timer where

import Char
import Color
import Debug
import Graphics.Collage as Collage
import Graphics.Element as Element
import Graphics.Input.Field as Field
import Keyboard
import Result
import String
import Text
import Time
import Window

main : Signal Element.Element
main =
    Signal.map2 view Window.dimensions model

-- MODEL

type alias Model =
    { setTime:Field.Content
    , elapsed:Time.Time
    , running:Bool
    }

isFinished : Model -> Bool
isFinished state =
    state.elapsed > total state

total : Model -> Time.Time
total { setTime } =
    case String.toFloat setTime.string of
        Result.Ok i -> i * Time.minute
        Result.Err e -> 12 * Time.hour

-- UPDATE

type Update = TimeDelta Time.Time | TimerState Bool | SetTime Field.Content

update : Update -> Model -> Model
update action model =
    case action of
        TimeDelta elapsed -> { model | elapsed <- model.elapsed + elapsed }
        TimerState on -> { model | running <- on }
        SetTime newTime -> { model | setTime <- newTime }

-- Signals

model : Signal Model
model =
    let initialState =
        { setTime =
            { string = "4"
            , selection =
                { start = 1
                , end = 1
                , direction = Field.Forward
                }
            }
        , elapsed = 0
        , running = False
        }
    in Signal.foldp update initialState updates

updates : Signal Update
updates =
    let allDigits content = String.all Char.isDigit content.string
        -- signals
        timerRunning = Signal.foldp xor False Keyboard.space
        timePassed = Time.fpsWhen 30 timerRunning
        setTime =
            setTimeUpdates.signal
            |> Signal.filter allDigits Field.noContent
    in Signal.mergeMany
       [ (Signal.map TimerState timerRunning)
       , (Signal.map TimeDelta timePassed)
       , (Signal.map SetTime setTime)
       ]

setTimeUpdates : Signal.Mailbox Field.Content
setTimeUpdates =
    Signal.mailbox Field.noContent

port ringBell : Signal Bool
port ringBell
    = model
    |> Signal.map isFinished
    |> Signal.dropRepeats

-- VIEW

view : (Int, Int) -> Model -> Element.Element
view (width, height) state =
    Element.flow Element.right
        [ timer (((width * 2) // 3), height) state
        , settings ((width // 3), height) state
        ]

-- timer

timer : (Int, Int) -> Model -> Element.Element
timer (width, height) state =
    let backgroundColor = if isFinished state then base02 else base2
    in Element.container width height Element.middle (face state)
       |> Element.color backgroundColor

face : Model -> Element.Element
face state =
    Collage.collage totalDiameter totalDiameter
        [ Collage.filled blue (Collage.circle faceRadius)
        , marker state
        ]

marker : Model -> Collage.Form
marker state =
    let fractionElapsed = state.elapsed / total state
        locationAngle = (degrees 90) - (turns fractionElapsed)
        location = fromPolar (faceRadius, locationAngle)
        markerAngle = (degrees 270) - (turns fractionElapsed)
        color = if isFinished state then base2 else base02
        shape = if state.running
                   then Collage.ngon 3 markerRadius
                   else Collage.ngon 6 (markerRadius / 2)
    in shape
       |> Collage.filled color
       |> Collage.rotate markerAngle
       |> Collage.move location

-- settings

settings : (Int, Int) -> Model -> Element.Element
settings (width, height) state =
    let finished = isFinished state
        backgroundColor = if finished then base03 else base3
        textStyle =
            Text.Style
                [ "Source Code Pro", "Courier New", "monospace" ]
                (Just 16)
                (if finished then base00 else base0)
                False
                False
                Nothing
        fieldStyle =
            Field.Style
                (Field.uniformly 0)
                (Field.Outline blue (Field.uniformly 4) 0)
                Field.noHighlight
                textStyle
    in state.setTime
       |> Field.field fieldStyle (Signal.message setTimeUpdates.address) "set timer"
       |> Element.color backgroundColor
       |> Element.container width height Element.middle 
       |> Element.color backgroundColor

-- Sizes

faceRadius = 120
markerRadius = 20
totalDiameter = 2 * (faceRadius + markerRadius)

-- Colors http://ethanschoonover.com/solarized

base03 = Color.rgb 00 43 54 -- dark primary background
base02 = Color.rgb 7 54 66 -- dark alternate background
base01 = Color.rgb 88 110 117 -- light alternate text / dark de-emphasized text
base00 = Color.rgb 101 123 131 -- light primary text
base0 = Color.rgb 131 148 150 -- dark primary text
base1 = Color.rgb 147 161 161 -- dark alternate text / light de-emphasized text
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
