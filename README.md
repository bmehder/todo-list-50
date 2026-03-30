# 🧠 Elm Todo App with Time Travel Debugger

This project is a Todo application built in Elm with a custom, app-agnostic Time Travel Debugger.

It demonstrates a powerful architectural idea:

> An app can remain pure and simple, while additional behavior (like time travel) is layered on top as reusable wrappers.

---

# 🧩 Core Architecture

The app is split into two main parts:

## 1. The Core App (Main.elm)

This contains all of the domain logic:

- `Model` — the state of the app
- `Msg` — all possible user actions
- `update : Msg -> Model -> Model` — how state changes
- `view : Model -> Html Msg` — how the UI is rendered

👉 This part has **no knowledge of time travel** and represents a standard Elm-style application using a pure update function.

---

## 2. The TimeTravel Module (TimeTravel.elm)

This module wraps an application and adds:

- History tracking
- Prev / Next navigation
- Debug UI
- Export / import timeline replay
- Runtime debugger visibility toggle

It introduces:

- `TimeTravel msg model` — a wrapper around your model that stores timeline and debugger state
- `Msg msg` — an internal message type that wraps your app messages (`AppMsg msg`) and adds debugger controls like `Prev`, `Next`, and import/export actions

---

# 🔁 How Time Travel Works

Each time the app updates:

1. The current model, message, and resulting model are saved as a "frame"
2. The new model becomes the "present"
3. Previous transitions are stored in `past`
4. Future transitions are stored in `future`

So the state becomes:

```
type alias Timeline msg model =
    { past : List (Frame msg model)
    , present : model
    , future : List (Frame msg model)
    }

type alias Frame msg model =
    { msg : msg
    , prev : model
    , next : model
    }
```

This allows you to:

- Go backward (Prev)
- Go forward (Next)
- Inspect every state transition

---

# 🧱 The Key Idea: A Wrapper (Decorator)

The TimeTravel module does not replace the application.

Instead, it wraps it.

```
App
↓
TimeTravel.withTimeTravel
↓
Enhanced App
```

This is similar to the Decorator pattern.

---

# ⚙️ The withTimeTravel Helper

The magic happens here:

```elm
withTimeTravel :
    AppConfig msg model
    -> Program Flags (TimeTravel msg model) (Msg msg)
```

Developers provide the application configuration:

```elm
{ init = initModel
, update = update
, view = view
, msgToDebug = msgToDebugInfo
, modelToString = modelToPrettyString
, decodeMsg = decodeMsg
}
```

It returns a fully working Elm program with time travel enabled.

The debugger visibility is controlled via flags passed from `index.html` at initialization time, allowing it to be enabled or disabled without changing Elm code.

```html
<div id="todo-app"></div>
<script src="main.js"></script>
<script>
  Elm.Main.init({
    node: document.getElementById("todo-app"),
    flags: { visibleByDefault: true }
  })
</script>
```

The debugger is initially controlled via flags passed from `index.html`, but can also be toggled at runtime using a checkbox in the debugger UI.

---

# 🧠 Why This Is Powerful

## ✅ The application stays clean

Main.elm only defines:

- state
- behavior
- UI

No debugging concerns leak into it.

---

## ✅ Time travel is reusable

This module can be used with any Elm app by providing:

- update
- view
- model serializer (for readable debugging output)

---

## ✅ It scales to more features

This pattern allows developers to add more "wrappers":

- withLogger
- withPersistence
- withAnalytics

Each one enhances the app without changing it.

---

## ⚠️ Current Limitation

The wrapped application must use a pure update function:

    update : Msg -> Model -> Model

Side effects (Cmd) are not currently supported by the TimeTravel module.

This keeps the debugger simple and predictable, but means features like
focus management or HTTP requests would require extending the wrapper.

---

# 🎮 User Interactions

- Checkbox → Toggle complete
- Click task text → Start editing a task
- Enter → Save changes
- Escape → Cancel editing
- Save / Cancel buttons provide explicit control
- Shift + Click → Toggle important

---

# 🧪 Debugger Features

- Step through history (Prev / Next)
- See each message (Msg) that caused a transition
- Inspect model changes with inline diffing and grouped previous-state lines
- View previous and next states
- Export timeline as JSON
- Import timeline and replay state transitions
- Press Enter inside the import textarea to trigger import
- Show inline import status feedback
- Toggle debugger visibility from the UI

---

# 🧠 Big Takeaway

This project demonstrates a key idea:

> Architecture can be composed just like functions.

An application is not tightly coupled to its runtime behavior.

Instead, behavior is layered on top in a clean, functional way.

---

# 🚀 Future Ideas

- Highlight changed fields more precisely
- Persist history or app state to localStorage
- Explore additional reusable wrappers around the core app

---

This approach keeps Elm code:

- Simple
- Predictable
- Extensible

And opens the door to building reusable architectural tools.

---

# 🔄 Export / Import & Replay

This project now supports exporting and importing timelines, turning the debugger into a deterministic replay engine.

## Export

Clicking "Export Timeline" generates JSON like:

```json
[
  { "index": 0, "label": "ToggledStatus", "id": "2" },
  { "index": 1, "label": "SetFilter All", "id": null }
]
```

This format is:

- Human-readable
- Easy to copy/paste
- Uses a `label` field (derived from `msgToDebug`) for decoding
- Sufficient to reconstruct application behavior

---

## Import

You can paste JSON back into the app and click "Import Timeline", or press Enter while focused in the import textarea.

The system will:

1. Decode JSON
2. Reconstruct real `Msg` values from labels
3. Replay them from the initial model

---

## Replay Engine

Instead of storing snapshots, the app rebuilds state using the update function:

```elm
List.foldl
    (\msg (prevModel, frames) ->
        let
            nextModel =
                update msg prevModel
        in
        ( nextModel
        , { msg = msg, prev = prevModel, next = nextModel } :: frames
        )
    )
    ( initModel, [] )
    messages
```

This ensures:

- Accurate reproduction of behavior
- No hidden state
- Deterministic debugging

---

### Decoding Strategy

Not all messages need to be fully decoded for replay to work.

- Some messages (like `NoOp`) may safely decode to `Nothing`
- Others must be decoded to preserve state transitions

Replay remains correct as long as all **state-changing messages** are included.

---

## Why This Matters

This turns the debugger into a powerful tool:

- Reproduce bugs from real sessions
- Share timelines between developers
- Verify behavior deterministically

This is conceptually similar to Redux DevTools, but implemented in pure Elm.