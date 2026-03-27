# 🧠 Elm Todo App with Time Travel Debugger

This project is a Todo application built in Elm with a custom, app-agnostic Time Travel Debugger.

It demonstrates a powerful architectural idea:

> An app can stay pure and simple, while additional behavior (like time travel) is layered on top.

---

# 🧩 Core Architecture

The app is split into two main parts:

## 1. The Core App (Main.elm)

This contains all of the domain logic:

- `Model` — the state of the app
- `Msg` — all possible user actions
- `update : Msg -> Model -> Model` — how state changes
- `view : Model -> Html Msg` — how the UI is rendered

👉 This part has **no knowledge of time travel** and represents a standard Elm application.

---

## 2. The TimeTravel Module (TimeTravel.elm)

This module wraps an application and adds:

- History tracking
- Prev / Next navigation
- Debug UI

It introduces:

- `TimeTravel msg model`
- `Msg msg` (internal messages like Toggle, Prev, Next, and wrapped app messages)

---

# 🔁 How Time Travel Works

Each time the app updates:

1. The current model is saved as a "frame"
2. The new model becomes the "present"
3. Older states are stored in `past`
4. Future states are stored in `future`

So the state becomes:

```
{ past : List (Frame msg model)
, present : model
, future : List (Frame msg model)
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
    -> Program () (TimeTravel msg model) (Msg msg)
```

Developers provide the application configuration:

```elm
{ init = initModel
, update = update
, view = view
, msgToDebug = todoMsgToDebug
, modelToString = modelToPrettyString
, visibleByDefault = False
}
```

It returns a fully working Elm program with time travel enabled.

The debugger can be enabled or disabled by default using the `visibleByDefault` flag.

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
- model serializer

---

## ✅ It scales to more features

This pattern allows developers to add more "wrappers":

- withLogger
- withPersistence
- withAnalytics

Each one enhances the app without changing it.

---

# 🎮 User Interactions

- Click → Toggle complete
- Double Click → Start editing a task

---

# 🧪 Debugger Features

- Step through history
- See each message (Msg)
- Inspect model changes
- View previous states

---

# 🧠 Big Takeaway

This project demonstrates a key idea:

> Architecture can be composed just like functions.

An application is not tightly coupled to its runtime behavior.

Instead, behavior is layered on top in a clean, functional way.

---

# 🚀 Future Ideas

- Add model diffs between states
- Highlight changed fields
- Persist history to localStorage
- Export/import timelines

---

This approach keeps Elm code:

- Simple
- Predictable
- Extensible

And opens the door to building reusable architectural tools.