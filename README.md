

# Elm Todo App with Time‑Travel Debugger

This project is a **Todo application written in Elm**.  

In addition to the standard Todo features, it includes a **custom time‑travel debugger** that records every state transition and allows stepping backward and forward through application history.


The goal of the project is to demonstrate:

- Clean Elm Architecture
- Functional domain logic
- A simple but powerful state history mechanism
- A visual debugger

---

# Live Demo

You can try the application here:

https://elm-todo-list-50.vercel.app/

---

# Features

## Todo functionality

- Add todos
- Toggle status (`Active → Completed → Important`)
- Edit todos with **Shift‑click**
- Delete todos with confirmation
- Filter todos:
  - All
  - Active
  - Completed
  - Important

## Time‑travel debugger

The application records every message and resulting model transition.

You can:

- Step **backward** through state history
- Step **forward** again
- Inspect **exact model snapshots**
- View a **simple diff highlighting changed fields**

Debugger controls appear below the todo list and can be toggled with:

```
Show Time Travel Debugger
```

---

# Architecture Overview

The codebase follows a layered design inspired by **The Elm Architecture**.

```
MODEL
  │
  ▼
VIEW
  │
  ▼
MSG
  │
  ▼
UPDATE
  │
  ▼
MODEL
```

The debugger is implemented as a parallel UI layer that observes model transitions but does not affect domain logic.

```
          VIEW
         /    \
  Todo UI   Debugger UI
```

---

# Core Data Structures

## Model

The domain state for the todo application.

```
type alias Model =
    { todos : List Todo
    , draft : String
    , filter : Filter
    , editing : Editing
    , pendingDelete : Maybe Id
    }
```

## Timeline

Tracks history for time‑travel debugging.

```
type alias Timeline =
    { past : List Step
    , present : Model
    , future : List Step
    }
```

Each `Step` stores the transition:

```
type alias Step =
    { msg : Msg
    , prev : Model
    , next : Model
    }
```

## AppModel

Wraps the timeline and debugger UI state.

```
type TimelineVisibility
    = TimelineHidden
    | TimelineVisible


type alias AppModel =
    { timeline : Timeline
    , timelineVisibility : TimelineVisibility
    }
```

---

# Update Flow

Messages are processed in two stages:

1. **Todo update**

```
updateTodo : TodoMsg -> Model -> Model
```

Handles domain behavior only.
`TodoMsg` represents domain messages for the todo logic, while the outer `Msg` type wraps these messages for the application layer (timeline debugger and UI controls).

2. **Timeline wrapper**

```
update : Msg -> AppModel -> AppModel
```

Records transitions and manages time‑travel navigation.

---

# Call Graph (Simplified)

```
main
 └── view
     ├── viewNewTodoForm
     ├── viewFilterButtons
     ├── viewTodos
     │    └── viewTodo
     │         ├── viewTask
     │         │    ├── viewEditing
     │         │    └── viewTaskStatus
     │         └── viewDeleteButton
     ├── viewTodosCount
     ├── viewConfirmDialog
     ├── viewTimelineToggle
     └── timeline debugger
          ├── viewTimeline
          └── viewHistory
               ├── viewInitialStep
               │    └── viewModel
               │         └── viewTodoDebug
               └── viewStep
                    ├── msgToString
                    └── viewModelDiff
```

---

# Project Structure

Major sections of the source file are organized like this:

```
MODEL
INIT
UPDATE
UTILITY FUNCTIONS
DOMAIN HELPERS
VIEW
VIEW HELPERS
VIEW UTILITIES
TIME TRAVEL DEBUGGER
PROGRAM
```

This keeps domain logic, UI rendering, and debugging tools clearly separated.

---

# Why This Project Exists

The project explores how far you can go implementing **debugging and developer tooling directly in Elm**, without relying on external tools.

It demonstrates that Elm's:

- immutable state
- explicit message handling
- pure update functions

make building a debugger surprisingly straightforward.

---

# Running the App

Install Elm:

```
npm install -g elm
```

Then run:

```
elm reactor
```

Open the project in your browser:

```
http://localhost:8000
```

---

# Editing Todos

You can edit a task by:

```
Shift + Click on a todo
```

This toggles the editing mode for that item.

---

# License

MIT