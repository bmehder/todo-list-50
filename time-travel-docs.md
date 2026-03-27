# Time Travel in Elm: A Decorator-Based Architecture

## Overview

This module introduces a reusable, app-agnostic Time Travel Debugger for Elm applications. It wraps an existing Elm application—without modifying its core logic—and adds the ability to move backward and forward through state transitions.

The design is intentionally minimal, composable, and aligned with Elm’s functional architecture.

---

## Core Idea

Instead of rewriting your application, TimeTravel _wraps_ it.

Your app:

```elm
update : Msg -> Model -> Model
view : Model -> Html Msg
```

Becomes:

```elm
update : TimeTravel.Msg Msg -> TimeTravel Model -> TimeTravel Model
view : TimeTravel Model -> Html (TimeTravel.Msg Msg)
```

This transformation happens through `withTimeTravel`, which acts as a structural wrapper around your app.

---

## The Timeline Model

At the heart of the system is a `Timeline`:

```elm
type alias Timeline model =
    { past : List model
    , present : model
    , future : List model
    }
```

This represents time as a zipper-like structure:

```text
past ← present → future
```

- `past`: previously visited states
- `present`: the current model
- `future`: states you can "redo" into

---

## Message Wrapping

Your application messages are lifted into a larger type:

```elm
type Msg msg
    = AppMsg msg
    | Prev
    | Next
```

This allows TimeTravel to intercept messages and decide whether to:

- delegate to your app (`AppMsg msg`)
- manipulate history (`Prev`, `Next`)

---

## 🧾 Full Flow (End-to-End)

When a user interacts with your app, the flow looks like this:

```text
1. User triggers an event (click, input, etc.)
2. DOM produces a Msg
3. Msg is wrapped as AppMsg msg
4. TimeTravel.update receives the message
5. Your update function is called
6. A new model is produced
7. Previous model is pushed into `past`
8. `future` is cleared
9. New TimeTravel model is returned
```

In code, the critical step looks like:

```elm
AppMsg msg ->
    let
        newModel =
            updateModel msg app.timeline.present
    in
    { past = app.timeline.present :: app.timeline.past
    , present = newModel
    , future = []
    }
```

---

## Time Navigation

### Going Back (Prev)

```elm
case timeline.past of
    previous :: rest ->
        { past = rest
        , present = previous
        , future = timeline.present :: timeline.future
        }
```

### Going Forward (Next)

```elm
case timeline.future of
    next :: rest ->
        { past = timeline.present :: timeline.past
        , present = next
        , future = rest
        }
```

---

## Relationship to the Decorator Pattern

This design closely mirrors the **Decorator Pattern**.

### Traditional Decorator

In object-oriented systems, a decorator wraps an object to extend its behavior without modifying it.

### Functional Equivalent in Elm

TimeTravel acts as a _functional decorator_:

- It wraps your `update` function
- It wraps your `view`
- It augments your `Model`

Without changing your original code.

Conceptually:

```text
Your App
   ↓
TimeTravel Wrapper
   ↓
Enhanced App (with history)
```

Or more explicitly:

```text
Msg -> Model -> Model
becomes
TimeTravel.Msg Msg -> TimeTravel Model -> TimeTravel Model
```

This is structural composition, not inheritance or mutation.

---

## Separation of Concerns

Your application remains:

- unaware of time travel
- focused on business logic
- purely functional

TimeTravel handles:

- history tracking
- navigation
- debugging UI

---

## Config vs Runtime Control

The debugger can be controlled in two ways:

### 1. Elm Configuration

```elm
visibleByDefault = True
```

### 2. Runtime Flags (index.html)

```html
<div id="todo-app"></div>
<script src="main.js"></script>
<script>
	Elm.Main.init({
		node: document.getElementById('todo-app'),
		flags: { visibleByDefault: true },
	})
</script>
```

Flags override the default configuration.

---

## Key Properties of This Design

### 1. Pure

No side effects are introduced. All state transitions are explicit.

### 2. Immutable

Every model is preserved as a value.

### 3. Composable

TimeTravel can wrap any Elm application that follows the standard architecture.

### 4. App-Agnostic

No knowledge of your domain is required.

---

## Mental Model

Think of TimeTravel as:

```text
A recorder that observes your app
```

It does not control your logic—it records and replays it.

---

## Summary

TimeTravel works by:

- wrapping your app’s `Model`, `Msg`, and `update`
- storing every state transition
- allowing navigation through those states

All while preserving the simplicity and purity of Elm.

This approach demonstrates how powerful composition can be in functional programming, especially when combined with immutable data structures and explicit state transitions.
