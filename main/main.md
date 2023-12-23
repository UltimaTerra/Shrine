#### Welcome to Shrine! The Free and Open Source Game Engine


## Shrine Architecture

Shrine is built on top of Crystal and C Bindings for a 'full stack' game engine complete with a simple GUI, editor, debugger and ECS. It has its own build and runtime environment and comes with out of the box settings for support with Crystal, Ruby and SWift. It uses the LLVM toolchain, but any beginner and expert will have to install them yourself to make it work! 
Shrine tries to be as simple as possible, with generic functions that handle API calls, C Bindings, Front End and Back End synergy and with a practically extendable ECS.


## Shrine Rendering Core
Shrine uses SDL and OpenGL with a unique GUI abstraction called Foam, a DSL that is written in Swift and embedded as a VM runtime. This DSL essentially depends on an abstraction of Cairo, which creates vector images for windows, tabs, forms, buttons, widgets, etc.
## Shrine Language Core
Shrine is written mostly with Crystal and simple C bindings, but can bind to any language via an API called Interlink (check interfaces), and with EDAL (also check interfaces). EDAL or Easy Direct API Layer communicates C and Crystal so the abstraction layer is minimal and direct. Combined with Cairo Calls and Foam, EDAL creates a UI that is full featured and native to all platforms via C Calls embedded into the language. 
## Foam Architecture
Foam (check modules) has this general architecture.
Foam DSL -> C Platform Bindings + Cairo <- Vyu.XML and VM
Vyu is both the XML configuration file and the entire stack. It uses a simple stack machine that updates the abstract syntax tree
whenever reloaded to create an near instant GUI refresh that doesn't depend on a GUI framework/toolkit.
## Project Structure
main
>
modules
>
build
>
configuration
>
interfaces
>
systems
>