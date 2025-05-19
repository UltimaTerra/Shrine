-- main_cc.adb

with Ada.Text_IO;
with Ada.Exceptions;
with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces;
with System;

procedure Main_CC is
   use Interfaces.C;
   use Interfaces.C.Strings;
   use Interfaces;

   -----------------------------
   -- Raylib FFI Bindings 
   -----------------------------

   ---------------------------------------------------------------------------
   -- SDL3 FFI Bindings (Minimal Set for Testing)
   ---------------------------------------------------------------------------

   -- SDL Init flags (from SDL_init.h)
   SDL_INIT_VIDEO : constant Unsigned_32 := 16#00000020#;

   function SDL_Init (flags : Unsigned_32) return Int
      with Import        => True, Convention => C, External_Name => "SDL_Init";
   procedure SDL_Quit
      with Import        => True, Convention => C, External_Name => "SDL_Quit";

   -- Define opaque pointer type for SDL_Window*
   type SDL_Window_Ptr is new System.Address;
   -- Define a null constant for our SDL_Window_Ptr type
   Null_SDL_Window  : constant SDL_Window_Ptr := SDL_Window_Ptr(System.Null_Address); -- <<< CHANGED

   function SDL_CreateWindow (title  : Chars_Ptr;
                              w      : Int; h : Int;
                              flags  : Unsigned_32) return SDL_Window_Ptr
      with Import        => True, Convention => C, External_Name => "SDL_CreateWindow";
   procedure SDL_DestroyWindow (window : SDL_Window_Ptr)
      with Import        => True, Convention => C, External_Name => "SDL_DestroyWindow";
   procedure SDL_Delay (ms : Unsigned_32)
      with Import        => True, Convention => C, External_Name => "SDL_Delay";
   function SDL_GetError return Chars_Ptr
      with Import        => True, Convention => C, External_Name => "SDL_GetError";

--- 
-- Miniaudio Bindings
---


---
-- Nuklear Bindings 
---


---
---
---



   ---------------------------------------------------------------------------
   -- Main Application Logic (SDL3 Test)
   ---------------------------------------------------------------------------
   Window_Title_Ada : constant String    := "Ada SDL3 FFI Test";
   C_SDL_Title      : Chars_Ptr := New_String(Window_Title_Ada);
   Window           : SDL_Window_Ptr := Null_SDL_Window; -- <<< CHANGED

begin
   Ada.Text_IO.Put_Line("Attempting to initialize SDL_VIDEO...");
   if SDL_Init(SDL_INIT_VIDEO) < 0 then
      declare
         Error_Msg_C : constant Chars_Ptr := SDL_GetError;
         Error_Msg_Ada : constant String    := Value(Error_Msg_C);
      begin
         Ada.Text_IO.Put_Line("SDL_Init Error: " & Error_Msg_Ada);
      end;
      return;
   else
      Ada.Text_IO.Put_Line("SDL_VIDEO initialized successfully.");
   end if;

   Ada.Text_IO.Put_Line("Attempting to create SDL window...");
   Window := SDL_CreateWindow(C_SDL_Title, 1280, 720, 0);

   if Window = Null_SDL_Window then -- <<< CHANGED
      declare
         Error_Msg_C : constant Chars_Ptr := SDL_GetError;
         Error_Msg_Ada : constant String    := Value(Error_Msg_C);
      begin
         Ada.Text_IO.Put_Line("SDL_CreateWindow Error: " & Error_Msg_Ada);
      end;
   else
      Ada.Text_IO.Put_Line("SDL Window created successfully. Waiting for 3 seconds...");
      SDL_Delay(3000);
      Ada.Text_IO.Put_Line("Destroying SDL window...");
      SDL_DestroyWindow(Window);
      Window := Null_SDL_Window; -- Mark as destroyed <<< CHANGED
   end if;

   Ada.Text_IO.Put_Line("Quitting SDL...");
   SDL_Quit;
   Ada.Text_IO.Put_Line("SDL Quit successfully.");

   Free(C_SDL_Title);

exception
   when E : others =>
      Ada.Text_IO.Put_Line("Unhandled Exception in Main_CC: " & Ada.Exceptions.Exception_Message(E));
      if C_SDL_Title /= Null_Ptr then -- Comparison with Interfaces.C.Strings.Null_Ptr is fine
         Free(C_SDL_Title);
      end if;
      if Window /= Null_SDL_Window then -- <<< CHANGED
         Ada.Text_IO.Put_Line("Attempting to destroy window due to exception...");
         SDL_DestroyWindow(Window);
      end if;
      SDL_Quit;
      raise;
end Main_CC;