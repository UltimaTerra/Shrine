``` ada

core_c.adb

-- our core C FFI code with Ada, or the Ada+C Backend for 'GooStack'
--
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces.C; use Interfaces.C;


package body Core_C is


begin 

end Core_C; 


core_c.ads

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion; use Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation; use Ada.Unchecked_Deallocation;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C; use Interfaces.C;

package Core_C is
    -- Define a type for the character array
    type Char_Array is array (Natural range <>) of Interfaces.C.Char;
    type Char_Array_Ptr is access all Char_Array;

    -- Define a procedure to initialize the window
    procedure InitWindow(Width, Height: Int; Title: Char_Array)
        with Import => True,
             Convention => C,
             External_Name => "InitWindow";

    -- Define a function to check if the window should close
    function Window_Should_Close return C_Bool
        with Import => True,
             Convention => C,
             External_Name => "WindowShouldClose";

    -- Define procedures to begin and end drawing
    procedure BeginDrawing
        with Import => True,
             Convention => C,
             External_Name => "BeginDrawing";

    procedure EndDrawing
        with Import => True,
             Convention => C,
             External_Name => "EndDrawing";
end Core_C;

main_cc.ads



procedure Main_CC; 


main_cc.adb


with Core_C; use Core_C;

procedure Main_CC is 

begin 
       -- Initialize the window
       InitWindow(800, 600, "GooStack - Ada Raylib");
       -- Main loop
       while Window_Should_Close = C.False loop
            BeginDrawing;
            -- Drawing code goes here
            EndDrawing;
       end loop;
       -- Close the window
       EndDrawing;
end Main_CC; 


shrine.gpr

project Shrine is
   
   
   for Source_Dirs use ("Sources", "Sources/GSCore");
   for Object_Dir use "Artifacts";
   for Exec_Dir use "Binary";
   for Main use ("main_cc.adb");

   


end Shrine;


```