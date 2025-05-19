-- Standard Library Ada Packages
with Ada.Containers.Hashed_Maps;  -- For Hashed_Maps if used elsewhere or by other packages 'with'ing this
with Ada.Containers.Hashed_Sets;  -- For Hashed_Sets if used elsewhere
with Ada.Containers.Vectors;      -- For Vectors, which we will use for Mode_Arena
with Ada.Interrupts;              -- If any other part of Mode_Core actually needs it (currently not directly used by Mode_Arena)
with Ada.Strings.Unbounded;       -- For Unbounded_String if used in Mode_Metadata or elsewhere
with Ada.Text_IO;                 -- For Text_IO if used in the body or by other packages

-- Your Packages
with sh7any_tables;               -- This package defines your 'UUID' type and 'Static_Arena'

-- 'use' clauses make names directly visible.
-- Be mindful that extensive use of 'use' in package specifications can sometimes lead
-- to name clashes if many packages define entities with common names.
-- For widely used types like 'Unbounded_String' or 'Put_Line', it's common.
-- For types from your own project like 'UUID' from 'sh7any_tables', it's also fine
-- as long as the scope is well understood.
use Ada.Text_IO; -- Assuming this might be used by clients or in the package body
use Ada.Strings.Unbounded;
use sh7any_tables; -- This makes 'UUID' and 'Static_Arena' directly visible

package Mode_Core is
   pragma Preelaborate (Mode_Core);

   --  Valid modes:
   -- Preview:  The default mode.  This shows the current scene of the game engine project
   -- Bucket :  The mode is used to create a bucket of virtual files. It is a essentially half a file manager and half a file browser.
   -- Develop : The mode is used to develop the game engine project. It is an embedded terminal and embeds syntax highlighting for Nim/Ada
   -- Debug : The mode is used to debug the current game project. It runs a reversible debugger and allows you to step through the code.
   -- Test : The mode is used to test the current game project. It runs a test suite and allows you to run tests on the code.
   -- Play : The mode is used to build + run the current game project. It runs a build system and allows you to run the game.
   type Mode is (Preview, Bucket, Develop, Debug, Test, Play);
   -- Per-mode UUID capacities: (Preview:1024, Bucket:8192, Develop:1024, Debug:4096, Test:4096, Play:16384)

   -- A submode is formally defined as a shadow mode that borrows the process of one or more primary modes
   type Sub_Mode is (Palette, Commander, Settings);

   type Mode_Array is array (Positive range <>) of Mode;

   -- Refactored Mode_Arena:
   -- We instantiate Ada.Containers.Vectors to create a specific package for vectors of UUIDs.
   -- This is the standard Ada idiom for creating a specialized container type.
   package Mode_UUID_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => UUID
   );

   -- Mode_Arena is now a subtype of the Vector type from our specialized instantiation.
   -- This means an object of type 'Mode_Arena' *is* a vector of (aliased) UUIDs.
   subtype Mode_Arena is Mode_UUID_Vectors.Vector;

   type Mode_Metadata is record
      Name                 : String (1 .. 128); -- Assuming a fixed max length for Name
      Description          : String (1 .. 256); -- Assuming a fixed max length for Description
      LocationLine         : Integer;
      LocationColumn       : Integer;
      Current_Mode         : Mode;     -- Renamed from 'Mode' to avoid conflict with the type 'Mode'
      Current_Sub_Mode     : Sub_Mode; -- Renamed from 'Sub_Mode'
      Is_Sub_Mode_Active   : Boolean;
   end record;


   --  | Mode API:
   --  | 1. There must be one and only one mode at a time.
   --  | 2. Modes are allocated with their own Mode_Arena (UUID vector), sized by memory allowance.
   --  |    The comment "share a pool" likely refers to the underlying heap memory from which
   --  |    the vectors' storage is ultimately allocated by the Ada runtime system.
   --  | 3. Modes are not allowed to be created in the main thread except for the preview (main) mode.
   --  | 4. New submodes may be allowed to borrow previous, 'main' modes, but they are just treated as one 'mode', and thus follow rules 1-3


   -- | Mode Functions
   -- Throughout, you will see Function_Name => Parameters(0..N) -> Return_Type as our document signature for functions/procedures.

   -- Creates/initializes a Mode_Arena (which is a vector of UUIDs) for a given mode.
   -- This will likely involve setting its capacity based on the 'Current_Mode'.
   procedure Create_Mode_Arena (Arena : out Mode_Arena; For_Mode : Mode);

   procedure Create_Mode (M : Mode; SM : Sub_Mode := Settings); 
   function Get_Mode (M : Mode) return Mode; 
   function Get_Sub_Mode_From_Modes (Modes : Mode_Array) return Sub_Mode; 
   function Return_Mode_Metadata (M : Mode; SM : Sub_Mode) return Mode_Metadata;
   function Run_Mode_Lifecycle (M : Mode; SM : Sub_Mode := Settings) return Boolean;
   function Kill_Mode (M : Mode; SM : Sub_Mode := Settings) return Boolean;


   -- Mode Specific Functions (IE: Preview_Mode_View, or Bucket_Mode_View)
   -- These would typically be declared in child packages or separate packages 'with'ing Mode_Core.


   type Shrine_Mode (M : Mode) is tagged record
      case M is -- Using the discriminant M directly
         when Preview =>
            Preview_Specific_Data : Integer; -- Example: Replace with actual data for Preview mode
            Mode_UUIDs            : Mode_Arena; -- Each variant can have its own arena/vector
         when Bucket =>
            Bucket_Max_Files : Natural;
            Mode_UUIDs       : Mode_Arena;
         when Develop =>
            Terminal_Font_Size : Positive;
            Mode_UUIDs         : Mode_Arena;
         when Debug =>
            Breakpoint_Count : Natural;
            Mode_UUIDs       : Mode_Arena;
         when Test =>
            Test_Suite_Name : Unbounded_String;
            Mode_UUIDs      : Mode_Arena;
         when Play =>
            Build_Target : Unbounded_String;
            Mode_UUIDs   : Mode_Arena;
      end case;

   end record;

end Mode_Core;